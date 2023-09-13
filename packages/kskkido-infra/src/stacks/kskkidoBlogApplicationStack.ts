import * as cdk from 'aws-cdk-lib';
import * as iam from 'aws-cdk-lib/aws-iam';
import * as s3 from 'aws-cdk-lib/aws-s3';
import * as s3Deployment from 'aws-cdk-lib/aws-s3-deployment';
import * as lambda from 'aws-cdk-lib/aws-lambda';
import * as cloudfront from 'aws-cdk-lib/aws-cloudfront';
import * as cloudfrontOrigins from 'aws-cdk-lib/aws-cloudfront-origins';
import * as acm from 'aws-cdk-lib/aws-certificatemanager';
import { Construct } from 'constructs';

type Props = cdk.StackProps & {
  context: {
    stage: string;
    blogS3BucketName: string;
    blogSourceFilePath: string;
    blogTargetFilePath: string;
    blogMetadata: {
      artifacts: ReadonlyArray<{
        filePath: string;
        route: string;
      }>;
    };
    domainName?: string;
    certificateArn?: string;
  };
};

export class KskkidoBlogApplicationStack extends cdk.Stack {
  readonly bucket: s3.IBucket;
  readonly cdn: cloudfront.IDistribution;

  constructor(scope: Construct, id: string, props: Props) {
    super(scope, id, props);

    this.bucket = new s3.Bucket(this, 'BlogBucket', {
      bucketName: [
        props.context.blogS3BucketName,
        props.context.stage,
        this.account,
        this.region,
      ].join('-'),
    });
    const cdnOai = new cloudfront.OriginAccessIdentity(this, `BlogCdnOai`, {});
    this.bucket.addToResourcePolicy(
      new iam.PolicyStatement({
        actions: ['s3:GetObject'],
        effect: iam.Effect.ALLOW,
        principals: [
          new iam.CanonicalUserPrincipal(
            cdnOai.cloudFrontOriginAccessIdentityS3CanonicalUserId
          ),
        ],
        resources: [`${this.bucket.bucketArn}/*`],
      })
    );
    const routeMap = props.context.blogMetadata.artifacts.reduce(
      (acc, artifact) => ({ ...acc, [artifact.route]: artifact.filePath }),
      {} as Record<string, string>
    );
    const router = new cloudfront.experimental.EdgeFunction(
      this,
      'BlogRouter',
      {
        runtime: lambda.Runtime.NODEJS_18_X,
        handler: 'index.handler',
        code: lambda.Code.fromInline(`
          const routeMap = ${JSON.stringify(routeMap)}
          exports.handler = (event, context, callback) => {
            const request = event.Records[0]?.cf?.request
            if (!request) {
              throw new Error('missing request');
            } else {
              console.log('Received request')
              console.log(request)
              const filePath = routeMap[request.uri]
              request.uri = filePath ?? request.uri;
              callback(null, request);
            }
          }
      `),
      }
    );
    this.cdn = new cloudfront.Distribution(this, 'BlogCdn', {
      enabled: true,
      defaultRootObject: '',
      priceClass: cloudfront.PriceClass.PRICE_CLASS_200,
      domainNames: props.context.domainName
        ? [props.context.domainName]
        : undefined,
      certificate: props.context.certificateArn
        ? acm.Certificate.fromCertificateArn(
            this,
            'WebCdnCertificate',
            props.context.certificateArn
          )
        : undefined,
      defaultBehavior: {
        origin: new cloudfrontOrigins.S3Origin(this.bucket, {
          originAccessIdentity: cdnOai,
        }),
        allowedMethods: cloudfront.AllowedMethods.ALLOW_GET_HEAD,
        cachedMethods: cloudfront.CachedMethods.CACHE_GET_HEAD,
        cachePolicy: cloudfront.CachePolicy.CACHING_OPTIMIZED,
        viewerProtocolPolicy: cloudfront.ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
        edgeLambdas: [
          {
            functionVersion: router.currentVersion,
            eventType: cloudfront.LambdaEdgeEventType.VIEWER_REQUEST,
          },
        ],
      },
    });
    new s3Deployment.BucketDeployment(this, `BlogBucketDeployment`, {
      distribution: this.cdn,
      distributionPaths: ['/*'],
      destinationKeyPrefix: props.context.blogTargetFilePath,
      destinationBucket: this.bucket,
      sources: [s3Deployment.Source.asset(props.context.blogSourceFilePath)],
    });
    new cdk.CfnOutput(this, 'BlogdnUrl', {
      value: `https://${this.cdn.distributionDomainName}`,
    });
    new cdk.CfnOutput(this, 'BlogdnDomainName', {
      value: this.cdn.distributionDomainName,
    });
  }
}
