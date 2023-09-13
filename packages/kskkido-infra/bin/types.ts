import * as t from 'io-ts';

export const Input = t.type({
  rootFilePath: t.string,
  configFilePath: t.string,
});

export type Input = t.TypeOf<typeof Input>;

export const Config = t.intersection([
  t.type({
    awsRegion: t.string,
    awsAccount: t.string,
    stage: t.string,
    serviceName: t.string,
    applicationStackName: t.string,
    blogServiceName: t.string,
    blogS3BucketName: t.string,
    blogSourceFilePath: t.string,
    blogTargetFilePath: t.string,
    blogMetadataFileName: t.string,
    deploymentStackName: t.string,
    deployRoleName: t.string,
    deployUserName: t.string,
    deployCiUserName: t.string,
  }),
  t.partial({
    domainName: t.string,
    certificateArn: t.string,
  }),
]);

export type Config = t.TypeOf<typeof Config>;
