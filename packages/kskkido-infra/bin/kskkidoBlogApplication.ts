#!/usr/bin/env node
import 'source-map-support/register';
import * as fs from 'fs/promises';
import * as path from 'path';
import * as yaml from 'yaml';
import * as t from 'io-ts';
import { flow, pipe } from 'fp-ts/lib/function';
import * as IO from 'fp-ts/IO';
import * as Task from 'fp-ts/Task';
import * as ReaderTask from 'fp-ts/lib/ReaderTask';
import * as cdk from 'aws-cdk-lib';
import * as validation from 'src/lib/validation';
import { KskkidoBlogApplicationStack } from 'src/stacks/kskkidoBlogApplicationStack';
import * as types from './types';

type Context = {};

const BlogMetadata = t.type({
  artifacts: t.array(
    t.type({
      filePath: t.string,
      route: t.string,
    })
  ),
});

type BlogMetadata = t.TypeOf<typeof BlogMetadata>;

const main = async () => {
  const app = new cdk.App();
  try {
    await pipe(
      ReaderTask.Do,
      ReaderTask.apS('context', ReaderTask.ask<Context>()),
      ReaderTask.apS('input', inputFromEnv),
      ReaderTask.bind('config', ({ input }) => configFromInput(input)),
      ReaderTask.bind('blogMetadata', ({ config }) =>
        blogMetadataFromConfig(config)
      ),
      ReaderTask.map((values) => {
        new KskkidoBlogApplicationStack(
          app,
          [values.config.applicationStackName].join('-'),
          {
            env: {
              account: values.config.awsAccount,
              region: values.config.awsRegion,
            },
            context: {
              stage: values.config.stage,
              blogS3BucketName: values.config.blogS3BucketName,
              blogSourceFilePath: values.config.blogSourceFilePath,
              blogTargetFilePath: values.config.blogTargetFilePath,
              blogMetadata: values.blogMetadata,
              domainName: values.config.domainName,
              certificateArn: values.config.certificateArn,
            },
          }
        );
      })
    )({})();
    app.synth();
  } catch (error) {
    console.error(error);
    process.exit(1);
  }
};

const inputFromEnv = pipe(
  ReaderTask.ask<Context>(),
  ReaderTask.chain((context) =>
    ReaderTask.fromIO(
      pipe(
        () =>
          types.Input.decode({
            rootFilePath: process.env.ROOT_FILE_PATH,
            configFilePath: process.env.INFRA_CONFIG_FILE_PATH,
          }),
        IO.chain(validation.toIO),
        IO.map((input) => ({
          ...input,
          configFilePath: path.join(input.rootFilePath, input.configFilePath),
        }))
      )
    )
  )
);

const configFromInput = (input: types.Input) =>
  pipe(
    ReaderTask.ask<Context>(),
    ReaderTask.chain(() =>
      ReaderTask.fromTask(
        pipe(
          async () =>
            yaml.parse(await fs.readFile(input.configFilePath, 'utf8')),
          Task.chain(Task.fromIOK(flow(types.Config.decode, validation.toIO))),
          Task.map((config) => ({
            ...config,
            blogSourceFilePath: path.join(
              input.rootFilePath,
              config.blogSourceFilePath
            ),
          }))
        )
      )
    )
  );

const blogMetadataFromConfig = (config: types.Config) =>
  pipe(
    ReaderTask.ask<Context>(),
    ReaderTask.chain((context) =>
      ReaderTask.fromTask(
        pipe(
          async () =>
            JSON.parse(
              await fs.readFile(
                path.join(
                  config.blogSourceFilePath,
                  config.blogMetadataFileName
                ),
                'utf8'
              )
            ),
          Task.chain(Task.fromIOK(flow(BlogMetadata.decode, validation.toIO)))
        )
      )
    )
  );

main();
