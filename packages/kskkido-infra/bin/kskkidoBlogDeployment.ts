#!/usr/bin/env node
import 'source-map-support/register';
import * as fs from 'fs/promises';
import * as path from 'path';
import * as yaml from 'yaml';
import { flow, pipe } from 'fp-ts/lib/function';
import * as IO from 'fp-ts/IO';
import * as Task from 'fp-ts/Task';
import * as ReaderTask from 'fp-ts/lib/ReaderTask';
import * as cdk from 'aws-cdk-lib';
import * as validation from 'src/lib/validation';
import { KskkidoBlogDeploymentStack } from 'src/stacks/kskkidoBlogDeploymentStack';
import * as types from './types';

type Context = {};

const main = async () => {
  const app = new cdk.App();
  try {
    await pipe(
      ReaderTask.Do,
      ReaderTask.apS('context', ReaderTask.ask<Context>()),
      ReaderTask.apS('input', inputFromEnv),
      ReaderTask.bind('config', ({ input }) => configFromInput(input)),
      ReaderTask.map((values) => {
        new KskkidoBlogDeploymentStack(
          app,
          [values.config.deploymentStackName].join('-'),
          {
            env: {
              account: values.config.awsAccount,
              region: values.config.awsRegion,
            },
            context: {
              stage: values.config.stage,
              serviceName: values.config.serviceName,
              deployRoleName: values.config.deployRoleName,
              deployUserNames: [
                values.config.deployUserName,
                values.config.deployCiUserName,
              ],
            },
          }
        );
      })
    )({})();
    app.synth();
    process.exit(0);
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
          Task.chain(Task.fromIOK(flow(types.Config.decode, validation.toIO)))
        )
      )
    )
  );

main();
