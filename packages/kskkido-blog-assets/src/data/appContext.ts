import { pipe } from 'fp-ts/lib/function';
import * as IO from 'fp-ts/lib/IO';
import * as types from 'src/types';
import * as appConfig from './appConfig';

export const fromEnv: IO.IO<types.AppContext> = pipe(
  appConfig.fromEnv,
  IO.map((config) => ({
    logger: appConfig.toLogger(config),
  }))
);
