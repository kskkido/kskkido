import { constant, pipe } from 'fp-ts/lib/function';
import * as IO from 'fp-ts/lib/IO';
import * as Either from 'fp-ts/lib/Either';
import * as types from 'src/types';
import * as logger from './logger';

export const fromEnv: IO.IO<types.AppConfig> = pipe(
  types.AppConfig.decode({
    enableLogger: process.env.ENABLE_LOGGER === 'true',
  }),
  Either.fold(() => {
    throw new Error('invalid config');
  }, constant)
);

export const toLogger = (
  config: types.AppConfig
): types.Logger<types.LogEntry<string | Error>> => {
  return logger.pipe(logger.addContext({ config }))(
    logger.filter(
      (x): x is typeof x => config.enableLogger,
      logger.toConsoleLogger()
    )
  );
};
