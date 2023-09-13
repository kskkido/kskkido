import * as t from 'io-ts';

export const AppConfig = t.type({
  enableLogger: t.boolean,
});

export type AppConfig = t.TypeOf<typeof AppConfig>;

export type AppContext = {
  logger: Logger<LogEntry<string | Error>>;
};

export type Logger<A> = (log: A) => void;

// Whatever we're interested in tracking
export type LogEntry<A> = {
  message: A;
  level: LogLevel;
  tags?: LogTags;
  context?: LogContext;
};

export const LogLevel = t.union([
  t.literal('log'),
  t.literal('info'),
  t.literal('warning'),
  t.literal('error'),
]);

export type LogLevel = t.TypeOf<typeof LogLevel>;

export enum LogLevelOrd {
  'log',
  'info',
  'warning',
  'error',
}

export type LogTags = Record<string, string>;

export type LogContext = Record<string, unknown>;

export type Subscription = () => void;
