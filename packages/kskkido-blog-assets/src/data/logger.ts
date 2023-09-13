import * as types from 'src/types';

export const pipe =
  <A, B>(fn: (x: A) => B) =>
  (logger: types.Logger<B>): types.Logger<A> => {
    return (x) => logger(fn(x));
  };

export const merge = <A>(
  loggers: ReadonlyArray<types.Logger<A>>
): types.Logger<A> => {
  return (x) => loggers.forEach((fn) => fn(x));
};

export const filter = <A, B extends A>(
  pred: (x: A) => x is B,
  logger: types.Logger<B>
): types.Logger<A> => {
  return (x) => (pred(x) ? logger(x) : null);
};

export const toConsoleLogger = <A>() =>
  merge<types.LogEntry<A>>([
    filter(
      (x): x is typeof x => x.level === 'log',
      (x) => console.log(x)
    ),
    filter(
      (x): x is typeof x => x.level === 'info',
      (x) => console.info(x)
    ),
    filter(
      (x): x is typeof x => x.level === 'warning',
      (x) => console.warn(x)
    ),
    filter(
      (x): x is typeof x => x.level === 'error',
      (x) => console.error(x)
    ),
  ]);

export const addTags =
  (tags: types.LogTags) =>
  <A>(entry: types.LogEntry<A>): types.LogEntry<A> => {
    return {
      ...entry,
      tags: {
        ...tags,
        ...entry.tags,
      },
    };
  };

export const addContext =
  (context: types.LogContext) =>
  <A>(entry: types.LogEntry<A>): types.LogEntry<A> => {
    return {
      ...entry,
      context: {
        ...context,
        ...entry.context,
      },
    };
  };
