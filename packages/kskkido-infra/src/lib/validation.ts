import { Validation, ValidationError } from 'io-ts';
import { pipe } from 'fp-ts/function';
import * as ReadonlyArray from 'fp-ts/ReadonlyArray';
import * as ReadonlyNonEmptyArray from 'fp-ts/ReadonlyNonEmptyArray';
import * as Record from 'fp-ts/Record';
import * as IO from 'fp-ts/IO';
import * as Either from 'fp-ts/Either';

export const toIO =
  <A>(vx: Validation<A>): IO.IO<A> =>
  () =>
    pipe(
      vx,
      Either.fold(
        (errors) => {
          throw errorsToError(errors);
        },
        (value) => value
      )
    );

// TODO: Support nested properties
export const errorsToSchema = (
  errors: ReadonlyArray<ValidationError>
): Record<string, ReadonlyArray<string>> => {
  return pipe(
    errors,
    ReadonlyArray.chain((error) => error.context),
    ReadonlyNonEmptyArray.groupBy((entry) => entry.key),
    Record.map(ReadonlyNonEmptyArray.map((entry) => entry.type.name))
  );
};

export const errorsToPaths = (
  errors: ReadonlyArray<ValidationError>
): ReadonlyArray<string> => {
  return pipe(
    errors,
    ReadonlyArray.map((error) =>
      pipe(
        error.context,
        ReadonlyArray.filter((c) => c.key.length > 0),
        ReadonlyArray.map((c) => `${c.key}: ${c.type.name}`)
      ).join('/')
    )
  );
};

export const errorsToError = (
  errors: ReadonlyArray<ValidationError>
): Error => {
  return new Error(`Invalid values at: ${errorsToPaths(errors).join(',')}`);
};
