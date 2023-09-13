import * as t from 'io-ts';
import { pipe } from 'fp-ts/lib/function';
import * as IO from 'fp-ts/lib/IO';
import * as IOEither from 'fp-ts/lib/IOEither';
import * as appContext from 'src/data/appContext';
import * as pageSearch from 'src/core/pageSearch';

const Config = t.type({
  searchElementId: t.string,
});

export class PageSearch extends HTMLElement {
  constructor() {
    super();
    pipe(
      appContext.fromEnv,
      IO.chain((context) =>
        pipe(
          Config.decode({
            searchElementId: this.dataset.elementId,
          }),
          IOEither.fromEither,
          IOEither.mapLeft(() => new Error('invalid config')),
          IOEither.chain((config) =>
            pageSearch.main({
              ...config,
              parentElement: this,
              window,
            })
          ),
          IOEither.fold(
            (error) => () => {
              context.logger({
                message: error,
                level: 'error',
              });
            },
            (unsubscribe) => () => {
              context.logger({
                message: 'success',
                level: 'info',
              });
              return unsubscribe;
            }
          )
        )
      )
    )();
  }
}
