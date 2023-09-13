import * as t from 'io-ts';
import { pipe } from 'fp-ts/lib/function';
import * as IO from 'fp-ts/lib/IO';
import * as IOEither from 'fp-ts/lib/IOEither';
import * as appContext from 'src/data/appContext';
import * as article from 'src/core/article';

const Config = t.type({
  articleElementQuery: t.string,
  tableOfContentsElementQuery: t.string,
});

export class ArticleContainer extends HTMLElement {
  constructor() {
    super();
    pipe(
      appContext.fromEnv,
      IO.chain((context) =>
        pipe(
          Config.decode({
            articleElementQuery: this.dataset.articleElementQuery,
            tableOfContentsElementQuery:
              this.dataset.tableOfContentsElementQuery,
          }),
          IOEither.fromEither,
          IOEither.mapLeft(() => new Error('invalid config')),
          IOEither.chain((config) =>
            article.main({
              window,
              parentElement: this,
              logger: context.logger,
              ...config,
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
