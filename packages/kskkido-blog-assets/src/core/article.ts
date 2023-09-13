import * as rxjs from 'rxjs';
import { pipe } from 'fp-ts/lib/function';
import * as Option from 'fp-ts/lib/Option';
import * as ReaderIO from 'fp-ts/lib/ReaderIO';
import * as Either from 'fp-ts/Either';
import * as IOEither from 'fp-ts/lib/IOEither';
import * as ReadonlyArray from 'fp-ts/lib/ReadonlyArray';
import * as subscription from 'src/data/subscription';
import * as types from 'src/types';

type Context = {
  window: Window;
  parentElement: HTMLElement;
  articleElementQuery: string;
  tableOfContentsElementQuery: string;
  logger: types.Logger<types.LogEntry<string | Error>>;
};

type Action = EnterAction | LeaveAction;

type EnterAction = {
  type: 'enter';
  id: string;
};

type LeaveAction = {
  type: 'leave';
  id: string;
};

export const main: ReaderIO.ReaderIO<
  Context,
  Either.Either<Error, () => void>
> = pipe(
  ReaderIO.ask<Context>(),
  ReaderIO.chain(
    (context): ReaderIO.ReaderIO<Context, Either.Either<Error, () => void>> =>
      pipe(
        IOEither.Do,
        IOEither.apS(
          'article',
          pipe(
            context.parentElement.querySelector(context.articleElementQuery),
            Option.fromNullable,
            Option.chain((element) =>
              element instanceof HTMLElement ? Option.of(element) : Option.none
            ),
            IOEither.fromOption(() => new Error('article not found'))
          )
        ),
        IOEither.apS(
          'tableOfContents',
          pipe(
            context.parentElement.querySelector(
              context.tableOfContentsElementQuery
            ),
            Option.fromNullable,
            Option.chain((element) =>
              element instanceof HTMLElement ? Option.of(element) : Option.none
            ),
            IOEither.fromOption(() => new Error('table of contents not found'))
          )
        ),
        IOEither.bind('articleHeaders', ({ article }) =>
          pipe(
            Array.from(article.querySelectorAll('h1, h2, h3, h4, h5, h6')),
            Option.traverseArray((element) =>
              element instanceof HTMLElement ? Option.of(element) : Option.none
            ),
            IOEither.fromOption(() => new Error('article headers not found'))
          )
        ),
        IOEither.bind('articleHeaderLinks', ({ article }) =>
          pipe(
            Array.from(article.querySelectorAll(`a[data-type="anchor"]`)),
            Option.traverseArray((element) =>
              element instanceof HTMLAnchorElement
                ? Option.of(element)
                : Option.none
            ),
            IOEither.fromOption(
              () => new Error('article header links not found')
            ),
            IOEither.chain(
              IOEither.traverseArray((element) =>
                IOEither.fromIO(() => {
                  const icon = context.window.document.createElement('i');
                  icon.setAttribute('class', 'link fa-solid fa-link');
                  element.parentNode?.appendChild(icon);
                  return [element, icon] as const;
                })
              )
            )
          )
        ),
        IOEither.chain((elements) =>
          IOEither.fromIO(() => {
            const $intersection = new rxjs.Subject<IntersectionObserverEntry>();
            const intersectionObserver = new IntersectionObserver(
              (entries) => {
                entries.forEach((entry) => {
                  $intersection.next(entry);
                });
              },
              {
                root: context.window.document,
                threshold: 0,
              }
            );
            elements.articleHeaders.forEach((element) => {
              intersectionObserver.observe(element);
            });
            const $action: rxjs.Observable<Action> = $intersection.pipe(
              rxjs.switchMap((entry) => {
                context.logger({
                  message: JSON.stringify(entry),
                  context: {
                    entry,
                  },
                  level: 'info',
                });
                if (entry.isIntersecting) {
                  return rxjs.of({
                    type: 'enter' as const,
                    id: entry.target.id,
                  });
                } else {
                  return rxjs.of({
                    type: 'leave' as const,
                    id: entry.target.id,
                  });
                }
              }),
              rxjs.share()
            );

            return subscription.merge([
              rxjs
                .merge(
                  ...elements.articleHeaderLinks.map(([link, icon]) =>
                    rxjs.fromEvent(icon, 'click').pipe(
                      rxjs.tap(() => {
                        context.window.navigator.clipboard.writeText(link.href);
                      })
                    )
                  )
                )
                .subscribe(() => {
                  context.logger({
                    message: 'clicked link',
                    level: 'info',
                  });
                }).unsubscribe,
              rxjs
                .merge(
                  $action.pipe(
                    rxjs.filter(
                      (action): action is EnterAction => action.type === 'enter'
                    ),
                    rxjs.tap(({ id }) =>
                      pipe(
                        Option.Do,
                        Option.apS(
                          'articleHeader',
                          pipe(
                            elements.articleHeaders,
                            ReadonlyArray.findFirst(
                              (header) => header.id === id
                            )
                          )
                        ),
                        Option.apS(
                          'articleHeadersRest',
                          pipe(
                            elements.articleHeaders,
                            ReadonlyArray.filter((header) => header.id !== id),
                            Option.of
                          )
                        ),
                        Option.apS(
                          'tableOfContentsHeaders',
                          pipe(
                            Array.from(
                              elements.tableOfContents.querySelectorAll(
                                `[data-target="${id}"]`
                              )
                            ),
                            Option.traverseArray((element) =>
                              element instanceof HTMLElement
                                ? Option.of(element)
                                : Option.none
                            )
                          )
                        ),
                        Option.apS(
                          'tableOfContentsHeadersRest',
                          pipe(
                            Array.from(
                              elements.tableOfContents.querySelectorAll(
                                `[data-target]:not([data-target="${id}"])`
                              )
                            ),
                            Option.traverseArray((element) =>
                              element instanceof HTMLElement
                                ? Option.of(element)
                                : Option.none
                            )
                          )
                        ),
                        Option.map((elements) => {
                          elements.articleHeader.classList.remove('inactive');
                          elements.articleHeader.classList.add('active');
                          elements.tableOfContentsHeaders.forEach((element) => {
                            element.classList.remove('inactive');
                            element.classList.add('active');
                          });
                        })
                      )
                    )
                  )
                )
                .subscribe((action) => {
                  context.logger({
                    message: JSON.stringify(action),
                    level: 'info',
                  });
                }).unsubscribe,
              $action
                .pipe(
                  rxjs.filter(
                    (action): action is LeaveAction => action.type === 'leave'
                  ),
                  rxjs.tap(({ id }) =>
                    pipe(
                      Option.Do,
                      Option.apS(
                        'articleHeader',
                        pipe(
                          elements.articleHeaders,
                          ReadonlyArray.findFirst((header) => header.id === id)
                        )
                      ),
                      Option.apS(
                        'tableOfContentsHeaders',
                        pipe(
                          Array.from(
                            elements.tableOfContents.querySelectorAll(
                              `[data-target="${id}"]`
                            )
                          ),
                          Option.traverseArray((element) =>
                            element instanceof HTMLElement
                              ? Option.of(element)
                              : Option.none
                          )
                        )
                      ),
                      Option.map((elements) => {
                        elements.articleHeader.classList.remove('active');
                        elements.articleHeader.classList.add('inactive');
                        elements.tableOfContentsHeaders.forEach((element) => {
                          element.classList.remove('active');
                          element.classList.add('inactive');
                        });
                      })
                    )
                  )
                )
                .subscribe((action) => {
                  context.logger({
                    message: JSON.stringify(action),
                    level: 'info',
                  });
                }).unsubscribe,

              () => intersectionObserver.disconnect(),
            ]);
          })
        ),
        ReaderIO.fromIO
      )
  )
);
