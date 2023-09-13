import { pipe } from 'fp-ts/lib/function';
import * as Option from 'fp-ts/lib/Option';
import * as ReaderIO from 'fp-ts/lib/ReaderIO';
import * as Either from 'fp-ts/Either';
import * as IOEither from 'fp-ts/lib/IOEither';
import { PagefindUI } from '@pagefind/default-ui';

type Context = {
  window: Window;
  parentElement: HTMLElement;
  searchElementId: string;
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
          'dialog',
          pipe(
            context.parentElement.querySelector('dialog'),
            Option.fromNullable,
            IOEither.fromOption(() => new Error('dialog not found'))
          )
        ),
        IOEither.apS(
          'dialogFrame',
          pipe(
            context.parentElement.querySelector('[data-type=dialog-frame]'),
            Option.fromNullable,
            IOEither.fromOption(() => new Error('dialog not found'))
          )
        ),
        IOEither.apS(
          'showModalButton',
          pipe(
            context.parentElement.querySelector('button[data-type=show-modal]'),
            Option.fromNullable,
            Option.chain((element) =>
              element instanceof HTMLButtonElement
                ? Option.of(element)
                : Option.none
            ),
            IOEither.fromOption(() => new Error('dialog not found'))
          )
        ),
        IOEither.apS(
          'hideModalButton',
          pipe(
            context.parentElement.querySelector('button[data-type=hide-modal]'),
            Option.fromNullable,
            Option.chain((element) =>
              element instanceof HTMLButtonElement
                ? Option.of(element)
                : Option.none
            ),
            IOEither.fromOption(() => new Error('dialog not found'))
          )
        ),
        IOEither.chain((elements) =>
          IOEither.fromIO(() => {
            const showModal = (event?: MouseEvent) => {
              elements.dialog.showModal();
              context.parentElement.querySelector('input')?.focus();
              event?.stopPropagation();
              context.window.addEventListener('click', onWindowClick);
            };
            const hideModal = () => {
              elements.dialog.close();
              context.window.removeEventListener('click', onWindowClick);
            };
            const onWindowClick = (event: MouseEvent) => {
              if (
                document.body.contains(event.target as Node) &&
                !elements.dialogFrame.contains(event.target as Node)
              ) {
                hideModal();
              }
            };
            const onWindowKeyDown = (event: KeyboardEvent) => {
              if (event.key === '/' && !elements.dialog.open) {
                showModal();
                event.preventDefault();
              }
            };
            const onDomContentLoaded = () => {
              new PagefindUI({
                element: `#${context.searchElementId}`,
                resetStyles: false,
                showImages: false,
              });
            };

            elements.showModalButton.addEventListener('click', showModal);
            elements.hideModalButton.addEventListener('click', hideModal);
            context.window.addEventListener(
              'DOMContentLoaded',
              onDomContentLoaded
            );
            context.window.addEventListener('keydown', onWindowKeyDown);

            return () => {
              elements.showModalButton.removeEventListener('click', showModal);
              elements.hideModalButton.removeEventListener('click', hideModal);
              context.window.removeEventListener(
                'DOMContentLoaded',
                onDomContentLoaded
              );
              context.window.removeEventListener('keydown', onWindowKeyDown);
            };
          })
        ),
        ReaderIO.fromIO
      )
  )
);
