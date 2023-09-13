import * as t from 'io-ts';
import { pipe } from 'fp-ts/function';
import * as Either from 'fp-ts/Either';

const LazyLoadInitializedEvent = t.type({
  detail: t.type({
    instance: t.type({
      update: t.Function,
    }),
  }),
});

const main = (window: Window) => {
  window.lazyLoadOptions = {
    // Your custom settings go here
  };
  window.addEventListener(
    'LazyLoad::Initialized',
    (event: unknown) => {
      pipe(
        event,
        LazyLoadInitializedEvent.decode,
        Either.fold(
          () => {
            console.log('yaaa?');
          },
          (event) => {
            console.log('noooo?');
            event.detail.instance.update();
          }
        )
      );
    },
    false
  );
};

main(window);
