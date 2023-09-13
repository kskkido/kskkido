import { Subscription } from 'src/types';

// patch work to make sure scripts run on load
export const onWindowLoad =
  <A>(fn: (x: A) => Subscription) =>
  (w: Window): typeof fn => {
    return (x: A) => {
      const subscriptions: Subscription[] = [];
      const listener = () => subscriptions.push(fn(x));
      w.addEventListener('load', listener);
      subscriptions.push(() => w.removeEventListener('load', listener));
      return merge(subscriptions);
    };
  };

export const onDomContentLoaded =
  <A>(fn: (x: A) => Subscription) =>
  (w: Window): typeof fn => {
    return (x: A) => {
      if (w.document.readyState !== 'loading') {
        return fn(x);
      } else {
        const subscriptions: Subscription[] = [];
        const listener = () => subscriptions.push(fn(x));
        w.document.addEventListener('DOMContentLoaded', listener, {
          once: true,
        });
        subscriptions.push(() =>
          w.document.removeEventListener('DOMContentLoaded', listener)
        );
        return merge(subscriptions);
      }
    };
  };

export const merge = (xs: Subscription[]): Subscription => {
  return xs.reduce(concat, empty);
};

export const concat = (x: Subscription, y: Subscription): Subscription => {
  return () => {
    x();
    y();
  };
};

export const empty: Subscription = () => null;
