import * as pageSearch from 'src/elements/pageSearch';

const main = (window: Window) => {
  window.document.addEventListener('DOMContentLoaded', () => {
    window.customElements.define('page-search', pageSearch.PageSearch);
  });
};

main(window);
