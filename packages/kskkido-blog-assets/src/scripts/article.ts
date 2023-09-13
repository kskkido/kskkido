import * as articleContainer from 'src/elements/articleContainer';

const main = (window: Window) => {
  window.document.addEventListener('DOMContentLoaded', () => {
    window.customElements.define(
      'article-container',
      articleContainer.ArticleContainer
    );
  });
};

main(window);
