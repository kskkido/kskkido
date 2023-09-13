declare module '@pagefind/default-ui' {
  type PageFindUIConfig = {
    element: string;
    resetStyles: boolean;
    showImages: boolean;
  };

  class PagefindUI {
    constructor(config: PageFindUIConfig);
  }
}

interface Window {
  lazyLoadOptions?: Record<string, unknown>;
}
