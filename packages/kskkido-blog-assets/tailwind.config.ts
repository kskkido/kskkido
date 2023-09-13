import type { Config } from 'tailwindcss';

export default {
  darkMode: 'class',
  content: [
    './src/**/*.{astro,html,js,jsx,md,svelte,ts,tsx,vue}',
    '../kskkido-blog/.build/**/*.html',
  ],
  theme: {
    fontSize: {
      xs: '0.7rem',
      sm: '0.8rem',
      base: '0.925rem',
      lg: '1.125rem',
      xl: '1.25rem',
      '2xl': '1.563rem',
      '3xl': '1.953rem',
      '4xl': '2.441rem',
      '5xl': '3rem',
    },
  },
} as Config;
