import * as fs from 'fs';
import { build } from 'esbuild';
import fg from 'fast-glob';

export const main = async () => {
  const config = {
    entryPoints: await fg('./src/scripts/**/*.ts'),
    outdir: './public/scripts',
    tsconfig: './tsconfig.json',
    minify: true,
    bundle: true,
    sourcemap: true,
    define: Object.entries({
      ENABLE_LOGGER: process.env.WEB_ENABLE_LOGGER,
    }).reduce(
      (acc, [key, value]) => ({
        ...acc,
        [`process.env.${key}`]: JSON.stringify(value),
      }),
      {}
    ),
  };

  fs.rmSync(config.outdir, { recursive: true, force: true });
  await build(config);
};

await main();
