import * as fs from 'fs';
import * as path from 'path';

export const listFilePaths = (directory: string): string[] => {
  const iter = (curr: string, deferred: string[]): string[] => {
    const files = fs.readdirSync(curr, { withFileTypes: true });
    return files.reduce((acc, file) => {
      const filePath = path.join(curr, file.name);
      return file.isDirectory() ? iter(filePath, acc) : [...acc, filePath];
    }, deferred);
  };
  return iter(directory, []);
};
