import {readFileSync, writeFileSync, copyFileSync, cpSync, mkdirSync} from 'node:fs';
import {createRequire} from 'node:module';
import {dirname, join} from 'node:path';
const require = createRequire(import.meta.url);
const dist = dirname(require.resolve('katex'));
const target = new URL('../public/vendor/katex/', import.meta.url);
mkdirSync(target, {recursive: true});
// Lightning CSS bundles imports without rebasing their font URLs. Keep the
// packaged fonts on the application's own origin in development and releases.
writeFileSync(new URL('katex.min.css', target),
  readFileSync(join(dist, 'katex.min.css'), 'utf8').replaceAll('url(fonts/', 'url(/vendor/katex/fonts/'));
cpSync(join(dist, 'fonts'), new URL('fonts/', target), {recursive: true});
copyFileSync(join(dist, '..', 'LICENSE'), new URL('LICENSE', target));
