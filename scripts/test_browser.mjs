// No app server or model credentials: run compiled tests in a fresh browser.
import {spawn} from 'node:child_process';
import {createServer} from 'node:http';
import {access, mkdtemp, readFile, rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {resolve, sep, extname} from 'node:path';
import {once} from 'node:events';
import {fileURLToPath} from 'node:url';

const root = fileURLToPath(new URL('../target/browser-tests/', import.meta.url));
let browser, socket, profile;
let failure;
let nextId = 0;
const pending = new Map();
const abort = new AbortController();
const server = createServer(async (req, res) => {
  try {
    const path = resolve(root, '.' + decodeURIComponent(new URL(req.url, 'http://localhost').pathname));
    if (!path.startsWith(root.endsWith(sep) ? root : root + sep)) throw Error('Invalid path');
    const data = await readFile(path);
    res.writeHead(200, {'Content-Type': extname(path) === '.js' ? 'application/javascript' : 'text/html'});
    res.end(data);
  } catch {
    res.writeHead(404);
    res.end();
  }
});

function command(method, params = {}) {
  const id = ++nextId;
  return new Promise((resolve, reject) => {
    abort.signal.throwIfAborted();
    pending.set(id, {resolve, reject});
    socket.send(JSON.stringify({id, method, params}));
  });
}

async function chromeBinary() {
  if (process.env.CHROME_BIN) return process.env.CHROME_BIN;
  for (const path of ['/usr/bin/google-chrome', '/usr/bin/chromium', '/usr/bin/chromium-browser', '/snap/bin/chromium']) {
    try { await access(path); return path; } catch {}
  }
  throw Error('Chrome/Chromium not found; set CHROME_BIN to its executable');
}

async function run() {
  await access(resolve(root, 'index.html'));
  server.listen(0, '127.0.0.1');
  await once(server, 'listening');
  profile = await mkdtemp(resolve(tmpdir(), 'simmis-browser-tests-'));
  const args = ['--headless=new', '--disable-gpu', '--no-first-run', '--no-default-browser-check',
    '--disable-background-networking', '--disable-extensions', '--disable-dev-shm-usage',
    '--remote-debugging-port=0', `--user-data-dir=${profile}`, 'about:blank'];
  // Only needed in CI containers without usable kernel sandboxing. Never
  // implied for desktop use; the tests load only this loopback static server.
  if (process.env.SIMMIS_TEST_CHROME_NO_SANDBOX === '1') args.unshift('--no-sandbox');
  browser = spawn(await chromeBinary(), args, {stdio: ['ignore', 'ignore', 'pipe']});
  const debuggerUrl = await new Promise((resolve, reject) => {
    let stderr = '';
    abort.signal.addEventListener('abort', () => reject(abort.signal.reason), {once: true});
    browser.once('error', reject);
    browser.once('exit', code => reject(Error(`Browser exited before readiness (${code})`)));
    browser.stderr.on('data', chunk => {
      stderr = (stderr + chunk.toString()).slice(-32768);
      const found = stderr.match(/DevTools listening on (ws:\/\/[^\s]+)/);
      if (found) resolve(new URL(found[1]));
    });
  });
  const target = await fetch(`http://${debuggerUrl.host}/json/new?about:blank`, {method: 'PUT', signal: abort.signal}).then(r => r.json());
  socket = new WebSocket(target.webSocketDebuggerUrl);
  const outcome = new Promise((resolve, reject) => {
    failure = reject;
    socket.addEventListener('message', event => {
      const msg = JSON.parse(event.data);
      if (msg.id) {
        const p = pending.get(msg.id);
        pending.delete(msg.id);
        if (p) msg.error ? p.reject(Error(msg.error.message)) : p.resolve(msg.result);
      } else if (msg.method === 'Runtime.bindingCalled' && msg.params.name === '__simmisReport') {
        resolve(JSON.parse(msg.params.payload));
      } else if (msg.method === 'Runtime.exceptionThrown') {
        reject(Error(msg.params.exceptionDetails.exception?.description || msg.params.exceptionDetails.text));
      } else if (msg.method === 'Runtime.consoleAPICalled' && msg.params.type === 'error') {
        console.error(msg.params.args.map(x => x.value ?? x.description).join(' '));
      }
    });
    socket.addEventListener('error', () => reject(Error('Browser debugger connection failed')));
    socket.addEventListener('close', () => reject(Error('Browser debugger closed before test completion')));
    browser.once('exit', code => reject(Error(`Browser exited during tests (${code})`)));
  });
  // Attach a rejection handler before commands: a protocol error must not
  // become an unhandled rejection while Page.navigate is still pending.
  outcome.catch(() => {});
  await once(socket, 'open', {signal: abort.signal});
  await command('Runtime.enable');
  await command('Runtime.addBinding', {name: '__simmisReport'});
  await command('Page.enable');
  await command('Page.navigate', {url: `http://127.0.0.1:${server.address().port}/index.html`});
  const result = await outcome;
  if (result.version !== 1 || !['test', 'pass', 'fail', 'error'].every(k => Number.isSafeInteger(result[k]) && result[k] >= 0)
      || result.test === 0 || result.pass + result.fail + result.error === 0)
    throw Error(`Missing or invalid browser test summary: ${JSON.stringify(result).slice(0, 4096)}`);
  console.log(JSON.stringify(result));
  if (result.fail || result.error) throw Error('Browser tests failed');
}

const deadline = setTimeout(() => {
  const error = Error('Browser tests exceeded the 120 second deadline');
  abort.abort(error);
  failure?.(error);
  for (const p of pending.values()) p.reject(error);
  browser?.kill('SIGKILL');
}, 120000);

try {
  await run();
} catch (error) {
  console.error(error.message);
  process.exitCode = 1;
} finally {
  clearTimeout(deadline);
  socket?.close();
  if (browser?.pid && browser.exitCode === null && browser.signalCode === null) {
    const stopped = once(browser, 'exit');
    browser.kill('SIGKILL');
    await stopped;
  }
  await new Promise(resolve => server.close(resolve));
  if (profile) await rm(profile, {recursive: true, force: true});
}
