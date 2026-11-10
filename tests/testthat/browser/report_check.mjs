// Headless Chrome check of one detqc processing report.
// Usage: node report_check.mjs <report.html> <chrome-binary> <port> [screenshot.png]
// Prints one JSON object: console errors, exceptions, every request that is
// not the file itself or a data: URI, the section ids, and the images that
// decoded. Adapted from card_check.mjs beside it.
import { spawn } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const [, , reportPath, chromeBin, portArg, shotPath] = process.argv;
const PORT = Number(portArg || 9354);
const prof = fs.mkdtempSync(path.join(os.tmpdir(), 'detqc_report_'));
const chrome = spawn(chromeBin, ['--headless=new', '--disable-gpu', '--no-first-run',
  '--no-default-browser-check', `--user-data-dir=${prof}`, `--remote-debugging-port=${PORT}`,
  '--window-size=1300,2400', 'about:blank'], { stdio: 'ignore' });
const sleep = ms => new Promise(r => setTimeout(r, ms));
const fileUrl = pathToFileURL(path.resolve(reportPath)).href;
const out = { report: reportPath, errors: [], console_errors: [], requests: [] };

let targets;
for (let i = 0; i < 300 && !targets; i++) {
  try { targets = await (await fetch(`http://127.0.0.1:${PORT}/json/list`)).json(); } catch { await sleep(200); }
}
const page = (targets || []).find(t => t.type === 'page');
if (!page) {
  console.log(JSON.stringify({ report: reportPath, failure: 'Chrome did not open a page target in 60 s' }));
  chrome.kill('SIGKILL');
  fs.rmSync(prof, { recursive: true, force: true });
  process.exit(0);
}
const ws = new WebSocket(page.webSocketDebuggerUrl);
await new Promise(r => ws.addEventListener('open', r));
let seq = 0; const pending = new Map();
ws.addEventListener('message', ev => {
  const m = JSON.parse(ev.data);
  if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); }
  if (m.method === 'Runtime.exceptionThrown') out.errors.push(m.params.exceptionDetails.text);
  if (m.method === 'Runtime.consoleAPICalled' && m.params.type === 'error') {
    out.console_errors.push(m.params.args.map(a => a.value || a.description || '').join(' '));
  }
  if (m.method === 'Log.entryAdded' && m.params.entry.level === 'error') {
    out.console_errors.push(m.params.entry.text);
  }
  if (m.method === 'Network.requestWillBeSent') {
    const u = m.params.request.url;
    if (u !== fileUrl && !u.startsWith('data:')) out.requests.push(u);
  }
});
const send = (method, params = {}) => new Promise(r => { const i = ++seq; pending.set(i, r);
  ws.send(JSON.stringify({ id: i, method, params })); });
const js = async expr => {
  const r = await send('Runtime.evaluate', { expression: expr, awaitPromise: true, returnByValue: true });
  if (r.result.exceptionDetails) throw new Error(r.result.exceptionDetails.text + ' in ' + expr.slice(0, 60));
  return r.result.result.value;
};

try {
  await send('Page.enable'); await send('Runtime.enable'); await send('Network.enable'); await send('Log.enable');
  await send('Page.navigate', { url: fileUrl });
  for (let i = 0; i < 100; i++) {
    try { if (await js('document.readyState === "complete"')) break; } catch { /* loading */ }
    await sleep(100);
  }
  await sleep(300);
  out.title = await js('document.title');
  out.sections = await js('[...document.querySelectorAll("main > section, main > header")].map(s => s.id)');
  out.images = await js('[...document.images].length');
  out.images_decoded = await js('[...document.images].filter(i => i.complete && i.naturalWidth > 0).length');
  out.details = await js('document.querySelectorAll("details").length');
  if (shotPath) {
    const r = await send('Page.captureScreenshot', { format: 'png', captureBeyondViewport: true });
    fs.writeFileSync(shotPath, Buffer.from(r.result.data, 'base64'));
    out.screenshot = shotPath;
  }
} catch (e) {
  out.failure = String(e);
}
console.log(JSON.stringify(out));
ws.close();
chrome.kill('SIGKILL');
await sleep(300);
fs.rmSync(prof, { recursive: true, force: true });
