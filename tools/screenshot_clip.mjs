// Screenshot one region of a local HTML page in headless Chrome.
// Usage: node screenshot_clip.mjs <page.html> <out.png> <window-width> <scale> <prepare-js> <clip-js>
// <prepare-js> runs after the page loads, such as moving a slider. <clip-js>
// returns {x, y, width, height} in CSS pixels of the page. Prints the clip
// and the size of the PNG written. Used by tools/vignette_screenshots.R.
import { spawn } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const [, , page, out, widthArg, scaleArg, prepare, clipJs] = process.argv;
const CHROME = process.env.CHROME ||
  '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const PORT = 9300 + Math.floor(Math.random() * 600);
const prof = fs.mkdtempSync(path.join(os.tmpdir(), 'detqc_shot_'));
const chrome = spawn(CHROME, ['--headless=new', '--disable-gpu', '--no-first-run',
  '--hide-scrollbars', `--user-data-dir=${prof}`, `--remote-debugging-port=${PORT}`,
  `--window-size=${widthArg},1000`, 'about:blank'], { stdio: 'ignore' });
const sleep = ms => new Promise(r => setTimeout(r, ms));
let targets;
for (let i = 0; i < 300 && !targets; i++) {
  try { targets = await (await fetch(`http://127.0.0.1:${PORT}/json/list`)).json(); } catch { await sleep(200); }
}
const ws = new WebSocket(targets.find(t => t.type === 'page').webSocketDebuggerUrl);
await new Promise(r => ws.addEventListener('open', r));
let seq = 0; const pending = new Map();
ws.addEventListener('message', ev => {
  const m = JSON.parse(ev.data);
  if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); }
});
const send = (method, params = {}) => new Promise(r => {
  const i = ++seq; pending.set(i, r); ws.send(JSON.stringify({ id: i, method, params }));
});
const js = async e => (await send('Runtime.evaluate',
  { expression: e, returnByValue: true, awaitPromise: true })).result.result.value;
await send('Page.enable');
// Pages that follow the system colour scheme are shot in their light form.
await send('Emulation.setEmulatedMedia', { features: [{ name: 'prefers-color-scheme', value: 'light' }] });
await send('Page.navigate', { url: pathToFileURL(path.resolve(page)).href });
await sleep(2500);
if (prepare) { await js(prepare); await sleep(1500); }
const clip = await js(clipJs);
const shot = await send('Page.captureScreenshot', { format: 'png', captureBeyondViewport: true,
  clip: { ...clip, scale: Number(scaleArg) } });
fs.writeFileSync(out, Buffer.from(shot.result.data, 'base64'));
console.log(JSON.stringify({ out, clip, bytes: fs.statSync(out).size }));
chrome.kill('SIGKILL');
await sleep(500);
fs.rmSync(prof, { recursive: true, force: true, maxRetries: 5 });
process.exit(0);
