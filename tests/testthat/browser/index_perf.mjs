// Headless Chrome timing of a detqc review index: largest contentful paint,
// script errors, network requests, and the frame gaps and long tasks while the
// page is scrolled from top to bottom in 40 steps.
// Usage: node index_perf.mjs <index.html> <chrome-binary> <port>
// Prints one JSON object.
import { spawn } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const [, , indexPath, chromeBin, portArg] = process.argv;
const PORT = Number(portArg || 9355);
const prof = fs.mkdtempSync(path.join(os.tmpdir(), 'detqc_perf_'));
const chrome = spawn(chromeBin, ['--headless=new', '--disable-gpu', '--no-first-run',
  '--no-default-browser-check', `--user-data-dir=${prof}`, `--remote-debugging-port=${PORT}`,
  '--window-size=1500,1000', 'about:blank'], { stdio: 'ignore' });
const sleep = ms => new Promise(r => setTimeout(r, ms));
const out = { index: indexPath, errors: [], requests: [] };
let targets;
for (let i = 0; i < 300 && !targets; i++) {
  try { targets = await (await fetch(`http://127.0.0.1:${PORT}/json/list`)).json(); } catch { await sleep(200); }
}
const page = (targets || []).find(t => t.type === 'page');
if (!page) { console.log(JSON.stringify({ failure: 'no page target' })); chrome.kill('SIGKILL'); process.exit(0); }
const ws = new WebSocket(page.webSocketDebuggerUrl);
await new Promise(r => ws.addEventListener('open', r));
let seq = 0; const pending = new Map();
ws.addEventListener('message', ev => {
  const m = JSON.parse(ev.data);
  if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); }
  if (m.method === 'Runtime.exceptionThrown') out.errors.push(m.params.exceptionDetails.text);
  if (m.method === 'Network.requestWillBeSent') out.requests.push(m.params.request.url);
});
const send = (method, params = {}) => new Promise(r => { const i = ++seq; pending.set(i, r);
  ws.send(JSON.stringify({ id: i, method, params })); });
const js = async expr => {
  const r = await send('Runtime.evaluate', { expression: expr, awaitPromise: true, returnByValue: true });
  if (r.result.exceptionDetails) throw new Error(r.result.exceptionDetails.text);
  return r.result.result.value;
};
try {
  await send('Page.enable'); await send('Runtime.enable'); await send('Network.enable');
  // The observers are installed before the page's own script runs.
  await send('Page.addScriptToEvaluateOnNewDocument', { source: `
    window.__perf = { lcp: null, longtasks: [] };
    new PerformanceObserver(l => { for (const e of l.getEntries()) window.__perf.lcp = e.startTime; })
      .observe({ type: 'largest-contentful-paint', buffered: true });
    new PerformanceObserver(l => { for (const e of l.getEntries()) window.__perf.longtasks.push(e.duration); })
      .observe({ type: 'longtask', buffered: true });` });
  const t0 = Date.now();
  await send('Page.navigate', { url: pathToFileURL(path.resolve(indexPath)).href });
  for (let i = 0; i < 100; i++) {
    try { if (await js('document.readyState === "complete"')) break; } catch { /* loading */ }
    await sleep(100);
  }
  out.load_ms = Date.now() - t0;
  await sleep(500);
  out.lcp_ms = await js('window.__perf.lcp');
  out.dom_content_loaded_ms = await js('performance.getEntriesByType("navigation")[0].domContentLoadedEventEnd');
  out.rows = await js('document.querySelectorAll("#tables tbody tr").length');
  out.page_height = await js('document.documentElement.scrollHeight');
  out.longtasks_load_ms = await js('window.__perf.longtasks.slice()');
  out.scroll = await js(`new Promise(done => {
    const gaps = []; let last = performance.now(), step = 0;
    const H = document.documentElement.scrollHeight - window.innerHeight;
    window.__perf.longtasks = [];
    function frame(now) {
      gaps.push(now - last); last = now;
      if (step <= 40) { window.scrollTo(0, H * step / 40); step++; requestAnimationFrame(frame); }
      else {
        gaps.shift(); gaps.sort((a, b) => a - b);
        done({ frames: gaps.length, max_frame_ms: gaps[gaps.length - 1],
               p95_frame_ms: gaps[Math.floor(gaps.length * 0.95)],
               frames_over_50ms: gaps.filter(g => g > 50).length,
               longtasks_ms: window.__perf.longtasks.slice(),
               spy_band_at_end: (document.querySelector("#toclist .grp.cur") || {dataset: {}}).dataset.band || null });
      }
    }
    requestAnimationFrame(frame);
  })`);
  out.network_requests_not_file = out.requests.filter(u => !u.startsWith('file:') && !u.startsWith('data:') && u !== 'about:blank');
} catch (e) {
  out.failure = String(e);
} finally {
  delete out.requests;
  console.log(JSON.stringify(out));
  ws.close(); chrome.kill('SIGKILL');
  await sleep(300);
  try { fs.rmSync(prof, { recursive: true, force: true }); } catch { /* left in tmp */ }
  process.exit(0);
}
