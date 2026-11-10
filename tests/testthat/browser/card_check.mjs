// Headless Chrome check of one detqc review card and its index.
// Usage: node card_check.mjs <card.html> <index.html> <chrome-binary> <port>
// Prints one JSON object.
import { spawn } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const [, , cardPath, indexPath, chromeBin, portArg] = process.argv;
const PORT = Number(portArg || 9344);
const prof = fs.mkdtempSync(path.join(os.tmpdir(), 'detqc_chrome_'));
const chrome = spawn(chromeBin, ['--headless=new', '--disable-gpu', '--no-first-run',
  '--no-default-browser-check', `--user-data-dir=${prof}`, `--remote-debugging-port=${PORT}`,
  '--window-size=1500,1000', 'about:blank'], { stdio: 'ignore' });
const sleep = ms => new Promise(r => setTimeout(r, ms));
const out = { card: cardPath, errors: [], requests: [] };

let targets;
for (let i = 0; i < 300 && !targets; i++) {
  try { targets = await (await fetch(`http://127.0.0.1:${PORT}/json/list`)).json(); } catch { await sleep(200); }
}
const page = (targets || []).find(t => t.type === 'page');
if (!page) {
  console.log(JSON.stringify({ card: cardPath, failure: 'Chrome did not open a page target in 60 s' }));
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
  if (m.method === 'Runtime.exceptionThrown') out.errors.push(m.params.exceptionDetails.text + ' ' +
    ((m.params.exceptionDetails.exception || {}).description || ''));
  if (m.method === 'Network.requestWillBeSent') out.requests.push(m.params.request.url);
});
const send = (method, params = {}) => new Promise(r => { const i = ++seq; pending.set(i, r);
  ws.send(JSON.stringify({ id: i, method, params })); });
const js = async expr => {
  const r = await send('Runtime.evaluate', { expression: expr, awaitPromise: true, returnByValue: true });
  if (r.result.exceptionDetails) throw new Error(r.result.exceptionDetails.text + ' in ' + expr.slice(0, 60));
  return r.result.result.value;
};
const load = async file => {
  await send('Page.navigate', { url: pathToFileURL(path.resolve(file)).href });
  for (let i = 0; i < 100; i++) {
    try { if (await js('document.readyState === "complete"')) break; } catch { /* loading */ }
    await sleep(100);
  }
  await sleep(300);
};
const VK = { ArrowRight: 39, ArrowLeft: 37, ' ': 32 };
const key = async (k, modifiers = 0) => {
  for (const type of ['keyDown', 'keyUp']) {
    await send('Input.dispatchKeyEvent', { type, key: k, code: k === ' ' ? 'Space' : k,
      windowsVirtualKeyCode: VK[k] || 0, modifiers });
  }
};
// A real mouse click in the middle of an element, scrolled into view first:
// the call bar and the slider sit below the fold at 1500 x 1000.
const click = async sel => {
  const r = await js(`(() => { const el = document.querySelector(${JSON.stringify(sel)});
    el.scrollIntoView({block: "center"}); const b = el.getBoundingClientRect();
    return {x: b.left + b.width / 2, y: b.top + b.height / 2}; })()`);
  for (const type of ['mousePressed', 'mouseReleased']) {
    await send('Input.dispatchMouseEvent', { type, x: r.x, y: r.y, button: 'left', clickCount: 1 });
  }
  await sleep(50);
};

try {
  await send('Page.enable'); await send('Runtime.enable'); await send('Network.enable');
  await load(cardPath);
  // A real D key press on open, before the cursor moves, must stamp the
  // candidate date (or nothing), never the first day in view.
  const STORE = 'window.__card.data.store';
  const mine = `(JSON.parse(localStorage.getItem(${STORE}) || "{}")[window.__card.data.animal.id] || {})`;
  out.i_on_open = await js('window.__card.state.i');
  // The call bar must load in view, with no scroll, at 1500 x 1000.
  out.callbar_top = await js('Math.round(document.querySelector(".adjud").getBoundingClientRect().top)');
  out.callbar_bottom = await js('Math.round(document.querySelector(".adjud").getBoundingClientRect().bottom)');
  out.inner_h = await js('window.innerHeight');
  // The top of the time panels.
  out.stack_top = await js('Math.round(document.getElementById("stack").getBoundingClientRect().top)');
  out.readout_beside_map = await js(`(() => { const m = document.getElementById("map").getBoundingClientRect();
    const r = document.querySelector(".readout").getBoundingClientRect(); return r.left >= m.right; })()`);
  out.candidate = await js('window.__card.data.band.death_date || null');
  await js('(() => { document.getElementById("autoadv").checked = false; return true; })()');
  // X and "= cursor" on a fresh card set no date; the day in view
  // is the first day of the record, which is not a choice yet.
  await key('x');
  out.x_on_open = await js(`${mine}.death_date || null`);
  await js('(() => { document.getElementById("deathbtn").click(); return true; })()');
  out.button_on_open = await js(`${mine}.death_date || null`);
  out.hint_on_open = await js('document.getElementById("hint").textContent');
  await key('d');
  out.stamp_on_open = await js(`${mine}.death_date || null`);
  out.stamp_src_on_open = await js(`${mine}.death_src || null`);
  out.deathdate_text_on_open = await js('document.getElementById("deathdate").textContent');
  // A call from another review in the same store is not counted here.
  await js(`(() => { localStorage.setItem(${STORE}, JSON.stringify({"OTHER-STUDY-1": {call: "alive"}})); return true; })()`);
  await load(cardPath);
  out.navpos_with_foreign_call = await js('document.getElementById("navpos").textContent');
  await js(`(() => { localStorage.removeItem(${STORE}); return true; })()`);
  await load(cardPath);
  out.title = await js('document.title');
  out.has_payload = await js('!!document.getElementById("detqc-data") && JSON.parse(document.getElementById("detqc-data").textContent).nd > 0');
  out.nd = await js('window.__card.data.nd');
  out.date_start = await js('document.getElementById("curdate").textContent');
  out.i_start = await js('window.__card.state.i');
  await key('ArrowRight'); await key('ArrowRight'); await key('ArrowRight');
  out.date_after_3_right = await js('document.getElementById("curdate").textContent');
  out.i_after_3_right = await js('window.__card.state.i');
  await js('(() => { const s = document.getElementById("slider"); s.value = 40; s.dispatchEvent(new Event("input")); return true; })()');
  out.date_after_slider_40 = await js('document.getElementById("curdate").textContent');
  await js('(() => { document.getElementById("playbtn").click(); return true; })()');
  await sleep(600);
  out.i_after_play_600ms = await js('window.__card.state.i');
  out.playing = await js('window.__card.state.playing');
  await js('(() => { document.getElementById("playbtn").click(); return true; })()');
  out.date_after_play = await js('document.getElementById("curdate").textContent');
  out.readout_window_row = await js('document.querySelectorAll("#rotab tr")[2].textContent');
  out.map_legend_drawn = await js('document.getElementById("map").width > 0');
  await js('(() => { document.getElementById("autoadv").checked = false; return true; })()');
  out.cursor_before_call = await js('document.getElementById("curdate").textContent');
  await key('d');
  out.stamp_after_move = await js(`${mine}.death_date || null`);
  await key('x');
  out.x_after_move = await js(`${mine}.death_date || null`);
  out.hint_after_move = await js('document.getElementById("hint").textContent');
  // A typed date replaces the stamp and is exported with its source.
  await js(`(() => { const d = document.getElementById("deathinput"); d.value = "2023-09-02";
    d.dispatchEvent(new Event("change", {bubbles: true})); return true; })()`);
  out.typed_store = await js(`${mine}`);
  out.typed_export = await js('window.__card.exportCalls().join("\\n")');
  out.panel_titles = await js('window.__card.data.streams.map(s => s.label)');
  out.store_after_call = await js('localStorage.getItem(window.__card.data.store)');
  out.readout_text = await js('document.getElementById("rotab").textContent');
  // Keys after a real click on the slider, then on the checkbox: both are
  // inputs, and keys must work after a click on an input. The store is put
  // back afterwards for the index checks.
  const saved = await js(`localStorage.getItem(${STORE})`);
  await js(`(() => { localStorage.removeItem(${STORE}); return true; })()`);
  await load(cardPath);
  await js('(() => { document.getElementById("autoadv").checked = false; return true; })()');
  await click('#slider');
  out.focus_after_slider = await js('document.activeElement.id');
  out.i_after_slider_click = await js('window.__card.state.i');
  await key('d');
  out.slider_d = await js(`${mine}.death_date || null`);
  out.slider_d_src = await js(`${mine}.death_src || null`);
  await key('x');
  out.slider_x = await js(`${mine}.death_date || null`);
  out.slider_cursor = await js('document.getElementById("curdate").textContent');
  const i0 = await js('window.__card.state.i');
  await key('ArrowRight', 8);
  out.slider_shift_step = (await js('window.__card.state.i')) - i0;
  await key(' ');
  out.slider_space_playing = await js('window.__card.state.playing');
  await js('(() => { window.__card.stop(); return true; })()');
  await click('#autoadv'); await click('#autoadv');
  out.focus_after_checkbox = await js('document.activeElement.id');
  out.autoadv_after_two_clicks = await js('document.getElementById("autoadv").checked');
  await key('u');
  out.checkbox_u = await js(`${mine}.call || null`);
  // A finger drag across the time panels moves the cursor.
  await js(`(() => { localStorage.removeItem(${STORE}); return true; })()`);
  await send('Emulation.setTouchEmulationEnabled', { enabled: true, maxTouchPoints: 1 });
  await load(cardPath);
  // The stack can be taller than the window, and the call bar is sticky at
  // the top: scroll the stack's top to just below the sticky zone and touch
  // 60 px into it, so the touch lands on the panels, not off screen.
  const st = await js(`(() => { const c = document.getElementById("stack");
    const z = document.getElementById("stickyzone"); window.scrollTo(0, 0);
    const top0 = c.getBoundingClientRect().top + window.scrollY;
    window.scrollTo(0, Math.max(0, top0 - z.getBoundingClientRect().height - 10));
    const b = c.getBoundingClientRect(); return {l: b.left, w: b.width, y: b.top + 60}; })()`);
  out.touch_i_before = await js('window.__card.state.i');
  const pt = f => [{ x: st.l + st.w * f, y: st.y, id: 1 }];
  await send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: pt(0.3) });
  for (const f of [0.4, 0.5, 0.6, 0.7, 0.8]) {
    await send('Input.dispatchTouchEvent', { type: 'touchMove', touchPoints: pt(f) });
  }
  await send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
  await sleep(100);
  out.touch_i_after = await js('window.__card.state.i');
  out.touch_nd = await js('window.__card.data.nd');
  await send('Emulation.setTouchEmulationEnabled', { enabled: false });
  // With no candidate date, D saves an undated call, asks for the date and
  // stays on the card, though advance on call is on.
  if (out.candidate === null) {
    await load(cardPath);
    out.nocand_autoadv = await js('document.getElementById("autoadv").checked');
    out.nocand_has_next = await js('!!window.__card.data.nav.next_slug');
    await key('d');
    await sleep(900);
    out.nocand_href_same = await js(`location.href === ${JSON.stringify(pathToFileURL(path.resolve(cardPath)).href)}`);
    out.nocand_store = await js(`${mine}`);
    out.nocand_hint = await js('document.getElementById("hint").textContent');
  }
  await js(`(() => { localStorage.setItem(${STORE}, ${JSON.stringify(saved)}); return true; })()`);
  out.export_lines = await js('document.getElementById("autoadv").checked = false, window.__card.exportCalls().length');
  if (indexPath) {
    await load(indexPath);
    out.index_rows = await js('document.querySelectorAll("#tables tbody tr").length');
    // The index fits a 1,500 px window.
    out.index_scroll_w = await js('document.documentElement.scrollWidth');
    out.index_inner_w = await js('window.innerWidth');
    out.index_client_w = await js('document.documentElement.clientWidth');
    out.index_table_w = await js('Math.round(Math.max(...[...document.querySelectorAll("#tables table")].map(t => t.getBoundingClientRect().right)))');
    out.index_heads = await js('[...document.querySelectorAll("#tables table:first-of-type th")].map(h => h.textContent)');
    out.index_count_text = await js('document.getElementById("count").textContent');
    out.index_bandheads = await js('[...document.querySelectorAll("h2.bandhead")].map(h => h.id)');
    await js('(() => { const rows = document.querySelectorAll("#tables tbody tr"); rows[rows.length - 1].scrollIntoView({block:"start"}); window.__review.markCurrent(); return true; })()');
    await sleep(200);
    out.spy_band_after_scroll = await js('(document.querySelector("#toclist .grp.cur") || {}).dataset ? document.querySelector("#toclist .grp.cur").dataset.band : null');
    out.spy_row_after_scroll = await js('(document.querySelector("#toclist .row.cur .fid") || {}).textContent || null');
    out.index_export = await js('window.__review.exportCsv()');
    // An index call on a row with a candidate date takes that date; a row with
    // no candidate takes the date typed in its field.
    out.index_cand_call = await js(`(() => {
      const tr = [...document.querySelectorAll("#tables tbody tr")].find(r => r.dataset.cand && !window.__review.ann()[r.dataset.id]);
      if (!tr) return null;
      const sel = tr.querySelector("select.call"); sel.value = "dead_or_shed";
      sel.dispatchEvent(new Event("change", {bubbles: true}));
      return {id: tr.dataset.id, cand: tr.dataset.cand, stored: window.__review.ann()[tr.dataset.id].death_date};
    })()`);
    out.index_typed_call = await js(`(() => {
      const tr = [...document.querySelectorAll("#tables tbody tr")].find(r => !r.dataset.cand);
      if (!tr) return null;
      const dd = tr.querySelector("input.dd"); dd.value = "2023-09-09";
      dd.dispatchEvent(new Event("change", {bubbles: true}));
      const sel = tr.querySelector("select.call"); sel.value = "dead_or_shed";
      sel.dispatchEvent(new Event("change", {bubbles: true}));
      return {id: tr.dataset.id, stored: window.__review.ann()[tr.dataset.id].death_date};
    })()`);
    out.index_export_after = await js('window.__review.exportCsv()');
    // The clear button deletes this review's calls only.
    out.after_clear = await js(`(() => {
      const st = window.__review.P.store, a = window.__review.ann();
      a["OTHER-STUDY-1"] = {call: "alive"}; localStorage.setItem(st, JSON.stringify(a));
      window.confirm = () => true; document.getElementById("tocclear").click();
      return Object.keys(JSON.parse(localStorage.getItem(st) || "{}"));
    })()`);
  }
  out.network_requests_not_file = out.requests.filter(u => !u.startsWith('file:') && !u.startsWith('data:') && u !== 'about:blank');
} catch (e) {
  out.failure = String(e);
} finally {
  delete out.requests;
  console.log(JSON.stringify(out));
  ws.close(); chrome.kill('SIGKILL');
  // Chrome may still be writing its profile as it exits; a failed clean-up of
  // a temporary folder is not a failed check.
  await sleep(300);
  try { fs.rmSync(prof, { recursive: true, force: true }); } catch { /* left in tmp */ }
  process.exit(0);
}
