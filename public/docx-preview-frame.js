// This page runs with an opaque sandbox origin. It receives document bytes,
// never an authenticated URL, cookie, token, application state or host handle.
(() => {
  'use strict';
  const id = location.hash.slice(1);
  const parentOrigin = new URL(location.href).origin;
  if (!/^[0-9a-f-]{36}$/.test(id) || parent === window) return;
  const send = type => parent.postMessage({type, id}, parentOrigin);
  let consumed = false;
  addEventListener('message', async event => {
    const data = event.data;
    if (consumed || event.source !== parent || event.origin !== parentOrigin ||
        !data || data.type !== 'simmis:docx-load' || data.id !== id ||
        !(data.bytes instanceof ArrayBuffer) || data.bytes.byteLength > 8 * 1024 * 1024) return;
    consumed = true;
    const status = document.getElementById('status');
    try {
      // Render detached: remove navigation/active elements BEFORE inserting
      // content. A contenteditable overlay is deliberately not a save path.
      const container = document.createElement('div');
      await docx.renderAsync(data.bytes, container, container, {
        renderAltChunks: false,
        useBase64URL: true,
        renderHeaders: true, renderFooters: true,
        renderFootnotes: true, renderEndnotes: true,
        renderChanges: true, renderComments: true,
        experimental: false,
        ignoreLastRenderedPageBreak: false
      });
      container.querySelectorAll('a').forEach(a => {
        const href = a.getAttribute('href');
        for (const attr of ['href', 'target', 'download', 'ping']) a.removeAttribute(attr);
        if (href?.startsWith('#')) a.setAttribute('href', href);
      });
      container.querySelectorAll('iframe,object,embed,script,form,input,button,base,meta,link').forEach(n => n.remove());
      container.querySelectorAll('[contenteditable]').forEach(n => n.removeAttribute('contenteditable'));
      document.getElementById('document').replaceChildren(container);
      status.remove();
      send('simmis:docx-rendered');
    } catch (_) {
      status.textContent = 'This document could not be previewed. Use Text and corrections or download the original.';
      send('simmis:docx-error');
    }
  });
  send('simmis:docx-ready');
})();
