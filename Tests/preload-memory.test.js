'use strict';

const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

const preload = fs.readFileSync(path.join(__dirname, '..', 'src', 'Preload', 'preload.js'), 'utf8');

function bridgeWith(getSystemMemoryInfo) {
  let bridge;
  const electron = {
    contextBridge: { exposeInMainWorld: (_name, api) => { bridge = api; } },
    ipcRenderer: { sendSync: () => ({}) },
    webFrame: {},
  };
  vm.runInNewContext(preload, {
    require: (name) => {
      assert.equal(name, 'electron');
      return electron;
    },
    process: { contextIsolated: true, getSystemMemoryInfo },
  });
  return bridge;
}

test('preload keeps the system memory reading when Electron provides it', () => {
  const memory = { total: 12345, free: 6789 };
  assert.equal(bridgeWith(() => memory).diagnostics.systemMemory(), memory);
});

test('preload keeps renderer startup alive when Electron cannot read system memory', () => {
  const bridge = bridgeWith(() => { throw new Error('Unable to retrieve system memory information'); });
  assert.equal(bridge.diagnostics.systemMemory().total, 0);
});
