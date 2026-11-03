// Cross-architecture releases run on one host per OS. npm ci installs only that host's optional
// native USB binding, so stage the locked binding for each target before electron-builder packs it.
'use strict';

const { createHash } = require('crypto');
const { spawnSync } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');

const [targetOs, targetArch] = process.argv.slice(2);
const binding = {
  linux: { x64: 'usb-linux-x64-gnu', arm64: 'usb-linux-arm64-gnu' },
  win: { x64: 'usb-win32-x64-msvc', arm64: 'usb-win32-arm64-msvc' },
}[targetOs]?.[targetArch];

if (!binding) {
  console.error(`[usb] unsupported release target: ${targetOs}/${targetArch}`);
  process.exit(1);
}

const root = path.join(__dirname, '..');
const lock = JSON.parse(fs.readFileSync(path.join(root, 'package-lock.json'), 'utf8'));
const usbVersion = lock.packages['node_modules/usb'].version;
const packageName = `@node-usb/${binding}`;
const locked = lock.packages[`node_modules/${packageName}`];
const destination = path.join(root, 'node_modules', '@node-usb', binding);

if (!locked || locked.version !== usbVersion || !locked.resolved || !locked.integrity) {
  throw new Error(`[usb] missing matching locked package for ${packageName}@${usbVersion}`);
}

function hasBinding() {
  try {
    const meta = JSON.parse(fs.readFileSync(path.join(destination, 'package.json'), 'utf8'));
    return meta.name === packageName && meta.version === usbVersion &&
      fs.existsSync(path.join(destination, meta.main));
  } catch {
    return false;
  }
}

async function main() {
  if (hasBinding()) {
    console.log(`[usb] ${packageName}@${usbVersion} already staged`);
    return;
  }

  const response = await fetch(locked.resolved);
  if (!response.ok) throw new Error(`[usb] download failed: HTTP ${response.status}`);
  const archive = Buffer.from(await response.arrayBuffer());
  const integrity = `sha512-${createHash('sha512').update(archive).digest('base64')}`;
  if (integrity !== locked.integrity) {
    throw new Error(`[usb] integrity mismatch for ${packageName}@${usbVersion}`);
  }

  const temp = fs.mkdtempSync(path.join(os.tmpdir(), 'issie-usb-'));
  try {
    const tarball = path.join(temp, 'binding.tgz');
    fs.writeFileSync(tarball, archive);
    fs.rmSync(destination, { recursive: true, force: true });
    fs.mkdirSync(destination, { recursive: true });
    const extract = spawnSync('tar', ['-xzf', tarball, '-C', destination, '--strip-components=1'],
      { stdio: 'inherit' });
    if (extract.error || extract.status !== 0 || !hasBinding()) {
      throw extract.error || new Error(`[usb] could not stage ${packageName}@${usbVersion}`);
    }
    console.log(`[usb] staged ${packageName}@${usbVersion} for ${targetOs}/${targetArch}`);
  } finally {
    fs.rmSync(temp, { recursive: true, force: true });
  }
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
