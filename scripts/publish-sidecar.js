// Publish the dotnet sidecar as self-contained single-file binaries, one per architecture of the
// HOST operating system - dotnet publish cannot cross OS boundaries, and the dist pipeline is
// already split one job per OS (build_dist.sh/.ps1, CI), so that is all any one machine needs.
//
// Output lands in build-sidecar/<os>-<arch>/, and those directory names deliberately spell
// electron-builder's ${os}-${arch} macro values: the extraResources entry in package.json picks
// the right one up per target and ships it as resources/sidecar/, which is where
// src/Main/Bridge.fs looks for it in a packaged app.
'use strict';

const { execFileSync } = require('child_process');
const path = require('path');

const plans = {
  win32: [['win-x64', 'win-x64'], ['win-arm64', 'win-arm64']],
  darwin: [['osx-x64', 'mac-x64'], ['osx-arm64', 'mac-arm64']],
  linux: [['linux-x64', 'linux-x64'], ['linux-arm64', 'linux-arm64']],
}[process.platform];

if (!plans) {
  console.error(`[sidecar] no publish plan for platform ${process.platform}`);
  process.exit(1);
}

const root = path.join(__dirname, '..');

for (const [rid, outName] of plans) {
  const outDir = path.join(root, 'build-sidecar', outName);
  console.log(`[sidecar] publishing ${rid} -> build-sidecar/${outName}`);
  execFileSync(
    'dotnet',
    [
      'publish', path.join(root, 'src', 'Sidecar'),
      '-c', 'Release',
      '-r', rid,
      '--self-contained',
      '-p:PublishSingleFile=true',
      '-o', outDir,
    ],
    { stdio: 'inherit' }
  );
}
