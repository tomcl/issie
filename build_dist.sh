#!/bin/bash

# 0. Log current working directory
echo "[INFO] Building from: $PWD"

# 1. Create temp directory to store publish files
rm -rf dist_tmp
mkdir dist_tmp

# 2. Build for each os/arch combinations
# 3. Replace name for each os/arch combinations
# for target in "mac,x64,dmg" "mac,arm64,dmg" "linux,x64,zip" "linux,arm64,zip" "win,x64,zip"
for target in "mac,x64,dmg" "mac,arm64,dmg" "linux,arm64,zip" "win,x64,zip"
do
  os=$(cut -d',' -f1 <<< "$target")
  arch=$(cut -d',' -f2 <<< "$target")
  filetype=$(cut -d',' -f3 <<< "$target")
  echo "[INFO] Building binary for $os,$arch..."
  npm run dist -- --$os --$arch # FIX: hack, relies on `electron-builder` pos
  pathname_old=$(find ./dist/ -name "*.$filetype")
  pathname_new=$(sed 's/dist/dist_tmp/g' <<< "$pathname_old")
  pathname_new=$(sed "s/.$filetype//g" <<< "$pathname_new")
  pathname_new=$(sed "s/-$arch//g" <<< "$pathname_new")
  pathname_new="$pathname_new-$os-$arch.$filetype"
  mv $pathname_old $pathname_new
done

# 4. Copy everything back into dist
rm -rf dist
mv dist_tmp dist
