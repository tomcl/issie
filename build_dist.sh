#!/bin/bash
#
# Build ISSIE distributions for multiple operating system and architectures.
#
# Author  : Samuel Wang (@samuelpswang)
# Year    : 2025

set -euo pipefail

# Create temp directory for files to publish
rm -rf dist_tmp
mkdir dist_tmp

# Parse target list
default_targets="mac:x64:dmg mac:arm64:dmg linux:x64:zip linux:arm64:zip \
win:x64:zip"
build_targets=
for option in "$@"; do
  case "${option}" in
    --default)
      build_targets="${default_targets} ${build_targets}"
      shift
      ;;
    --add=*)
      build_targets="${option#*=} ${build_targets}"
      shift
      ;;
    --remove=*)
      build_targets=$(echo -n "${build_targets}" | sed "s/${option#*=}//g")
      shift
      ;;
    -*|--*)
      echo "Unknown option ${option} received, exiting..."
      exit 1
      ;;
  esac
done
build_targets=$(echo -n "${build_targets}" | tr -s ' ' '\n' | sort -u \
  | tr '\n' ' ')

# Build for each os/arch combinations
for target in $build_targets; do
  os=$(echo -n "${target}" | cut -d':' -f1)
  arch=$(echo -n "${target}" | cut -d':' -f2)
  filetype=$(echo -n "${target}" | cut -d':' -f3)
  
  # TODO: This is a hack, this relies on `electron-builder` being the last
  # command in the package.json `dist` script
  npm run dist -- --$os --$arch -p never

  dist_path=$(find ./dist/ -name "*.${filetype}")
  temp_path=$(echo -n "${dist_path}" | sed -e 's/dist/dist_tmp/g' \
    -e "s/.${filetype}//g" -e "s/-${arch}//g" -e "s/-${os}//g")
  temp_path="${temp_path}-${os}-${arch}.${filetype}"
  mv $dist_path $temp_path
done

# Copy everything back into dist
rm -rf dist
mv dist_tmp dist
