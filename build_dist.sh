#!/bin/bash

# File Name   : build_dist.sh
# Description : Builds ISSIE distributions for multiple operating systems
#               and architectures using Electron Builder.
# Author      : Samuel Wang (@samuelpswang)
# Date        : 2025/10/26

set -euo pipefail

help() {
cat << EOF
Build ISSIE distributions for multiple operating systems and architectures.
Usage:
  $(basename "$0") [OPTIONS]

Options:
  --default           Build for all default targets:
                      mac:x64:dmg mac:arm64:dmg linux:x64:zip linux:arm64:zip
                      win:x64:zip win:arm64:zip
  --add=<target>      Add a specific target to the build list
  --remove=<target>   Remove a specific target from the build list
  --help              Show this help message and exit
  
Examples:
  $(basename "$0") --default
  $(basename "$0") --add=linux:arm64:zip
  $(basename "$0") --default --remove=win:x64:zip
EOF
}

# Retry a command a few times, pausing longer after each failure.
#
# electron-builder fetches the Electron runtime for the target architecture while packaging, and a
# cross-architecture build always needs one that `npm install` did not cache. On the v6.0.16 Linux
# release that download dropped mid-stream - "socket hang up" from got, thirteen seconds into
# packaging linux arm64 - and nothing in electron-builder retries it. Under `set -e` the script
# died there, so that release lost its x64 zip as well: the loop had not reached it yet.
#
# Retrying is cheap. A second `npm run dist` recompiles nothing - Fable skips a project whose
# output is already up to date and webpack takes seconds - so an extra attempt costs about as
# much as the packaging step it is repeating.
max_attempts=3
retry() {
  local attempt=1
  while true; do
    if "$@"; then
      return 0
    fi
    if [ "${attempt}" -ge "${max_attempts}" ]; then
      echo "Failed after ${attempt} attempts: $*" >&2
      return 1
    fi
    echo "Attempt ${attempt} of ${max_attempts} failed, retrying in $((attempt * 15))s: $*" >&2
    sleep $((attempt * 15))
    attempt=$((attempt + 1))
  done
}

# Create temp directory for files to publish
rm -rf dist_tmp
mkdir dist_tmp

# Parse target list
default_targets="mac:x64:dmg mac:arm64:dmg linux:x64:zip linux:arm64:zip \
win:x64:zip win:arm64:zip"
build_targets=
for option in "$@"; do
  case "${option}" in
    --help)
      help
      exit 0
      ;;
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
  retry npm run dist -- --$os --$arch -p never

  dist_path=$(find ./dist/ -name "*.${filetype}")
  temp_path=$(echo -n "${dist_path}" | sed -e 's/dist/dist_tmp/g' \
    -e "s/.${filetype}//g" -e "s/-${arch}//g" -e "s/-${os}//g")
  temp_path="${temp_path}-${os}-${arch}.${filetype}"
  mv $dist_path $temp_path
done

# Copy everything back into dist
rm -rf dist
mv dist_tmp dist
