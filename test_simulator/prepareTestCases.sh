#!/usr/bin/env bash

set -e

# Get the folder path from the command-line argument
FOLDER=$1
FOLDER_NAME=$(basename "${FOLDER}")
TARGET_FILE="${FOLDER_NAME}.fs"

bash scripts/dgm2CanvasState.sh "${FOLDER}" >"${TARGET_FILE}"

# Check if fantomas exists
if which fantomas >/dev/null; then
	echo "fantomas is installed, formatting code"
	fantomas "${TARGET_FILE}"
else
	echo "fantomas is not installed, skipping formatting"
fi
