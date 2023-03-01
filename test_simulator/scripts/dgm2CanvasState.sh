#!/usr/bin/env bash

set -e

# Get the folder path from the command-line argument
FOLDER=$1

# Get the absolute path to the script file
SCRIPT_PATH=$(realpath "$0")
SCRIPT_DIR=$(dirname "${SCRIPT_PATH}")

echo "module TestCases

open CommonTypes
open SimulatorTypes

let canvasStates: (CanvasState * string) list = ["
# Loop through every file in the folder
for FILE in $(find "${FOLDER}" -maxdepth 1 -type f -name "*.dgm"); do
	# Print the name of the file
	node ${SCRIPT_DIR}/dgm2CanvasState.mjs "${FILE}"
done
echo "]"
