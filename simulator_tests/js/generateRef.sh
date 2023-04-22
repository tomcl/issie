#!/usr/bin/env bash

set -e

SIMULATION_ARRAY_SIZE=500
LAST_STEP_NEEDED=2000
TEST_CASES_PATH=${1:-"/home/yujie/workspace/EEE1labs/DECA/Part3-Section1-Lab3/lab3i"}

test_cases=($(ls ${TEST_CASES_PATH}/*.dgm))
mkdir -p reference

NODE_ARGS=(${SIMULATION_ARRAY_SIZE} ${LAST_STEP_NEEDED} ${TEST_CASES_PATH})

for i in $(seq 0 $((${#test_cases[@]} - 1))); do
	result=$(node index.js ${i} ${NODE_ARGS[@]} 2>/dev/null | tail -n -1)
	fileName="$(echo ${result} | jq -c .sheetName | tr -d '"')"
	echo "${i}"
	echo "${test_cases[i]}"
	echo "${result}" | jq -cSr . >"reference/${fileName}.dgm.ref"
done
