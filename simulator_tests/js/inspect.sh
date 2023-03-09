#!/usr/bin/env bash

SIMULATION_ARRAY_SIZE=500
LAST_STEP_NEEDED=2000
TEST_CASES_PATH="../testcases"
TEST_CASE=3
SIMULATOR="new"

NODE_OPTIONS=""
NODE_ARGS=(${SIMULATOR} ${TEST_CASE} ${SIMULATION_ARRAY_SIZE} ${LAST_STEP_NEEDED} ${TEST_CASES_PATH})

if [[ -n "${CPU_PROF}" ]]; then
	NODE_OPTIONS="${NODE_OPTIONS} --cpu-prof --cpu-prof-interval=1 --cpu-prof-dir=cpuprof"
fi

if [[ -n "${HEAP_PROF}" ]]; then
	NODE_OPTIONS="${NODE_OPTIONS} --heap-prof --heap-prof-interval=1 --heap-prof-dir=heapprof"
fi

node ${NODE_OPTIONS} index.js ${NODE_ARGS[@]} 2>/dev/null
