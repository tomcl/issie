#!/usr/bin/env bash

set -e

SIMULATION_ARRAY_SIZE=500
LAST_STEP_NEEDED=2000
TEST_CASES_PATH=${1:-"../testcases"}
# TEST_CASES_PATH="/home/yujie/workspace/EEE1labs/DECA/Part3-Section1-Lab3/lab3i"
# TIMEOUT=null

test_cases=($(ls ${TEST_CASES_PATH}/*.dgm))

mkdir -p output

printf "%18s, %7s, %11s, %9s, %6s, %23s, %28s\n" "Test Case" "Result" "#Components" "StepArray" "#Steps" "Avg Execution Time (ms)" "Avg Speed (comp * step / ms)"

passed_test=0

# component * step / millisecond
speed_list=()

NODE_OPTIONS=()
NODE_OPTIONS+="--allow-natives-syntax"
NODE_ARGS=(${SIMULATION_ARRAY_SIZE} ${LAST_STEP_NEEDED} ${TEST_CASES_PATH})

for i in $(seq 0 $((${#test_cases[@]} - 1))); do
	output="output/$(basename ${test_cases[$i]}).out"
	node "${NODE_OPTIONS[@]}" index.js ${i} "${NODE_ARGS[@]}" 2>/dev/null | tail -n 1 >"${output}"

	time=$(cat "${output}" | jq .time)

	result=$(cat "${output}" | jq -cS .result)

	num_comps=$(cat "${output}" | jq .numComps)

	speed=$(node -p "${LAST_STEP_NEEDED}*${num_comps}/${time}")

	test_case_name=$(cat "${output}" | jq -r .sheetName)

	ref=$(cat "reference/${test_case_name}.dgm.ref" | jq -cS .result)

	if diff <(echo "${result}") <(echo "${ref}"); then
		printf "%18s, %6s, %11d, %9d, %6d, %23f, %28.3f\n" ${test_case_name} "✅ PASS" ${num_comps} ${SIMULATION_ARRAY_SIZE} ${LAST_STEP_NEEDED} ${time} ${speed}
		speed_list+=($speed)
		passed_test=$((passed_test + 1))
	else
		printf "%18s, %6s, %11d, %9d, %6d, %23f, %28.3f\n" ${test_case_name} "❌ FAIL" ${num_comps} ${SIMULATION_ARRAY_SIZE} ${LAST_STEP_NEEDED} ${time} ${speed}
	fi
done

echo >&2
printf "Passed %d/%d tests\n" ${passed_test} ${#test_cases[@]} >&2

if [ ${passed_test} -eq ${#test_cases[@]} ]; then
	exit 0
fi

exit 1
