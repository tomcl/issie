#!/usr/bin/env bash

set -e

SIMULATION_ARRAY_SIZE=500
LAST_STEP_NEEDED=2000
TEST_CASES_PATH=${1:-"../testcases"}
# TEST_CASES_PATH="/home/yujie/workspace/EEE1labs/DECA/Part3-Section1-Lab3/lab3i"
# TIMEOUT=null

test_cases=($(ls ${TEST_CASES_PATH}/*.dgm))

function geometric_mean() {
	node -p "Math.pow([$(printf '%s,' ${@})].reduce((acc,v)=>acc*v,1),1/${#test_cases[@]})"
}

printf "%18s, %7s, %11s, %6s, %24s, %24s, %25s, %25s\n" "Test Case" "Result" "#Components" "#Steps" "Avg Execution Time (New)" "Avg Execution Time (Old)" "Avg Execution Speed (New)" "Avg Execution Speed (Old)"

passed_test=0

# component * step / millisecond
speed_list_new=()
speed_list_old=()

NODE_OPTIONS=""
NODE_ARGS=(${SIMULATION_ARRAY_SIZE} ${LAST_STEP_NEEDED} ${TEST_CASES_PATH})

for i in $(seq 0 $((${#test_cases[@]} - 1))); do
	result_new=$(node ${NODE_OPTIONS} index.js new ${i} ${NODE_ARGS[@]} 2>/dev/null)
	result_old=$(node ${NODE_OPTIONS} index.js old ${i} ${NODE_ARGS[@]} 2>/dev/null)

	time_new=$(echo "${result_new}" | jq .time)
	time_old=$(echo "${result_old}" | jq .time)

	values_new=$(echo "${result_new}" | jq -cS .values)
	values_old=$(echo "${result_old}" | jq -cS .values)

	num_comps=$(echo "${result_new}" | jq .numComps)

	speed_new=$(node -p "${LAST_STEP_NEEDED}/${time_new}/${num_comps}")
	speed_old=$(node -p "${LAST_STEP_NEEDED}/${time_old}/${num_comps}")

	test_cases_name=$(basename ${test_cases[$i]} .dgm)

	if diff <(echo "${values_new}") <(echo "${values_old}"); then
		printf "%18s, %6s, %11d, %6d, %24f, %24f, %25.3f, %25.3f\n" ${test_cases_name} "✅ PASS" ${num_comps} ${LAST_STEP_NEEDED} ${time_new} ${time_old} ${speed_new} ${speed_old}
		speed_list_new+=($speed_new)
		speed_list_old+=($speed_old)
		passed_test=$((passed_test + 1))
	else
		printf "%15s, %6s, , ," ${test_cases_name} "❌ FAIL"
	fi
done

echo >&2
printf "Passed %d/%d tests\n" ${passed_test} ${#test_cases[@]} >&2

if [ ${passed_test} -eq ${#test_cases[@]} ]; then
	geo_mean_new=$(geometric_mean "${speed_list_new[@]}")
	geo_mean_old=$(geometric_mean "${speed_list_old[@]}")
	improvement=$(node -p "${geo_mean_new}/${geo_mean_old}")
	printf "Geometric mean of speed of new simulator is %6.3f\n" ${geo_mean_new} >&2
	printf "Geometric mean of speed of old simulator is %6.3f\n" ${geo_mean_old} >&2
	printf "x%.3f speed up\n" ${improvement} >&2
	exit 0
fi

exit 1
