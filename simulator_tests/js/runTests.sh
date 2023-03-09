#!/usr/bin/env bash

set -e

SIMULATION_ARRAY_SIZE=500
LAST_STEP_NEEDED=2000
TEST_CASES_PATH="../testcases"
# TIMEOUT=null

test_cases=($(ls ${TEST_CASES_PATH}/*.dgm))

function geometric_mean() {
	node -p "Math.sqrt([$(printf '%s,' ${@})].reduce((acc,v)=>acc*v,1),${#test_cases[@]})"
}

printf "%15s, %6s, %11s, %6s, %37s, %37s\n" "Test Case" "Result" "#Components" "#Steps" "Speed of New simulator (comp*step/ms)" "Speed of Old simulator (comp*step/ms)"

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
		printf "%15s, %6s, %11d, %6d, %37.3f, %37.3f\n" ${test_cases_name} "PASS" ${num_comps} ${LAST_STEP_NEEDED} ${speed_new} ${speed_old}
		speed_list_new+=($speed_new)
		speed_list_old+=($speed_old)
		passed_test=$((passed_test + 1))
	else
		printf "%15s, %6s, , ," ${test_cases_name} "FAIL"
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
