NODE_OPTIONS=()
NODE_OPTIONS+="--enable-source-maps" # Enable JS to F# source maps
# NODE_OPTIONS+="--allow-natives-syntax" # Enable V8 natives syntax

# NODE_OPTIONS += "--cpu-prof" # Enable CPU profiling
# NODE_OPTIONS += "--cpu-prof-interval=1" # Set CPU profiling interval to 1ms
# NODE_OPTIONS += "--cpu-prof-dir='cpu-profiles'" # Set CPU profiling output directory
#
# NODE_OPTIONS += "--heap-prof" # Enable HEAP profiling
# NODE_OPTIONS += "--heap-prof-interval=1" # Set HEAP profiling interval to 1ms
# NODE_OPTIONS += "--heap-prof-dir='heap-profiles'" # Set HEAP profiling output directory

JS_SCRIPT="index.js"
SIMULATION_ARRAY_SIZE=500
SIMULATION_STEPS=2000
TEST_CASES_DIR=${1:-"../testcases"}
TEST_CASE=${2:-0}

# 0             addsub
# 1          aludecode
# 2                alu
# 3               cond
# 4      controldecode
# 5        controlpath
# 6           datapath
# 7           dpdecode
# 8               eep1
# 9              eep1x
# 10            extend
# 11  externalhardware
# 12         intdecode
# 13         memwithio
# 14              next
# 15             nzgen
# 16           reg16x8
# 17            shift1
# 18            shift2
# 19            shift4
# 20            shift8
# 21             shift

export NODE_TRACE=1

node "${NODE_OPTIONS[@]}" "${JS_SCRIPT}" "${TEST_CASE}" "${SIMULATION_ARRAY_SIZE}" "${SIMULATION_STEPS}" "${TEST_CASES_DIR}" |
	tail -n 1
