$NODE_OPTIONS = @()
$NODE_OPTIONS += "--enable-source-maps" # Enable JS to F# source maps
# $NODE_OPTIONS += "--cpu-prof" # Enable CPU profiling
# $NODE_OPTIONS += "--cpu-prof-interval=1" # Set CPU profiling interval to 1ms
# $NODE_OPTIONS += "--cpu-prof-dir='cpu-profiles'" # Set CPU profiling output directory
$NODE_OPTIONS = [System.String]::Join(' ', $NODE_OPTIONS)

$JS_SCRIPT = "index.js"
$SIMULATION_ARRAY_SIZE = 500
$SIMULATION_STEPS = 2000
$TEST_CASE_DIR = "C:\Users\yujie\Documents\EEE1labs\DECA\Part3-Section1-Lab3\lab3i"
$TEST_CASE = 0

# $TRACE = "`$env:NODE_TRACE = 1 ;"
$FILTER_STDERR = "2>`$null"


Invoke-Expression -Command "$TRACE node.exe $NODE_OPTIONS $JS_SCRIPT $TEST_CASE $SIMULATION_ARRAY_SIZE $SIMULATION_STEPS $TEST_CASE_DIR $FILTER_STDERR | Select-Object -SkipLast 1"

$env:NODE_TRACE = ''