#!/bin/bash

# Run the Issie test suite and type-check the renderer.
#
# This is a convenience wrapper. The suite itself is just:
#     npm run test          (dotnet run --project Tests/Issie.Tests -c Release)
# and, much quicker while iterating, one group at a time:
#     dotnet run --project Tests/Issie.Tests -c Release -- --filter Issie.DrawBlock
#
# Neither this script nor CI compiles the #if FABLE_COMPILER branches. If you have changed code
# inside one, run `npm run compile` as well - nothing else will check it.

set -u

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

fail=0

step() { echo ""; echo "== $1"; }

report() {
    if [ "$1" -eq 0 ]; then
        echo -e "${GREEN}ok${NC}   $2"
    else
        echo -e "${RED}FAIL${NC} $2"
        fail=1
    fi
}

for tool in dotnet node npm; do
    command -v $tool >/dev/null 2>&1 || { echo -e "${RED}$tool is not installed.${NC}" >&2; exit 1; }
done

step "Restoring dependencies"
dotnet tool restore && dotnet paket restore
report $? "dependencies restored"

step "Running tests"
dotnet run --project Tests/Issie.Tests -c Release
report $? "test suite"

step "Type checking"
dotnet build src/Renderer/Renderer.fsproj --nologo --verbosity quiet
report $? "renderer type checks"

echo ""
if [ $fail -eq 0 ]; then
    echo -e "${GREEN}All checks passed.${NC}"
else
    echo -e "${RED}Something failed - see above.${NC}"
fi
exit $fail
