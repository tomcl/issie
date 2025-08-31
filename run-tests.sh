#!/bin/bash

# Issie Test Runner Script
# Run all tests locally with proper setup

set -e

echo "========================================="
echo "      Issie Comprehensive Test Suite     "
echo "========================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✓${NC} $2"
    else
        echo -e "${RED}✗${NC} $2"
    fi
}

# Check prerequisites
echo "Checking prerequisites..."
command -v dotnet >/dev/null 2>&1 || { echo -e "${RED}Error: dotnet CLI is not installed.${NC}" >&2; exit 1; }
command -v node >/dev/null 2>&1 || { echo -e "${RED}Error: Node.js is not installed.${NC}" >&2; exit 1; }
command -v npm >/dev/null 2>&1 || { echo -e "${RED}Error: npm is not installed.${NC}" >&2; exit 1; }

echo -e "${GREEN}✓${NC} All prerequisites installed"
echo ""

# Restore dependencies
echo "Restoring dependencies..."
echo "  - Restoring .NET tools..."
dotnet tool restore
print_status $? "  .NET tools restored"

echo "  - Restoring Paket dependencies..."
dotnet paket restore
print_status $? "  Paket dependencies restored"

echo "  - Installing npm packages..."
npm install --silent
print_status $? "  npm packages installed"
echo ""

# Build the project
echo "Building project..."
echo "  - Building Main..."
dotnet fable src/Main --noCache
print_status $? "  Main built"

echo "  - Building Renderer..."
dotnet fable src/Renderer --noCache
print_status $? "  Renderer built"

echo "  - Building Tests..."
dotnet build Tests/Tests.fsproj --configuration Release --nologo --verbosity quiet
print_status $? "  Tests built"
echo ""

# Run unit tests
echo "Running unit tests..."
dotnet run --project Tests/Tests.fsproj --configuration Release --no-build
TEST_RESULT=$?
print_status $TEST_RESULT "Unit tests completed"
echo ""

# Run type checking
echo "Running type checking..."
dotnet build src/Renderer/Renderer.fsproj --no-restore --nologo --verbosity quiet
print_status $? "Type checking completed"
echo ""

# Run simulator tests if they exist
if [ -d "simulator_tests/js" ]; then
    echo "Running simulator tests..."
    cd simulator_tests/js
    if [ -f "package.json" ]; then
        npm install --silent
        npm test
        print_status $? "Simulator tests completed"
    else
        echo -e "${YELLOW}  Simulator tests skipped (no package.json)${NC}"
    fi
    cd ../..
    echo ""
fi

# Summary
echo "========================================="
echo "           Test Summary                  "
echo "========================================="

if [ $TEST_RESULT -eq 0 ]; then
    echo -e "${GREEN}All tests passed successfully!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed. Please check the output above.${NC}"
    exit 1
fi