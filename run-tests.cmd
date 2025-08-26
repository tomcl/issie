@echo off
REM Issie Test Runner Script for Windows
REM Run all tests locally with proper setup

echo =========================================
echo       Issie Comprehensive Test Suite     
echo =========================================
echo.

REM Check prerequisites
echo Checking prerequisites...
where dotnet >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo Error: dotnet CLI is not installed.
    exit /b 1
)

where node >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo Error: Node.js is not installed.
    exit /b 1
)

where npm >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo Error: npm is not installed.
    exit /b 1
)

echo [OK] All prerequisites installed
echo.

REM Restore dependencies
echo Restoring dependencies...
echo   - Restoring .NET tools...
call dotnet tool restore
if %ERRORLEVEL% EQU 0 (
    echo   [OK] .NET tools restored
) else (
    echo   [FAIL] .NET tools restoration failed
)

echo   - Restoring Paket dependencies...
call dotnet paket restore
if %ERRORLEVEL% EQU 0 (
    echo   [OK] Paket dependencies restored
) else (
    echo   [FAIL] Paket restoration failed
)

echo   - Installing npm packages...
call npm install --silent
if %ERRORLEVEL% EQU 0 (
    echo   [OK] npm packages installed
) else (
    echo   [FAIL] npm installation failed
)
echo.

REM Build the project
echo Building project...
echo   - Building Main...
call dotnet fable src/Main --noCache
if %ERRORLEVEL% EQU 0 (
    echo   [OK] Main built
) else (
    echo   [FAIL] Main build failed
)

echo   - Building Renderer...
call dotnet fable src/Renderer --noCache
if %ERRORLEVEL% EQU 0 (
    echo   [OK] Renderer built
) else (
    echo   [FAIL] Renderer build failed
)

echo   - Building Tests...
call dotnet build Tests/Tests.fsproj --configuration Release --nologo --verbosity quiet
if %ERRORLEVEL% EQU 0 (
    echo   [OK] Tests built
) else (
    echo   [FAIL] Test build failed
)
echo.

REM Run unit tests
echo Running unit tests...
call dotnet run --project Tests/Tests.fsproj --configuration Release --no-build
set TEST_RESULT=%ERRORLEVEL%
if %TEST_RESULT% EQU 0 (
    echo [OK] Unit tests completed
) else (
    echo [FAIL] Unit tests failed
)
echo.

REM Run type checking
echo Running type checking...
call dotnet build src/Renderer/Renderer.fsproj --no-restore --nologo --verbosity quiet
if %ERRORLEVEL% EQU 0 (
    echo [OK] Type checking completed
) else (
    echo [FAIL] Type checking failed
)
echo.

REM Run simulator tests if they exist
if exist simulator_tests\js (
    echo Running simulator tests...
    cd simulator_tests\js
    if exist package.json (
        call npm install --silent
        call npm test
        if %ERRORLEVEL% EQU 0 (
            echo [OK] Simulator tests completed
        ) else (
            echo [FAIL] Simulator tests failed
        )
    ) else (
        echo   Simulator tests skipped - no package.json
    )
    cd ..\..
    echo.
)

REM Summary
echo =========================================
echo            Test Summary                  
echo =========================================

if %TEST_RESULT% EQU 0 (
    echo All tests passed successfully!
    exit /b 0
) else (
    echo Some tests failed. Please check the output above.
    exit /b 1
)