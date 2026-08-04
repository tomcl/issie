@echo off
REM Run the Issie test suite and type-check the renderer.
REM
REM This is a convenience wrapper. The suite itself is just:
REM     npm run test          (dotnet run --project Tests/Issie.Tests -c Release)
REM and, much quicker while iterating, one group at a time:
REM     dotnet run --project Tests/Issie.Tests -c Release -- --filter Issie.DrawBlock
REM
REM Neither this script nor CI compiles the #if FABLE_COMPILER branches. If you have changed
REM code inside one, run "npm run compile" as well - nothing else will check it.

setlocal
set FAIL=0

for %%T in (dotnet node npm) do (
    where %%T >nul 2>nul || (echo Error: %%T is not installed. & exit /b 1)
)

echo.
echo == Restoring dependencies
call dotnet tool restore
if %ERRORLEVEL% NEQ 0 set FAIL=1
call dotnet paket restore
if %ERRORLEVEL% EQU 0 (echo ok   dependencies restored) else (echo FAIL dependencies & set FAIL=1)

echo.
echo == Running tests
call dotnet run --project Tests/Issie.Tests -c Release
if %ERRORLEVEL% EQU 0 (echo ok   test suite) else (echo FAIL test suite & set FAIL=1)

echo.
echo == Type checking
call dotnet build src/Renderer/Renderer.fsproj --nologo --verbosity quiet
if %ERRORLEVEL% EQU 0 (echo ok   renderer type checks) else (echo FAIL type check & set FAIL=1)

echo.
if %FAIL% EQU 0 (
    echo All checks passed.
) else (
    echo Something failed - see above.
)
exit /b %FAIL%
