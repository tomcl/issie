# Team 1 README
Patrick Beart, Jongmin Choi, Chang Liu, Anlan Qiu, Dhruv Devgan-Sharma and Kishan Vijayarajah

## Wiki is at https://github.com/kishan-v/issie-team1/wiki/

## Beautify function invocation
The beautify function is invoked as a function taking a `ModelType.Model`. It calls each beautification step (D1-3) on the sheet model in an appropriate order and returns the updated `ModelType.Model`.

It is called (in normal ISSIE operation) in `UpdateHelpers.fs` line 558 (in the ` "Reroute all wires"` case inside `processContextMenuClick`), so that beautification is performed as well as fixing of broken segments.

## List of which modules your code is in.

## Statement of anything that has changed in repo since demo and why.

Full integration of D3, bug fix to D1. Finished testing D2.

## Statement of anything important to be considered about functionality not shown in demo.
todo

## Rationale for any existing source files in Issie that your code changes other than the expected ones: TestDrawBlock, SheetBeautify, SheetBeautifyHelpers, any module you add.
- Changed `UpdateHelpers.fs` to add context menu beautification