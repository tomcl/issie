# Issie - yc3821

N.B. My part in team phase is T1.
## Individual Phase Work

### 1. SheetBeautifyHelpers.fs
- Completed B1-B8
- Competed T1-T6
- Added a small custom helper to that gets visible segments
- T1-T6 have been tested and are working as expected

### 2. RotateScale.fs Improvement
I am assigned line 131-194 when splitting the work among the team. Below is a summary of the improvements:
- Added DU to group pairs of variables
- Extracted a helper function to improve readability
- Improved variable names
- Added pipeline operator to improve readability

### 3. TestDrawBlock.fs
I wrote this file such that it would provide a strong starting point for T1.
Right now I have hooked up a function, that runs upon pressing Ctrl + 1. Below is a summary of what I have done:
- Deleted unnecessary codes from tick3
- Used SheetBeautifyHelpers.fs to get test metrics into a record type
- Can perform test metric analysis on current sheet, and print result to developer console.
