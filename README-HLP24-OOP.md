# Issie Team Submission - OOP
Members:
* Arnav Kohli (sk1421)
* Ana Dimoska (ad2121)
* Gavin Vasandani (gv220)
* Sohailul Islam Alvi (aia221)
* Samuel Wang (sw2521)
* Bharathaan Sukumaran (bs1121)



This is the documentation for the team submission of team OOP for the HLP 2024 Project Team Phase. For a brief overview on the module submissions from our team please refer to our [Team Wiki](https://github.com/samuelpswang/oop-hlp24/wiki "HLP24-OOP Wiki").

## Invoking and Testing Beautify Functions
All beautification functions can be tested from the ISSIE menu. For more information about the tests please see [Team Wiki](https://github.com/samuelpswang/oop-hlp24/wiki "HLP24-OOP Wiki").

Menu Tests For D1 & D3:

* `CmdOrCtrl+1`: Invokes test for `sheetOrderFlip`, `sheetAlignScale` and `sheetWireLabelSymbol` incrementally
* `CmdOrCtrl+2`: Invokes `testD3EasyIndiv`. It's a set of tests to evaluate D3 designed using DSL with given components and connections list
* `CmdOrCtrl+3`: Invokes `testD3Easy`. A set of easy tests to evaluate D3
* `CmdOrCtrl+4`: Invokes `testD3Hard`. A set of hard tests to evaluate D3
* `CmdOrCtrl+5`: Invokes `multiConnect`. A test case to present custom scaling and alignment of gates
* `CmdOrCtrl+6`: Invokes `tripleMUX`. A test case to present the realignment of triple mux circuit with multiple degrees of freedom
* `CmdOrCtrl+7`: Invokes `customComponents`. A test case to present the scaling and realignment of custom components
* `CmdOrCtrl+8`: Invokes `andGate`. A test case to present the realignment of rotated inputs with gate when possible
* `CmdOrCtrl+9`: Invoke next beautification/ error case

Menu Tests For D2:
* `Shift+4` : Invokes a set of test cases on a fixes circuit to test `sheetOrderFlip`
* `Shift+5` : Invokes a set of test cases on a randomly generated circuit to test `sheetOrderFlip`
*`Shift+9` : Invoke next error case

## List of Modules
* [TestDrawBlock.fs](./src/Renderer/TestDrawBlock.fs)
* [TestDrawBlockD1.fs](./src/Renderer/TestDrawBlockD1.fs)
* [TestDrawBlockD2.fs](./src/Renderer/TestDrawBlockD2.fs)
* [TestDrawBlockD3.fs](./src/Renderer/TestDrawBlockD3.fs)
* [Renderer.fs](./src/Renderer/Renderer.fs)
* [SheetBeautify.fs](./src/Renderer/DrawBlock/SheetBeautify.fs)
* [SheetBeautifyD1.fs](./src/Renderer/DrawBlock/SheetBeautifyD1.fs)
* [SheetBeautifyD2.fs](./src/Renderer/DrawBlock/SheetBeautifyD2.fs)
* [SheetBeautifyD3.fs](./src/Renderer/DrawBlock/SheetBeautifyD3.fs)
* [RotateScale.fs](./src/Renderer/DrawBlock/RotateScale.fs)
## Changes made after demo
* Minor bug fixes to `TestDrawBlockD2`
* Added test cases to show beautfication applied to sheet sequentially
* Minor fixes and improvements to D1 tests
<!-- ## Additional changes made within ISSIE codebase -->

