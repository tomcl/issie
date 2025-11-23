# Issie Source Code Overview

The Issie application comprises F# modules under [./src/renderer](https://github.com/tomcl/issie/tree/master/src/Renderer) which make the Electron renderer process. The Electron main process [./src/main/](https://github.com/tomcl/issie/blob/master/src/Main/Main.fs) is very rarely changed and performs startup and some low-level machine I/O functions.

Issie is an **Elmish** MVU (Model-View-Update) application. 

* For an introduction to Elmish see [The Elmish Book](https://zaid-ajaj.github.io/the-elmish-book/#/). 
* For an overview of the Issie Elmish model record see [The Elmish Structure](https://github.com/tomcl/issie/wiki/1---Introduction-to-Issie-Elmish-Structure) page.

The sections below describe all of the Issie renderer modules by source directory. Source directories correspond to logically related modules.

## Naming Conventions

Many UI and Drawblock logical modules are too large to be handled by a single source file. They are split into multiple F# modules, each in a separate file. These sets of modules form a *module group*. Module group naming is fairly standard, e.g. module group `MyGrp` can consist of modules `MyGrpHelpers`,`MyGrpView`,`MyGrpUpdate`. Where not needed these may be omitted, where code is too large for this split modules are further separated - with all modules for group `MyGrp` starting with `MyGrp`.

An F# submodule `Constants` may be inserted at the start of any of these modules to contain module-local constant parameters. See [Buswire.fs](https://github.com/tomcl/issie/blob/31f4383e6660ad45969ad0f1eb9b67da45bb8722/src/Renderer/DrawBlock/BusWire.fs#L27) for an example. `[<AutoOpen>]` property is optional but good practice, it allows constants to be accessed without the submodule name prefix `Constants`.

|Standard Naming | Function | F# compile order within group|
|------------------|-------------|-----------|
| GroupHelpers | Helper functions for group | 1 |
| GroupView | Elmish view function, and associated subfunctions, for group| 2|
| GroupUpdate | Elmish update function, and associated subfunctions, for group | 3|

## Libraries

|Directory|File & Module | Contents|
|----------|-------------------|--------------|
./src/Common | CommonTypes | Issie types for connections, components, etc|
./src/Common|DrawHelpers| Helper functions used for SVG and geometry in schematic editor and elsewhere|
./src/DrawBlock | BlockHelpers | Helper functions for DrawBlock with scope more than one DrawBlock module
./src/Common| EEEHelpers| legacy helpers, should be refactored
./src/Common| EEExtensions | standard F# library extensions used by Issie and other apps - e.g. FP ways to use Regexes.
./src/Common| ElectronAPI | auto-generated interface used to get static typed access to the (javascript) electronAPI. Shows all the electron API calls - it can be linked to the [electron API](https://www.electronjs.org/docs/latest/README)
./src/Common|HashMap | Not currently used - legacy |
./src/Common| Helpers| Misc helper functions|
./src/Common| Optics | [standard functions](https://github.com/tomcl/issie/wiki/Optics:-Lenses-and-prisms) for FP read and write of structures|
./src/Common| TimeHelpers| Functions to instrument how long things take |
./src/Common| WidthInferror| Issie bus width inference algorithm (applies to a whole design sheet)|
./src/Interface | FilesIO | Low-level IO functions|
./src/Interface |JSHelpers| Low-level javascript helpers (not serialise/deserialise) |
./src/Interface| Version| Version number!|

## Draw Block

The **symbol**, **buswire**, **sheet** module groups implement the Draw Block AKA Schematic Editor.

See [separate wiki page](https://github.com/tomcl/issie/wiki/Draw%E2%80%90Block) for details.


|Module group | Module | Contents | 
|------------------|-------------|-----------|


## Simulator

Functions to implement Issie simulator types and legacy code. The key type is `SimulationGraph` which was originally used to simulate circuits but now is an intermediate type used for convenience (it is not very convenient so this is technical debt) when constructing a `FastSimulation`.

See [the Simulation wiki page](https://github.com/tomcl/issie/wiki/Simulation) for more details.

## Simulator/Fast

The main (new) and very fast simulator.

* **FastCreate**. Functions to transform a simulatable circuit into a `FastSimulation` structure in which each Component becomes a `FastComponent`. 
    * Custom components are not transformed. Instead the component's hardware (copies of the components on the relevant sheet) are inserted.
    * `FastComponent` records have an arrays for each output which hold simulation values indexed by timestep. Each *input* is then linked to the 
array of the output that drives it. 
    * Output (and input) data digital can be represented by `uint32` (busses <= 32 bits) or `bigint` (longer busses). These arrays are contained in an `IOArray` record with fields for `bigint` and `int32` arrays. For a given simulation only one of these slots will have a non-zero size array and be used.
 * **FastReduce**. 
    * This contains the large `match` expression which, when given a component and its inputs, calculates the corresponding outputs for the next timestep (a process called reduction). 
    * Some components (e.g. flipflops) contain *State* which is updated each timestep.
*  **FastRun**. 
    * This contains the code which runs the simulation. 
    * Before the simulation start, ordered lists of `FastComponent` records are created so that components can be reduced in an order that ensures inputs have already been generated before a component is reduced.
    * The `FastSimulation` can be *run* for any number of timesteps. The `IOArrays` will hold the simulation results of the previous N timesteps where N is  e.g. `SimulationView.Constants.FastSimulationArraySize` for the Step simulator. Thus for long simulations the arrays act as *circular buffers*.
    * After having been run, simulation data for timestep currently held in the `IOArrays` can be retrieved from the `FastSimulation`.

See the [Fast Simulation Section](https://github.com/tomcl/issie/wiki/Simulation#fast-simulation) of the Simulation Wiki page for more details. See [Yujie Wang's dissertation](https://github.com/tomcl/issie/blob/master/docsrc/1714652_Rpt_A%20high%20performance%20digital%20circuit%20simulator%20for%20ISSIE.pdf) for more details.


## UARTFiles

Files to implement separate `IssieStick` Interface to download verilog to hardware FPGA

## VerilogComponent

Files to implement an integrated Verilog editor with synthesis to an Issie design sheet. See [Petra Ratkai's dissertation](https://github.com/tomcl/issie/blob/master/docsrc/1572747_Rpt_A%20Verilog%20Compiler%20for%20Issie.pdf) for a detailed guide to the Verilog compiler. See also the [Verolog Component UI](https://github.com/tomcl/issie/wiki/Verilog-Component). NB - this should be updated.

|Module group | Module | Contents | 
|------------------|-------------|-----------|

## UI

Various elements of the UI - corresponding to specific parts of the app visible screen. [See the the Simulation UI page](https://github.com/tomcl/issie/wiki/Simulation-UI)

| Module | Contents | 
|-------------|-----------|

## UI/WaveSim

Subset of UI files that implement the *waveform simulator* waveform selector and display.

|Module group | Module | Contents | 
|------------------|-------------|-----------|

## UI/TruthTable

Subset of UI files that implement truth tables

|Module group | Module | Contents | 
|------------------|-------------|-----------|