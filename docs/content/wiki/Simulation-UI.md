# Overview

The user interface controls the generation of elements, notably the results of the wave simulation and the truth tables. Key files include:
* `MainView`,`ModelType` and `ModelHelpers`: top-level design of Issie UI.
* `WaveSim`: Corresponds to the Wave Simulator, displaying values to an appropriate number of clock cycles as scaled on the screen, and the values for the highlighted clock cycle. Also includes functions for updating waves as the simulation is extended.
* `TruthTable`: Generates truth table based on function input. Functions to reduce truth tables on don't cares and common outputs are present.
* `CatalogueView`: Describes the right-hand-side menu together with `SelectedComponentView`.
*  `CustomCompPorts`: Manages the presence of custom components and how the I/O of a constituent component changing may affect the top-level design, while highlighting new bad connections as required.
* `MemoryEditorView`: Allows the editing of memory elements (e.g. RAM)

# Step Simulation

The Step Simulator can simulate both combinational and clocked logic. It is contained in [UI/SimulationView.fs](https://github.com/tomcl/issie/blob/master/src/Renderer/UI/SimulationView.fs).

# Truth Tables

The files within the `Simulation/TruthTables` folder seeks to generate a truth table and then subsequently simplify it as required, due to the presence of Don't Cares. It also allows for the creation of truth tables from a subset of the components and the resolution of incomplete connections as a result. 

## Key Operators

* `CorrectCanvasState` within `TruthTableView.fs` adds relevant connections and IOs for components which are not connected to anything by the creation of dummy IO ports and connections. The width is used in conjunction with `BusWidthInferrer`
* `makeSimDataSelected` provides a simulation view for selected elements
* Operators in `TruthTableReduce.fs` help to reduce the size of the truth table based on don't cares, with `reduceTruthTable'` recursively reducing the truth table until no further changes can be made.
* `TruthTableCreate.fs` creates the truth table by partitioning items into numeric and algebraic inputs, which is then fast simulated (the input values in the fastSimulation are mutable) to get the output values on the RHS.

# Wave Simulation

The purpose of the wave simulator is to produce a SVG that is based on the simulation currently running, with the user having a choice of what waves they want to see. This is not just true for waves for electrical components (e.g. counters and logic gates that are clocked by a FF), but also true of how memory is managed in the simulation. For example, if a series of instructions is stored within RAM as a series of hexadecimal numbers, the user can view both the executing instruction and the impact of the instruction on the selected waveforms. The user will also be able to see the effect of instructions on values stored in memory. 

## Simulation Mechanism
* Waveforms consist of many rows (depending on what the user selects) of three columns, the three columns being the label of the waveform, the waveform, and the associated value of the waveform at a given clock cycle. This is given by `nameRows`, `waveRows` and `valueRows` within `WaveSim.fs`.
* `SelectedWaves` has to be called before the three columns are generated.
* If possible, text elements corresponding to the values in the waveforms will be shown
* `refreshWaveSim` is a recursive function that generates waves for extended wave simulations. It can only be called if there has been an initial simulation done. The extension of simulation, if any, is estimated by the length of the initial simulation, and a progress bar created if needed.
* `getWaveValues` in `WaveSimHelpers.fs` obtains the values of waves at each clock cycle as an `int`. The data is stored (with improvements from the FYP) as typed arrays.

## Wave Selection
* The choice of wave depends on the component label, port name, and bit width of wave.
* Functions are present in `WaveSimSelect.fs` that gets names for waves from input and output ports as a subset of all waves that are able to be simulated.
