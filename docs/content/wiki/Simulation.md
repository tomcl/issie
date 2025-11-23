## Introduction

Issie simulation contains some legacy technical debt because the original simulation method has been replaced by a completely different one.

See [Yujie Wang's dissertation](https://github.com/tomcl/issie/blob/master/docsrc/1714652_Rpt_A%20high%20performance%20digital%20circuit%20simulator%20for%20ISSIE.pdf) for more details.

### Original Method

Issie sheets are recursively examined and turned into `SimulationGraph` datastructures each of which contains one `SimulationComponent` for each corresponding `Component`. Custom component instances of a given component have the underlying sheet graph shared (not copied) as a `SimulationGraph`. The resulting large linked `SimulationGraph` is then simulated by recursively pulling values required for each input from the corresponding connected output, evaluating components as needed to obtain values. Sequential logic is implemented by holding current state in the component definition and having a `feedClock` operation that updates state to its correct value for the next cycle.

This method of simulation has the dubious merit of being purely functional, with no immutable data. It is slow, and does not correctly deal with sheets that combine combinational and clokced outputs in sufficiently complex ways.

Therefore this method was abandoned and is no longer used. However, `SimulationGraph` and `SimulationComponent` structures are still generated and used as an intermediate step in implementing the new *Fast Simulation*.

### Fast Simulation

The current simulation method is *much* faster - probably close to optimal - and conceptually simpler. It consists of the following steps:

1. Turn circuit into a `SimulationGraph` (for convenience)
2. Turn `SimulationGraph` into a sea of interconnected `FastSimulation` components, removing sheets boundaries.
3. Order components linearly so that evalutation can proceed with all inputs guaranteed to be already evaluated
4. Simulate filling up arrays of time-step data for one for each circuit node. The simulation object is very large and contains simulation results for (a large number of time steps) of time.
   * To allow arbitrary-length simulation these arrays are managed as circular buffers so that data from the last simulationArraySize simulation steps only is available.
   * The simulation results are cached so that they are only recomputed when this is necessary and do not normally make the view function very low.

### Simulation Functions

The simulator is used in several different contexts:
* Step simulation (combinational + advance n steps)
* Wave simulation (return simulation results for 0 - n steps)
* Truth table simulation (combinational only, but will simulate an arbitrary top-level circuit - this need not be the whole of the current open sheet)
* Verilog output - used minimally to determine the bus widths of every component - in a few cases these are not fixed in the design and must be determined by propagating from the driving component (eventually a known-width component must be found). Currently these widths are determined by propagating known widths dynamically through the simulator. Combinational propagation is enough since all clocked components have fixed widths. TODO: add a pre-simulation phase to do this propagation. Then simulation itself can operate with all widths known.

The simulator is accessed via many layers of functions which show some technical debt.

* makeSimData or makeSimDataSelected - thin wrapper round prepareSimulationMemoised). Is it needed?
* prepareSimulationMemoised - same as prepareSimulation but caches last simulation for performance reasons
* prepareSimulation - takes current canvasState and loadedComponents and calls buildFastSimulation to simulate current sheet (or subset of), or else returns error message. Has parameter to determine array size - of interest to waveform simulator which (currently) needs to keep track of all simulation history. (To be changed).
* runFastSimulation - runs a built simulation from 0 up to required number of steps. Note that this will only need to run the simulation if teh required steps have not already been calculated, so calling this twice with same number is very fast.  The simulation structure is mutable and holds up to simulationArraySize steps of history. Running beyond this deletes older historic steps.
* buildFastSimulation - main function creating a new simulation with given top sheet (the current sheet, or a subset thereof)
* createEmptySimulation - creates an empty (not runnable yet) fast simulation container. Used outside of simulator only to construct a placeholder - a blank simulation tagged with TopSheet = "" to allow data structures for convenience to hold a simulation rather than a simulation option. This is a bit lazy.

**Verilog**

Calls prepareSimulation (no cache needed since this is invoked as part of a one-off long synthesis chain)

**TruthTableView**

Calls makeSimDataSelected -> prepareSimulation (with internal cache). This is makeSimData but modified so that only part of open sheet needs to be simulated.

**SimulationView.**

ViewSimulation calls both prepareSimulationMemoised and makeSimData

**WaveSimHelpers**

refreshButtonAction  calls makeSimData



