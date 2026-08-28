//(*
//    SimulationView.fs
//
//    View for simulation in the right tab.
//*)
//
module SimulationView

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props
open Elmish

open NumberHelpers
open Helpers
open TimeHelpers
open JSHelpers
open DiagramStyle
open Notifications
open PopupHelpers
open MemoryEditorView
open ModelType
open CommonTypes
open SimGraphTypes
open SimTypes
open CanvasExtractor
open Simulator
open Sheet.SheetInterface
open DrawModelType
open ModelHelpers

open Optics
open Optics.Optic
open Optics.Operators

module Constants =
    let maxArraySize = 550
    let boxMaxChars = 34
    let ncPortDist = 30.

/// Write one sheet, and everything below it, as a Verilog file beside the project.
///
/// The sheet is a parameter rather than always the open one, so any sheet can be written from its
/// pill in the sheet menu. Verilog output was already rooted at a single sheet and flattened over
/// its subtree - buildFastSimulation inlines custom components, and getVerilog emits one module
/// called main - so this only makes explicit which sheet is the root.
///
/// TODO: the simulation error display here is shared with step simulation and also waveform simulation -
/// maybe it should be a subfunction.
let verilogOutputForSheet (sheetName: string) (vType: Verilog.VMode) (model: Model) (dispatch: Msg -> Unit) =
    match MenuHelpers.updateProjectFromCanvas model dispatch with
        | Some proj ->
            match model.UIState with  //TODO should this be its own UI operation?
            | Some _ ->
                () // do nothing if in middle of I/O operation
            | None ->
                // updateProjectFromCanvas has just refreshed the open sheet's saved state from the
                // canvas, so reading the chosen sheet's LoadedComponent is current whether or not
                // it is the sheet on screen.
                match proj.LoadedComponents |> List.tryFind (fun lc -> lc.Name = sheetName) with
                | None ->
                    Error $"Cannot write Verilog: there is no sheet called '{sheetName}' in this project."
                    |> Notifications.displayAlertOnError dispatch
                | Some ldc ->
                startCircuitSimulation 2 sheetName ldc.CanvasState proj.LoadedComponents
                |> (function
                    | Ok sim ->
                        let path = FilesIO.pathJoin [| proj.ProjectPath; sheetName + ".v" |]
                        try
                            let code = (Verilog.getVerilog vType sim.FastSim Verilog.CompilationProfile.Release)
                            FilesIO.writeFile path code
                        with
                        | e ->
                            Log.error $"generating Verilog output: {e.Message}"
                            Error e.Message
                        |> Notifications.displayAlertOnError dispatch
                        dispatch <| ChangeRightTab Simulation
                        let note = successSimulationNotification $"verilog output written to file {path}"
                        dispatch  <| SetSimulationNotification note
                    | Error simError ->
                       Log.error $"simulation error prevents Verilog output: {(errMsg simError.ErrType)}"
                       dispatch <| ChangeRightTab Simulation
                       // Highlight the affected components and connections only when they are on
                       // the sheet being displayed: the error may be in a sheet the user cannot
                       // see, in which case highlighting would mark unrelated components.
                       if simError.InDependency.IsNone && sheetName = proj.OpenFileName then
                           (simError.ComponentsAffected, simError.ConnectionsAffected)
                           |> SetHighlighted |> dispatch
                       Error simError
                       |> StartSimulation
                       |> dispatch)
        | _ -> () // do nothing if no project is loaded

let setFastSimInputsToDefault (fs:FastSimulation) =
    fs.FCompsByIndex
    |> Array.filter (fun fc -> fc.AccessPath = [] && match fc.FType with | Input1 _ -> true | _ -> false)
    |> Array.map (fun fc -> fc.cId, (fc.cId, match fc.FType with | Input1 (w,defVal) -> (w,defVal) | _ -> failwithf "What? Impossible"))
    |> Array.toList
    |> List.map (fun ( _, (cid, (w,defaultVal ))) -> 
        match w,defaultVal with
        | _, Some defaultVal -> cid, convertBigintToFastData w defaultVal
        | _, None -> cid, convertBigintToFastData w 0I)
    |> List.iter (fun (cid, wire) -> FastExtract.changeInput cid (FSInterface.IData wire) 0 fs)

/// The top sheet's components of one kind, as DRAWN, in canvas order - what the step panel's
/// rows are derived from. A fact about the design: the panel shows top-level components only, so
/// no instance walk is needed, and the one thing the design cannot say - each port's width - comes
/// from the top instance's port slice like every other width.
let private topComponentsWhere (chosen: Component -> bool) (fs: FastSimulation) =
    fs.Design.DesignSheets
    |> List.tryFind (fun ldc -> ldc.Name = fs.Design.DesignTopSheet)
    |> Option.map (fun ldc -> fst ldc.CanvasState |> List.filter chosen)
    |> Option.defaultValue []

/// The panel's register rows: top-level clocked components that are signals - a memory's contents
/// are not on a wire and get their own rows.
let private topRegisters (fs: FastSimulation) =
    fs
    |> topComponentsWhere (fun comp ->
        isClockedPrimitive comp.Type
        && (match comp.Type with
            | RAM1 _
            | AsyncRAM1 _
            | ROM1 _ -> false
            | _ -> true))

let private topLevelInputs (fs: FastSimulation) =
    fs
    |> topComponentsWhere (fun comp ->
        match comp.Type with
        | Input1 _ -> true
        | _ -> false)

/// The value one top-level input holds at a clock, from whichever simulator is running.
///
/// The renderer's own simulation is built for its structure and never run when the .NET simulator
/// is the one simulating, so reading its arrays would say every input is zero - and these three
/// functions decide whether the inputs on screen match the defaults saved in the sheet, and write
/// those defaults. Getting that wrong saves zeros over what the user set. In sidecar mode the
/// value comes from the panel snapshot, which holds exactly these signals for exactly this clock.
let private inputValueAt (model: Model) (fs: FastSimulation) (comp: Component) (tick: int) =
    if model.SimulateInRenderer then
        match FastExtract.extractFastSimulationOutput fs tick (comp.Id, []) (OutputPortNumber 0) with
        | IData fd -> fd.GetBigInt
        | IAlg _ -> 0I
    else
        StepPanelData.valueAt tick { Comp = componentIdValue comp.Id; Path = []; Port = 0 }
        |> Option.defaultValue 0I

let InputDefaultsEqualInputs fs (model:Model) (clocktick : int)=
    let tick = clocktick

    topLevelInputs fs
    |> List.forall (fun comp ->
        let cid = comp.Id

        if Map.containsKey cid (Optic.get SheetT.symbols_ model.Sheet) then
            let newDefault = inputValueAt model fs comp tick
            let typ = (Optic.get (SheetT.symbolOf_ cid) model.Sheet).Component.Type

            match typ with
            | Input1(_, Some d) -> d = newDefault
            | _ -> newDefault = 0I
        else
            true)

/// Whether Refresh can rebuild without asking: have the inputs held their default values for the
/// whole run so far, so that restarting loses nothing the user set?
///
/// With the .NET simulator that question cannot be asked here - the whole run is in the sidecar,
/// and this process's arrays were never written - so the one it CAN ask is asked instead: do the
/// inputs match their defaults at the clock on screen. The two differ only for an input that was
/// changed and then put back, where this says nothing was changed and Refresh does not stop to
/// ask. Reading the run's history over the wire is what would give the exact question back.
let InputDefaultsEqualInputsRefresh fs (model:Model) (clocktick: int) =
    if not model.SimulateInRenderer then InputDefaultsEqualInputs fs model clocktick else
    let tick = fs.ClockTick
    fs.FCompsByIndex
    |> Array.filter (fun fc -> fc.AccessPath = [] && match fc.FType with | Input1 _ -> true | _ -> false)
    |> Array.map (fun fc ->
        let cid = fc.cId
        if Map.containsKey cid (Optic.get SheetT.symbols_ model.Sheet) then
            let typ = (Optic.get (SheetT.symbolOf_ cid) model.Sheet).Component.Type
            let currdefault = match typ with
                                    | Input1(_, Some d) -> d
                                    | _ -> 0I
            FastExtract.outputsAreTheSameAsDefault fs fc tick currdefault
        else
            true)
    |> Seq.forall id


let setInputDefaultsFromInputs (model: Model) fs (dispatch: Msg -> Unit) (clocktick: int) =
    let setInputDefault (newDefault: bigint) (sym: SymbolT.Symbol) =
        let comp = sym.Component
        let comp' = 
            let ct =
                match comp.Type with 
                | Input1(w,defVal) -> Input1(w,Some newDefault)
                | x -> x
            {comp with Type = ct}
        {sym with Component = comp'}
    let tick = clocktick

    topLevelInputs fs
    |> List.iter (fun comp ->
        let cid = comp.Id
        let newDefault = inputValueAt model fs comp tick

        SymbolUpdate.updateSymbol (setInputDefault newDefault) cid
        |> Optic.map DrawModelType.SheetT.symbol_
        |> Optic.map ModelType.sheet_
        |> UpdateModel
        |> dispatch)



let changeBase dispatch numBase = numBase |> SetSimulationBase |> dispatch

/// A line that can be used for an input, an output, or a state.
let private splittedLine leftContent rightConent =
    Level.level [Level.Level.Props [Style [MarginBottom "10px"]]] [
        Level.left [] [
            Level.item [] [ leftContent ]
        ]
        Level.right [] [
            Level.item [] [ rightConent ]
        ]
    ]

//--------------------------------------------------------------------------------------//
//----------The step panel, from whichever simulator is running--------------------------//
//--------------------------------------------------------------------------------------//
//
// The step simulator used to run in the renderer whatever Model.SimulateInRenderer said - nothing
// in this file consulted it - so "Run to clock N" on a large design was V8's job even with the
// .NET simulator selected, and the components/ms the progress bar quotes was a V8 number.
//
// What changes here is only WHERE the work happens. The panel shows one cycle of one simulation,
// and every value on it is a component OUTPUT: the top-level inputs and outputs, the viewers
// wherever they sit in the hierarchy, and the registers and counters, whose state IS Outputs[0].
// So one SimRead names all of them and one reply fills the panel. The renderer still builds its
// own simulation for STRUCTURE - which components exist, their labels and widths - and in sidecar
// mode never runs it.
//
// Two things this does not move, both deliberate:
//   - a RAM's contents, which are in the memory store rather than on a wire and have no command
//     yet. Shown as unavailable, the way WaveSimRams already shows them, because a memory as it
//     was before the first clock edge looks exactly like a correct one.
//   - a ROM's contents, which are part of its type and need no simulation at all.

/// The viewers the step panel shows, in the order it shows them.
///
/// A fact about the DESIGN - a viewer drawn on a sheet is a viewer in every instance of that
/// sheet - so it is worked out from the design, following only the subtrees that hold one. It used
/// to be filtered out of the expansion-sized component map, which is about 480,000 records on
/// main6 of largeTest, and separately in each of the two simulators' branches.
///
/// One list, so the two simulators cannot show different viewers or show them in a different
/// order. They agreed before only because both happened to fold over the same map in its key
/// order, which is not a thing either of them said.
let private viewerInstances (fs: FastSimulation) =
    fs.Design.InstancesOfComponents (fun comp ->
        match comp.Type with
        | Viewer _ -> true
        | _ -> false)

/// The width of one instance's output port 0 - a fact about the elaborated instance rather than
/// about the component, since a parameterised sheet resolves its widths per instance.
let private outputWidthOf (fs: FastSimulation) ((compId, ap): FComponentId) =
    (PortView.ofInstanceCached fs (InstancePath ap)).ViewPorts
    |> List.tryFind (fun p ->
        p.PortComp = compId && p.PortIs = CommonTypes.PortType.Output && p.PortNum = 0)
    |> Option.map (fun p -> p.PortWidth)
    |> Option.defaultValue 0

/// One clock of the panel's signals, named the way the sidecar names them. Every one is an
/// output port 0: an input's own value, an output's value, a viewer's, or a register's state.
let panelSignals (simData: SimulationData) : StepPanelData.PanelSignal list =
    let asSignal ((ComponentId cid), path) =
        { StepPanelData.Comp = cid
          StepPanelData.Path = path |> List.map (fun (ComponentId p) -> p)
          StepPanelData.Port = 0 }

    let ios =
        simData.Inputs @ simData.Outputs
        |> List.map (fun (cid, _, _) -> asSignal (cid, []))

    let viewers =
        viewerInstances simData.FastSim
        |> List.map (fun (comp, InstancePath ap) -> asSignal (comp.Id, ap))

    // the stateful rows that are signals; a RAM's and a ROM's contents are not. The same design
    // list statefulValues displays from, so a row cannot be shown that was never asked for
    let registers =
        topRegisters simData.FastSim
        |> List.map (fun comp -> asSignal (comp.Id, []))

    ios @ viewers @ registers |> List.distinct

/// A value the sidecar sent back, as the panel's own value type. `None` - nothing fetched for
/// this cycle - is shown as zero of the right width, which is what an unread port already looks
/// like; the alternative is a blank row appearing and disappearing as replies land.
let private panelValue (cycle: int) (width: int) (signal: StepPanelData.PanelSignal) =
    StepPanelData.valueAt cycle signal
    |> Option.defaultValue 0I
    |> convertBigintToFastData width
    |> IData

/// The clock a run should advance FROM: what the panel is showing now.
///
/// Locally the FastSimulation's own tick and the model's are the same number - the model is
/// incremented by exactly what was run - and the FastSimulation's is used because it is the one
/// that cannot be stale. In sidecar mode the local simulation is built and never run, so its tick
/// is 0 for ever; the sidecar's own clock is not the answer either, since it only ever goes
/// forwards while the panel can be stepped back. The model's clock is what is on screen.
let clockNow (model: Model) (simData: SimulationData) =
    if model.SimulateInRenderer then simData.FastSim.ClockTick else simData.ClockTickNumber

/// Where the step panel is after a chunk that was aiming for `target`, given what the chunk
/// answered: the clock it reached, and whether it got there.
///
/// **Not the clock the simulator reports.** Those are different numbers - section G2 of
/// docs/dev/sidecarInvariants.md says so in as many words: the simulator's clock is how far it has
/// RUN, the panel's is where the user is LOOKING, and stepping back to a cycle still inside the
/// circular buffer needs no running at all. `runFastSimulationCore` answers such a step
/// `RunCompleted` with its clock untouched and AHEAD of the target, which taken for the panel's
/// position sent a goto backwards forwards instead - to wherever the simulation happened to have
/// run to, with the progress bar past its own maximum.
///
/// So a chunk that finished puts the panel at the cycle that was asked for, and one that ran out
/// of budget puts it at the clock reached, which is honest progress towards it. The local
/// simulator has always been read this way (`RunCompleted -> cycle`); this is the same rule said
/// once, for both.
let panelClockAfter (target: int) ((reached, finished): int * bool) =
    if finished then target else reached

/// How many clock cycles of history the step simulator can afford on this design, or why it
/// cannot be simulated at all.
///
/// The waveform simulator has a configuration dialog that prices the design and refuses a last
/// clock that will not fit (`FastCreate.maxLastClockFor`, `UIPopups`); the step simulator has no
/// dialog and used to take `Constants.maxArraySize` whatever the design cost - so a design too
/// big for it was refused by the build's own memory check, in words about a waveform
/// configuration the user was not looking at.
///
/// The budget binds here instead, on the one number this simulator has. It asks for the full
/// array - enough past to step back through - and takes what fits if that is too much, down to
/// `minStepArraySize`. Below that the design is refused, with a message about the design.
///
/// Priced from the design's merged graph, so this allocates nothing: the check happens before the
/// arrays it is deciding the size of exist, which is the whole point of it.
let stepSimArraySize (model: Model) : Result<int, SimulationError> =
    let sheet =
        model.CurrentProj
        |> Option.map (fun p -> p.OpenFileName)
        |> Option.defaultValue ""

    match ModelHelpers.designStepCost model sheet with
    // a design that will not price is one that will not build; let the build say why, in its own
    // words, rather than reporting a pricing failure as a size problem
    | Error _ -> Ok Constants.maxArraySize
    | Ok cost ->
        match ModelHelpers.stepSimCycles Constants.maxArraySize cost with
        | Some size ->
            if size < Constants.maxArraySize then
                let perCycle = SimTypes.SimulationBudget.formatBytes (float cost.TotalBytes)
                Log.dbg Log.Sim $"step simulation of '{sheet}' shortened to {size} cycles of history: {perCycle} a cycle"

            Ok size
        | None ->
            let least = ModelHelpers.Constants.minStepArraySize
            let perCycle = SimTypes.SimulationBudget.formatBytes (float cost.TotalBytes)
            let needed = SimTypes.SimulationBudget.formatBytes (float cost.TotalBytes * float least)

            Error
                { ErrType =
                    GenericSimError(
                        $"This design needs {perCycle} of simulation memory for every clock cycle, so even "
                        + $"the {least} clock cycles the step simulator keeps would need {needed} - more "
                        + "than Issie will risk. Simulate one subsheet rather than the whole design."
                    )
                  InDependency = None
                  ComponentsAffected = []
                  ConnectionsAffected = [] }

/// How many clock cycles of history the RENDERER's own step simulation holds.
///
/// The same as the simulation's, when the renderer is running it. When the .NET simulator is, this
/// copy is built for its structure and never run - the panel's values, its memories and its clock
/// all come from the sidecar - so it holds the least a built simulation can. What the SIDECAR
/// holds is `stepSimArraySize` either way: that is the history the step simulator can be stepped
/// back through, and it is a fact about the design rather than about which process is running it.
let rendererStepArraySize (model: Model) : Result<int, SimulationError> =
    // priced either way, so a design too big to simulate is refused before anything is built
    stepSimArraySize model
    |> Result.map (fun size ->
        if model.SimulateInRenderer then
            size
        else
            ModelHelpers.Constants.rendererArraySizeWhenSidecarSimulates)

/// Advance whichever simulator is running TOWARDS `cycle`, for at most one chunk, and say what
/// clock it reached.
///
/// **One chunk, not a run.** A second of simulation, or the cycle asked for, whichever comes
/// first. Getting all the way there is the caller's business: `simulateWithProgressBar` asks
/// again until it arrives, which is the same shape the waveform simulator's run has and for the
/// same reasons - the loop is in the update function, nothing is ever interrupted, and stopping is
/// not asking for the next chunk.
///
/// Both simulators can do this. `FastRun.runFastSimulation` takes a time budget and says where it
/// stopped; `SidecarSession.runChunk` does the same over the wire. So there is nothing to estimate
/// and no per-simulator arithmetic: how far a second gets is measured, by running for a second.
///
/// Local: the FastSimulation is run here and `whenReady` is called at once, exactly as before.
/// Sidecar: the session is run a chunk and the panel's signals read back into StepPanelData, then
/// `whenReady`. If the sidecar does not hold the design, a build is STARTED - an operation, with
/// the model saying so until it answers - and this advance does nothing; the next one finds a
/// session and runs it.
let advanceTo (model: Model) (simData: SimulationData) (cycle: int) (dispatch: Msg -> unit) (whenReady: int -> unit) : unit =
    if model.SimulateInRenderer then
        let reached =
            match FastRun.runFastSimulation (Some(float Constants.advanceChunkMs)) cycle simData.FastSim with
            | RunCompleted -> cycle
            | RunStoppedAt clock -> clock

        StepPanelData.forget ()
        whenReady reached
    else
        match model.CurrentProj with
        | None -> whenReady simData.ClockTickNumber
        | Some project ->
            let top = simData.FastSim.SimulatedTopSheet

            let failed (what: string) (e: string) =
                // The panel keeps whatever it last held, which is of an earlier cycle, so say so
                // rather than let the clock move under values that did not.
                Log.error $"the .NET simulator could not {what}: {e}"
                StepPanelData.forget ()

            // The SIDECAR's array size, which is the history the step simulator wants - not this
            // process's MaxArraySize, which in this mode is two and would leave the sidecar unable
            // to be stepped back at all.
            let sidecarArraySize =
                stepSimArraySize model |> Result.defaultValue Constants.maxArraySize

            /// Run one chunk towards `cycle`. Reading the panel's values at whatever it reached
            /// is not done here: WaveSimTop.fetchWhatIsMissing sees that the panel is showing a
            /// cycle it has no values for and asks for them, by the same mechanism and in the
            /// same kind of operation as everything else the sidecar is asked. This used to be a
            /// promise of its own, which gave the step simulator a second way of talking to the
            /// sidecar that had to be kept in step with that one.
            let runOneChunk epoch =
                let seq = ModelHelpers.newSeq ()
                dispatch (SidecarOpStarted(seq, OpStep cycle))

                /// However the chunk ends, the operation is ANSWERED and the caller told where the
                /// panel now is. The entry this put in the in-flight table is the gate every other
                /// issuer waits behind, so a promise that rejects without dispatching leaves the
                /// table holding an operation that can never complete and nothing is issued on this
                /// wire again - no fetch, no run, and nothing on screen saying why. Every other
                /// issuer gets this from `Cmd.OfPromise.either`; this one is started from a click
                /// and has to say it.
                let finish (reached: int) =
                    dispatch (SidecarReply(seq, AnsStepped))
                    whenReady reached

                promise {
                    match! SidecarSession.runChunk epoch cycle with
                    | Error e ->
                        failed $"run to cycle {cycle}" e
                        finish simData.ClockTickNumber
                    | Ok outcome -> finish (panelClockAfter cycle outcome)
                }
                // a rejection, not an error reply: the socket closed under it, which fails every
                // request in flight (invariant A4)
                |> Promise.catch (fun e ->
                    failed $"run to cycle {cycle}" e.Message
                    finish simData.ClockTickNumber)

            match model.SidecarSession with
            | _ when ModelHelpers.sidecarIsBusy model ->
                // Something is already outstanding - a build, a fetch, or another advance's chunk -
                // and one operation at a time is the rule the whole protocol is sequenced by
                // (docs/dev/sidecarInvariants.md, sections C3 and J). Nothing is retried here: the
                // panel's clock moves anyway, so the end-of-update checks see a cycle the session
                // has not reached and run to it as soon as the wire is free.
                whenReady simData.ClockTickNumber

            | session when session.Holds(top, sidecarArraySize) ->
                runOneChunk (Option.get session.Epoch) |> Promise.start

            | _ ->
                // No session: the START issues builds (StartSimulation, StartStepRun - see
                // docs/dev/sidecarInvariants.md section J), so an advance finding none is a
                // stopped simulation or a coding error, said out loud rather than quietly
                // repaired by a second builder nothing sequences.
                Log.error
                    "cannot advance the step simulation: the .NET simulator holds no session for it"

                whenReady simData.ClockTickNumber

/// Set a top-level input at the shown cycle, on whichever simulator is running, and make the
/// panel's values for that cycle current again.
///
/// Locally that is one call: changeInput sets the value and re-runs the combinational logic at
/// that step. Over the wire it is SimSetInputs and then a re-read, because the values the panel
/// is showing were computed from the input this just changed.
///
/// **Values up to 2^53.** SimSetInputs carries a value as two 32-bit words, so a wider input
/// cannot be set from here yet; it is refused by name rather than sent truncated, which would
/// show the user a value the simulation is not running on.
let setInput (model: Model) (simData: SimulationData) (compId: ComponentId) (value: FastData) (whenReady: unit -> unit) : unit =
    if model.SimulateInRenderer then
        FastExtract.changeInput compId (IData value) simData.ClockTickNumber simData.FastSim
        whenReady ()
    else
        let (ComponentId cid) = compId
        let cycle = simData.ClockTickNumber
        let asBigInt = value.GetBigInt

        match model.SidecarSession.Epoch with
        | _ when asBigInt > 9007199254740992I ->
            Log.error
                $"the .NET simulator cannot yet be given a {value.Width}-bit input value this large - Development > Simulate In Renderer can set it"
            whenReady ()
        | None ->
            Log.error "there is no .NET simulation to set an input on"
            whenReady ()
        | Some epoch ->
            promise {
                let! reply = SidecarClient.simSetInputs epoch cycle [ cid, float asBigInt ]

                match SidecarSession.errorIn reply with
                | Some e ->
                    Log.error $"the .NET simulator could not set an input: {e}"
                    StepPanelData.forget ()
                | None ->
                    match! StepPanelData.fill epoch cycle (panelSignals simData) with
                    | Error e ->
                        Log.error $"the .NET simulator could not read cycle {cycle} back: {e}"
                        StepPanelData.forget ()
                    | Ok() -> ()

                whenReady ()
            }
            // as advanceTo: a rejection is the socket closing under the request, and the caller
            // must still be told, or the poke leaves the panel waiting for a redraw that never comes
            |> Promise.catch (fun e ->
                Log.error $"the .NET simulator could not set an input: {e.Message}"
                StepPanelData.forget ()
                whenReady ())
            |> Promise.start

/// The panel's top-level input or output values, from whichever simulator is running.
let ioValues (model: Model) (simData: SimulationData) (ios: SimulationIO list) =
    if model.SimulateInRenderer then
        FastExtract.extractFastSimulationIOs ios simData
    else
        ios
        |> List.map (fun ((ComponentId cid, _, width) as io) ->
            io, panelValue simData.ClockTickNumber width { Comp = cid; Path = []; Port = 0 })

/// The panel's viewer values, from whichever simulator is running.
///
/// The LIST is the design's, so both simulators show the same viewers under the same names in the
/// same order; only where the value comes from differs, which is the one thing that should.
let viewerValues (model: Model) (simData: SimulationData) =
    let fs = simData.FastSim

    viewerInstances fs
    |> List.map (fun ((comp, InstancePath ap) as instance) ->
        let fId = comp.Id, ap
        let width = outputWidthOf fs fId

        let value =
            if model.SimulateInRenderer then
                FastExtract.extractFastSimulationOutput fs simData.ClockTickNumber fId (OutputPortNumber 0)
            else
                panelValue
                    simData.ClockTickNumber
                    width
                    { Comp = componentIdValue comp.Id
                      Path = ap |> List.map (fun (ComponentId p) -> p)
                      Port = 0 }

        (comp.Label, fs.Design.FullNameOf instance), width, value)

/// The panel's stateful rows, from whichever simulator is running.
///
/// In sidecar mode this is the registers and counters only - their state is Outputs[0], so it is
/// a signal like any other - and a memory is left out because its contents cannot be read over
/// the wire yet. `ramsAreLocalOnly` below is what tells the user that, rather than a table of
/// whatever the unrun local simulation happens to hold.
let statefulValues (model: Model) (simData: SimulationData) =
    if model.SimulateInRenderer then
        FastExtract.extractStatefulComponents simData.ClockTickNumber simData.FastSim
        |> Array.map (fun (fc, state) -> fc.FullName, fc.FType, state)
    else
        topRegisters simData.FastSim
        |> List.map (fun comp ->
            let width = outputWidthOf simData.FastSim (comp.Id, [])

            let value =
                StepPanelData.valueAt simData.ClockTickNumber
                    { Comp = componentIdValue comp.Id; Path = []; Port = 0 }
                |> Option.defaultValue 0I

            comp.Label, comp.Type, RegisterState(convertBigintToFastData width value))
        |> Array.ofList

/// The top-level memories the stateful panel offers a View button for.
///
/// They are not part of `statefulValues` in sidecar mode - a memory's contents live in its store
/// rather than on a wire, so they are not a signal to be read with the rest - and are listed here
/// instead, to be read only when the button is pressed.
let ramComponents (simData: SimulationData) =
    simData.FastSim
    |> topComponentsWhere (fun comp ->
        match comp.Type with
        | RAM1 _
        | AsyncRAM1 _ -> true
        | _ -> false)

/// Read one memory from the .NET simulator and open the diff against its initial contents.
///
/// No cache and no held state, unlike the waveform simulator's RAM table: this happens when a
/// button is pressed, and a button press can wait for a reply the way any other click can. What
/// makes the table different is that it is drawn from `view`, which cannot wait for anything.
///
/// A diff needs the whole memory on both sides, so the request asks for a listing and nothing
/// else. A window coming back means the memory is written in too many places to read whole -
/// `RamStore.Constants.maxSlotsForWholeRead` - which is the same answer the local button gives by
/// disabling itself, said after the fact because only the far side knows.
let openRemoteRamDiff (ram: Component) (cycle: int) (model: Model) (dispatch: Msg -> unit) =
    let initial =
        match ram.Type with
        | RAM1 m
        | AsyncRAM1 m -> m
        | _ -> failwithf $"what? openRemoteRamDiff expected a RAM but got {ram.Type}"

    match model.SidecarSession.Epoch with
    | None -> Log.error "there is no .NET simulation to read a memory from"
    | Some epoch ->
        let cid, path = ram.Id, []

        promise {
            let! reply =
                SidecarClient.simReadRam
                    epoch
                    cycle
                    (componentIdValue cid)
                    (path |> List.map componentIdValue)
                    RamStore.Constants.maxSlotsForWholeRead
                    0I
                    0

            match reply with
            | Error e -> Log.error $"the .NET simulator could not read memory '{ram.Label}': {e}"
            | Ok(RamView.RamWindow _) ->
                errorNotification
                    $"'{ram.Label}' has been written in too many places to compare with its initial                       contents. The waveform simulator's RAM table shows a window of it."
                    CloseSimulationNotification
                |> SetSimulationNotification
                |> dispatch
            | Ok(RamView.RamSparse rows) ->
                let data =
                    rows
                    |> List.filter (fun r -> r.Value <> 0I)
                    |> List.map (fun r -> r.Addr, r.Value)
                    |> Map.ofList

                openMemoryDiffViewer initial { initial with Data = data } model dispatch
        }
        |> Promise.start

/// Pretty print a label with its width.
let makeIOLabel label width =
    let label = cropToLength 15 true label
    match width with
    | 1 -> label
    | w -> sprintf "%s (%d bits)" label w

let private viewSimulationInputs
        (numberBase : NumberBase)
        (model: Model)
        (simulationData : SimulationData)
        (inputs : (SimulationIO * FSInterface) list)
        dispatch =

    let simulationGraph = simulationData.Graph
    let makeInputLine ((ComponentId inputId, ComponentLabel inputLabel, width), inputVals) =
        let valueHandle =
            match inputVals with
            | IData {Dat = (Word bit); Width =1} ->
                // For simple bits, just have a Zero/One button.
                Button.button [
                    Button.Props [ simulationBitStyle ]
                    //Button.Color IsPrimary
                    (match bit with 0u -> Button.Color Color.IsGreyLighter | _ -> Button.Color IsPrimary)
                    Button.IsHovered false
                    Button.OnClick (fun _ ->
                        let newBit = 1u - bit
                        let graph = simulationGraph
                        setInput model simulationData (ComponentId inputId) {Dat = Word newBit; Width = 1} (fun () ->
                            // the poke changed simulator state the model cannot hold, so the
                            // model counts it - the count is in the fetch viewport, and bumping
                            // it is what refetches every value computed under the old stimulus
                            dispatch <| UpdateModel(Optic.map stimulusGeneration_ ((+) 1))
                            dispatch <| SetSimulationGraph(graph, simulationData.FastSim))
                    )
                ] [ str <| bitToString (match bit with 0u -> Zero | _ -> One)]
            | IData bits ->
                let defValue = fastDataToPaddedString  Constants.boxMaxChars numberBase bits
                Input.text [
                    Input.Key (numberBase.ToString())
                    Input.DefaultValue defValue
                    Input.Props [
                        simulationNumberStyle
                        OnChange (getTextEventValue >> (fun text ->
                            match strToIntCheckWidth width text with
                            | Error err ->
                                let note = errorPropsNotification err
                                dispatch  <| SetSimulationNotification note
                            | Ok num ->
                                let bits = convertBigintToFastData width num
                                // Close simulation notifications.
                                CloseSimulationNotification |> dispatch
                                // Feed input.
                                let graph = simulationGraph
                                setInput model simulationData (ComponentId inputId) bits (fun () ->
                                    // as the bit toggle above: the poke count is viewport state
                                    dispatch <| UpdateModel(Optic.map stimulusGeneration_ ((+) 1))
                                    dispatch <| SetSimulationGraph(graph, simulationData.FastSim))
                        ))
                    ]
                ]
            | IAlg _ -> failwithf "what? Algebra in Step Simulation (not yet implemented)"
        splittedLine (str <| makeIOLabel inputLabel width) valueHandle
    div [] <| List.map makeInputLine inputs

let private staticBitButton bit =
    Button.button [
        Button.Props [ simulationBitStyle ]
        //Button.Color IsPrimary
        (match bit with Zero -> Button.Color IsGreyLighter | One -> Button.Color IsPrimary)
        Button.IsHovered false
        Button.Disabled true
    ] [ str <| bitToString bit ]

let private staticNumberBox maxChars numBase (bits: FastData) =
    let value = fastDataToPaddedString maxChars numBase bits
    Input.text [
        Input.IsReadOnly true
        Input.Value value
        Input.Props [simulationNumberStyle]
    ]

let private viewSimulationOutputs numBase (simOutputs : (SimulationIO * FSInterface) list) =
    let makeOutputLine ((ComponentId _, ComponentLabel outputLabel, width), inputVals) =
        let valueHandle =
            match inputVals with
            | IData {Dat = Word b; Width = 1} -> staticBitButton (match b with 0u -> Zero | _ -> One)
            | IData bits -> staticNumberBox Constants.boxMaxChars numBase bits
            | IAlg _ -> failwithf "what? Algebra in Step Simulation (not yet implemented)"
        splittedLine (str <| makeIOLabel outputLabel width) valueHandle
    div [] <| List.map makeOutputLine simOutputs

let private viewViewers numBase (simViewers : ((string*string) * int * FSInterface) list) =
    let makeViewerOutputLine ((label,fullName), width, inputVals) =
        let valueHandle =
            match inputVals with
            | IData {Dat = Word b; Width = 1} -> staticBitButton (match b with 0u -> Zero | _ -> One)
            | IData bits -> staticNumberBox Constants.boxMaxChars numBase bits
            | IAlg _ -> failwithf "what? Algebra in Step Simulation (not yet implemented)"
        let addToolTip tip react = 
            div [ 
                HTMLAttr.ClassName $"{Tooltip.ClassName} has-tooltip-right"
                Tooltip.dataTooltip tip
            ] [react]
        let line = 
            str <| makeIOLabel label width
            |> (fun r -> if fullName <> "" then addToolTip fullName r else r)
        splittedLine line valueHandle
    div [] <| List.map makeViewerOutputLine simViewers

let private viewStatefulComponents step comps numBase model dispatch =
    let getWithDefault (lab:string) = if lab = "" then "no-label" else lab
    let makeStateLine ((fullName, compType, state): string * ComponentType * SimulationComponentState) =
        let label = getWithDefault fullName
        match state with
        | RegisterState fd when fd.Width = 1 ->
            let bit = if fd = SimGraphTypes.fastDataZero then Zero else One
            let label = sprintf "DFF: %s" <| label
            [ splittedLine (str label) (staticBitButton bit) ]
        | RegisterState bits ->
            let label = sprintf "Register: %s (%d bits)" label bits.Width
            [ splittedLine (str label) (staticNumberBox Constants.boxMaxChars numBase bits) ]
        | RamState ram ->
            let label = sprintf "RAM: %s" <| label
            let initialMem compType =
                match compType with
                | RAM1 m | AsyncRAM1 m ->
                    m
                | _ ->
                    failwithf "what? viewStatefulComponents expected RAM component but got: %A" compType
            // Whether the diff can be shown at all: it needs the whole memory on both sides, and
            // reading one is linear in the addresses it has ever held. RamStore says when that is
            // affordable; past its bound the button says so rather than freezing the pane for a
            // tenth of a second, or showing a memory it only partly read. Asked while the line is
            // DRAWN because the answer decides what the button is - it is a slot count, not a read.
            let fits = ram.SlotCount <= RamStore.Constants.maxSlotsForWholeRead
            let viewDiffBtn =
                Button.button [
                    Button.Props [
                        simulationBitStyle
                        if not fits then
                            Tooltip.dataTooltip
                                "This memory has been written in too many places to compare with its \
                                 initial contents. The waveform simulator's RAM table shows a window of it."
                    ]
                    Button.Color (if fits then IsPrimary else IsGreyLight)
                    Button.Disabled (not fits)
                    // The whole memory is built here, when the button is pressed, and not while
                    // this line is drawn: toMemory walks every address the memory has ever held,
                    // and this pane is redrawn on every message.
                    Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage(
                        (fun model _ ->
                            match RamStore.toMemoryIfSmall ram step with
                            | Some mem -> openMemoryDiffViewer (initialMem compType) mem model dispatch
                            | None -> ()), dispatch)
                    )
                ] [ str "View" ]
            [ splittedLine (str label) viewDiffBtn ]
        | _ -> []
    div [] (List.collect makeStateLine comps )

let getSimErrFeedbackMessages (simError:SimGraphTypes.SimulationError) (model:Model) : (Msg list) =
    if simError.InDependency.IsNone then
        // Highlight the affected components and connection only if
        // the error is in the current diagram and not in a
        // dependency.
        let (badComps,badConns) = (simError.ComponentsAffected, simError.ConnectionsAffected)
        let msgs = [SetHighlighted (badComps,badConns)]
        if not (Sheet.isAllVisible model.Sheet badConns badComps) then
            // make whole diagram visible if any of the errors are not visible
            msgs @ [Sheet (SheetT.KeyPress SheetT.KeyboardMsg.CtrlW)]
        else
            msgs
    else
        []

let setSimErrorFeedback (simError:SimGraphTypes.SimulationError) (model:Model) (dispatch: Msg -> Unit) =
    // let sheetDispatch sMsg = dispatch (Sheet sMsg)
    // let keyDispatch = SheetT.KeyPress >> sheetDispatch
    // if simError.InDependency.IsNone then
    //     // Highlight the affected components and connection only if
    //     // the error is in the current diagram and not in a
    //     // dependency.
    //     let (badComps,badConns) = (simError.ComponentsAffected, simError.ConnectionsAffected)
    //     dispatch <| SetHighlighted (badComps,badConns)
    //     if not (Sheet.isAllVisible model.Sheet badConns badComps) then
    //         // make whole diagram visible if any of the errors are not visible
    //         keyDispatch <| SheetT.KeyboardMsg.CtrlW
    getSimErrFeedbackMessages simError model
    |> List.iter dispatch



/// get the position and rotation for inserting a new component next to the given port
/// at a given distance
/// the rotation is such that the original left side of the component (input side)
/// faces the given port
/// returns None if another symbol is in the way
let getPosRotNextToPort (port: Port) (model: SymbolT.Model) (dist: float) =
    let isPosInBoundingBox  (pos: XYPos) (boundingBox: BoundingBox) =
        (pos.X > boundingBox.TopLeft.X && pos.X < boundingBox.TopLeft.X + boundingBox.W &&
        pos.Y > boundingBox.TopLeft.Y && pos.Y < boundingBox.TopLeft.Y + boundingBox.H)
    
    let sym =
        model.Symbols
        |> Map.toList
        |> List.tryFind (fun (_, sym) -> sym.Component.Id = port.HostId)
        |> function
            | Some (_, sym) -> sym
            | None -> failwithf "The given component should be in the list of symbols"

    let edge = sym.PortMaps.Orientation[port.Id]
    let portPos = Symbol.getPortPos sym port
    let pos, rot =
        match edge with
        | Right ->
            {X = sym.Pos.X + portPos.X + dist; Y = sym.Pos.Y + portPos.Y},
            Degree0
        | Top ->
            {X = sym.Pos.X + portPos.X; Y = sym.Pos.Y + portPos.Y - dist},
            Degree90
        | Left ->
            {X = sym.Pos.X + portPos.X - dist; Y = sym.Pos.Y + portPos.Y},
            Degree180
        | Bottom ->
            {X = sym.Pos.X + portPos.X; Y = sym.Pos.Y + portPos.Y + dist},
            Degree270

    model.Symbols
    |> Map.toList
    |> List.map (fun (_, sym) -> Symbol.getSymbolBoundingBox sym)
    |> List.exists (isPosInBoundingBox pos)
    |> function
        | true -> None
        | false -> Some (pos, rot)
    

let viewSimulationError
    (comps: Component list, conns: Connection list)
    (simError : SimulationError)
    (model: Model)
    simType
    dispatch
    =
    let sheetDispatch sMsg = dispatch <| Sheet sMsg
    let busWireDispatch bMsg = sheetDispatch <| SheetT.Msg.Wire bMsg
    let symbolDispatch symMsg = busWireDispatch <| BusWireT.Msg.Symbol symMsg

    let changeAdderType (compId: ComponentId) (targetType: ComponentType) (model: Model) _ =
        model.Sheet.ChangeAdderComp sheetDispatch compId (targetType)
    
    let changeCounterType (compId: ComponentId) (targetType: ComponentType) (model: Model) _ =
        model.Sheet.ChangeCounterComp sheetDispatch compId (targetType)

    // Only the list-returning lookups below. The two that threw when an id was missing are gone:
    // they were unused, and throwing inside the code that renders an error would have lost the
    // user the error as well as whatever caused it. A component named by an error can genuinely be
    // absent, having been deleted between the simulation and this render.
    let getComponentByIdListOpt (compId: ComponentId) =
        comps
        |> List.tryFind (fun comp -> comp.Id = compId)
        |> function | Some comp -> [comp]
                    | None ->
                        Log.warn "an errored component from the simulation is missing, and will be ignored"
                        []

    let getConnectionByIdLstOpt connId =
        conns
        |> List.tryFind (fun conn -> conn.Id = connId)
        |> function | Some comp -> [comp]
                    | None ->
                        Log.warn "an errored connection from the simulation is missing, and will be ignored"
                        []


    /// If affected component has been deleted after simulation started we do not include it -
    /// so worst case this list can be empty!
    let reacListOfCompsAffected =
        simError.ComponentsAffected
        |> List.collect getComponentByIdListOpt
        |> List.map (fun comp -> li [] [str comp.Label])

    let getCompAndPortAffectedMsg (comp: Component) (port: Port) = comp.Label + "." + CanvasStateAnalyser.getPortName comp port


    let cleanup() =
        simReset dispatch
        dispatch (TryStartSimulationAfterErrorFix simType)

    let error =
        let comps = List.collect getComponentByIdListOpt simError.ComponentsAffected
        match comps, simError.ErrType with
        | [comp], OutputConnError (0, port, rmInfo) ->

            let buttonOrText =
                match rmInfo with
                | Removable targetType ->
                    let deletePort model _ =
                        changeAdderType (comp.Id) targetType model ()
                        cleanup()
                    Button.button [
                        Button.Color IsSuccess
                        Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage((deletePort,dispatch)))
                    ] [ str "Fix by deleting the port on the component" ]
                | Unremovable ->
                    getPosRotNextToPort port model.Sheet.Wire.Symbol Constants.ncPortDist
                    |> function
                        | Some (pos, rot) ->
                            let addNCComp model _ =
                                sheetDispatch <| SheetT.AddNotConnected
                                    ((ModelHelpers.tryGetLoadedComponents model),
                                    port,
                                    pos,
                                    rot)
                                cleanup()

                            Button.button [
                                Button.Color IsSuccess
                                Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage(addNCComp,dispatch))
                            ] [ str "Fix by adding 'Not Connected' component" ]
                        | None ->
                            str "Please insert a 'Not Connected' component manually"
            

            div [] [
                ErrorDisplay.errorMessage dispatch (errMsg simError.ErrType)
                br []
                br []
                str (getCompAndPortAffectedMsg comp port)
                br []
                buttonOrText
            ]
        | [comp], InputConnError (0, port, rmInfo) ->
            let compAndPortAffectedMsg = comp.Label + "." + CanvasStateAnalyser.getPortName comp port
            let compId = comp.Id
            let removeInPorts (moel: Model) _ =
                match rmInfo with
                | Removable targetType ->
                    match targetType with
                    | NbitsAdder _ | NbitsAdderNoCin _ | NbitsAdderNoCout _ | NbitsAdderNoCinCout _ ->
                        changeAdderType compId targetType model ()
                    | Counter _ | CounterNoEnable _ | CounterNoLoad _ | CounterNoEnableLoad _ ->
                        changeCounterType compId targetType model ()
                    | _ -> ()
                | Unremovable -> failwithf "This function should never be called if not input ports can be removed"
                simReset dispatch
                dispatch (TryStartSimulationAfterErrorFix simType)
                // restartFn (comps, conns) model dispatch ()
            
            let showButton =
                match rmInfo with
                | Removable _ -> true
                | Unremovable -> false
            div [] [
                ErrorDisplay.errorMessage dispatch (errMsg simError.ErrType)
                br []
                br []
                str (getCompAndPortAffectedMsg comp port)
                br []
                if showButton then
                    Button.button [
                        Button.Color IsSuccess
                        Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage(removeInPorts,dispatch))
                    ] [str "Fix by deleting input port"]
            ]
        | _, UnnecessaryNC ->
            let removeNCAndChangeAdderType() =
                let NCsToDelete =
                    simError.ConnectionsAffected
                    |> List.collect getConnectionByIdLstOpt
                    |> List.map (fun conn ->
                        conn.Target.HostId)
                // delete NotConnected components
                symbolDispatch <| SymbolT.DeleteSymbols NCsToDelete
                // delete affected connections
                busWireDispatch <| BusWireT.DeleteWires simError.ConnectionsAffected

                simError.ComponentsAffected
                |> List.collect getComponentByIdListOpt
                |> List.iter (fun comp ->
                    match comp.Type with
                    | NbitsAdder w -> dispatch <| ExecFuncInMessage ((changeAdderType (comp.Id) (NbitsAdderNoCout w)),dispatch)
                    | NbitsAdderNoCin w -> dispatch <| ExecFuncInMessage((changeAdderType (comp.Id) (NbitsAdderNoCinCout w)),dispatch)
                    | _ -> failwithf "Unexpected adder type. Should only encounter these 2 types with this error message")
                
                simReset dispatch
                // restartFn (comps, conns) model dispatch ()
                dispatch (TryStartSimulationAfterErrorFix simType)

            div [] [
                ErrorDisplay.errorMessage dispatch (errMsg simError.ErrType)
                br []
                br []
                ul [] reacListOfCompsAffected
                br []
                Button.button [
                    Button.Color IsSuccess
                    Button.OnClick (fun _ -> removeNCAndChangeAdderType())
                ] [str "Fix by deleting unnecessary 'Not Connected' components"]
            ]
        | _ ->
            match simError.InDependency with
            | None ->
                div [] [
                    ErrorDisplay.errorMessage dispatch (errMsg simError.ErrType)
                    br []
                    str <| "Please fix the error and retry."
                ]
            | Some dep ->
                div [] [
                    str <| "Error found in sheet '" + dep + "' which is a dependency:"
                    br []
                    ErrorDisplay.errorMessage dispatch (errMsg simError.ErrType)
                    br []
                    str <| "Please fix the error in this sheet and retry."
                ]
    div [] [
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Errors" ]
        error
    ]

let private simulationClockChangePopup (simData: SimulationData) (dispatch: Msg -> Unit) (model':Model) =
    let dialog = model'.PopupDialogData
    let step = simData.ClockTickNumber
    let restartsimrequired (lastStepNeeded: int) = (simData.FastSim.ClockTick - lastStepNeeded) >= simData.FastSim.MaxArraySize
    div [] 
        [
            h6 [] [str $"This simulation contains {simData.FastSim.Design.ExpandedComponentCount} components"]
            (match dialog.Int with 
            | Some n when restartsimrequired n -> 
                Text.p 
                    [Modifiers [
                        Modifier.TextWeight TextWeight.Bold
                        Modifier.TextColor IsDanger] 
                    ] 
                    [str $"To generate data for time step {n}, 
                          the hardware will be resimulated using default inputs. "]
            | _ -> 
                Text.p [Modifiers [
                    Modifier.TextWeight TextWeight.Bold]] 
                    [str $"Go to Tick:"])
            br []
            Input.number [
                Input.Props [AutoFocus true;Style [Width "100px"]]
                Input.DefaultValue <| sprintf "%d" step
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]

        ]

let cmd block =
    Elmish.Cmd.OfAsyncWith.perform block

let doBatchOfMsgsAsynch (msgs: seq<Msg>) =
    msgs
    |> Seq.map Elmish.Cmd.ofMsg 
    |> Elmish.Cmd.batch
    |> ExecCmdAsynch
    |> Elmish.Cmd.ofMsg



let simulateWithProgressBar (simProg: SimulationProgress) (model:Model) =
    match model.CurrentStepSimulationStep, model.PopupDialogData.Progress with
    | Some (Ok simData), Some barData ->
        let nComps = float simData.FastSim.Design.ExpandedComponentCount
        let oldClock = clockNow model simData
        let t1 = getTimeMs()
        // Cmd.ofEffect rather than a run and a batch of messages: with the .NET simulator the
        // chunk is a round trip, so the messages that report it can only be sent once it lands.
        // The local path still runs synchronously inside advanceTo and dispatches immediately.
        model, Elmish.Cmd.ofEffect (fun dispatch ->
            advanceTo model simData simProg.FinalClock dispatch (fun reached ->
                let t2 = getTimeMs()
                let speed = if t2 = t1 then 0. else (float reached - float oldClock) * nComps / (t2 - t1)
                // Where it GOT to, not where it was aimed. A chunk is a second of simulation, so
                // how many clocks it covers is measured rather than predicted - which is why there
                // is nothing here estimating a chunk size, and why a design that turns out slower
                // than expected simply takes more chunks.
                let messages =
                    if reached >= simProg.FinalClock then [
                        SetSimulationGraph(simData.Graph, simData.FastSim)
                        IncrementSimulationClockTick (reached - oldClock);
                        SetPopupProgress None ]
                    else [
                        SetSimulationGraph(simData.Graph, simData.FastSim)
                        IncrementSimulationClockTick (reached - oldClock)
                        UpdatePopupProgress (fun barData -> {barData with Value = reached - simProg.InitialClock; Speed = speed})
                        SimulateWithProgressBar simProg ]
                messages |> List.iter dispatch))
    | _ -> 
        model, Elmish.Cmd.ofMsg (SetPopupProgress None)

    
    

let simulationClockChangeAction dispatch simData (model': Model) =
    let dialog = model'.PopupDialogData
    let clock = 
        match dialog.Int with
        | None -> failwithf "What - must have some number from dialog"
        | Some clock -> clock
    // Going back restarts from nothing, so the run to watch is the whole of it; going forward it
    // is the part not already simulated.
    let initClock = 
        if clock > simData.ClockTickNumber then 
            simData.ClockTickNumber
        else 
            0
    let steps = 
        if clock > simData.ClockTickNumber then 
            clock - simData.ClockTickNumber
        else 
            clock
    let numComps = float simData.FastSim.Design.ExpandedComponentCount
    let t1 = getTimeMs ()

    if not model'.SimulateInRenderer then
        // The .NET simulator runs it. No loop starts here: the target becomes model state, the
        // bar goes up, and the update pipeline (continueStepRun below) issues one chunk whenever
        // the wire is free and the clock is short of the target - so the run cannot collide with
        // the panel's own fetches, and Cancel (closing the bar) simply stops it being asked.
        // Going back is a restart: the model's clock is set to the start the bar counts from,
        // and the sidecar restarts itself when asked for a cycle behind its own clock.
        [ ClosePopup
          SetPopupProgress(
              Some
                  { Speed = 0.0
                    Value = 0
                    Max = steps
                    Title = "running simulation..." })
          StartStepRun { InitialClock = initClock; FinalClock = clock } ]
        |> List.iter dispatch
    else

    // ONE CHUNK FIRST, then decide whether there is anything to show a bar for.
    //
    // Nothing is estimated. This used to SAMPLE the design - run the first few clocks, time them,
    // scale up - and from that estimate both how long the run would take and how many clocks a
    // chunk should be. Two floors had to be defended (a design of more than 20,000 components
    // sampled no clocks, which divides into an infinite estimate and a chunk of none, and the
    // progress loop then redispatched itself for ever without advancing), the estimate was wrong
    // whenever a design's speed was not uniform, and it could not be made at all for the .NET
    // simulator without doing in the renderer exactly the work that mode exists to move.
    //
    // A chunk is a second of simulation. How far a second gets is measured by running for one, so
    // a run that finishes inside the first chunk never shows a bar, and one that does not shows a
    // bar whose speed is what the last chunk actually did.
    advanceTo model' simData clock dispatch (fun reached ->
        let t2 = getTimeMs ()
        let speed = if t2 = t1 then 0. else (float reached - float simData.ClockTickNumber) * numComps / (t2 - t1)

        [
            SetSimulationGraph(simData.Graph, simData.FastSim)
            IncrementSimulationClockTick (reached - simData.ClockTickNumber)
            ClosePopup
            if reached < clock then
                SetPopupProgress(Some {
                    Speed = speed
                    Value = reached - initClock
                    Max = steps
                    Title = "running simulation..." })
                SimulateWithProgressBar { FinalClock = clock; InitialClock = initClock }
        ]
        |> Seq.map Elmish.Cmd.ofMsg
        |> Elmish.Cmd.batch
        |> ExecCmdAsynch
        |> dispatch)



let viewSimulationData (step: int) (simData : SimulationData) model dispatch =
    let viewerWidthList =
        viewerValues model simData
        |> List.map (fun (_, width, _) -> width)
    let outputWidthList =
        simData.Outputs 
        |> List.map (fun (_,_,w) -> w)       
    let hasMultiBitOutputs =
        (List.append outputWidthList viewerWidthList)|> List.map ((>) 1) |> List.isEmpty |> not
    let maybeBaseSelector =
        match hasMultiBitOutputs with
        | false -> div [] []
        | true -> baseSelector simData.NumberBase (changeBase dispatch)
    let maybeClockTickBtn =
        let step = simData.ClockTickNumber
        match simData.IsSynchronous with
        | false -> div [] []
        | true ->
            div [] [
                Button.button [
                    Button.Color IsSuccess
                    Button.Disabled (simData.ClockTickNumber = 0)
                    Button.OnClick (fun _ ->
                        advanceTo model simData (simData.ClockTickNumber-1) dispatch (fun _ ->
                            dispatch <| SetSimulationGraph(simData.Graph, simData.FastSim)
                            IncrementSimulationClockTick -1 |> dispatch)
                    )
                ] [ str "◀" ]
                str " "
                str " "
                Button.button [
                    Button.Props [Tooltip.dataTooltip "Click to goto"]
                    Button.Color IsSuccess
                    Button.OnClick (fun _ ->
                        let isDisabled (model': Model) =
                            let dialogData = model'.PopupDialogData
                            match dialogData.Int with
                            | Some n -> n < 0
                            | None -> true
                        dialogPopup 
                            "Advance Simulation"
                            (simulationClockChangePopup simData dispatch)
                            "Goto Tick"
                            (simulationClockChangeAction dispatch simData)
                            isDisabled
                            []
                            dispatch)
                        ] [ str <| sprintf "Clock Tick %d" simData.ClockTickNumber ]
                str " "
                str " "
                Button.button [
                    Button.Color IsSuccess
                    Button.OnClick (fun _ ->
                        advanceTo model simData (simData.ClockTickNumber+1) dispatch (fun _ ->
                            dispatch <| SetSimulationGraph(simData.Graph, simData.FastSim)
                            IncrementSimulationClockTick 1 |> dispatch)
                    )
                ] [ str "▶" ]
            ]
    let maybeStatefulComponents() =
        let stateful = 
            statefulValues model simData
            |> Array.toList
        // With the .NET simulator a memory is not in the stateful list above - its contents are in
        // its store rather than on a wire, so they are not read with the signals - and it gets its
        // own row here, whose View button reads it over the wire when pressed.
        let remoteRams =
            if model.SimulateInRenderer then []
            else
                ramComponents simData
                |> List.map (fun ram ->
                    splittedLine
                        (str $"RAM: %s{ram.Label}")
                        (Button.button [
                            Button.Props [ simulationBitStyle ]
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> dispatch <| ExecFuncInMessage(
                                (fun model _ -> openRemoteRamDiff ram simData.ClockTickNumber model dispatch), dispatch))
                         ] [ str "View" ]))
        match List.isEmpty stateful && List.isEmpty remoteRams with
        | true -> div [] []
        | false -> div [] [
            Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Stateful components" ]
            viewStatefulComponents step stateful simData.NumberBase model dispatch
            yield! remoteRams
        ]
    let questionIcon = str "\u003F"

    let tip tipTxt txt =
        span [
                // Style [Float FloatOptions.Left]
                HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
                Tooltip.dataTooltip tipTxt
            ]
            [
                Text.span [
                    Modifiers [
                        Modifier.TextColor IsPrimary
                    ]
                    Props [
                        Style [
                            Display DisplayOptions.InlineBlock
                            Width "80px"
                            TextAlign TextAlignOptions.Center]]
            ] [str txt] ]
    // The number base and clock tick controls apply to every signal below them, so they stay put
    // while the signals scroll: a design with more of them than fit is exactly the one where
    // stepping the clock and reading a viewer at the same time matters.
    div [Style [Flex "1 1 auto"; MinHeight "0px"; Display DisplayOptions.Flex; FlexDirection "column"]] [
        div [Style [Flex "0 0 auto"]] [
            splittedLine maybeBaseSelector maybeClockTickBtn
        ]
        div [Style [Flex "1 1 auto"; MinHeight "0px"; OverflowY OverflowOptions.Auto]] [
            Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Inputs" ]
            viewSimulationInputs
                simData.NumberBase
                model
                simData
                (ioValues model simData simData.Inputs)
                dispatch


            Heading.h5 [
                Heading.Props [ Style [ MarginTop "15px" ] ]
                ] [
                    str "Outputs &"
                    tip "Add Viewer components to any sheet in the simulation" "Viewers"
                ]
            viewViewers simData.NumberBase <| List.sort (viewerValues model simData)
            viewSimulationOutputs simData.NumberBase
            <| ioValues model simData simData.Outputs

            maybeStatefulComponents()
        ]
    ]


let tryGetSimData isWaveSim canvasState model =
    let model = MemoryEditorView.updateAllMemoryComps model
    // A Start is a restart: force the build past the memo, so a fresh simulation begins at
    // cycle 0 rather than the memoised one resuming wherever its clock had got to.
    simCache <- simCacheInit ()
    match rendererStepArraySize model with
    | Error e -> Error e
    | Ok arraySize ->
    simulateModel model.SimulateInRenderer isWaveSim None arraySize canvasState model
    |> function
        | Ok (simData), state -> 
            if simData.FastSim.ClockTick = 0 then 
                setFastSimInputsToDefault simData.FastSim
            Ok simData
        | Error simError, state ->
            Log.dbg Log.Sim $"simulation error: {simError.ErrType}"
            Error simError


/// Issue the .NET build a step simulation needs: the START's operation - StartSimulation and
/// the goto's cascade both come here, and nothing else ever builds for the step simulator. Its
/// completion (AnsBuilt) creates the session the reads and runs require.
let issueStepBuild (model: Model) (simData: SimulationData) : Model * Elmish.Cmd<Msg> =
    match model.CurrentProj with
    | None -> model, Elmish.Cmd.none
    | Some project ->
        let top = simData.FastSim.SimulatedTopSheet

        let design =
            ModelHelpers.designOf project (model.Sheet.GetCanvasState())
            |> CanvasExtractor.simpleDesignOfLoadedComponents
            |> fun d -> { d with TopSheet = top }

        let arraySize = stepSimArraySize model |> Result.defaultValue Constants.maxArraySize
        let seq = ModelHelpers.newSeq ()

        let build =
            Elmish.Cmd.OfPromise.either
                (fun () -> SidecarSession.build design arraySize)
                ()
                (fun result -> SidecarReply(seq, AnsBuilt result))
                (fun exn -> SidecarReply(seq, AnsBuilt(Error exn.Message)))

        model |> Optic.map sidecarInFlight_ (Map.add seq (OpBuild(top, arraySize))), build

/// One step of the step-simulation run's cascade: issue the next piece of sidecar work the run
/// needs, or finish, or - if the wire is held by something else - do nothing.
///
/// The sequencing is Elmish and nothing else. The cascade is STARTED by a command (StartStepRun);
/// every completion comes back as a message whose handler records the fact and calls this to
/// issue the next chunk - so the set of places the run can continue from is exactly the reply
/// handlers (AnsSteppedTo, AnsBuilt, and AnsFetched for a panel read that borrowed the wire),
/// never "after every message". Cancelling is clearing StepRunTarget - closing the bar does it -
/// and the next completion finds nothing to issue, which is the cancellation's response.
///
/// The renderer's own simulator never comes through here: its chunks are synchronous and
/// message-sequenced (SimulateWithProgressBar), so nothing can interleave with them.
let continueStepRun (model: Model) : Model * Elmish.Cmd<Msg> =
    match model.StepRunTarget, model.PopupDialogData.Progress, model.CurrentStepSimulationStep with
    | Some prog, Some _, Some(Ok simData) when not model.SimulateInRenderer ->
        if simData.ClockTickNumber >= prog.FinalClock then
            // arrived: the run is over, and the bar with it
            model |> Optic.set stepRunTarget_ None, Elmish.Cmd.ofMsg (SetPopupProgress None)
        elif not (Map.isEmpty model.SidecarInFlight) then
            // the wire is held - a panel read that got in while the last chunk was ending. Its
            // reply handler calls back here, so the run resumes the moment the wire is free.
            model, Elmish.Cmd.none
        else
            match model.SidecarSession.Epoch, model.CurrentProj with
            | Some epoch, _ ->
                let seq = ModelHelpers.newSeq ()
                let before = simData.ClockTickNumber
                let t1 = TimeHelpers.getTimeMs ()

                let chunk =
                    Elmish.Cmd.OfPromise.either
                        (fun () -> SidecarSession.runChunk epoch prog.FinalClock)
                        ()
                        (fun result -> SidecarReply(seq, AnsSteppedTo(before, t1, result)))
                        (fun exn -> SidecarReply(seq, AnsSteppedTo(before, t1, Error exn.Message)))

                model |> Optic.map sidecarInFlight_ (Map.add seq (OpStep prog.FinalClock)), chunk
            | None, Some _ ->
                // no session yet: the first thing the run needs is the build, issued by the one
                // place that issues step builds, and its reply brings us back for the first chunk
                issueStepBuild model simData
            | None, None -> model, Elmish.Cmd.none
    | _ -> model, Elmish.Cmd.none
