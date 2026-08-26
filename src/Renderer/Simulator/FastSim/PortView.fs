/// The ports of one elaborated sheet instance that a waveform can be taken of.
///
/// **This is the narrow question the renderer should be asking a simulator**, and the one
/// `SimInterface` describes: not "give me every wave in the design" - which is proportional to the
/// expansion and is what a remote simulator exists to keep out of the renderer - but "which ports
/// does THIS instance offer", asked of the handful of instances on screen.
///
/// Everything here that a design could answer is answered from the design instead; what is left is
/// what only an elaborated simulation knows. That is two things, and they are the reason this
/// cannot be a design-time query:
///
///   the WIDTH of a port, because parameters are resolved when a design is elaborated, so two
///   instances of one sheet are meant to differ;
///
///   where the port's data LIES - its driver and its step array - which is a fact about one build
///   and is invalidated by the next.
///
/// Below the UI for the same reason `RamView` and `WaveNames` are: the .NET sidecar has a
/// FastSimulation of its own and must answer this from exactly this code, or the two agree until
/// they do not.
module PortView

open CommonTypes
open SimGraphTypes
open SimTypes

/// One port of one instance, with everything a waveform needs that the design cannot supply.
///
/// The field names are prefixed because a record field resolves to the last type declaring one,
/// and the wave simulator is full of ports, widths and names.
type InstancePort =
    { /// the DESIGN component this is a port of - which, with the instance, names it
      PortComp: ComponentId
      PortIs: PortType
      PortNum: int
      /// where this port's data lies in the simulation that answered: the step array...
      PortArrayIndex: int
      /// ...and the driver, which is what a waveform is read from. This is the build's read
      /// HANDLE: the fetch quotes it back, and the simulator answers with no lookup by name at
      /// all. An input port's is already the array of the output driving it - the simulator's own
      /// linker resolved that, crossing sheet boundaries where the net does.
      PortDriver: int
      PortWidth: int
      /// comp.port(w:0), as the selector lists it and the viewer titles it
      PortDisplayName: string
      /// the port's own name, without the component - what the port-selection dialog shows
      PortLabel: string
      /// the label on the component the port belongs to
      PortCompLabel: string }

/// What one instance offers, with the facts that are about the INSTANCE rather than about any one
/// of its ports worked out once. Both were being recomputed per wave, and each walks the instance
/// path - which made reconciling a hundred waves quadratic in the depth of the design for no
/// reason at all.
type InstanceView =
    { /// the design-time sheet this instance is a copy of
      ViewSheet: string
      /// the labels of the instances containing this one, outermost first, upper-cased - what
      /// tells two instances of one sheet apart when a name cannot
      ViewSubSheet: string list
      ViewPorts: InstancePort list }

/// The width and driver index of one port: what a wave needs from the BUILD, and nothing else.
///
/// Two ints, because this is what travels when the simulator is in another process. Everything
/// else a selector shows - names, labels, which ports carry a wave at all - is a fact about the
/// design, which the renderer holds; only elaboration facts need the simulation, and these are
/// they. The driver index doubles as the build's read handle: `SimRead` accepts it for as long as
/// this build lives, and the next build invalidates it wholesale.
type PortSlot =
    { SlotWidth: int
      /// the step array this port reads: an output's own, an input's the output driving it.
      /// Resolved by the SIMULATOR (an input's driver can cross a sheet boundary, and the linker
      /// has already followed it), so the renderer never re-derives it.
      SlotDriver: int }

/// Every port of one component of one instance, positionally: array index = port number, which
/// is the design's own numbering (a Component's port lists are position-numbered).
type ComponentSlots =
    { SlotsComp: ComponentId
      SlotsIns: PortSlot array
      SlotsOuts: PortSlot array }

/// Width and driver of every port of every component on one instance's sheet.
///
/// ALL ports, positionally - not the wave-carrying subset. Filtering would mean naming what was
/// kept, which costs more than the entries it saves, and the rule for what carries a wave is a
/// design fact the caller already has. The one build fact inside that rule - which member of an
/// IOLabel group is elected to drive it - arrives implicitly: the members share a driver index,
/// so deduplicating by driver keeps one of them, and name and data are the same whichever it is.
///
/// A width of 0 is a port with no signal: an unconnected input, still holding the dummy array it
/// was created with.
///
/// This is the sidecar's answer to "what does this instance offer" and the renderer's local
/// answer to the same question, from one body of code - which is the point.
let sheetSliceOf (fs: FastSimulation) (InstancePath ap as instance) : ComponentSlots list =
    let sheet = fs.Design.SheetOfInstance instance

    let slotsOf (arrays: IOArray array) =
        arrays |> Array.map (fun io -> { SlotWidth = io.Width; SlotDriver = io.Index })

    fs.Design.DesignSheets
    |> List.tryFind (fun ldc -> ldc.Name = sheet)
    |> Option.map (fun ldc ->
        fst ldc.CanvasState
        |> List.choose (fun comp ->
            fs.ComponentOf(ComponentId comp.Id, ap)
            |> Option.map (fun fc ->
                { SlotsComp = ComponentId comp.Id
                  SlotsIns = slotsOf fc.InputLinks
                  SlotsOuts = slotsOf fc.Outputs })))
    |> Option.defaultValue []

/// The wave index of a port: how the rest of Issie names one, and what a selection is made of.
let waveIndexOf (instance: InstancePath) (port: InstancePort) : WaveIndexT =
    let (InstancePath ap) = instance

    { SimArrayIndex = port.PortArrayIndex
      Id = port.PortComp, ap
      PortType = port.PortIs
      PortNumber = port.PortNum }

/// The ports of one instance that carry a waveform.
///
/// Worked out from the DESIGN - the sheet that instance is a copy of names its own components -
/// and each of those looked up in the simulation for its ports and their arrays. So this costs the
/// size of one sheet, however many times that sheet is instantiated.
///
/// The rule deciding which ports carry a wave is `FastCreate.portCarriesWave`, the same one the
/// built wave index comes from, so the two cannot disagree.
let ofInstance (fs: FastSimulation) (InstancePath ap as instance) : InstanceView =
    let sheet = fs.Design.SheetOfInstance instance

    let ports =
        fs.Design.DesignSheets
        |> List.tryFind (fun ldc -> ldc.Name = sheet)
        |> Option.map (fun ldc ->
            fst ldc.CanvasState
            |> List.collect (fun comp ->
                match fs.ComponentOf(ComponentId comp.Id, ap) with
                | None -> []
                | Some fc ->
                    let portsOf portType (arrays: IOArray array) =
                        if not (FastCreate.portCarriesWave fs fc portType) then
                            []
                        else
                            arrays
                            |> Array.toList
                            |> List.mapi (fun pn io ->
                                let displayName, portLabel =
                                    match portType with
                                    | PortType.Input ->
                                        WaveNames.getInputName true fc (InputPortNumber pn),
                                        WaveNames.getInputName false fc (InputPortNumber pn)
                                    | PortType.Output ->
                                        WaveNames.getOutputName true fc (OutputPortNumber pn) fs,
                                        WaveNames.getOutputName false fc (OutputPortNumber pn) fs

                                // The array's own width, not the driver table's. They are the
                                // same number - a driver's width is set from the width of the
                                // output whose array it is, and an input link IS that array after
                                // linking - and this one is there in a build made without the
                                // wave tables.
                                let width = io.Width

                                { PortComp = ComponentId comp.Id
                                  PortIs = portType
                                  PortNum = pn
                                  PortArrayIndex = io.Index
                                  PortDriver = io.Index
                                  PortWidth = width
                                  PortDisplayName = WaveNames.caseCompAndPortName displayName
                                  PortLabel = portLabel
                                  PortCompLabel = fc.FLabel })

                    portsOf PortType.Output fc.Outputs @ portsOf PortType.Input fc.InputLinks))
        |> Option.defaultValue []

    { ViewSheet = sheet
      ViewSubSheet =
        [ 1 .. ap.Length ]
        |> List.map (fun i ->
            (fs.Design.LabelOfInstance(InstancePath ap[0 .. i - 1]) |> Option.defaultValue "?").ToUpper())
      ViewPorts = ports }

/// The ports of an instance, remembered for as long as the simulation is.
///
/// `ofInstance` costs one sheet. That is the right cost to pay once for an instance and the wrong
/// cost to pay per selected wave: reconciling a hundred waves walked their sheet a hundred times,
/// and the test suite went from 27 seconds to 43 when it started to.
///
/// Only instances actually asked about are held - the ones the selector is showing, and the ones
/// selected waves are in - so what this holds is bounded by what the UI touches rather than by the
/// expansion. Keyed on the simulation, which is rebuilt rather than mutated, so a new one is the
/// signal that all of it is stale; emptied when a simulation ends, by Helpers.clearIdentityMemos.
let ofInstanceCached: FastSimulation -> InstancePath -> InstanceView =
    Helpers.memoizeByIdentity (fun (fs: FastSimulation) ->
        let held = System.Collections.Generic.Dictionary<InstancePath, InstanceView>()

        fun instance ->
            match held.TryGetValue instance with
            | true, ports -> ports
            | _ ->
                let ports = ofInstance fs instance
                held[instance] <- ports
                ports)
