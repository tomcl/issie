module ArrayElaborate

(*
    ArrayElaborate.fs

    Turning an array design sheet into ordinary ones. ArrayExpand says what such a sheet MEANS -
    which copy joins to which, and what ports the sheet therefore has; this says what it IS, as
    hardware the rest of Issie already understands.

    One array sheet becomes TWO ordinary sheets:

      the BODY     one copy of what is drawn on the sheet, with its array IO rewritten into
                   ordinary Input1 and Output components. Every component keeps its id, so the
                   sheet's parameter slots apply to it unchanged.

      the WRAPPER  the array sheet itself, replaced: n instances of the body, wired to each other
                   by the matched joins, with the sheet's derived ports and the glue that makes
                   them - an ArrayMerge per BusOut and an ArrayMux per declared multiplexer.

    That is what the waveform selector sees, so an array shows as its own ports plus n numbered
    instances of the body, grouped and sliced like any other sheet, with nothing in the selector
    knowing what an array sheet is.

    WHY THIS IS A PLAIN REWRITE. The number of copies is a fixed integer on the sheet, not a
    parameter expression, so the expansion is a fact about the sheet and not about who instantiated
    it. That is what lets this run once per sheet before anything else, and why GraphMerger needs no
    part in it: an instance of the wrapper is an ordinary custom component, its parameters resolve
    as any sheet's do, and the copies inside it are ordinary instances of the body.

    Separate from ArrayExpand because it needs CanvasExtractor - the body's ports are read the way
    every other sheet's are - and ArrayExpand is compiled before CanvasExtractor so that
    parseDiagramSignature can ask it what an array sheet's ports are.
*)

open CommonTypes
open ParameterTypes
open ArrayExpand

module Constants =
    /// The most copies an array component may have.
    ///
    /// A bound of the same kind as CommonTypes.Constants.maxIssieBusWidth: there is no real need
    /// for one, since the expansion is priced properly by GraphMerger's budget check, but a copy
    /// count is a small number by nature and a mistyped one should be a message rather than a
    /// design Issie tries to build. It is also what makes a select width an int calculation.
    let maxArrayCopies = 1024

    /// The most copies an array component may have before the waveform selector stops offering
    /// every copy's ports at once.
    ///
    /// Below it, the copies' ports are listed flat on the array itself, which is the quickest way
    /// to compare one copy with another. Above it that list is the wrong shape - 64 copies of a
    /// five-port body is 320 signals in one group, and the bound is 1024 - so the ports are offered
    /// INSIDE the copy instead, one copy at a time, through the combo box that is already there for
    /// choosing which. Nothing is unreachable either way; what changes is whether they arrive all
    /// at once or one copy at a time.
    let copiesShownFlattened = 64

/// The character that makes a generated sheet name unreachable as a user's: a sheet name is a file
/// name, so a path separator can never be typed into one.
///
/// The body sheet's name is seen - it is what the waveform selector shows over the copies - so it
/// is otherwise the sheet's own name, which is what someone reading the viewer expects.
let private bodyMarker = "/"

/// The name of the sheet holding one copy of an array design sheet.
let bodyNameOf (sheetName: string) = sheetName + bodyMarker + "instance"

/// The label a body port takes from the array component that becomes it.
///
/// A join's own label names a CHANNEL and is shared with the join at the other end, so the two
/// would be one name on the body; the direction tells them apart. Everything else keeps its label,
/// which is already unique among the sheet's outputs.
let private bodyPortLabel (comp: Component) =
    match comp.Type with
    // A join's own label names a CHANNEL, and a sheet may have several joins on one channel at
    // different numbers - a copy that reads two of its neighbours, say. Each is a separate port of
    // the copy, so the body label carries the number as well as the direction. The number here is
    // the one the sheet is DRAWN at, which is only an identifier: what makes it distinct is the
    // rule that two joins facing the same way may not share a channel and a number.
    //
    // Named by the same functions that name the array's own loose-end ports. A body port and an
    // outline port are different things, but they are spelt the same way, and spelling it twice is
    // how one of them changes and the other does not.
    | JoinIn (_, n) -> joinInPortName comp.Label n
    | JoinOut (_, n) -> joinOutPortName comp.Label n
    | _ -> comp.Label

/// The array design sheet as ONE copy: an ordinary sheet.
///
/// Every component keeps its id and its ports, so the connections are untouched and the sheet's
/// parameter slots - keyed by component id, and matching an IO slot by kind rather than by label -
/// apply to the body exactly as they applied to the sheet.
let bodyCanvasOf ((comps, conns): CanvasState) : CanvasState =
    let rewrite (comp: Component) =
        let newType =
            match comp.Type with
            // a Join in supplies its copy's value: on the body it is where that value comes in
            | JoinIn (w, _) -> Some (Input1 (w, None))
            // everything else that is array IO takes one value per copy, so on the body it is one
            // ordinary output; what the copies DO with the value is the wrapper's business
            | JoinOut (w, _) | BusOut w | MuxOut w -> Some (Output w)
            | _ -> None
        match newType with
        | None -> comp
        | Some t -> { comp with Type = t; Label = bodyPortLabel comp }
    List.map rewrite comps, conns

/// The parameter data the body sheet declares.
///
/// The array sheet's own parameters, plus the LOOP VARIABLE - which is not a declared property of
/// the array sheet (it is named by its array settings and has a value only inside a copy) and must
/// be an ordinary one here, because binding it per copy is how one copy differs from the next.
///
/// The channel-number slots are dropped: a body component is an Input1 or an Output and has no
/// channel, and which copy joins to which has been settled by the time this is built.
let private bodyDefsOf (info: ArrayInfo) (defs: ParameterDefs option) : ParameterDefs =
    let sheetDefs = defs |> Option.defaultValue {DefaultBindings = Map.empty; ParamSlots = Map.empty}
    { DefaultBindings =
        sheetDefs.DefaultBindings
        |> Map.add info.LoopParam
            { Expression = PInt 0I
              Description = "which copy of the array design sheet this is, counting from 0" }
      ParamSlots = sheetDefs.ParamSlots |> Map.filter (fun slot _ -> slot.CompSlot <> JoinNum) }

//-------------------------------------------------------------------------------------------//
//----------------------------------BUILDING THE WRAPPER-------------------------------------//
//-------------------------------------------------------------------------------------------//

/// The port labels an array copy is SHOWN with, by the copy's component id.
///
/// Display only. A Custom component's port labels are what wire it to its sheet's Input and Output
/// components, so they cannot carry anything but the body's; these go on the design's copy of the
/// component, which is what names ports in the waveform selector. See wrapperOf.copyPortNames.
type CopyPortNames = Map<ComponentId, (string * int) list * (string * int) list>

/// What feeds one of the wrapper's own outputs.
type private OutputDriver =
    /// one copy's body port, straight through - an ordinary Output, or a loose join end
    | FromCopy of Copy: int * BodyPort: int
    /// every copy's body port, concatenated - a BusOut
    | FromMerge of BodyPort: int
    /// every copy's body port, selected between - a declared multiplexer, and which of the
    /// generated select inputs drives it
    | FromMux of BodyPort: int * Select: Component

/// A source of ids for the components, ports and connections the expansion makes.
///
/// Handing out ids IS state: each one must differ from the last. It is confined to withIdSource
/// below - the counters are locals of that one function, reachable only through these three
/// functions and only while it runs - so nothing else in Issie can hold one, reset one, or hand out
/// an id after the expansion that owns it has finished. Both the type and the function are private,
/// so what may be inside that scope is one function long and can be read in one go.
type private IdSource = {
    NewComp: unit -> ComponentId
    NewPort: unit -> PortId
    NewConn: unit -> ConnectionId
}

/// The largest id of each kind anywhere in the design, so that generated ones can start above them.
let private firstFreeIds (ldcs: LoadedComponent list) =
    let maxOf f = ldcs |> List.collect f |> function | [] -> 0 | xs -> List.max xs
    let comps (ldc: LoadedComponent) = fst ldc.CanvasState |> List.map (fun c -> cToInt c.Id)
    let ports (ldc: LoadedComponent) =
        fst ldc.CanvasState
        |> List.collect (fun c -> c.InputPorts @ c.OutputPorts |> List.map (fun p -> pToInt p.Id))
    let conns (ldc: LoadedComponent) =
        snd ldc.CanvasState |> List.map (fun c -> let (ConnectionId n) = c.Id in n)
    maxOf comps + 1, maxOf ports + 1, maxOf conns + 1

/// Run `body` with a source of ids starting above every id the design already uses.
///
/// The counters live and die here, which is the whole point of the shape: an expansion cannot
/// outlive its ids and nothing outside can reach them. Deterministic despite being state - the same
/// design makes the same calls in the same order, so it gets the same ids twice, which is what lets
/// one simulation of a design be compared with the one before it.
///
/// NOT Helpers.IdAllocator, which is the design's own and is never freed: expansion runs on every
/// build, so taking ids from it would consume that namespace without bound and break the density
/// its own users depend on.
///
/// Dense and positive because FastCreate indexes arrays by the raw integer id: a negative one
/// throws under .NET and silently corrupts the build under Fable, and a sparse one allocates an
/// array as long as the largest id in it.
let private withIdSource (ldcs: LoadedComponent list) (body: IdSource -> 'a) : 'a =
    let firstComp, firstPort, firstConn = firstFreeIds ldcs
    // ref cells rather than `let mutable`, which F# will not let a closure capture
    let comp = ref firstComp
    let port = ref firstPort
    let conn = ref firstConn
    let next (counter: int ref) =
        let taken = counter.Value
        counter.Value <- taken + 1
        taken
    body
        { NewComp = fun () -> ComponentId (next comp)
          NewPort = fun () -> PortId (next port)
          NewConn = fun () -> ConnectionId (next conn) }

/// The wrapper as it is being put together.
///
/// Components carry a position because a sheet's ports are read off its Input1 and Output
/// components in (Y, X) order: laying them down in the order they are made is what makes the
/// wrapper's own signature come out as the outline its instances were given.
type private Build = {
    /// Newest first, reversed when the build is finished.
    Comps: Component list
    Conns: Connection list
    Slots: ComponentSlotExpr
    /// How far down the sheet the next component goes.
    Row: int
}

let private emptyBuild = { Comps = []; Conns = []; Slots = Map.empty; Row = 0 }

let private addComp (ids: IdSource) (b: Build) (compType: ComponentType) (label: string) (nIn: int) (nOut: int) =
    let id = ids.NewComp ()
    let ports n portType =
        [ for i in 0 .. n - 1 ->
            { Id = ids.NewPort ()
              PortNumber = Some i
              PortType = portType
              HostId = id } ]
    let comp =
        { Id = id
          Type = compType
          Label = label
          InputPorts = ports nIn PortType.Input
          OutputPorts = ports nOut PortType.Output
          X = 0.
          Y = float (b.Row * 100)
          H = 30.
          W = 60.
          SymbolInfo = None
          SlotInfo = None }
    comp, { b with Comps = comp :: b.Comps; Row = b.Row + 1 }

let private wire (ids: IdSource) (b: Build) (src: Component) (srcPort: int) (tgt: Component) (tgtPort: int) =
    let conn =
        { Id = ids.NewConn ()
          Source = { src.OutputPorts[srcPort] with PortNumber = None }
          Target = { tgt.InputPorts[tgtPort] with PortNumber = None }
          Vertices = [] }
    { b with Conns = conn :: b.Conns }

/// Give a generated port the width expression its array component had, where it had one. Without
/// this an instance binding a width would resize the copies and leave the array's own ports as
/// they were.
let private addWidthSlot (b: Build) (comp: Component) (expr: ConstrainedExpr option) =
    match expr with
    | None -> b
    | Some e -> { b with Slots = addSlot {CompId = comp.Id; CompSlot = IO comp.Label} e b.Slots }

/// The wrapper: n instances of the body, wired up, with the sheet's derived ports and the glue
/// that makes them.
///
/// Returns the canvas, the parameter data it declares, and the port labels each copy is SHOWN
/// with - see copyPortNames, which says why those are not the labels on the canvas.
let private wrapperOf
        (ids: IdSource)
        (sheet: LoadedComponent)
        (info: ArrayInfo)
        (bodyName: string)
        ((bodyIns, bodyOuts): (string * int) list * (string * int) list)
        : CanvasState * ParameterDefs * CopyPortNames =
    let addComp = addComp ids
    let wire = wire ids
    let copies = copiesOfArray info
    let (outlineIns, outlineOuts), wiring =
        outlinePortsOf info sheet.LCParameterSlots sheet.CanvasState
    let sheetDefs =
        sheet.LCParameterSlots |> Option.defaultValue {DefaultBindings = Map.empty; ParamSlots = Map.empty}

    /// Which port of a copy carries the value of the array component `comp`.
    let bodyIn (comp: Component) = bodyIns |> List.findIndex (fun (l, _) -> l = bodyPortLabel comp)
    let bodyOut (comp: Component) = bodyOuts |> List.findIndex (fun (l, _) -> l = bodyPortLabel comp)

    /// The ports of ONE copy, named by the channels that copy is actually joined by.
    ///
    /// FOR DISPLAY ONLY, and it has to be: a Custom component's port labels are what links it to
    /// its sheet's Input and Output components - FastCreate.indexOf finds each by label and width -
    /// so a copy whose labels said anything but the body's would not be wired to its own body.
    /// These are put on the design's copy of the component, which is what the waveform selector
    /// names ports from, and never on the canvas the simulation is built from.
    ///
    /// Worth the trouble because a join's body port carries the channel the SHEET is drawn at,
    /// which is copy 0's. Every copy being an instance of that one sheet, four carry-ins would
    /// otherwise all read C_in_0 - saying nothing about which copy's output feeds which copy's
    /// input, which is the whole of what a channel number is for. It matters most where it is
    /// hardest to work out by hand: a novice reading a chain, and anyone reading a channel whose
    /// number is a complicated expression in the loop variable.
    let copyPortNames =
        let byBodyLabel =
            wiring.Ends
            |> List.map (fun e -> (bodyPortLabel e.Comp, e.Copy), e)
            |> Map.ofList
        let renamed (copy: int) ((label, width): string * int) =
            match Map.tryFind (label, copy) byBodyLabel with
            | None -> label, width
            | Some e ->
                match e.Comp.Type with
                | JoinIn _ -> joinInPortName e.Comp.Label e.Num, width
                | _ -> joinOutPortName e.Comp.Label e.Num, width
        fun (copy: int) -> List.map (renamed copy) bodyIns, List.map (renamed copy) bodyOuts

    /// The width expression the sheet gives an array component, where it gives one.
    let widthExprOf (comp: Component) =
        tryFindSlot {CompId = comp.Id; CompSlot = IO comp.Label} sheetDefs.ParamSlots


    // ---- the copies ----
    // Each binds every parameter the sheet declares straight through by name, and the loop variable
    // to its own index. Evaluated in the wrapper's environment - whatever the instance of the array
    // sheet bound - so a width given to the array reaches every copy.
    let copyBindings (i: int) =
        sheetDefs.DefaultBindings
        |> Map.map (fun name _ -> PParameter name)
        |> Map.add info.LoopParam (PInt (bigint i))

    let copyComps, copyNames, b =
        ((([], []), emptyBuild), [0 .. copies - 1])
        ||> List.fold (fun ((acc, names), b) i ->
            let ct =
                Custom
                    { Name = bodyName
                      InputLabels = bodyIns
                      OutputLabels = bodyOuts
                      Form = Some User
                      ParameterBindings = Some (copyBindings i)
                      Description = None }
            let comp, b = addComp b ct $"{sheet.Name}{i}" (List.length bodyIns) (List.length bodyOuts)
            (comp :: acc, (comp.Id, copyPortNames i) :: names), b)
        |> fun ((acc, names), b) -> List.rev acc |> Array.ofList, Map.ofList names, b

    // ---- the sheet's own ports ----
    // Which ports there are, what they are called and what the array does with each is
    // ArrayExpand's answer, not a second opinion formed here: the sheet's SIGNATURE and the wrapper
    // that realises it have to be the same list in the same order, and stating the rules twice is
    // how those come to disagree. This turns each of them into a component and its wiring.
    //
    // The width EXPRESSION is added here rather than there because it is not part of what a port
    // IS: it is the sheet's parameter slot for the component the port comes from, which only the
    // expansion needs. A concatenated port is as many times as wide as one copy, so its expression
    // is multiplied to match the width the outline already gave it.
    let widthExprFor (p: OutlinePort) =
        match p.Role with
        | SelectOf _ -> None
        | Concatenated ->
            widthExprOf p.Comp
            |> Option.map (fun e -> {e with Expression = PMultiply (PInt (bigint copies), e.Expression)})
        | _ -> widthExprOf p.Comp

    let b =
        (b, outlineIns)
        ||> List.fold (fun b p ->
            let comp, b = addComp b (Input1 (p.Width, None)) p.Name 0 1
            let b = addWidthSlot b comp (widthExprFor p)
            // A select drives no copy - the multiplexer reads it - so it is wired with the glue.
            let intoCopies =
                match p.Role with
                | ToEveryCopy -> [0 .. copies - 1]
                | ToCopy copy -> [copy]
                | _ -> []
            intoCopies |> List.fold (fun b copy -> wire b comp 0 copyComps[copy] (bodyIn p.Comp)) b)

    /// The select input of each MuxOut, by the MuxOut it belongs to. Found by the name it was given
    /// rather than remembered through the fold, so the two cannot drift apart.
    let selectOf (source: Component) =
        b.Comps |> List.find (fun c -> c.Label = muxSelectPortName source.Label)

    let outputComps, b =
        (([], b), outlineOuts)
        ||> List.fold (fun (acc, b) p ->
            let comp, b = addComp b (Output p.Width) p.Name 1 0
            let b = addWidthSlot b comp (widthExprFor p)
            let driver =
                match p.Role with
                // qualified: OutputDriver has a FromCopy of its own, being the same idea one step
                // further on - which copy AND which of its ports
                | PortRole.FromCopy copy -> OutputDriver.FromCopy (copy, bodyOut p.Comp)
                | Concatenated -> FromMerge (bodyOut p.Comp)
                | Multiplexed -> FromMux (bodyOut p.Comp, selectOf p.Comp)
                | ToEveryCopy | ToCopy _ | SelectOf _ ->
                    failwithf "%A is an input role and cannot drive the output '%s'" p.Role p.Name
            (comp, driver) :: acc, b)
        |> fun (acc, b) -> List.rev acc, b

    // ---- the glue, after the ports, where it cannot be mistaken for one ----
    let b =
        outputComps
        |> List.fold (fun b (outComp, driver) ->
            match driver with
            | FromCopy (copy, port) -> wire b copyComps[copy] port outComp 0
            | FromMerge port ->
                let merge, b = addComp b (ArrayMerge copies) $"{outComp.Label}_merge" copies 1
                let b = (b, [0 .. copies - 1]) ||> List.fold (fun b i -> wire b copyComps[i] port merge i)
                wire b merge 0 outComp 0
            | FromMux (port, sel) ->
                // the data inputs, then the select LAST, as a Mux2's is
                let mux, b = addComp b (ArrayMux copies) $"{outComp.Label}_mux" (copies + 1) 1
                let b = (b, [0 .. copies - 1]) ||> List.fold (fun b i -> wire b copyComps[i] port mux i)
                let b = wire b sel 0 mux copies
                wire b mux 0 outComp 0) b

    // ---- and the wires between the copies: one per matched join ----
    let b =
        wiring.Matched
        |> List.fold (fun b (outEnd, inEnd) ->
            wire b copyComps[outEnd.Copy] (bodyOut outEnd.Comp) copyComps[inEnd.Copy] (bodyIn inEnd.Comp)) b

    (List.rev b.Comps, List.rev b.Conns),
    { DefaultBindings = sheetDefs.DefaultBindings; ParamSlots = b.Slots },
    copyNames

//-------------------------------------------------------------------------------------------//
//------------------------------------THE WHOLE PASS-----------------------------------------//
//-------------------------------------------------------------------------------------------//

/// Names the body sheet derives twice.
///
/// A join's label gains a direction and everything else keeps its own, so two array components can
/// still meet on the body: a Join in called A and an ordinary Input called A_in are one port there.
let private bodyNameProblems (sheetName: string) ((comps, _): CanvasState) =
    comps
    |> List.filter (fun comp ->
        match comp.Type with
        | Input1 _ | Output _ | BusOut _ | MuxOut _ | JoinOut _ | JoinIn _ -> true
        | _ -> false)
    |> List.countBy bodyPortLabel
    |> List.filter (fun (_, n) -> n > 1)
    |> List.map (fun (label, _) ->
        $"'{sheetName}' has two components that would both be a port called '{label}' of one copy: \
          rename one of them")

/// Every array design sheet replaced by the two ordinary sheets it expands to, and everything that
/// is wrong with any of them - each problem paired with the sheet it is about, so that a caller can
/// tell which of them the design being simulated actually depends on.
///
/// After this the simulator has never seen an array sheet: BusOut, MuxOut, JoinOut and JoinIn are
/// gone from every canvas, and what is left is custom components, wires and the two array glue
/// types. That is why the evaluators, the truth table and the dependency machinery need no part in
/// this, and why the wave selector shows the copies without being told about arrays.
let expandArraySheets (ldcs: LoadedComponent list) : LoadedComponent list * (string * string) list * CopyPortNames =
    withIdSource ldcs (fun ids ->

    let expandOne (sheet: LoadedComponent) (info: ArrayInfo) =
        let copies = copiesOfArray info
        let sizeProblems =
            if copies < 1 then
                [$"'{sheet.Name}' asks for {copies} copies, and an array component is at least one"]
            elif copies > Constants.maxArrayCopies then
                [$"'{sheet.Name}' asks for {copies} copies, and Issie expands at most {Constants.maxArrayCopies}"]
            else []
        match sizeProblems with
        | _ :: _ ->
            // nothing can be built from a copy count that makes no sense, so the sheet is passed
            // through as it is and the message is what the caller acts on
            [sheet], (sizeProblems |> List.map (fun msg -> sheet.Name, msg)), Map.empty
        | [] ->
            let bodyCanvas = bodyCanvasOf sheet.CanvasState
            let bodyIns, bodyOuts = CanvasExtractor.parseDiagramSignature bodyCanvas
            let bodyName = bodyNameOf sheet.Name
            let body =
                { sheet with
                    Name = bodyName
                    CanvasState = bodyCanvas
                    InputLabels = bodyIns
                    OutputLabels = bodyOuts
                    LCParameterSlots = Some (bodyDefsOf info sheet.LCParameterSlots)
                    ArrayInfo = None
                    // the body is not a sheet of the project and is never its top
                    IsTopSheet = false }
            let canvas, defs, copyNames = wrapperOf ids sheet info bodyName (bodyIns, bodyOuts)
            let wrapperIns, wrapperOuts = CanvasExtractor.parseDiagramSignature canvas
            let wrapper =
                { sheet with
                    CanvasState = canvas
                    InputLabels = wrapperIns
                    OutputLabels = wrapperOuts
                    LCParameterSlots = Some defs
                    // after expansion it IS an ordinary sheet, and must be read as one
                    ArrayInfo = None }
            let _, problems = ArrayExpand.arrayOutlineOf info sheet.LCParameterSlots sheet.CanvasState
            [wrapper; body],
            (problems @ bodyNameProblems sheet.Name sheet.CanvasState
             |> List.map (fun msg -> sheet.Name, $"array design sheet '{sheet.Name}': {msg}")),
            copyNames

    (([], [], Map.empty), ldcs)
    ||> List.fold (fun (sheets, problems, names) ldc ->
        match ldc.ArrayInfo with
        | None -> ldc :: sheets, problems, names
        | Some info ->
            let made, newProblems, newNames = expandOne ldc info
            // the wrapper keeps the sheet's place in the list; the body follows it
            (List.rev made) @ sheets,
            problems @ newProblems,
            // ids are design-unique, so no two arrays can name the same copy
            Map.fold (fun acc k v -> Map.add k v acc) names newNames)
    |> fun (sheets, problems, names) -> List.rev sheets, problems, names)
