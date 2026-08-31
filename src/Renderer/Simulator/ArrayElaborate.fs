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
    /// The most copies an array design sheet may have.
    ///
    /// A backstop rather than a considered limit: the expansion is n copies of a whole sheet, and
    /// GraphMerger's own budget check prices that properly once it exists. This is here so that a
    /// mistyped number is a message rather than a design Issie tries to build.
    let maxArrayCopies = 256

/// The character that makes a generated sheet name unreachable as a user's: a sheet name is a file
/// name, so a path separator can never be typed into one.
///
/// The body sheet's name is seen - it is what the waveform selector shows over the copies - so it
/// is otherwise the sheet's own name, which is what someone reading the viewer expects.
let private bodyMarker = "/"

/// The name of the sheet holding one copy of an array design sheet.
let bodyNameOf (sheetName: string) = sheetName + bodyMarker + "copy"

/// The label a body port takes from the array component that becomes it.
///
/// A join's own label names a CHANNEL and is shared with the join at the other end, so the two
/// would be one name on the body; the direction tells them apart. Everything else keeps its label,
/// which is already unique among the sheet's outputs.
let private bodyPortLabel (comp: Component) =
    match comp.Type with
    | JoinIn _ -> comp.Label + "_in"
    | JoinOut _ -> comp.Label + "_out"
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
            | JoinOut (w, _) | BusOut w | ArrayOut w -> Some (Output w)
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

/// What feeds one of the wrapper's own outputs.
type private OutputDriver =
    /// one copy's body port, straight through - an ordinary Output, or a loose join end
    | FromCopy of Copy: int * BodyPort: int
    /// every copy's body port, concatenated - a BusOut
    | FromMerge of BodyPort: int
    /// every copy's body port, selected between - a declared multiplexer, and which of the
    /// generated select inputs drives it
    | FromMux of BodyPort: int * SelectIndex: int

/// The wrapper as it is being put together.
///
/// Components carry a position because a sheet's ports are read off its Input1 and Output
/// components in (Y, X) order: laying them down in the order they are made is what makes the
/// wrapper's own signature come out as the outline its instances were given.
type private Build = {
    /// Ids handed out from above every id the design already uses, densely and in order.
    ///
    /// NOT from Helpers.IdAllocator, which is global mutable state that is never freed: expansion
    /// runs on every build, so an allocator would grow without bound, and a pure function that
    /// gives the same design the same ids twice is what lets one simulation be compared with the
    /// one before it. Dense and positive because FastCreate indexes arrays by the raw integer: a
    /// negative id throws under .NET and silently corrupts the build under Fable, and a sparse one
    /// allocates an array as long as the largest id in it.
    NextComp: int
    NextPort: int
    NextConn: int
    /// Newest first, reversed when the build is finished.
    Comps: Component list
    Conns: Connection list
    Slots: ComponentSlotExpr
    /// How far down the sheet the next component goes.
    Row: int
}

let private addComp (b: Build) (compType: ComponentType) (label: string) (nIn: int) (nOut: int) =
    let id = ComponentId b.NextComp
    let ports first n portType =
        [ for i in 0 .. n - 1 ->
            { Id = PortId (b.NextPort + first + i)
              PortNumber = Some i
              PortType = portType
              HostId = id } ]
    let comp =
        { Id = id
          Type = compType
          Label = label
          InputPorts = ports 0 nIn PortType.Input
          OutputPorts = ports nIn nOut PortType.Output
          X = 0.
          Y = float (b.Row * 100)
          H = 30.
          W = 60.
          SymbolInfo = None
          SlotInfo = None }
    comp,
    { b with
        NextComp = b.NextComp + 1
        NextPort = b.NextPort + nIn + nOut
        Comps = comp :: b.Comps
        Row = b.Row + 1 }

let private wire (b: Build) (src: Component) (srcPort: int) (tgt: Component) (tgtPort: int) =
    let conn =
        { Id = ConnectionId b.NextConn
          Source = { src.OutputPorts[srcPort] with PortNumber = None }
          Target = { tgt.InputPorts[tgtPort] with PortNumber = None }
          Vertices = [] }
    { b with NextConn = b.NextConn + 1; Conns = conn :: b.Conns }

/// Give a generated port the width expression its array component had, where it had one. Without
/// this an instance binding a width would resize the copies and leave the array's own ports as
/// they were.
let private addWidthSlot (b: Build) (comp: Component) (expr: ConstrainedExpr option) =
    match expr with
    | None -> b
    | Some e -> { b with Slots = addSlot {CompId = comp.Id; CompSlot = IO comp.Label} e b.Slots }

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

/// The wrapper: n instances of the body, wired up, with the sheet's derived ports and the glue
/// that makes them. Returns the canvas, the parameter data it declares, and the ids left over.
let private wrapperOf
        (sheet: LoadedComponent)
        (info: ArrayInfo)
        (bodyName: string)
        ((bodyIns, bodyOuts): (string * int) list * (string * int) list)
        (start: Build)
        : CanvasState * ParameterDefs * Build =
    let comps, _ = sheet.CanvasState
    let copies = copiesOfArray info
    let wiring = joinsOf info sheet.LCParameterSlots sheet.CanvasState
    let sheetDefs =
        sheet.LCParameterSlots |> Option.defaultValue {DefaultBindings = Map.empty; ParamSlots = Map.empty}

    /// Which port of a copy carries the value of the array component `comp`.
    let bodyIn (comp: Component) = bodyIns |> List.findIndex (fun (l, _) -> l = bodyPortLabel comp)
    let bodyOut (comp: Component) = bodyOuts |> List.findIndex (fun (l, _) -> l = bodyPortLabel comp)

    /// The width expression the sheet gives an array component, where it gives one.
    let widthExprOf (comp: Component) =
        tryFindSlot {CompId = comp.Id; CompSlot = IO comp.Label} sheetDefs.ParamSlots

    /// The unmatched ends of one join, one port per channel - the reading arrayOutlineOf takes.
    let looseEndsOf (ends: JoinEnd list) (comp: Component) =
        ends
        |> List.filter (fun e -> e.Comp.Id = comp.Id)
        |> List.distinctBy (fun e -> e.Num)
        |> List.sortBy (fun e -> e.Num)

    let ordered = comps |> List.sortBy (fun comp -> comp.Y, comp.X)

    /// The multiplexers whose source is actually on the sheet. One that names an Array out the
    /// sheet does not have is reported by arrayOutlineOf and contributes nothing here.
    let muxes =
        info.Muxes
        |> List.choose (fun spec ->
            ordered
            |> List.tryFind (fun comp ->
                match comp.Type with | ArrayOut _ -> comp.Label = spec.MuxSource | _ -> false)
            |> Option.map (fun source -> spec, source))

    // ---- the copies ----
    // Each binds every parameter the sheet declares straight through by name, and the loop variable
    // to its own index. Evaluated in the wrapper's environment - whatever the instance of the array
    // sheet bound - so a width given to the array reaches every copy.
    let copyBindings (i: int) =
        sheetDefs.DefaultBindings
        |> Map.map (fun name _ -> PParameter name)
        |> Map.add info.LoopParam (PInt (bigint i))

    let copyComps, b =
        (([], start), [0 .. copies - 1])
        ||> List.fold (fun (acc, b) i ->
            let ct =
                Custom
                    { Name = bodyName
                      InputLabels = bodyIns
                      OutputLabels = bodyOuts
                      Form = Some User
                      ParameterBindings = Some (copyBindings i)
                      Description = None }
            let comp, b = addComp b ct $"{sheet.Name}{i}" (List.length bodyIns) (List.length bodyOuts)
            comp :: acc, b)
        |> fun (acc, b) -> List.rev acc |> Array.ofList, b

    // ---- the sheet's own inputs, in outline order, wired to whatever they drive ----
    let b =
        ordered
        |> List.collect (fun comp ->
            match comp.Type with
            // one input, driven to EVERY copy
            | Input1 (w, _) -> [comp.Label, w, widthExprOf comp, [for i in 0 .. copies - 1 -> i, bodyIn comp]]
            // a loose join end: this copy takes its value from outside the array
            | JoinIn (w, _) ->
                looseEndsOf wiring.UnmatchedIn comp
                |> List.map (fun e -> joinInPortName comp.Label e.Num, w, widthExprOf comp, [e.Copy, bodyIn comp])
            | _ -> [])
        |> List.fold (fun b (label, w, expr, targets) ->
            let comp, b = addComp b (Input1 (w, None)) label 0 1
            let b = addWidthSlot b comp expr
            targets |> List.fold (fun b (copy, port) -> wire b comp 0 copyComps[copy] port) b) b

    // ---- a select input per multiplexer, after the ports the components generate ----
    // A select is a plain number of bits and never an expression: it follows the copy count, which
    // is a plain number on the sheet.
    let selectComps, b =
        (([], b), muxes)
        ||> List.fold (fun (acc, b) (spec, _) ->
            let comp, b = addComp b (Input1 (arraySelectWidth copies, None)) (muxSelectPortName spec) 0 1
            comp :: acc, b)
        |> fun (acc, b) -> List.rev acc |> Array.ofList, b

    // ---- the sheet's own outputs, in outline order, and what drives each ----
    let outputSpecs =
        (ordered
         |> List.collect (fun comp ->
            match comp.Type with
            // an ordinary Output is one port per copy
            | Output w ->
                [for i in 0 .. copies - 1 -> $"{comp.Label}_{i}", w, widthExprOf comp, FromCopy (i, bodyOut comp)]
            // the copies' values concatenated: as many times as wide, so the width expression is too
            | BusOut w ->
                [ comp.Label, w * copies,
                  widthExprOf comp
                  |> Option.map (fun e -> {e with Expression = PMultiply (PInt (bigint copies), e.Expression)}),
                  FromMerge (bodyOut comp) ]
            | JoinOut (w, _) ->
                looseEndsOf wiring.UnmatchedOut comp
                |> List.map (fun e ->
                    joinOutPortName comp.Label e.Num, w, widthExprOf comp, FromCopy (e.Copy, bodyOut comp))
            | _ -> []))
        // a multiplexer's output is as wide as the values it selects between
        @ (muxes
           |> List.mapi (fun i (spec, source) ->
                let w = match source.Type with | ArrayOut w -> w | _ -> 1
                spec.MuxName, w, widthExprOf source, FromMux (bodyOut source, i)))

    let outputComps, b =
        (([], b), outputSpecs)
        ||> List.fold (fun (acc, b) (label, w, expr, driver) ->
            let comp, b = addComp b (Output w) label 1 0
            let b = addWidthSlot b comp expr
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
            | FromMux (port, selIx) ->
                // the data inputs, then the select LAST, as a Mux2's is
                let mux, b = addComp b (ArrayMux copies) $"{outComp.Label}_mux" (copies + 1) 1
                let b = (b, [0 .. copies - 1]) ||> List.fold (fun b i -> wire b copyComps[i] port mux i)
                let b = wire b selectComps[selIx] 0 mux copies
                wire b mux 0 outComp 0) b

    // ---- and the wires between the copies: one per matched join ----
    let b =
        wiring.Matched
        |> List.fold (fun b (outEnd, inEnd) ->
            wire b copyComps[outEnd.Copy] (bodyOut outEnd.Comp) copyComps[inEnd.Copy] (bodyIn inEnd.Comp)) b

    (List.rev b.Comps, List.rev b.Conns),
    { DefaultBindings = sheetDefs.DefaultBindings; ParamSlots = b.Slots },
    b

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
        | Input1 _ | Output _ | BusOut _ | ArrayOut _ | JoinOut _ | JoinIn _ -> true
        | _ -> false)
    |> List.countBy bodyPortLabel
    |> List.filter (fun (_, n) -> n > 1)
    |> List.map (fun (label, _) ->
        $"'{sheetName}' has two components that would both be a port called '{label}' of one copy: \
          rename one of them")

/// Every array design sheet replaced by the two ordinary sheets it expands to, and everything that
/// is wrong with any of them.
///
/// After this the simulator has never seen an array sheet: BusOut, ArrayOut, JoinOut and JoinIn are
/// gone from every canvas, and what is left is custom components, wires and the two array glue
/// types. That is why the evaluators, the truth table and the dependency machinery need no part in
/// this, and why the wave selector shows the copies without being told about arrays.
let expandArraySheets (ldcs: LoadedComponent list) : LoadedComponent list * string list =
    let nextComp, nextPort, nextConn = firstFreeIds ldcs
    let start =
        { NextComp = nextComp; NextPort = nextPort; NextConn = nextConn
          Comps = []; Conns = []; Slots = Map.empty; Row = 0 }

    let expandOne (b: Build) (sheet: LoadedComponent) (info: ArrayInfo) =
        let copies = copiesOfArray info
        let sizeProblems =
            if info.EndValue < 0 then
                [$"'{sheet.Name}' says its loop variable ends at {info.EndValue}, which is before it starts"]
            elif copies > Constants.maxArrayCopies then
                [$"'{sheet.Name}' asks for {copies} copies, and Issie expands at most {Constants.maxArrayCopies}"]
            else []
        match sizeProblems with
        | _ :: _ ->
            // nothing can be built from a copy count that makes no sense, so the sheet is passed
            // through as it is and the message is what the caller acts on
            [sheet], sizeProblems, b
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
            let canvas, defs, b = wrapperOf sheet info bodyName (bodyIns, bodyOuts) b
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
             |> List.map (fun msg -> $"array design sheet '{sheet.Name}': {msg}")),
            b

    ((([], []), start), ldcs)
    ||> List.fold (fun ((sheets, problems), b) ldc ->
        match ldc.ArrayInfo with
        | None -> (ldc :: sheets, problems), b
        | Some info ->
            let made, newProblems, b = expandOne b ldc info
            // the wrapper keeps the sheet's place in the list; the body follows it
            ((List.rev made) @ sheets, problems @ newProblems), b)
    |> fun ((sheets, problems), _) -> List.rev sheets, problems
