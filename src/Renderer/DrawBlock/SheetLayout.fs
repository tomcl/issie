module SheetLayout

(*
    SheetLayout.fs

    Turns a SheetDescription - components and logical connections, no geometry - into a CanvasState
    that opens in Issie as a sheet a human can read.

    Two things are supplied here that the description deliberately lacks.

    POSITIONS. Components are laid out by recursive bisection: split them into two roughly equal
    halves with as few connections crossing between as possible, alternate the split axis with
    depth, and place each block in its own rectangle. Inputs and Outputs are not part of that -
    they are pinned to columns at the left and right edges, in the order they were declared. That
    is not decoration: CanvasExtractor.getOrderedCompLabels sorts a sheet's I/O by (Y, X) to decide
    the order of the sheet's own ports, and CanvasStateAnalyser compares that order against every
    instance of the sheet, so where an Input sits changes what the sheet means.

    WIRE ROUTES. None. Connections are written with no vertices, which is what makes Issie route
    them: BusWireUpdate.LoadConnections finds the saved ends nowhere near the ports, calls
    smartAutoroute on each wire, and the sheet-open sequence then runs the global separation pass.
    Both stages of the draw block's own wire creation therefore happen, on load, for free.

    Component sizes come from SymbolUpdate.createSymbolRecord - the same function the app uses when
    it loads a sheet - so blocks are spaced by what will actually be drawn rather than by a guess.
*)

open CommonTypes
open ParameterTypes
open DrawModelType
open SheetDescription

module Constants =
    /// Issie snaps symbols to this grid, so laying out on it makes the result look deliberate.
    let grid = float Symbol.Constants.gridSize
    /// Gap between two components, and between two blocks. Roughly a component wide, which is
    /// enough for a wire to turn a corner in without hugging a symbol.
    let componentGap = 2. * grid
    let blockGap = 3. * grid
    /// Gap between the input/output columns and the body of the sheet.
    let ioGap = 4. * grid
    let margin = 2. * grid
    /// Bound on the improvement passes when bisecting. Sheets here are tens of components, so this
    /// never binds in practice; it is here so a pathological graph cannot spin.
    let maxSwapPasses = 20

//------------------------------------------------------------------------------------------------//
//---------------------------------- Components and ports ----------------------------------------//
//------------------------------------------------------------------------------------------------//

/// The id a component gets in the generated canvas.
///
/// Component ids are normally uuids. These are readable and derived from the names instead, which
/// is a deliberate deviation: a generated sheet is far easier to debug when its ids say what they
/// are, the same description always produces the same file so fixtures can be diffed, and minting
/// a uuid goes through a Fable-only import that does not work under .NET. Nothing in Issie parses
/// a component id - they are opaque strings - but they must be unique across a PROJECT, not just
/// a sheet, which is why the sheet name is part of it.
let componentId (sheetName: string) (compName: string) = $"{sheetName}-{compName}"

let private labelOf (spec: CompSpec) = spec.Label |> Option.defaultValue spec.Name

/// Build the Component for one description entry.
///
/// Ids are readable and deterministic rather than uuids, both because a generated sheet is far
/// easier to debug that way and because minting a uuid goes through a Fable-only import that does
/// not work under .NET. They are qualified by the SHEET name because component ids must be unique
/// across a project - only labels are per-sheet. Ids unique per sheet alone are the legacy
/// convention, and Issie greets a project full of them with a "duplicate sheet ids corrected"
/// popup on load.
///
/// The size is whatever the app will give the component when it opens the sheet, because it is
/// worked out by the same function: SymbolUpdate.createSymbolRecord, which takes the type's
/// nominal size from getComponentProperties and then lets autoScaleHAndW widen a Custom component
/// to fit its port labels. Measuring those labels used to need a browser, so this had to guess at
/// a Custom component's width and always guessed the minimum; blocks around a wide one were then
/// spaced as though it were narrow.
let private buildComponent (sheetName: string) (spec: CompSpec) : Result<Component, string> =
    let id = componentId sheetName spec.Name
    let makePorts n portType =
        [ for i in 0 .. n - 1 ->
            { Id = $"{id}-{portType}-{i}"
              PortNumber = Some i
              PortType = portType
              HostId = id } ]
    try
        let nIn, nOut, h, w = Symbol.getComponentProperties spec.Type (labelOf spec)
        let comp = {
            Id = id
            Type = spec.Type
            Label = labelOf spec
            InputPorts = makePorts nIn PortType.Input
            OutputPorts = makePorts nOut PortType.Output
            X = 0.
            Y = 0.
            H = h
            W = w
            SymbolInfo = None
            SlotInfo = None
        }
        // no LoadedComponents to hand: they decide only whether a Custom component counts as
        // clocked, which colours it and leaves its size alone. Nor does the theme, which is
        // colour only.
        let sized = SymbolUpdate.createSymbolRecord [] SymbolT.ThemeType.Colourful comp
        Ok { comp with H = sized.Component.H; W = sized.Component.W }
    with e ->
        // getComponentProperties throws on the legacy ROM/RAM/Input types
        Error $"{spec.Name}: {spec.Type} cannot be placed on a sheet ({e.Message})"

/// Find the port a reference names. Tries the component type's port names first, then a port
/// index; an empty port name means "the only port in this direction".
let private resolvePort
        (byName: Map<string, Component>)
        (isOutput: bool)
        (portRef: PortRef)
        : Result<Port, string> =
    match Map.tryFind portRef.Comp byName with
    | None -> Error $"there is no component called {portRef.Comp}"
    | Some comp ->
        let ports = if isOutput then comp.OutputPorts else comp.InputPorts
        let direction = if isOutput then "output" else "input"
        let names =
            let inNames, outNames = CanvasStateAnalyser.portNames comp.Type
            (if isOutput then outNames else inNames) |> List.map (fun n -> n.Trim().ToUpper())
        let byIndex i =
            match List.tryItem i ports with
            | Some port -> Ok port
            | None ->
                Error $"{portRef.Comp} has {List.length ports} {direction} port(s), so {i} is out of range"
        match portRef.Port with
        | "" ->
            match ports with
            | [ only ] -> Ok only
            | _ ->
                Error $"{portRef.Comp} has {List.length ports} {direction} ports, so one must be named: write {portRef.Comp}/<name or number>"
        | wanted ->
            match names |> List.tryFindIndex ((=) (wanted.Trim().ToUpper())) with
            | Some i -> byIndex i
            | None ->
                match System.Int32.TryParse wanted with
                | true, i -> byIndex i
                | _ ->
                    let known =
                        match names |> List.filter (fun n -> n <> "") with
                        | [] -> $"it has no port names, so use a number 0..{List.length ports - 1}"
                        | ns -> "known names are " + String.concat ", " ns
                    Error $"{portRef.Comp} has no {direction} port called '{wanted}': {known}"

/// Connection ends carry no port number, as in a saved .dgm, and no vertices - Issie routes the
/// wire when the sheet is loaded.
let private buildConnection (index: int) (source: Port) (target: Port) : Connection =
    { Id = $"conn-{index}-{source.HostId}-{target.HostId}"
      Source = { source with PortNumber = None }
      Target = { target with PortNumber = None }
      Vertices = [] }

//------------------------------------------------------------------------------------------------//
//-------------------------------------- Bisection -----------------------------------------------//
//------------------------------------------------------------------------------------------------//

/// How many connections run between each pair of components.
let private weights (conns: Connection list) : Map<string * string, int> =
    (Map.empty, conns)
    ||> List.fold (fun acc conn ->
        let a, b = conn.Source.HostId, conn.Target.HostId
        match a = b with
        | true -> acc
        | false ->
            let key = if a < b then a, b else b, a
            Map.add key (1 + (Map.tryFind key acc |> Option.defaultValue 0)) acc)

let private weightBetween (w: Map<string * string, int>) a b =
    let key = if a < b then a, b else b, a
    Map.tryFind key w |> Option.defaultValue 0

/// Components ordered so that connected ones sit near each other: breadth-first from the most
/// connected component, which gives a starting partition whose cut is already small.
let private connectedOrder (w: Map<string * string, int>) (names: string list) : string list =
    let degree name = names |> List.sumBy (weightBetween w name)
    let rec walk (ordered: string list) (queue: string list) (remaining: string list) =
        match queue, remaining with
        // both empty, and only then: an earlier version stopped as soon as `remaining` emptied,
        // which threw away everything still queued. Those components got no position and stacked
        // up at the origin.
        | [], [] -> ordered
        | [], rest ->
            // a new component of the graph: restart from its most connected member
            let seed = rest |> List.maxBy degree
            walk ordered [ seed ] (List.filter ((<>) seed) rest)
        | next :: queueRest, rest ->
            let neighbours =
                rest
                |> List.filter (fun other -> weightBetween w next other > 0)
                |> List.sortByDescending (weightBetween w next)
            walk
                (ordered @ [ next ])
                (queueRest @ neighbours)
                (rest |> List.filter (fun r -> not (List.contains r neighbours)))
    match names with
    | [] -> []
    | _ ->
        let seed = names |> List.maxBy degree
        walk [] [ seed ] (List.filter ((<>) seed) names)

/// Split into two halves of roughly equal size with as few connections crossing as possible.
/// A breadth-first ordering gives the starting split; repeated best-swap passes improve it. Both
/// are heuristics - an exact minimum cut is NP-hard and would buy nothing here, where the point is
/// only that related components end up near each other.
let private bisect (w: Map<string * string, int>) (names: string list) : string list * string list =
    let ordered = connectedOrder w names
    let half = List.length ordered / 2
    let initialA = ordered |> List.take half
    let initialB = ordered |> List.skip half

    /// how much the cut would fall if this component moved to the other side
    let gain (side: string list) (other: string list) name =
        let external' = other |> List.sumBy (weightBetween w name)
        let internal' = side |> List.sumBy (weightBetween w name)
        external' - internal'

    let rec improve pass (a: string list) (b: string list) =
        match pass >= Constants.maxSwapPasses with
        | true -> a, b
        | false ->
            let best =
                List.allPairs a b
                |> List.map (fun (x, y) ->
                    (x, y), gain a b x + gain b a y - 2 * weightBetween w x y)
                |> function
                   | [] -> None
                   | candidates -> Some (List.maxBy snd candidates)
            match best with
            | Some ((x, y), g) when g > 0 ->
                improve (pass + 1) (y :: List.filter ((<>) x) a) (x :: List.filter ((<>) y) b)
            | _ -> a, b
    improve 0 initialA initialB

//------------------------------------------------------------------------------------------------//
//---------------------------------------- Placement ---------------------------------------------//
//------------------------------------------------------------------------------------------------//

/// A slicing floorplan: either one component, or two blocks side by side or one above the other.
type private Block =
    | Leaf of Component
    | Split of IsVertical: bool * Block * Block

/// Where a component belongs vertically, as a fraction of the sheet's height.
///
/// An I/O component sits at its own place in its column; everything else is pulled towards the I/O
/// it is connected to, by repeatedly averaging over neighbours. Bisection alone knows nothing
/// about where the I/O columns are, so without this a block dealing with the third input could
/// easily be placed above one dealing with the first, and its wires would run the height of the
/// sheet to get there.
let private verticalAffinity
        (isInput: Component -> bool)
        (isOutput: Component -> bool)
        (comps: Component list)
        (conns: Connection list)
        : Map<string, float> =
    let positionsIn (column: Component list) =
        let n = max 1 (List.length column)
        column |> List.mapi (fun i c -> c.Id, (float i + 0.5) / float n)
    let pinned =
        positionsIn (comps |> List.filter isInput) @ positionsIn (comps |> List.filter isOutput)
        |> Map.ofList
    let neighbours =
        (Map.empty, conns)
        ||> List.fold (fun acc conn ->
            let a, b = conn.Source.HostId, conn.Target.HostId
            let add k v (m: Map<string, string list>) =
                Map.add k (v :: (Map.tryFind k m |> Option.defaultValue [])) m
            acc |> add a b |> add b a)
    /// enough rounds for the pull to reach across any sheet this is meant for
    let rec relax rounds (values: Map<string, float>) =
        match rounds with
        | 0 -> values
        | _ ->
            (values, comps)
            ||> List.fold (fun acc comp ->
                match Map.tryFind comp.Id pinned with
                | Some fixedValue -> Map.add comp.Id fixedValue acc
                | None ->
                    Map.tryFind comp.Id neighbours
                    |> Option.defaultValue []
                    |> List.choose (fun n -> Map.tryFind n values)
                    |> function
                       | [] -> acc
                       | vs -> Map.add comp.Id (List.average vs) acc)
            |> relax (rounds - 1)
    relax 12 pinned

/// How far a component is downstream of the inputs, following connections in the direction the
/// signal travels. Decides which of two blocks goes on the left.
let private depthFromInputs
        (isInput: Component -> bool)
        (comps: Component list)
        (conns: Connection list)
        : Map<string, int> =
    let forward =
        (Map.empty, conns)
        ||> List.fold (fun acc conn ->
            let key = conn.Source.HostId
            Map.add key (conn.Target.HostId :: (Map.tryFind key acc |> Option.defaultValue [])) acc)
    let rec walk depth (frontier: string list) (found: Map<string, int>) =
        match frontier with
        | [] -> found
        | _ ->
            let next =
                frontier
                |> List.collect (fun name -> Map.tryFind name forward |> Option.defaultValue [])
                |> List.distinct
                |> List.filter (fun name -> not (Map.containsKey name found))
            let found = (found, next) ||> List.fold (fun acc name -> Map.add name (depth + 1) acc)
            walk (depth + 1) next found
    let inputs = comps |> List.filter isInput |> List.map (fun c -> c.Id)
    walk 0 inputs (inputs |> List.map (fun name -> name, 0) |> Map.ofList)

/// Build the floorplan by bisecting recursively, alternating the split axis with depth so blocks
/// stay roughly square rather than growing into a strip.
/// The two metrics that decide which sibling block is placed first.
type private Ordering = { Vertical: Map<string, float>; Depth: Map<string, int> }

let rec private floorplan
        (w: Map<string * string, int>)
        (ordering: Ordering)
        (depth: int)
        (comps: Component list)
        : Block option =
    match comps with
    | [] -> None
    | [ only ] -> Some (Leaf only)
    | _ ->
        let byName = comps |> List.map (fun c -> c.Id, c) |> Map.ofList
        let namesA, namesB = bisect w (comps |> List.map (fun c -> c.Id))

        // Which way to cut, and which half goes first, are the same question: put the halves
        // along whichever axis actually tells them apart.
        //   - one half further downstream than the other -> side by side, that half on the right,
        //     so signals run left to right
        //   - one half belonging higher up the I/O columns -> stacked, that half on top, so a
        //     block fed by the first input is not placed below one fed by the third
        // Alternating strictly by depth, as this used to, always cut side by side at the top
        // level; for independent parallel blocks the downstream ranks tie there, so the halves
        // were ordered arbitrarily and the I/O ordering never got a say where it mattered most.
        let depthRank names =
            names |> List.averageBy (fun n -> Map.tryFind n ordering.Depth |> Option.defaultValue 0 |> float)
        let verticalRank names =
            names |> List.averageBy (fun n -> Map.tryFind n ordering.Vertical |> Option.defaultValue 0.5)
        let depthSpread = max 1. (ordering.Depth |> Map.toList |> List.map (snd >> float) |> function [] -> 1. | ds -> List.max ds)
        let depthDiff = abs (depthRank namesA - depthRank namesB) / depthSpread
        let verticalDiff = abs (verticalRank namesA - verticalRank namesB)
        let isVertical, rank =
            match depthDiff > verticalDiff, verticalDiff > depthDiff with
            | true, _ -> true, depthRank
            | _, true -> false, verticalRank
            // neither says anything: keep blocks squarish by alternating, as before
            | _ -> depth % 2 = 0, (fun names -> depthRank names)
        let first, second =
            match rank namesA <= rank namesB with
            | true -> namesA, namesB
            | false -> namesB, namesA
        let pick names = names |> List.map (fun n -> byName[n])
        match
            floorplan w ordering (depth + 1) (pick first),
            floorplan w ordering (depth + 1) (pick second)
            with
        | Some a, Some b -> Some (Split (isVertical, a, b))
        | Some only, None | None, Some only -> Some only
        | None, None -> None

/// Size of a block including the gap that surrounds its contents.
let rec private blockSize (block: Block) : float * float =
    match block with
    | Leaf comp -> comp.W + Constants.componentGap, comp.H + Constants.componentGap
    | Split (isVertical, a, b) ->
        let wa, ha = blockSize a
        let wb, hb = blockSize b
        match isVertical with
        | true -> wa + wb + Constants.blockGap, max ha hb
        | false -> max wa wb, ha + hb + Constants.blockGap

let private snap (v: float) = System.Math.Round(v / Constants.grid) * Constants.grid

/// Assign each component a top-left position within the rectangle its block occupies.
let rec private placeBlock (originX: float) (originY: float) (block: Block) : (string * float * float) list =
    match block with
    | Leaf comp ->
        [ comp.Id, snap (originX + Constants.componentGap / 2.), snap (originY + Constants.componentGap / 2.) ]
    | Split (isVertical, a, b) ->
        let wa, ha = blockSize a
        match isVertical with
        | true ->
            placeBlock originX originY a
            @ placeBlock (originX + wa + Constants.blockGap) originY b
        | false ->
            placeBlock originX originY a
            @ placeBlock originX (originY + ha + Constants.blockGap) b

/// A column of components down the left or right edge, in the order given - which is the order
/// they were declared, and therefore the order of the sheet's own ports.
let private placeColumn (x: float) (startY: float) (comps: Component list) : (string * float * float) list =
    comps
    |> List.mapFold
        (fun y comp -> (comp.Id, snap x, snap y), y + comp.H + Constants.componentGap)
        startY
    |> fst

/// Give every component a position: Inputs down the left, Outputs down the right, everything else
/// bisected into blocks between them.
let private layout (comps: Component list) (conns: Connection list) : Component list =
    // both CompSpec and Component have a Type field and SheetDescription is opened last, so these
    // need annotating or inference picks CompSpec
    let isInput (comp: Component) = match comp.Type with | Input1 _ | Input _ -> true | _ -> false
    let isOutput (comp: Component) = match comp.Type with | Output _ -> true | _ -> false
    let inputs = comps |> List.filter isInput
    let outputs = comps |> List.filter isOutput
    let body = comps |> List.filter (fun c -> not (isInput c) && not (isOutput c))

    let ordering =
        { Vertical = verticalAffinity isInput isOutput comps conns
          Depth = depthFromInputs isInput comps conns }
    let bodyPlacement =
        floorplan (weights conns) ordering 0 body
        |> Option.map (fun plan -> plan, blockSize plan)
    let bodyWidth, bodyHeight =
        bodyPlacement |> Option.map snd |> Option.defaultValue (0., 0.)
    let columnHeight (column: Component list) =
        column |> List.sumBy (fun c -> c.H + Constants.componentGap)
    let contentHeight = List.max [ bodyHeight; columnHeight inputs; columnHeight outputs ]
    /// centre each column against the body so short columns do not sit at the top
    let columnTop column = Constants.margin + (contentHeight - columnHeight column) / 2.

    let bodyLeft =
        Constants.margin
        + (inputs |> List.fold (fun acc c -> max acc c.W) 0.)
        + (if List.isEmpty inputs then 0. else Constants.ioGap)
    let outputLeft =
        bodyLeft + bodyWidth + (if List.isEmpty outputs then 0. else Constants.ioGap)

    let positions =
        placeColumn Constants.margin (columnTop inputs) inputs
        @ (bodyPlacement
           |> Option.map (fun (plan, _) -> placeBlock bodyLeft (Constants.margin + (contentHeight - bodyHeight) / 2.) plan)
           |> Option.defaultValue [])
        @ placeColumn outputLeft (columnTop outputs) outputs
        |> List.map (fun (name, x, y) -> name, (x, y))
        |> Map.ofList

    comps
    |> List.map (fun comp ->
        match Map.tryFind comp.Id positions with
        | Some (x, y) -> { comp with X = x; Y = y }
        | None -> comp)

/// Lay out an existing canvas: resize every component to what the app will actually draw
/// (createSymbolRecord, as buildComponent does), position the body by recursive bisection and
/// pin Inputs and Outputs to edge columns in the order they appear in the component list —
/// which therefore must be the sheet's intended port order, since a sheet's own port order is
/// read off its I/O positions. Connections are untouched: with no vertices they are re-routed
/// on load. Used by the Verilog compiler for its generated sheets.
let layoutCanvas ((comps, conns): CanvasState) : CanvasState =
    let sized =
        comps
        |> List.map (fun comp ->
            let s = SymbolUpdate.createSymbolRecord [] SymbolT.ThemeType.Colourful comp
            { comp with H = s.Component.H; W = s.Component.W })
    layout sized conns, conns

//------------------------------------------------------------------------------------------------//
//------------------------------------------ Output ----------------------------------------------//
//------------------------------------------------------------------------------------------------//

/// The sheet's parameter declarations and slots, as Issie stores them.
///
/// Slot expressions go through ParameterTypes.parseExpression - the same parser the properties
/// pane uses - so an expression means here exactly what it would mean typed into a properties box.
/// A slot naming a component that is not on the sheet, or an expression that will not parse, is an
/// error rather than something silently dropped.
let paramDefsOf (sheet: SheetDescription) : Result<ParameterDefs option, string> =
    let known = sheet.Comps |> List.map (fun c -> c.Name, c.Type) |> Map.ofList
    let declarations =
        sheet.Params
        |> List.map (fun p ->
            ParamName p.Name, { Expression = PInt (bigint p.Default); Description = p.Description })
        |> Map.ofList
    let slot (spec: SlotSpec) : Result<ParamSlot * ConstrainedExpr, string> =
        match Map.tryFind spec.Comp known with
        | None -> Error $"slot on {spec.Comp}, which is not a component of {sheet.Name}"
        // A slot the component does not have would be recorded, shown in the properties pane, and
        // do nothing: ComponentSlots leaves a type it has no case for alone. Refuse it here, where
        // the mistake was made. GateN and MergeN have no slots at all, since their integer is an
        // input count; a SplitN output index past the end of the lists is no slot either.
        | Some compType when not (ComponentSlots.slotApplies spec.Slot compType) ->
            Error $"{spec.Comp} is a {compType} and has no {spec.Slot} slot, so no parameter can drive one"
        | Some _ ->
            ParameterTypes.parseExpression spec.Expression
            |> Result.mapError (fun e -> $"slot expression '{spec.Expression}' on {spec.Comp}: {e}")
            |> Result.bind (fun expr ->
                match ParameterTypes.paramNamesOfExpr expr
                      |> List.filter (fun n -> not (Map.containsKey n declarations)) with
                | [] ->
                    Ok ({ CompId = componentId sheet.Name spec.Comp; CompSlot = spec.Slot },
                        { Expression = expr; Constraints = [] })
                | (ParamName missing) :: _ ->
                    // every parameter used on a sheet must be declared on it
                    Error $"slot expression '{spec.Expression}' on {spec.Comp} uses '{missing}', which {sheet.Name} does not declare")
    match sheet.Params, sheet.Slots with
    | [], [] -> Ok None
    | _ ->
        sheet.Slots
        |> Helpers.ResultList.traverse slot
        |> Result.map (fun slots ->
            Some { DefaultBindings = declarations; ParamSlots = Map.ofList slots })

/// Put each parameterised slot's value into the component, as Issie does: the canvas holds the
/// resolved integer and the slot expression is kept beside it. Without this a sheet would be saved
/// with a slot saying W while the component still showed its unparameterised width.
///
/// An expression that cannot be evaluated is an error rather than a slot left alone: the sheet
/// would otherwise be written with a component showing one width and a slot claiming another, and
/// the disagreement would only surface when someone opened it.
/// Each slot is evaluated once and the components are rebuilt once, against the slots grouped by
/// the component they belong to. Folding a whole new component list per slot, as this did, is
/// quadratic in a sheet where most components are parameterised.
let private applySlotValues (defs: ParameterDefs option) (comps: Component list) : Result<Component list, string> =
    match defs with
    | None -> Ok comps
    | Some defs ->
        let bindings = bindingsOf defs.DefaultBindings
        defs.ParamSlots
        |> Map.toList
        |> Helpers.ResultList.traverse (fun (slot, exprSpec) ->
            ParameterTypes.evaluateParamExpression bindings exprSpec.Expression
            |> Result.mapError (fun e -> $"slot on {slot.CompId}: {e}")
            |> Result.map (fun value -> slot.CompId, (slot.CompSlot, value)))
        |> Result.map (fun resolved ->
            let byComp = resolved |> List.groupBy fst |> List.map (fun (id, vs) -> id, List.map snd vs) |> Map.ofList
            comps
            |> List.map (fun comp ->
                match Map.tryFind comp.Id byComp with
                | None -> comp
                | Some slotValues ->
                    let compType =
                        (comp.Type, slotValues)
                        ||> List.fold (fun t (slot, value) -> ComponentSlots.setSlotValue slot value t)
                    { comp with Type = compType }))

/// The description as a laid-out canvas: real positions, no wire geometry.
let toCanvasState (sheet: SheetDescription) : Result<CanvasState, string> =
    let duplicates =
        sheet.Comps
        |> List.countBy (fun c -> c.Name)
        |> List.filter (fun (_, n) -> n > 1)
        |> List.map fst
    match duplicates with
    | _ :: _ -> Error $"""these component names are used more than once: {String.concat ", " duplicates}"""
    | [] ->
        sheet.Comps
        |> Helpers.ResultList.traverse (buildComponent sheet.Name)
        |> Result.bind (fun comps ->
            // keyed by the name the description uses, which is the id minus the sheet prefix
            let byName =
                List.zip sheet.Comps comps
                |> List.map (fun (spec, comp) -> spec.Name, comp)
                |> Map.ofList
            sheet.Conns
            |> List.indexed
            |> Helpers.ResultList.traverse (fun (i, conn) ->
                match resolvePort byName true conn.From, resolvePort byName false conn.To with
                | Ok source, Ok target -> Ok (buildConnection i source target)
                | Error e, _ | _, Error e -> Error e)
            |> Result.bind (fun conns ->
                paramDefsOf sheet
                |> Result.bind (fun defs -> applySlotValues defs comps)
                |> Result.map (fun comps -> layout comps conns, conns)))

/// Write the description out as a .dgm in `folder`, named after the sheet.
/// A .dgm on its own is a sheet, not a project - use saveProject to make a directory Issie can
/// open.
let saveSheet (folder: string) (sheet: SheetDescription) : Result<unit, string> =
    toCanvasState sheet
    |> Result.bind (fun canvas ->
        paramDefsOf sheet
        |> Result.bind (fun defs ->
            let sheetInfo: SheetInfo =
                { Form = Some User; Description = None; ParameterDefinitions = defs; IsTopSheet = Some false }
            FilesIO.saveStateToFile folder sheet.Name (canvas, None, Some sheetInfo)))

/// The text a sheet would be saved as: exactly the body an .ldgm carries.
let private sheetBody (sheet: SheetDescription) : Result<string, string> =
    toCanvasState sheet
    |> Result.bind (fun canvas ->
        paramDefsOf sheet
        |> Result.bind (fun defs ->
            let sheetInfo: SheetInfo =
                { Form = Some User; Description = None; ParameterDefinitions = defs; IsTopSheet = Some false }
            Helpers.JsonHelpers.stateToJsonString (canvas, None, Some sheetInfo)))

/// Write `sheet` into the library at `libPath` as a component offered in the catalogue, along with
/// the sheets it uses, which are written too but not offered.
///
/// This runs under .NET as well as in the app: a library can be built by a program without Issie
/// ever starting.
let saveLibraryComponent
        (libPath: string)
        (description: string)
        (dependencies: SheetDescription list)
        (sheet: SheetDescription)
        : Result<unit, string> =
    let requiredBy (s: SheetDescription) =
        s.Comps
        |> List.choose (fun c -> match c.Type with | Custom cc -> Some cc.Name | _ -> None)
        |> List.distinct
    let write offered (s: SheetDescription) =
        sheetBody s
        |> Result.bind (fun body ->
            let header: ComponentLibraries.LibraryHeader = {
                FormatVersion = ComponentLibraries.Constants.currentFormatVersion
                Name = s.Name
                Description = if offered then description else s.Name
                Section = ComponentLibraries.Constants.defaultSection
                OfferedInCatalogue = offered
                Requires = requiredBy s
            }
            ComponentLibraries.writeComponentFile libPath header body)
    FilesIO.tryEnsureDirectory libPath
    |> Result.bind (fun libPath ->
        dependencies
        |> Helpers.ResultList.iter (write false)
        |> Result.bind (fun () -> write true sheet))

/// Write a whole project: every sheet, plus the empty .dprj marker that makes the directory a
/// project rather than a directory that happens to contain sheets. Issie will open a directory
/// without one, offering to put it back, but only a marked directory is recognised as a project
/// without being asked about.
let saveProject (folder: string) (sheets: SheetDescription list) : Result<unit, string> =
    match FilesIO.tryEnsureDirectory folder with
    | Error msg -> Error msg
    | Ok folder ->
        FilesIO.writeFile (FilesIO.projectMarkerPath folder) ""
        |> Result.bind (fun () -> sheets |> Helpers.ResultList.iter (saveSheet folder))
