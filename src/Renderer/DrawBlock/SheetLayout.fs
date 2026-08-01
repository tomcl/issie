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

    Component sizes come from Symbol.getComponentProperties, so blocks are spaced by what will
    actually be drawn rather than by a guess.
*)

open CommonTypes
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

let private labelOf (spec: CompSpec) = spec.Label |> Option.defaultValue spec.Name

/// Custom components report no size of their own - Symbol.autoScaleHAndW works it out later from
/// text widths, which needs a browser. Approximate it the same way so that layout has something to
/// space by; the app recomputes H and W on load regardless.
let private customSize (nIn: int) (nOut: int) =
    let h = Constants.grid + 40. * float (max nIn nOut)
    max (4. * Constants.grid) (2. * Constants.grid), max (2. * Constants.grid) h

/// Build the Component for one description entry, with readable deterministic port ids. Ids are
/// readable rather than uuids both because a generated sheet is easier to debug that way and
/// because minting a uuid goes through a Fable-only import that does not work under .NET.
let private buildComponent (spec: CompSpec) : Result<Component, string> =
    let makePorts n portType =
        [ for i in 0 .. n - 1 ->
            { Id = $"{spec.Name}-{portType}-{i}"
              PortNumber = Some i
              PortType = portType
              HostId = spec.Name } ]
    try
        let nIn, nOut, h, w = Symbol.getComponentProperties spec.Type (labelOf spec)
        let w, h =
            match spec.Type with
            | Custom _ -> customSize nIn nOut
            | _ -> w, h
        Ok {
            Id = spec.Name
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

/// Build the floorplan by bisecting recursively, alternating the split axis with depth so blocks
/// stay roughly square rather than growing into a strip.
let rec private floorplan (w: Map<string * string, int>) (depth: int) (comps: Component list) : Block option =
    match comps with
    | [] -> None
    | [ only ] -> Some (Leaf only)
    | _ ->
        let byName = comps |> List.map (fun c -> c.Id, c) |> Map.ofList
        let namesA, namesB = bisect w (comps |> List.map (fun c -> c.Id))
        let pick names = names |> List.map (fun n -> byName[n])
        match floorplan w (depth + 1) (pick namesA), floorplan w (depth + 1) (pick namesB) with
        | Some a, Some b -> Some (Split (depth % 2 = 0, a, b))
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

    let bodyPlacement =
        floorplan (weights conns) 0 body
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

//------------------------------------------------------------------------------------------------//
//------------------------------------------ Output ----------------------------------------------//
//------------------------------------------------------------------------------------------------//

let private allOk (results: Result<'a, string> list) : Result<'a list, string> =
    (Ok [], results)
    ||> List.fold (fun acc result ->
        match acc, result with
        | Error e, _ -> Error e
        | _, Error e -> Error e
        | Ok got, Ok value -> Ok (got @ [ value ]))

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
        |> List.map buildComponent
        |> allOk
        |> Result.bind (fun comps ->
            let byName = comps |> List.map (fun c -> c.Id, c) |> Map.ofList
            sheet.Conns
            |> List.mapi (fun i conn ->
                match resolvePort byName true conn.From, resolvePort byName false conn.To with
                | Ok source, Ok target -> Ok (buildConnection i source target)
                | Error e, _ | _, Error e -> Error e)
            |> allOk
            |> Result.map (fun conns -> layout comps conns, conns))

/// Write the description out as a .dgm in `folder`, named after the sheet.
/// A .dgm on its own is a sheet, not a project - use saveProject to make a directory Issie can
/// open.
let saveSheet (folder: string) (sheet: SheetDescription) : Result<unit, string> =
    toCanvasState sheet
    |> Result.bind (fun canvas ->
        let sheetInfo: SheetInfo =
            { Form = Some User; Description = None; ParameterDefinitions = None; IsTopSheet = Some false }
        FilesIO.saveStateToFile folder sheet.Name (canvas, None, Some sheetInfo))

/// Write a whole project: every sheet, plus the empty .dprj marker that makes the directory a
/// project rather than a directory that happens to contain sheets. Issie will not offer a
/// directory without one, and drops it from the recent list.
let saveProject (folder: string) (sheets: SheetDescription list) : Result<unit, string> =
    match FilesIO.tryEnsureDirectory folder with
    | Error msg -> Error msg
    | Ok folder ->
        let marker = FilesIO.pathJoin [| folder; FilesIO.baseName folder + ".dprj" |]
        (FilesIO.writeFile marker "", sheets)
        ||> List.fold (fun acc sheet -> acc |> Result.bind (fun () -> saveSheet folder sheet))
