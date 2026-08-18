/// What "the wiring looks good" means, as numbers.
///
/// Wire routing and separation are heuristics tuned by eye, and every change to them is an
/// argument until someone measures it. This module is the instrument: a handful of metrics over a
/// corpus of sheets chosen for the cases that are actually hard, recorded so that a change which
/// makes one of them worse fails here rather than being noticed months later on a student's
/// screen.
///
/// The numbers are printed on every run. They are meant to be looked at - a regression in this
/// suite is a table, not a boolean. See docs/dev/wireRouting.md for what the two passes do.
module WireQuality

open Expecto
open CommonTypes
open DrawModelType
open DrawModelType.BusWireT
open BlockHelpers
open SheetDescription
open SheetDescription.Operators

//-------------------------------------------------------------------------------------------//
//------------------------------------------METRICS------------------------------------------//
//-------------------------------------------------------------------------------------------//

/// Two segments are on the same line if their perpendicular coordinates are this close.
let private sameLine = 1.0

/// A drawn segment reduced to one dimension: which net drew it, which way it runs, the
/// coordinate perpendicular to it, and the interval it covers along itself.
type private Line =
    { Net: OutputPortId
      Ori: Orientation
      P: float
      Lo: float
      Hi: float }

let private linesOf (model: Model) : Line list =
    model.Wires
    |> Map.toList
    |> List.collect (fun (_, w) ->
        getAbsSegments w
        |> List.filter (fun s -> not s.IsZero)
        |> List.map (fun s ->
            match s.Orientation with
            | Horizontal ->
                { Net = w.OutputPort; Ori = Horizontal; P = s.Start.Y
                  Lo = min s.Start.X s.End.X; Hi = max s.Start.X s.End.X }
            | Vertical ->
                { Net = w.OutputPort; Ori = Vertical; P = s.Start.X
                  Lo = min s.Start.Y s.End.Y; Hi = max s.Start.Y s.End.Y }))

/// Total length of the union of a set of intervals - overlapping parts counted once.
let private unionLength (intervals: (float * float) list) =
    intervals
    |> List.sortBy fst
    |> List.fold
        (fun (covered, current) (lo, hi) ->
            match current with
            | Some (cLo, cHi) when lo <= cHi -> covered, Some(cLo, max cHi hi)
            | Some (cLo, cHi) -> covered + (cHi - cLo), Some(lo, hi)
            | None -> covered, Some(lo, hi))
        (0., None)
    |> fun (covered, current) ->
        match current with
        | Some (cLo, cHi) -> covered + (cHi - cLo)
        | None -> covered

/// Group lines that lie on the same line of the drawing: same net, same direction, and
/// perpendicular coordinates within sameLine of each other. Sorting by P and cutting where the
/// gap opens is not a clustering, but for segments which separation has either merged or pushed
/// maxSegmentSeparation apart there is nothing in between for it to get wrong.
let private byDrawnLine (lines: Line list) =
    lines
    |> List.groupBy (fun l -> l.Net, l.Ori)
    |> List.collect (fun (key, group) ->
        group
        |> List.sortBy (fun l -> l.P)
        |> List.fold
            (fun acc l ->
                match acc with
                | (p, ls) :: rest when abs (l.P - p) < sameLine -> (p, l :: ls) :: rest
                | _ -> (l.P, [ l ]) :: acc)
            []
        |> List.map (fun (p, ls) -> (key, p), ls))

type Metrics =
    { /// Length of wire drawn: what a reader sees. Same-net segments drawn on top of each other
      /// are one line and are counted once, so a net which shares a trunk costs less than the
      /// same net routed as separate wires. This is the number to minimise: it rewards short
      /// wires and same-net sharing together, without having to trade them off by hand.
      Ink: float
      /// Total of |segment length| over every wire, sharing not discounted.
      RawLength: float
      /// Visible corners: two wires of a net which run together have the corners of the shared
      /// part drawn on top of each other, and a reader sees one corner, so they count once. Ink
      /// counts the union for the same reason - counting per wire instead would report a wire
      /// which follows its net as having every corner of the wire it follows.
      Bends: int
      /// Segments of different nets crossing at right angles, strictly inside both.
      Crossings: int
      /// Length over which two different nets are drawn on top of each other. Separation exists
      /// to make this zero.
      CrossNetOverlap: float
      /// Segments drawn across the body of a symbol they do not connect to. Routing exists to make
      /// this zero, and when it cannot it says nothing and draws the wire anyway.
      SymbolCrossings: int
      /// Wire drawn for the nets that have more than one wire, and nothing else. Whole-sheet ink
      /// is the wrong instrument for judging how well a net is commoned up: a fan-out is a small
      /// part of a sheet, and every other wire moving in response drowns it. This is the part of
      /// the drawing the question is about.
      FannedNetInk: float }

/// Segments crossing the body of a symbol. The symbols at each end of a wire are exempt: a
/// multiplexer's SEL port sits inside its own bounding box, so that nub has to enter it. Boxes are
/// shrunk by a pixel so that running along an edge does not count.
let symbolCrossingsOf (model: Model) =
    let boxes =
        model.Symbol.Symbols
        |> Map.toList
        |> List.map (fun (id, sym) ->
            let b = Symbol.getSymbolBoundingBox sym
            id,
            { b with
                TopLeft = { X = b.TopLeft.X + 1.; Y = b.TopLeft.Y + 1. }
                W = b.W - 2.
                H = b.H - 2. })
    model.Wires
    |> Map.toList
    |> List.sumBy (fun (_, w) ->
        let ends =
            [ w.InputPort |> inputPortStr; w.OutputPort |> outputPortStr ]
            |> List.map (fun p -> ComponentId model.Symbol.Ports[p].HostId)
        let segs = getAbsSegments w
        segs
        |> List.indexed
        |> List.filter (fun (i, s) ->
            i > 0 && i < segs.Length - 1 && not s.IsZero
            && boxes
               |> List.exists (fun (id, b) ->
                   not (List.contains id ends)
                   && (segmentIntersectsBoundingBox b s.Start s.End).IsSome))
        |> List.length)

let metricsOf (model: Model) : Metrics =
    let lines = linesOf model
    let hs = lines |> List.filter (fun l -> l.Ori = Horizontal)
    let vs = lines |> List.filter (fun l -> l.Ori = Vertical)
    { Ink =
        byDrawnLine lines
        |> List.sumBy (fun (_, ls) -> unionLength (ls |> List.map (fun l -> l.Lo, l.Hi)))
      RawLength =
        model.Wires
        |> Map.toList
        |> List.sumBy (fun (_, w) -> w.Segments |> List.sumBy (fun s -> abs s.Length))
      Bends =
        model.Wires
        |> Map.toList
        |> List.collect (fun (_, w) ->
            // a corner is where two consecutive drawn segments of a wire meet
            getAbsSegments w
            |> List.filter (fun seg -> not seg.IsZero)
            |> List.pairwise
            |> List.map (fun (before, _) ->
                w.OutputPort, System.Math.Round(before.End.X, 1), System.Math.Round(before.End.Y, 1)))
        |> List.distinct
        |> List.length
      Crossings =
        List.allPairs hs vs
        |> List.filter (fun (h, v) ->
            h.Net <> v.Net
            && v.P > h.Lo + 0.01 && v.P < h.Hi - 0.01
            && h.P > v.Lo + 0.01 && h.P < v.Hi - 0.01)
        |> List.length
      CrossNetOverlap =
        [ hs; vs ]
        |> List.sumBy (fun sel ->
            List.allPairs sel sel
            |> List.filter (fun (a, b) ->
                a.Net <> b.Net && abs (a.P - b.P) < sameLine && min a.Hi b.Hi - max a.Lo b.Lo > 0.01)
            |> List.sumBy (fun (a, b) -> min a.Hi b.Hi - max a.Lo b.Lo)
            |> fun total -> total / 2.)
      SymbolCrossings = symbolCrossingsOf model
      FannedNetInk =
        let fanned =
            model.Wires
            |> Map.toList
            |> List.groupBy (fun (_, w) -> w.OutputPort)
            |> List.filter (fun (_, ws) -> ws.Length > 1)
            |> List.map fst
            |> Set.ofList
        byDrawnLine (lines |> List.filter (fun l -> fanned.Contains l.Net))
        |> List.sumBy (fun (_, ls) -> unionLength (ls |> List.map (fun l -> l.Lo, l.Hi))) }

//-------------------------------------------------------------------------------------------//
//-------------------------------------------CORPUS------------------------------------------//
//-------------------------------------------------------------------------------------------//

/// n bits split out of one bus and merged back into another, with the order reversed on the way.
/// An array of ports facing an array of ports with every connection crossed is the case most of
/// the complication in BusWireSeparate exists for.
let private crossedArrays n =
    describeSheet "crossedArrays" [
        comp "IN" (Input1(n, None))
        comp "SPLIT" (SplitN(n, List.replicate n 1, [ 0 .. n - 1 ]))
        comp "MERGE" (MergeN n)
        comp "OUT" (Output n)
    ] ([ "IN" ==> "SPLIT" ]
       @ [ for i in 0 .. n - 1 -> $"SPLIT/{i}" ==> $"MERGE/{n - 1 - i}" ]
       @ [ "MERGE" ==> "OUT" ])

/// The same sheet with the destination array placed to the LEFT of the source, so every wire in
/// the bundle has to turn back and go round both symbols. The other common unpleasant case.
let private wrappedPositions n =
    [ "IN", { X = 100.; Y = 100. }
      "SPLIT", { X = 700.; Y = 100. }
      "MERGE", { X = 300.; Y = 100. + 40. * float n }
      "OUT", { X = 100.; Y = 100. + 40. * float n } ]

/// One driver, many sinks whose ports are at slightly different heights and different distances -
/// which is what a fan-out looks like once components are anywhere but in a column. A sink port a
/// couple of units off its neighbours' gives that wire a trunk segment just off theirs, and if
/// same-net linking does not catch it, separation pushes it a full maxSegmentSeparation clear and
/// the net is drawn as two nearly-parallel lines instead of one trunk.
let private staggeredFanout n =
    describeSheet "staggeredFanout"
        (comp "SRC" (Input1(16, None))
         :: [ for i in 1 .. n -> comp $"R{i}" (Register 16) ]
         @ [ for i in 1 .. n -> comp $"O{i}" (Output 16) ])
        ([ for i in 1 .. n -> "SRC" ==> $"R{i}/D" ]
         @ [ for i in 1 .. n -> $"R{i}" ==> $"O{i}" ])

let private staggeredPositions n =
    ("SRC", { X = 100.; Y = 400. })
    :: [ for i in 1 .. n -> $"R{i}", { X = float (500 + 90 * i); Y = float (100 + 150 * i) } ]

/// A long net with several destinations, spread out and far from the driver. This is the case that
/// matters most: three long wires crossing a sheet nearly in parallel is not 20% worse than one
/// trunk with branches, it is unreadable - a reader can no longer see which wires are one signal.
let private longFanout n =
    describeSheet "longFanout"
        (comp "SRC" (Input1(16, None))
         :: [ for i in 1 .. n -> comp $"R{i}" (Register 16) ]
         @ [ for i in 1 .. n -> comp $"O{i}" (Output 16) ])
        ([ for i in 1 .. n -> "SRC" ==> $"R{i}/D" ]
         @ [ for i in 1 .. n -> $"R{i}" ==> $"O{i}" ])

let private longPositions n =
    ("SRC", { X = 100.; Y = 900. })
    :: [ for i in 1 .. n -> $"R{i}", { X = float (1400 + 40 * i); Y = float (100 + 190 * i) } ]

/// One driver, many sinks: a clock or reset net. The case where same-net sharing is worth most,
/// and where the cost of linking same-net lines is worst.
let private fanout n =
    describeSheet "fanout"
        (comp "CLK" (Input1(1, None))
         :: [ for i in 1 .. n -> comp $"G{i}" (GateN(And, 2)) ]
         @ [ for i in 1 .. n -> comp $"O{i}" (Output 1) ])
        ([ for i in 1 .. n -> "CLK" ==> $"G{i}/0" ]
         @ [ for i in 1 .. n -> "CLK" ==> $"G{i}/1" ]
         @ [ for i in 1 .. n -> $"G{i}" ==> $"O{i}" ])

/// n inputs crossing over to n gates, each gate also fed straight through: crossings and
/// fan-out together, over separate symbols rather than one array.
let private tangle n =
    describeSheet "tangle"
        ([ for i in 1 .. n -> comp $"I{i}" (Input1(1, None)) ]
         @ [ for i in 1 .. n -> comp $"G{i}" (GateN(And, 2)) ]
         @ [ for i in 1 .. n -> comp $"O{i}" (Output 1) ])
        ([ for i in 1 .. n -> $"I{i}" ==> $"G{n + 1 - i}/0" ]
         @ [ for i in 1 .. n -> $"I{i}" ==> $"G{i}/1" ]
         @ [ for i in 1 .. n -> $"G{i}" ==> $"O{i}" ])

let private canvasOf sheet =
    match SheetLayout.toCanvasState sheet with
    | Ok canvas -> canvas
    | Error e -> failtest $"sheet would not build: {e}"

let private movedTo (positions: (string * XYPos) list) ((comps, conns): CanvasState) : CanvasState =
    let moved (comp: Component) =
        positions
        |> List.tryPick (fun (label, pos) -> if label = comp.Label then Some pos else None)
        |> Option.map (fun pos -> { comp with X = pos.X; Y = pos.Y })
        |> Option.defaultValue comp
    List.map moved comps, conns

/// Symbols loaded, one unrouted wire per connection, then every wire autorouted: the state the
/// app is in after opening a sheet and before separation runs.
let private routedModel (canvas: CanvasState) =
    let comps, conns = canvas
    let wireModel, _ = BusWireUpdate.init ()
    let symbols = SymbolUpdate.loadComponents [] wireModel.Symbol comps
    let wireOf (conn: Connection) : Wire =
        { WId = ConnectionId conn.Id
          InputPort = InputPortId conn.Target.Id
          OutputPort = OutputPortId conn.Source.Id
          Color = HighLightColor.Red
          Width = 1
          Segments = []
          StartPos = { X = 0.; Y = 0. }
          InitialOrientation = Horizontal }
    let model =
        { wireModel with
            Symbol = symbols
            Wires = conns |> List.map (fun c -> ConnectionId c.Id, wireOf c) |> Map.ofList }
    // Routed one after another, each against a model holding the wires already done - which is
    // what the app does, and the only way a wire can see a routed wire of its own net. Shortest
    // first, as BusWireSeparate.redrawWires does: with wires able to branch off their own net the
    // order matters a great deal, and an arbitrary one is not what the app uses.
    let byLength =
        model.Wires
        |> Map.toList
        |> List.sortBy (fun (_, w) ->
            let d, s = Symbol.getTwoPortLocations model.Symbol w.InputPort w.OutputPort
            euclideanDistance s d)
        |> List.map fst
    (model, byLength)
    ||> List.fold (fun model wid ->
            { model with Wires = Map.add wid (BusWireRoute.smartAutoroute model model.Wires[wid]) model.Wires })

/// Symbols loaded and every wire given back the route it was saved with, manual segments and all.
/// This is what the app has after opening a sheet, and it is not what `routedModel` produces: a
/// saved sheet has been separated, and may have been routed by hand, so re-routing it from nothing
/// throws away the state a user's drag actually starts from. `BusWireUpdate`'s `LoadConnections`
/// does this inside the Elmish update, where a test cannot reach it.
let private loadedModel ((comps, conns): CanvasState) =
    let wireModel, _ = BusWireUpdate.init ()
    let symbols = SymbolUpdate.loadComponents [] wireModel.Symbol comps
    let empty = { wireModel with Symbol = symbols }
    /// A saved end that is not where the port now is has to be re-routed, exactly as
    /// LoadConnections does: the vertices were saved before whatever moved the symbol.
    let matchesPort (pos: XYPos) (vertex: (float * float * bool) option) =
        match vertex with
        | None -> false
        | Some(x, y, _) ->
            abs (pos.X - x) < BusWire.Constants.vertexLoadMatchTolerance
            && abs (pos.Y - y) < BusWire.Constants.vertexLoadMatchTolerance
    let wireOf (conn: Connection) : Wire =
        let outputId, inputId = OutputPortId conn.Source.Id, InputPortId conn.Target.Id
        let matchEnd inOut wire =
            let matches =
                match inOut with
                | true ->
                    matchesPort (Symbol.getInputPortLocation None symbols inputId) (List.tryLast conn.Vertices)
                | false ->
                    matchesPort (Symbol.getOutputPortLocation None symbols outputId) (List.tryHead conn.Vertices)
            if matches then wire else BusWireRoute.updateWire empty wire inOut
        { WId = ConnectionId conn.Id
          InputPort = inputId
          OutputPort = outputId
          Color = HighLightColor.DarkSlateGrey
          Width = 1
          Segments = BusWire.issieVerticesToSegments (ConnectionId conn.Id) conn.Vertices
          StartPos = Symbol.getOutputPortLocation None symbols outputId
          InitialOrientation =
            getOutputPortOrientation symbols outputId |> BusWireUpdateHelpers.getOrientationOfEdge }
        |> matchEnd false
        |> matchEnd true
        |> fun wire -> { wire with Segments = BusWireUpdateHelpers.makeEndsDraggable wire.Segments }
    { empty with Wires = conns |> List.map (fun c -> ConnectionId c.Id, wireOf c) |> Map.ofList }

let private separate (model: Model) =
    BusWireSeparate.updateWireSegmentJumpsAndSeparations
        (model.Wires |> Map.toList |> List.map fst) model

/// A drag, as the app does it on mouse-up: move a symbol, re-route the wires that end on it, and
/// re-separate the sheet. Here it goes there and back again, which should leave the wiring as it
/// was found.
let private dragRoundTrip (model: Model) =
    let moved = model.Symbol.Symbols |> Map.toList |> List.map fst |> List.item 1
    let shift (d: XYPos) (m: Model) =
        { m with Symbol = SymbolUpdate.moveSymbols m.Symbol [ moved ] d }
        |> fun m -> BusWireSeparate.routeAndSeparateSymbolWires m moved
    model |> shift { X = 90.; Y = 45. } |> shift { X = -90.; Y = -45. }

/// Wires which are not drawn between the two ports they connect. A wire that begins somewhere
/// other than its driver port is not attached to anything: it is the route it had before the
/// symbol moved, left lying where it was.
///
/// Half a unit of slack, not none: a sheet loaded from a file is drawn from vertices that were
/// rounded on the way out, which leaves ends a few thousandths adrift - the app's own load check
/// allows 0.01 for the same reason. A detachment is tens of units.
let private detachedWires (model: Model) =
    model.Wires
    |> Map.toList
    |> List.filter (fun (_, w) ->
        let startPos = Symbol.getOutputPortLocation None model.Symbol w.OutputPort
        let endPos = Symbol.getInputPortLocation None model.Symbol w.InputPort
        euclideanDistance w.StartPos startPos > 0.5
        || euclideanDistance (getAbsSegments w |> List.last).End endPos > 0.5)
    |> List.map (fun (ConnectionId id, _) -> id)

/// The symbol driving the most wires, which is where a re-route has the most chance of going
/// wrong: a wire of a multi-wire net can be routed as a branch off another wire of the same net.
let private busiestDriver (model: Model) =
    model.Wires
    |> Map.toList
    |> List.countBy (fun (_, w) -> w.OutputPort)
    |> List.maxBy snd
    |> fst
    |> fun (OutputPortId p) -> ComponentId model.Symbol.Ports[p].HostId

/// Which wires changed, by segment lengths.
let private wiresDiffering (a: Model) (b: Model) =
    let lengths (m: Model) =
        m.Wires |> Map.toList |> List.map (fun (id, w) -> id, w.Segments |> List.map (fun s -> s.Length))
    List.zip (lengths a) (lengths b)
    |> List.filter (fun ((_, x), (_, y)) -> x <> y)
    |> List.length

/// How many further separation passes are needed before nothing moves at all.
/// `None` means it never settles within the limit: the wiring a user is left with then depends on
/// how many times the pass happened to run, which is not something they can predict or control.
let private passesToSettle (model: Model) =
    let rec go n m =
        if n > 8 then None
        else
            let next = separate m
            if wiresDiffering m next = 0 then Some n else go (n + 1) next
    go 0 model

/// One wire that has to get past unconnected symbols, over a grid of obstacle positions. This is
/// the case the routing heuristics are for, and the only one where "it looks fine on my sheet" is
/// no evidence at all: the failures are a few positions out of a hundred, and they move as the
/// heuristics change.
///
/// Placements where an obstacle overlaps a symbol are skipped - Issie refuses to create those.
let private obstacleSweep (sheet: SheetDescription) (place: int -> int -> (string * XYPos) list) (obstacles: string list) =
    let canvas = canvasOf sheet
    let boxOf (m: Model) label =
        m.Symbol.Symbols
        |> Map.toList
        |> List.map snd
        |> List.find (fun s -> s.Component.Label = label)
        |> Symbol.getSymbolBoundingBox
    /// A route which does not join the two ports it names, or which is not made of right angles,
    /// is worse than one which crosses a symbol - and getting past an obstacle means rebuilding
    /// segment lengths wholesale, so this is checked at every placement rather than assumed.
    let checkWellFormed dx dy (m: Model) =
        m.Wires
        |> Map.iter (fun (ConnectionId id) w ->
            let expectedEnd, expectedStart =
                Symbol.getTwoPortLocations m.Symbol w.InputPort w.OutputPort
            let segs = getAbsSegments w
            let closeTo (a: XYPos) (b: XYPos) = abs (a.X - b.X) < 0.001 && abs (a.Y - b.Y) < 0.001
            Expect.isTrue (closeTo (List.head segs).Start expectedStart)
                $"at ({dx},{dy}) wire {id} does not start at its output port"
            Expect.isTrue (closeTo (List.last segs).End expectedEnd)
                $"at ({dx},{dy}) wire {id} does not end at its input port"
            segs
            |> List.iter (fun seg ->
                Expect.isTrue
                    (abs (seg.Start.Y - seg.End.Y) < 0.001 || abs (seg.Start.X - seg.End.X) < 0.001)
                    $"at ({dx},{dy}) wire {id} segment {seg.Segment.Index} is diagonal"))
    [ for dx in 100 .. 15 .. 620 do
        for dy in -200 .. 15 .. 200 do
            let m = routedModel (canvas |> movedTo (place dx dy))
            // A placement is only worth counting if a route could exist. An obstacle closer to a
            // symbol than a nub is long leaves the nub itself inside the obstacle, and no route
            // avoids that: the nub's length and direction are fixed by the port.
            let room = BusWire.Constants.nubLength + BusWireRoutingHelpers.Constants.minWireSeparation
            let legal =
                obstacles
                |> List.forall (fun o ->
                    let b = boxOf m o
                    let withRoom =
                        { b with
                            TopLeft = { X = b.TopLeft.X - room; Y = b.TopLeft.Y - room }
                            W = b.W + 2. * room
                            H = b.H + 2. * room }
                    [ "A"; "B" ] |> List.forall (fun s -> not (overlap2DBox withRoom (boxOf m s))))
            if legal then
                checkWellFormed dx dy m
                yield (dx, dy), symbolCrossingsOf m ]

let private obstacle = comp "OBS" (NbitsAdderNoCinCout 8)

/// name, sheet, placement, obstacle labels
let private sweeps =
    [ "one obstacle",
      describeSheet "sweep1" [ comp "A" (Input1(1, None)); comp "B" (Output 1); obstacle ] [ "A" ==> "B" ],
      (fun dx dy ->
          [ "A", { X = 100.; Y = 300. }; "B", { X = 700.; Y = 300. }
            "OBS", { X = float (100 + dx); Y = float (300 + dy) } ]),
      [ "OBS" ]

      // a port on the bottom edge - the shape the routing table gives 8 segments
      "into a bottom port",
      describeSheet "sweep2" [ comp "A" (Input1(1, None)); comp "B" Mux2; obstacle ] [ "A" ==> "B/SEL" ],
      (fun dx dy ->
          [ "A", { X = 100.; Y = 300. }; "B", { X = 700.; Y = 300. }
            "OBS", { X = float (100 + dx); Y = float (300 + dy) } ]),
      [ "OBS" ]

      // the wire has to double back on itself
      "target behind source",
      describeSheet "sweep3" [ comp "A" (Input1(1, None)); comp "B" (Output 1); obstacle ] [ "A" ==> "B" ],
      (fun dx dy ->
          [ "A", { X = 700.; Y = 300. }; "B", { X = 100.; Y = 300. }
            "OBS", { X = float (100 + dx); Y = float (300 + dy) } ]),
      [ "OBS" ]

      "a wall of two",
      describeSheet "sweep4"
          [ comp "A" (Input1(1, None)); comp "B" (Output 1); obstacle
            comp "OBS2" (NbitsAdderNoCinCout 8) ] [ "A" ==> "B" ],
      (fun dx dy ->
          [ "A", { X = 100.; Y = 300. }; "B", { X = 700.; Y = 300. }
            "OBS", { X = float (100 + dx); Y = float (300 + dy) }
            "OBS2", { X = float (100 + dx); Y = float (300 + dy) + 100. } ]),
      [ "OBS"; "OBS2" ]

      // The source is below a tall multiplexer and the destination is a SEL port on the bottom
      // edge of a symbol above it, so the wire has to climb past the tall one. Here dx and dy move
      // the SOURCE rather than the obstacle. The obstacle being a mux is the point: the final
      // segments of a wire reaching a SEL port used to be checked against no mux on the sheet at
      // all, so this crossing was invisible and no shift was even attempted.
      "climb past a mux",
      describeSheet "sweep5" [ comp "A" (Input1(3, None)); comp "B" Mux2; comp "TALL" Mux8 ]
          [ "A" ==> "B/SEL" ],
      (fun dx dy ->
          [ "A", { X = float dx; Y = 900. + float dy }
            "B", { X = 1500.; Y = 100. }
            "TALL", { X = 1490.; Y = 340. } ]),
      [ "TALL" ] ]

/// A real sheet: the register file from the 3cpu fixture. A demultiplexer fans out to eight
/// registers and two multiplexers read back from them, so it holds a bundle of vertical segments
/// whose order decides a great many crossings - the case a hand-built sheet is least likely to
/// reproduce, and the one where the ordering was seen to be leaving crossings on the table.
let private reg16x8 =
    (TestFixtures.loadProject "3cpu" |> List.find (fun c -> c.Name = "reg16x8")).CanvasState

let private corpus =
    [ "crossedArrays", canvasOf (crossedArrays 8)
      "wrappedArrays", canvasOf (crossedArrays 8) |> movedTo (wrappedPositions 8)
      "fanout", canvasOf (fanout 12)
      "staggeredFanout", canvasOf (staggeredFanout 4) |> movedTo (staggeredPositions 4)
      "longFanout", canvasOf (longFanout 4) |> movedTo (longPositions 4)
      "reg16x8", reg16x8
      "tangle", canvasOf (tangle 8) ]

//-------------------------------------------------------------------------------------------//
//--------------------------------------------TESTS------------------------------------------//
//-------------------------------------------------------------------------------------------//

/// What the corpus scores today. Every column is "lower is better", and `Unstable` should be 0.
/// These are recorded, not derived: a change to routing or separation that moves any of them is
/// meant to be noticed and argued for in the commit message, not absorbed silently.
///
/// Ink and Crossings trade against each other - a change that lowers one while raising the other
/// is a judgement call, which is why both are here rather than a single score.
type private Recorded =
    { Sheet: string
      Ink: float
      Bends: int
      Crossings: int
      /// Wire drawn for the multi-wire nets, which is what commoning a net up is judged by.
      FannedNetInk: float
      /// Further separation passes needed before nothing moves. 0 is right: separation is a fine
      /// adjustment applied after routing, and it should settle. `None` is a limit cycle - the
      /// pass moves the same wires back and forth for ever, so what the user is left with depends
      /// on how many times it ran.
      Settle: int option }

let private recorded =
    [ { Sheet = "crossedArrays"; Ink = 2601.; Bends = 44; Crossings = 28; FannedNetInk = 0.; Settle = Some 0 }
      { Sheet = "wrappedArrays"; Ink = 10966.; Bends = 58; Crossings = 36; FannedNetInk = 0.; Settle = Some 0 }
      { Sheet = "fanout"; Ink = 9002.; Bends = 98; Crossings = 0; FannedNetInk = 3257.; Settle = Some 0 }
      { Sheet = "staggeredFanout"; Ink = 3878.; Bends = 34; Crossings = 9; FannedNetInk = 1515.; Settle = Some 0 }
      { Sheet = "longFanout"; Ink = 9740.; Bends = 35; Crossings = 8; FannedNetInk = 3960.; Settle = Some 0 }
      { Sheet = "reg16x8"; Ink = 19960.; Bends = 149; Crossings = 81; FannedNetInk = 13046.; Settle = Some 0 }
      { Sheet = "tangle"; Ink = 11110.; Bends = 83; Crossings = 60; FannedNetInk = 8110.; Settle = Some 0 } ]

/// A settling result is no worse than what was recorded if it needs no more passes than before.
/// Not settling at all is the worst outcome, and only matches itself.
let private settlesNoWorseThan (recorded: int option) (actual: int option) =
    match recorded, actual with
    | None, _ -> true // it did not settle before; anything at all is an improvement or the same
    | Some r, Some a -> a <= r
    | Some _, None -> false

/// Placements in each sweep which still leave a wire drawn over a symbol. Lower is better and 0
/// is the goal; these are recorded so that a routing change has to say which way they moved.
let private recordedCrossings =
    [ "one obstacle", 0
      "into a bottom port", 0
      "target behind source", 0
      "a wall of two", 0
      "climb past a mux", 0 ]

let tests =
    testList "WireQuality" [

        test "the corpus is measured, and separation does what it is for" {
            let rows =
                corpus
                |> List.map (fun (name, canvas) ->
                    let routed = routedModel canvas
                    separate routed |> ignore // warm up: the first call in a process pays for JIT
                    let sw = System.Diagnostics.Stopwatch.StartNew()
                    let once = separate routed
                    let ms = sw.Elapsed.TotalMilliseconds
                    name, metricsOf routed, metricsOf once, once, ms, passesToSettle once)

            printfn "  wire quality (routed -> separated):"
            printfn "  %-14s %5s %18s %10s %12s %10s %6s %8s"
                "sheet" "wires" "ink" "bends" "crossings" "fanned net" "ms" "settles"
            rows
            |> List.iter (fun (name, before, after, model, ms, settle) ->
                printfn "  %-14s %5d %8.0f ->%8.0f %4d ->%4d %5d ->%5d %10.0f %6.1f %8s"
                    name (Map.count model.Wires) before.Ink after.Ink
                    before.Bends after.Bends before.Crossings after.Crossings after.FannedNetInk ms
                    (match settle with
                     | Some 0 -> "at once"
                     | Some n -> $"after {n}"
                     | None -> "NEVER"))

            // The one thing separation must always achieve, on every sheet.
            rows
            |> List.iter (fun (name, _, after, _, _, _) ->
                Expect.isLessThan after.CrossNetOverlap 1.0
                    $"{name}: separation left %.0f{after.CrossNetOverlap} of overlap between different nets")
        }

        test "recorded quality has not regressed" {
            corpus
            |> List.iter (fun (name, canvas) ->
                let once = separate (routedModel canvas)
                let m = metricsOf once
                let r = recorded |> List.find (fun r -> r.Sheet = name)
                let update = "If this is the price of an improvement elsewhere, update `recorded`."
                Expect.isLessThan m.Ink (r.Ink * 1.01)
                    $"{name}: more wire is drawn than recorded (%.0f{r.Ink} -> %.0f{m.Ink}). {update}"
                Expect.isLessThanOrEqual m.Bends r.Bends
                    $"{name}: more bends than recorded ({r.Bends} -> {m.Bends}). {update}"
                Expect.isLessThanOrEqual m.Crossings r.Crossings
                    $"{name}: more crossings than recorded ({r.Crossings} -> {m.Crossings}). {update}"
                Expect.isLessThan m.FannedNetInk (r.FannedNetInk * 1.01 + 1.)
                    $"{name}: the nets with more than one wire are drawn with more wire than                        recorded (%.0f{r.FannedNetInk} -> %.0f{m.FannedNetInk}). {update}"
                Expect.isTrue (settlesNoWorseThan r.Settle (passesToSettle once))
                    $"{name}: separation settles less well than recorded (%A{r.Settle} further passes, \
                       now %A{passesToSettle once}). {update}")
        }

        test "separation settles" {
            // Separation is a fine adjustment applied after routing, and running it again should
            // not move anything: if it does, the sheet the user ends up with depends on how many
            // times the pass happened to run - and it runs after every drag, paste and rotate.
            //
            // It does not hold on crossedArrays, where an array of ports faces an array of ports
            // with every connection crossed. That sheet also trips makeClusters' "nextIndex has
            // got lost" warning, which the source says should never happen. The two are worth
            // chasing together. Until then this pins how far off it is, per sheet.
            corpus
            |> List.iter (fun (name, canvas) ->
                let settle = passesToSettle (separate (routedModel canvas))
                let allowed = (recorded |> List.find (fun r -> r.Sheet = name)).Settle
                Expect.isTrue (settlesNoWorseThan allowed settle)
                    $"{name}: separation needed %A{settle} further passes to settle, recorded %A{allowed}")
        }

        test "a wire routes around a symbol it does not connect to" {
            // smartAutoroute tries a small set of shifts and, if none of them clears the
            // obstacles, returns the wire it started with - drawn straight over the component.
            // Nothing else records that, so this does: a count per sweep, recorded, to be driven
            // down rather than argued about.
            let results =
                sweeps
                |> List.map (fun (name, sheet, place, obstacles) ->
                    let sweep = obstacleSweep sheet place obstacles
                    name, sweep |> List.filter (fun (_, c) -> c > 0), sweep.Length)
            printfn "  wires left crossing a symbol:"
            results
            |> List.iter (fun (name, bad, total) ->
                printfn "  %-22s %3d of %3d placements%s" name bad.Length total
                    (if bad.IsEmpty then ""
                     else
                        bad
                        |> List.truncate 8
                        |> List.map (fun ((dx, dy), c) -> $"  ({dx},{dy})x{c}")
                        |> String.concat ""))
            results
            |> List.iter (fun (name, bad, _) ->
                let allowed = recordedCrossings |> List.pick (fun (n, a) -> if n = name then Some a else None)
                Expect.isLessThanOrEqual bad.Length allowed
                    $"{name}: {bad.Length} placements leave a wire over a symbol, was {allowed}")
        }

        test "no segment is lost from a cluster during separation" {
            // makeClusters builds the cluster around a segment by searching up from it and then
            // back down from the top of what it found. Where the downward search is stopped by a
            // barrier, the segments below it become a second cluster - and that cluster was built
            // with the bound of the first, which still described segments the first had taken. A
            // symbol edge nowhere near the second cluster could then stop its search too, dropping
            // every segment below it including the one the cluster was being built for. The code
            // noticed - "nextIndex has got lost" - and repaired it by leaving that segment in a
            // cluster of its own with no bounds, which means never moving it.
            //
            // The symptom is a log line, so that is what this holds on to. It takes a drag to
            // provoke: separating a freshly routed sheet does not, but re-separating after one
            // symbol has moved does - on crossedArrays, where a symbol edge spanning x=70..90
            // stopped a cluster whose every segment starts beyond x=186.
            corpus
            |> List.iter (fun (_, canvas) -> separate (routedModel canvas) |> dragRoundTrip |> ignore)
            let complaints =
                Log.recentLines ()
                |> Array.filter (fun line -> line.Contains "lost" && line.Contains "cluster")
            Expect.isEmpty complaints
                $"separation complained about its own clustering: %A{complaints}"
        }

        test "dragging a symbol leaves every wire on it attached to its ports" {
            // The one thing a re-route may never do. Routing a wire as a branch off another wire
            // of its own net copies that wire's start position, so a reference wire which has not
            // itself been re-routed yet hands over the position the driver port used to be at -
            // and the new route is drawn from there, detached from the symbol that just moved.
            corpus
            |> List.iter (fun (name, canvas) ->
                let start = separate (routedModel canvas)
                let driver = busiestDriver start
                let dragged =
                    { start with Symbol = SymbolUpdate.moveSymbols start.Symbol [ driver ] { X = 60.; Y = 30. } }
                    |> fun m -> BusWireSeparate.routeAndSeparateSymbolWires m driver
                Expect.isEmpty (detachedWires dragged)
                    $"{name}: dragging the symbol driving the most wires left \
                       {(detachedWires dragged).Length} wire(s) not joined to their ports"
                // And the net has to come out of the drag still drawn as one trunk. Refusing to
                // follow a wire that has not moved yet means none of the net's wires may follow
                // any other during the drag - they are all re-routed against the model as it was -
                // so what commons the net back up here is separation, not routing.
                //
                // Judged against redrawing the sheet from nothing at the same symbol positions,
                // since the drag has moved a symbol and the net genuinely needs different wire
                // afterwards. That is also what the user does when a drag disappoints them.
                let d, r = metricsOf dragged, metricsOf (BusWireSeparate.redrawAllWires dragged)
                let drag, redrawn = d.FannedNetInk, r.FannedNetInk
                printfn "  %-14s after a drag: fanned net %8.0f/%8.0f ink %8.0f/%8.0f crossings %4d/%4d"
                    name drag redrawn d.Ink r.Ink d.Crossings r.Crossings
                Expect.isLessThan drag (redrawn * 1.2 + 1.)
                    $"{name}: dragging the busiest driver broke the net up - %.0f{drag} of wire \
                       drawn for the multi-wire nets, against %.0f{redrawn} redrawing from nothing")
        }

        test "a net of two wires leaves its port as one line, wherever the symbols are put" {
            // TEST1/TEST2 from the user's eep1 copy: two custom components 60 apart, one output
            // driving two inputs on the far side. The corridor between them is barely wider than
            // a nub, so the second wire of the net has to be shifted to get past the first - the
            // case where following the net is hardest and matters most.
            //
            // The failure it pins: the two wires leaving the same port immediately going opposite
            // ways, each running to the midline of the other's destination and back, drawn as a
            // long thin loop out of the port. That is what a wire routed as a branch off a wire
            // that has not been re-routed yet looks like, and dragging TEST2 down and left used to
            // produce it.
            let canvas =
                (TestFixtures.loadLoadedComponent
                    (System.IO.Path.Combine(TestFixtures.fixturesDir, "customPair", "test1.dgm"))).CanvasState
            let start = separate (routedModel canvas)
            let boxes (m: Model) =
                m.Symbol.Symbols |> Map.toList |> List.map (snd >> Symbol.getSymbolBoundingBox)
            /// The wires of every net that has more than one wire, which here is the one net.
            let fannedWires (m: Model) =
                m.Wires
                |> Map.toList
                |> List.map snd
                |> List.groupBy (fun w -> w.OutputPort)
                |> List.filter (fun (_, ws) -> ws.Length > 1)
            let diverging (m: Model) =
                fannedWires m
                |> List.filter (fun (_, ws) -> ws |> List.exists (fun w -> not w.Segments[1].IsZero))
            let placements =
                List.allPairs [ -60.; -45.; -30.; -15.; 0.; 15.; 30.; 45.; 60. ]
                              [ -60.; -45.; -30.; -15.; 0.; 15.; 30.; 45.; 60. ]
            let bad =
                start.Symbol.Symbols
                |> Map.toList
                |> List.collect (fun (id, sym) ->
                    placements
                    |> List.choose (fun (dx, dy) ->
                        let d = { X = dx; Y = dy }
                        let m =
                            { start with Symbol = SymbolUpdate.moveSymbols start.Symbol [ id ] d }
                            |> fun m -> BusWireRoute.updateWires m [ id ] d
                            |> fun m -> BusWireSeparate.routeAndSeparateSymbolWires m id
                        // A corridor narrower than the two nubs that have to face each other
                        // across it has no room for a shared trunk, so wires leaving the port
                        // sideways there is the right answer rather than the failure being looked
                        // for. Issie would not let a user overlap the symbols at all.
                        let roomBetween =
                            match boxes m with
                            | [ a; b ] ->
                                max (b.TopLeft.X - (a.TopLeft.X + a.W)) (a.TopLeft.X - (b.TopLeft.X + b.W))
                            | _ -> infinity
                        if roomBetween < 2. * BusWire.Constants.nubLength || (diverging m).IsEmpty then
                            None
                        else
                            Some $"{sym.Component.Label} moved ({dx},{dy})"))
            Expect.isEmpty bad
                $"the net's two wires left the port in different directions at %d{bad.Length} placements:                    %A{List.truncate 6 bad}"
        }

        test "dragging the mux in addsub does not leave its output wires behind" {
            // The case as reported: dragging MUX1 on 3cpu's addsub left its two output wires
            // exactly where they were, joined to nothing, and only "redraw all wires" put them
            // back. The second wire of that net was routed as a branch off the first while the
            // first was still drawn from where the output port used to be, and a branch takes over
            // its reference wire's start position.
            let addsub =
                (TestFixtures.loadProject "3cpu" |> List.find (fun c -> c.Name = "addsub")).CanvasState
            // Loaded with the routing the sheet was saved with, which is where a user's drag
            // starts from - not re-routed from nothing, which is a different sheet.
            let start = loadedModel addsub
            let mux1 =
                start.Symbol.Symbols
                |> Map.toList
                |> List.find (fun (_, s) -> s.Component.Label = "MUX1")
                |> fst
            // Up and to the right, which is where it happens: MUX1's second output wire then finds
            // a branch off the first one nearer to its destination than the port is, and takes it.
            // Moving it anywhere else in an 80-unit sweep does not provoke it, which is what makes
            // this the kind of bug that survives being looked for by hand.
            let dragged =
                { start with Symbol = SymbolUpdate.moveSymbols start.Symbol [ mux1 ] { X = 40.; Y = -80. } }
                |> fun m -> BusWireSeparate.routeAndSeparateSymbolWires m mux1
            Expect.isEmpty (detachedWires start) "the sheet was already broken when it was loaded"
            Expect.isEmpty (detachedWires dragged)
                "dragging MUX1 left wires drawn from where its port used to be"
        }

        test "a hand-routed wire keeps its hand routing when a symbol on it moves" {
            // The wires a drag re-routes have their routing taken off before any of them is
            // routed. A wire the user routed by hand must not be stripped with them: what holds
            // the shape they dragged into place is partialAutoroute working from the segments that
            // are already there, and there is nothing to recover them from once they are gone.
            let start = separate (routedModel (canvasOf (fanout 12)))
            let driver = busiestDriver start
            let netPort =
                start.Wires |> Map.toList |> List.countBy (fun (_, w) -> w.OutputPort)
                |> List.maxBy snd |> fst
            let wid, wire = start.Wires |> Map.toList |> List.find (fun (_, w) -> w.OutputPort = netPort)
            // as the user does it: drag a middle segment, which is what marks it Manual
            let movable =
                wire.Segments
                |> List.filter (fun s -> s.Index > 0 && s.Index < wire.Segments.Length - 1 && not s.IsZero)
            let seg = movable[movable.Length / 2]
            let byHand =
                { start with Wires = Map.add wid (BusWireUpdateHelpers.moveSegment start seg 20.) start.Wires }
            Expect.isTrue (BusWireUpdateHelpers.isManuallyRouted byHand.Wires[wid])
                "the test moved a segment and the wire did not become hand-routed"
            // A small move. partialAutoroute keeps a wire manual only up to a point - it declines
            // once the port has moved past the first hand-routed corner - and that is what a drag
            // beyond this distance runs into, here as before.
            let dragged =
                { byHand with Symbol = SymbolUpdate.moveSymbols byHand.Symbol [ driver ] { X = 15.; Y = 10. } }
                |> fun m -> BusWireSeparate.routeAndSeparateSymbolWires m driver
            Expect.isTrue (BusWireUpdateHelpers.isManuallyRouted dragged.Wires[wid])
                "dragging the driver threw away a wire's hand routing"
            Expect.isEmpty (detachedWires dragged)
                "dragging the driver of a net with a hand-routed wire in it detached a wire"
        }

        test "moving a symbol and moving it back does not make the wiring worse" {
            // Routing is deliberately not idempotent: a drag re-routes from where the symbol now
            // is, so the user sees a fast route to judge the placement by, and separation then
            // settles it. What must not happen is that a round trip degrades the sheet - if it
            // does, repeated adjustment makes the drawing worse and worse.
            //
            // Wire-for-wire equality does NOT hold today (it fails on tangle), which is why this
            // asserts on the metrics instead. Tightening it to equality would be a real
            // improvement; see docs/dev/wireRouting.md.
            corpus
            |> List.iter (fun (name, canvas) ->
                let start = separate (routedModel canvas)
                let back = dragRoundTrip start
                let b, s = metricsOf back, metricsOf start
                Expect.isLessThan b.CrossNetOverlap 1.0 $"{name}: the round trip left nets overlapping"
                Expect.isLessThan b.Ink (s.Ink * 1.02) $"{name}: the round trip added wire"
                // 10 on tangle with same-net branching on: a re-routed wire branches off a
                // different wire of its net than it did before, which is a real instability and is
                // recorded rather than hidden - see openIssues
                Expect.isLessThanOrEqual b.Crossings (s.Crossings + 14)
                    $"{name}: the round trip added crossings ({s.Crossings} -> {b.Crossings})")
        }
    ]
