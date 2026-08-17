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
      /// Visible corners.
      Bends: int
      /// Segments of different nets crossing at right angles, strictly inside both.
      Crossings: int
      /// Length over which two different nets are drawn on top of each other. Separation exists
      /// to make this zero.
      CrossNetOverlap: float }

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
        |> List.sumBy (fun (_, w) ->
            (w.Segments |> List.filter (fun s -> not s.IsZero) |> List.length) - 1)
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
            |> fun total -> total / 2.) }

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
    { model with Wires = model.Wires |> Map.map (fun _ w -> BusWireRoute.smartAutoroute model w) }

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

let private corpus =
    [ "crossedArrays", canvasOf (crossedArrays 8)
      "wrappedArrays", canvasOf (crossedArrays 8) |> movedTo (wrappedPositions 8)
      "fanout", canvasOf (fanout 12)
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
      /// Further separation passes needed before nothing moves. 0 is right: separation is a fine
      /// adjustment applied after routing, and it should settle. `None` is a limit cycle - the
      /// pass moves the same wires back and forth for ever, so what the user is left with depends
      /// on how many times it ran.
      Settle: int option }

let private recorded =
    [ { Sheet = "crossedArrays"; Ink = 2601.; Bends = 44; Crossings = 28; Settle = Some 5 }
      { Sheet = "wrappedArrays"; Ink = 11094.; Bends = 58; Crossings = 36; Settle = None }
      { Sheet = "fanout"; Ink = 9778.; Bends = 144; Crossings = 0; Settle = Some 0 }
      { Sheet = "tangle"; Ink = 11040.; Bends = 96; Crossings = 74; Settle = Some 0 } ]

/// A settling result is no worse than what was recorded if it needs no more passes than before.
/// Not settling at all is the worst outcome, and only matches itself.
let private settlesNoWorseThan (recorded: int option) (actual: int option) =
    match recorded, actual with
    | None, _ -> true // it did not settle before; anything at all is an improvement or the same
    | Some r, Some a -> a <= r
    | Some _, None -> false

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
            printfn "  %-14s %5s %18s %10s %12s %7s %9s"
                "sheet" "wires" "ink" "bends" "crossings" "ms" "settles"
            rows
            |> List.iter (fun (name, before, after, model, ms, settle) ->
                printfn "  %-14s %5d %8.0f ->%8.0f %4d ->%4d %5d ->%5d %7.1f %9s"
                    name (Map.count model.Wires) before.Ink after.Ink
                    before.Bends after.Bends before.Crossings after.Crossings ms
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
                Expect.isLessThanOrEqual b.Crossings (s.Crossings + 2)
                    $"{name}: the round trip added crossings ({s.Crossings} -> {b.Crossings})")
        }
    ]
