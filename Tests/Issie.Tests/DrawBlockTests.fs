/// The draw block, exercised with nothing running.
///
/// Symbol sizing measures text, and text used to be measured by creating a canvas as
/// DrawHelpers loaded - which made every symbol, and so the whole draw block, unreachable
/// outside Electron. DrawHelpers now defers that and reconstructs the width from per-character
/// advances under .NET, so symbols can be built, wires routed and the separation pass run from a
/// plain `dotnet run`.
///
/// That reconstruction is checked here against widths recorded from a real browser, since every
/// piece of geometry below is only as trustworthy as it is. Everything else is asserted
/// structurally: which ports exist and on which edge, that a routed wire is orthogonal and joins
/// the two ports it names, and that the separation pass keeps both true.
module DrawBlockTests

open Expecto
open CommonTypes
open DrawModelType
open BlockHelpers
open SheetDescription
open SheetDescription.Operators

/// two 4-bit inputs into an adder, one output
let private adderSheet =
    describeSheet "adder" [
        comp "A" (Input1(4, None))
        comp "B" (Input1(4, None))
        comp "ADD" (NbitsAdderNoCinCout 4)
        comp "S" (Output 4)
    ] [ "A" ==> "ADD/P"; "B" ==> "ADD/Q"; "ADD/SUM" ==> "S" ]

/// An input feeding a multiplexer's SEL port, which is on the bottom edge of the symbol. Placed
/// up and to the left of it (see `muxSelPositions`) this is one of the wire shapes that
/// makeInitialWireVerticesList gives 8 segments.
let private muxSelSheet =
    describeSheet "muxSel" [
        comp "IN" (Input1(1, None))
        comp "MUX" Mux2
    ] [ "IN" ==> "MUX/SEL" ]

let private muxSelPositions = [ "IN", { X = 100.; Y = 100. }; "MUX", { X = 400.; Y = 300. } ]

let private canvasOf sheet =
    match SheetLayout.toCanvasState sheet with
    | Ok canvas -> canvas
    | Error e -> failtest $"sheet would not build: {e}"

/// A BusWire model holding every component of `canvas` as a symbol, and one unrouted wire per
/// connection. This is what the app builds when it loads a sheet.
let private modelOf (canvas: CanvasState) =
    let comps, conns = canvas
    let wireModel, _ = BusWireUpdate.init ()
    let symbols = SymbolUpdate.loadComponents [] wireModel.Symbol comps
    let wireOf (conn: Connection) : BusWireT.Wire =
        { WId = ConnectionId conn.Id
          InputPort = InputPortId conn.Target.Id
          OutputPort = OutputPortId conn.Source.Id
          Color = HighLightColor.Red
          Width = 1
          Segments = []
          StartPos = { X = 0.; Y = 0. }
          InitialOrientation = BusWireT.Horizontal }
    { wireModel with
        Symbol = symbols
        Wires = conns |> List.map (fun c -> ConnectionId c.Id, wireOf c) |> Map.ofList }

/// The DSL lays a sheet out for readability; a routing test needs the symbols somewhere chosen.
let private movedTo (positions: (string * XYPos) list) ((comps, conns): CanvasState) : CanvasState =
    let moved (comp: Component) =
        positions
        |> List.tryPick (fun (label, pos) -> if label = comp.Label then Some pos else None)
        |> Option.map (fun pos -> { comp with X = pos.X; Y = pos.Y })
        |> Option.defaultValue comp
    List.map moved comps, conns

let private routeAll (model: BusWireT.Model) =
    { model with Wires = model.Wires |> Map.map (fun _ w -> BusWireUpdateHelpers.autoroute model w) }

let private symbolNamed (model: BusWireT.Model) (label: string) =
    model.Symbol.Symbols
    |> Map.toList
    |> List.map snd
    |> List.find (fun s -> s.Component.Label = label)

/// Every wire runs from the port it names to the port it names, in right angles.
///
/// The ends come from getAbsSegments rather than from getStartAndEndWirePos, which returns the
/// second-to-last vertex rather than the last and so stops a nub short of the input port.
let private checkWiresAreOrthogonalAndConnected (model: BusWireT.Model) (context: string) =
    model.Wires
    |> Map.iter (fun (ConnectionId id) wire ->
        let expectedEnd, expectedStart =
            Symbol.getTwoPortLocations model.Symbol wire.InputPort wire.OutputPort
        let segments = getAbsSegments wire
        let actualStart = (List.head segments).Start
        let actualEnd = (List.last segments).End
        let closeTo (a: XYPos) (b: XYPos) = abs (a.X - b.X) < 0.001 && abs (a.Y - b.Y) < 0.001
        Expect.isTrue (closeTo actualStart expectedStart)
            $"{context}: wire {id} starts at %A{actualStart}, but its output port is at %A{expectedStart}"
        Expect.isTrue (closeTo actualEnd expectedEnd)
            $"{context}: wire {id} ends at %A{actualEnd}, but its input port is at %A{expectedEnd}"
        // a segment is drawn along one axis only, so every corner is a right angle
        segments
        |> List.iter (fun seg ->
            let horizontal = abs (seg.Start.Y - seg.End.Y) < 0.001
            let vertical = abs (seg.Start.X - seg.End.X) < 0.001
            Expect.isTrue (horizontal || vertical)
                $"{context}: wire {id} segment {seg.Segment.Index} is diagonal: %A{seg.Start} to %A{seg.End}"))

/// Widths Chromium's canvas measureText returns for "500 12px Verdana" - Symbol's
/// componentPortStyle, the font whose measurements decide how wide a Custom symbol is drawn.
/// Recorded from a real browser so that the .NET reconstruction can be checked against it.
let private measuredAt12px =
    [ "A", 8.203
      "B", 8.227
      "SUM", 27.100
      "CARRY", 40.535
      "CIN", 22.406
      "COUT", 34.002
      "CLK", 23.373
      "EN", 16.564
      "CARRY_IN_FROM_PREVIOUS", 175.102
      "SUM_OUT_TO_NEXT", 124.336
      "halfAdd", 45.469
      "fullAdd", 41.555
      "child", 27.908
      "Reg16x8", 52.664
      "(7:0)", 31.605
      "1234567890", 76.289
      "abcdefghij", 62.273
      // the extremes of the alphabet, which a per-character table has to get right and a single
      // average width cannot: Verdana's W is 3.6 times its i
      "MMMMMMMMMM", 101.133
      "iiiiiiiiii", 32.930 ]

/// The same, for componentLabelStyle's "500 16px Verdana"
let private measuredAt16px =
    [ "A", 10.938
      "SUM", 36.133
      "CARRY", 54.047
      "halfAdd", 60.625
      "CARRY_IN_FROM_PREVIOUS", 233.469 ]

let tests =
    testList "DrawBlock" [

        // Under .NET there is no canvas, so DrawHelpers reconstructs a text width from per-character
        // advances. Everything the draw block draws is sized from that, so if it drifts away from
        // what the browser measures, tests here stop describing the app.
        test "the .NET text width matches what the browser measures" {
            let check (style: DrawHelpers.Text) (label: string) (txt: string, measured: float) =
                let estimated = DrawHelpers.getTextWidthInPixels style txt
                let errorPct = abs (estimated - measured) / measured * 100.
                Expect.isLessThan errorPct 1.0
                    $"{label} '{txt}': browser measured {measured}, this gives %.3f{estimated} (%.2f{errorPct}%% out)"
            measuredAt12px |> List.iter (check Symbol.Constants.componentPortStyle "port label")
            measuredAt16px |> List.iter (check Symbol.Constants.componentLabelStyle "component label")
        }

        test "text width scales with font size and is zero for nothing" {
            let at size = DrawHelpers.getTextWidthInPixels { DrawHelpers.defaultText with FontSize = size } "SUM"
            Expect.equal (DrawHelpers.getTextWidthInPixels DrawHelpers.defaultText "") 0. "no text, no width"
            Expect.floatClose Accuracy.high (at "20px") (2. * at "10px") "twice the size, twice the width"
        }

        test "wheel zoom classifies gestures and bounds pinch factors" {
            match Sheet.wheelZoom 0.0 120.0 true true with
            | Some (Sheet.PhysicalWheelZoom factor) ->
                Expect.floatClose Accuracy.high (Sheet.Constants.fineZoomIncrement ** -4.0) factor
                    "a 120-pixel physical wheel delta is four fine steps"
            | _ -> failtest "a physical modifier wheel was not classified as discrete zoom"

            match Sheet.wheelZoom 0.0 120.0 true false with
            | Some (Sheet.PinchZoom factor) ->
                Expect.floatClose Accuracy.high (1. / Sheet.Constants.maxZoomFactorPerWheelEvent) factor
                    "a large pinch delta is clamped instead of halving the zoom"
            | _ -> failtest "a trackpad pinch was not classified as pinch zoom"

            Expect.equal (Sheet.wheelZoom 0.0 120.0 false false) None
                "an ordinary wheel event must not zoom"
        }

        test "no single wheel event may zoom by more than the bound" {
            // a precision touchpad or Magic Mouse sends trackpad-sized deltas while Ctrl is held,
            // and deltaMode 2 scales a delta of 1 to 800, so both branches must be bounded
            let factorOf deltaMode deltaY physicalModifierHeld =
                match Sheet.wheelZoom deltaMode deltaY true physicalModifierHeld with
                | Some (Sheet.PinchZoom factor) | Some (Sheet.PhysicalWheelZoom factor) -> factor
                | None -> failtest "a zoom gesture was not classified as a zoom"
            let bound = Sheet.Constants.maxZoomFactorPerWheelEvent
            [ 0.0, 400.0, true; 0.0, -400.0, true; 2.0, 1.0, true; 2.0, -1.0, true
              0.0, 400.0, false; 0.0, -400.0, false; 2.0, 1.0, false; 2.0, -1.0, false ]
            |> List.iter (fun (deltaMode, deltaY, physical) ->
                let factor = factorOf deltaMode deltaY physical
                Expect.isTrue (factor >= 1. / bound && factor <= bound)
                    $"deltaMode {deltaMode}, deltaY {deltaY}, physical {physical} gave {factor}")
        }

        test "zoom centering preserves the requested sheet point" {
            let centre = { X = 240.; Y = 180. }
            let scroll = Sheet.zoomCenteredScrollPosition centre 1.5 800. 600.
            Expect.equal scroll { X = -40.; Y = -30. } "the new scroll is computed from the new zoom and viewport"
        }

        test "symbols are built from a canvas with no browser" {
            let comps, _ = canvasOf adderSheet
            let model = modelOf (comps, [])
            Expect.equal (Map.count model.Symbol.Symbols) 4 "one symbol per component"

            let add = symbolNamed model "ADD"
            Expect.equal (List.length add.Component.InputPorts) 2 "the adder has two inputs"
            Expect.equal (List.length add.Component.OutputPorts) 1 "and one output"
            // sizing runs through the text measurement that used to need a canvas
            Expect.isGreaterThan add.Component.W 0. "the symbol has a width"
            Expect.isGreaterThan add.Component.H 0. "and a height"

            // inputs enter on the left, outputs leave on the right
            let edgeOf (port: Port) = add.PortMaps.Orientation[port.Id]
            Expect.equal (add.Component.InputPorts |> List.map edgeOf) [ Left; Left ]
                "both inputs are on the left edge"
            Expect.equal (add.Component.OutputPorts |> List.map edgeOf) [ Right ]
                "the output is on the right edge"

            Expect.equal (Map.count model.Symbol.Ports) 6 "every port is in the port map"
        }

        test "wires autoroute between the ports they name, at right angles" {
            let model = canvasOf adderSheet |> modelOf |> routeAll
            Expect.equal (Map.count model.Wires) 3 "one wire per connection"
            model.Wires
            |> Map.iter (fun (ConnectionId id) wire ->
                Expect.isNonEmpty wire.Segments $"wire {id} was not given any segments")
            checkWiresAreOrthogonalAndConnected model "after autoroute"
        }

        test "a placement lands centred on the position it is given, and a click there commits it" {
            // The contract dragging a component out of the catalogue is built on. A drop does not
            // place the component itself: it sends the move-then-click that placing it by hand
            // would have sent, so the dragged and clicked gestures can only agree for as long as
            // a placement goes exactly where the gesture puts it. Were the move to place the
            // component somewhere of its own choosing - at a grid point, at an offset from the
            // cursor - a drop would silently stop landing under the pointer.
            let mouseAt op pos : DrawHelpers.MouseT =
                { Pos = pos; ScreenMovement = { X = 0.; Y = 0. }; ScreenPage = { X = 0.; Y = 0. }
                  ShiftKeyDown = false; Op = op }
            /// CurrentAction carries a function, so it has no equality: say which case is wanted.
            let expectAction (wanted: string) (action: SheetT.CurrentAction) =
                let name =
                    match action with
                    | SheetT.Idle -> "Idle"
                    | SheetT.DragAndDrop -> "DragAndDrop"
                    | SheetT.InitialisedCreateComponent _ -> "InitialisedCreateComponent"
                    | other -> $"%A{other}"
                Expect.equal name wanted $"the sheet should be {wanted}"
            let dropPos = { X = 500.; Y = 400. }
            let sheet, _ = SheetUpdate.init ()
            let armed =
                { sheet with
                    Action = SheetT.InitialisedCreateComponent ([], GateN (And, 2), "", None) }

            let placed, _ = SheetUpdateHelpers.mMoveUpdate armed (mouseAt DrawHelpers.Move dropPos)

            let symbols = placed.Wire.Symbol.Symbols |> Map.toList |> List.map snd
            Expect.hasLength symbols 1 "the move created exactly one symbol"
            let sym = List.head symbols
            let centre =
                { X = sym.Pos.X + sym.Component.W / 2.; Y = sym.Pos.Y + sym.Component.H / 2. }
            Expect.floatClose Accuracy.high centre.X dropPos.X "the symbol is centred on the drop"
            Expect.floatClose Accuracy.high centre.Y dropPos.Y "the symbol is centred on the drop"
            expectAction "DragAndDrop" placed.Action

            // Bounding boxes are refreshed by SheetUpdate.update's postUpdateChecks, which a click
            // arriving in the same batch as the move relies on: the click asks whether the new
            // symbol overlaps anything, and cannot ask about a symbol it has no box for.
            let boxed =
                { placed with BoundingBoxes = Symbol.getBoundingBoxes placed.Wire.Symbol }
            let committed, _ = SheetUpdateHelpers.mDownUpdate boxed (mouseAt DrawHelpers.Down dropPos)

            expectAction "Idle" committed.Action
            Expect.hasLength (committed.Wire.Symbol.Symbols |> Map.toList) 1 "the symbol is still there"
            Expect.equal committed.SelectedComponents [ sym.Id ] "and is left selected"
        }

        // A Custom component reports its size as 0x0 until autoScaleHAndW has measured its port
        // labels, so createNewSymbol used to leave `pos` as its top-left while every built-in
        // component was centred there. The catalogue drag ghost is drawn centred on the cursor,
        // so a dragged custom component landed - and warned about overlap - half a symbol away
        // from where it was shown.
        test "a custom component is centred on the position it is given, like everything else" {
            let custom =
                Custom { Name = "WIDE_ENOUGH_TO_MATTER"
                         InputLabels = [ "DATA_IN", 8; "ENABLE", 1 ]
                         OutputLabels = [ "DATA_OUT", 8 ]
                         Form = Some User; ParameterBindings = None; Description = None }
            let pos = { X = 500.; Y = 400. }
            let sym = Symbol.createNewSymbol [] pos custom "C1" SymbolT.ThemeType.Colourful
            Expect.isGreaterThan sym.Component.W 0. "autoScaleHAndW has given it a real width"
            Expect.floatClose Accuracy.high (sym.Pos.X + sym.Component.W / 2.) pos.X
                "the symbol is centred horizontally on the position"
            Expect.floatClose Accuracy.high (sym.Pos.Y + sym.Component.H / 2.) pos.Y
                "and vertically"
            Expect.floatClose Accuracy.high sym.Component.X sym.Pos.X
                "the component record agrees with the symbol on X"
            Expect.floatClose Accuracy.high sym.Component.Y sym.Pos.Y
                "and on Y"
        }

        // A bus select is drawn small - half a grid square high - because it sits in the middle of
        // wiring laid out around it. Nothing is written inside a body that thin, and rotated it is
        // thinner still: the bit range goes beside it. Were the range ever moved inside, a rotated
        // bus select would be 15 pixels wide holding 45 pixels of text.
        test "a bus select stays small and holds no text, whatever range it selects" {
            [ 1, 0; 4, 0; 8, 0; 1, 31; 16, 16; 32, 0; 8, 1024 ]
            |> List.iter (fun (nBits, lsb) ->
                let ct = BusSelection (nBits, lsb)
                let comp = Symbol.makeComponent { X = 0.; Y = 0. } ct $"id{nBits}_{lsb}" "SEL"
                Expect.equal (Symbol.getComponentLegend ct Degree0) ""
                    "nothing is drawn inside the body, so no rotation of it can clip anything"
                Expect.equal comp.H (float Symbol.Constants.gridSize / 2.)
                    "it stays as thin as the line it replaced, so no sheet holding one reflows"
                Expect.equal comp.W (float Symbol.Constants.gridSize * 2.)
                    "and as wide, for the same reason"
                Expect.stringContains (Symbol.busSelectTitle nBits lsb) (string lsb)
                    "the range drawn beside it names the lsb")
        }

        // The spreader is the other way about: rare enough to be spelled out, so it is a body of
        // the usual size with a name on each port, and the wire shape it used to be drawn as is
        // gone. What matters is that the ports still exist and still say which is which.
        test "the bus spreader is a body with a named port on each side" {
            let comp = Symbol.makeComponent { X = 0.; Y = 0. } (NbitSpreader 8) "id" "S1"
            let inNames, outNames = CanvasStateAnalyser.portNames (NbitSpreader 8)
            Expect.equal inNames [ "IN" ] "the input is named"
            Expect.equal outNames [ "OUT" ] "and so is the output"
            Expect.hasLength comp.InputPorts 1 "one input port"
            Expect.hasLength comp.OutputPorts 1 "one output port"
            Expect.stringContains (Symbol.getComponentLegend (NbitSpreader 8) Degree0) "8"
                "the legend names the width it spreads to"
        }

        // makeInitialWireVerticesList produces wires of 6, 7, 8 and 9 segments. tryShiftHorizontalSeg
        // handled every count but 8 and silently returned the wire it was given, so a wire reaching a
        // top or bottom edge port from beyond it was never routed around anything: smartAutoroute
        // spent its whole recursion budget re-testing an unchanged wire and then kept it.
        test "a wire into a bottom-edge port can be shifted around an obstacle" {
            let model =
                canvasOf muxSelSheet |> movedTo muxSelPositions |> modelOf |> routeAll
            let wire = model.Wires |> Map.toList |> List.map snd |> List.exactlyOne
            Expect.equal wire.Segments.Length 8
                "the shape this test is about: 8 segments, ending in a vertical nub"

            // one obstacle, below both symbols, so that shifting the horizontal segment down to
            // clear it cannot take the wire through either symbol
            let obstacle = { TopLeft = { X = 300.; Y = 500. }; W = 40.; H = 40. }
            match BusWireRoute.tryShiftHorizontalSeg 5 model [ obstacle ] wire with
            | None -> failtest "no shifted route was found for an 8-segment wire"
            | Some shifted ->
                let lengths (w: BusWireT.Wire) = w.Segments |> List.map (fun s -> s.Length)
                Expect.notEqual (lengths shifted) (lengths wire) "the wire actually moved"
                checkWiresAreOrthogonalAndConnected
                    { model with Wires = Map.add shifted.WId shifted model.Wires }
                    "after shifting the horizontal segment"
        }

        test "the binary search over lines includes the first line" {
            // findInterval narrows towards lines[below].P < p <= lines[above].P, and took the
            // bottom end on trust: for a target at or below the first line it answered 1, hiding
            // that line - normally a symbol edge - from both corner-removal checks.
            let lineAt p : BusWireRoutingHelpers.Line =
                { P = p
                  B = { MinB = 0.; MaxB = 100. }
                  Orientation = BusWireT.Horizontal
                  Seg1 = None
                  LType = BusWireRoutingHelpers.BARRIERPOS
                  SameNetLink = []
                  Wid = ConnectionId ""
                  PortId = OutputPortId ""
                  Lid = BusWireRoutingHelpers.LineId 0 }
            let lines = [| 10.; 20.; 30. |] |> Array.map lineAt
            let at p = BusWireSeparate.findInterval lines p
            Expect.equal (at 5.) 0 "a target below every line starts the scan at the first line"
            Expect.equal (at 10.) 0 "and so does a target on the first line"
            Expect.equal (at 15.) 1 "otherwise the scan starts at the first line at or above it"
            Expect.equal (at 25.) 2 "wherever in the array that is"
        }

        test "every spike in a wire is removed, and the wire still reaches the same place" {
            // A spike is a segment doubling back on the one before it with a zero-length segment
            // between. Removing one shortens the segment list, so the scan for the next has to be
            // on the shortened list.
            let seg i len : BusWireT.Segment =
                { Index = i; Length = len; WireId = ConnectionId "w"
                  IntersectOrJumpList = []; Mode = BusWireT.Auto; Draggable = i <> 0 }
            let wire: BusWireT.Wire =
                { WId = ConnectionId "w"
                  InputPort = InputPortId "in"
                  OutputPort = OutputPortId "out"
                  Color = HighLightColor.Red
                  Width = 1
                  StartPos = { X = 0.; Y = 0. }
                  InitialOrientation = BusWireT.Horizontal
                  //          nub    0     out    0    back    down   0    back    nub
                  Segments = [ 10.; 0.; 50.; 0.; -20.; 40.; 0.; -15.; 10. ]
                             |> List.mapi seg }
            let travel (w: BusWireT.Wire) =
                w.Segments
                |> List.mapi (fun i s -> if i % 2 = 0 then s.Length, 0. else 0., s.Length)
                |> List.fold (fun (x, y) (dx, dy) -> x + dx, y + dy) (0., 0.)

            match BusWireSeparate.removeWireSpikes wire with
            | None -> failtest "the two spikes in this wire were not found"
            | Some cleaned ->
                Expect.equal cleaned.Segments.Length 5 "both spikes went, two segments each"
                Expect.equal (travel cleaned) (travel wire) "the wire still ends where it ended"
                Expect.equal (cleaned.Segments |> List.map (fun s -> s.Index)) [ 0 .. 4 ]
                    "and the segments left are indexed in order"
            Expect.isNone (BusWireSeparate.removeWireSpikes { wire with Segments = [ seg 0 10.; seg 1 0.; seg 2 10. ] })
                "a wire with no spike is left alone"
        }

        test "the separation pass leaves every wire connected and orthogonal" {
            // separation is the pass that nudges parallel wires apart, and the one most likely
            // to move a segment somewhere it should not go
            let routed = canvasOf adderSheet |> modelOf |> routeAll
            let separated =
                BusWireSeparate.updateWireSegmentJumpsAndSeparations
                    (routed.Wires |> Map.toList |> List.map fst)
                    routed
            Expect.equal (Map.count separated.Wires) 3 "no wire was lost"
            checkWiresAreOrthogonalAndConnected separated "after separation"
        }
    ]
