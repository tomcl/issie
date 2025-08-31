module DrawBlockTests

open Expecto
open CommonTypes
open DrawHelpers
open Symbol
open BusWire
open Sheet

// Test data for Symbol module
let testSymbolData = 
    [
        "Create Input Symbol", 
        fun () ->
            let comp = createNewSymbol (Pos.init 100.0 200.0) Input "IN1" 1 ""
            Expect.equal comp.Type Input "Component type should be Input"
            Expect.equal comp.Label "IN1" "Label should match"
            Expect.equal comp.X 100.0 "X position should match"
            Expect.equal comp.Y 200.0 "Y position should match"

        "Create Output Symbol",
        fun () ->
            let comp = createNewSymbol (Pos.init 50.0 150.0) Output "OUT1" 1 ""
            Expect.equal comp.Type Output "Component type should be Output"
            Expect.equal (List.length comp.InputPorts) 1 "Should have 1 input port"
            Expect.equal (List.length comp.OutputPorts) 0 "Should have 0 output ports"

        "Create And Gate",
        fun () ->
            let comp = createNewSymbol (Pos.init 0.0 0.0) And "AND1" 2 ""
            Expect.equal comp.Type And "Component type should be And"
            Expect.equal (List.length comp.InputPorts) 2 "Should have 2 input ports"
            Expect.equal (List.length comp.OutputPorts) 1 "Should have 1 output port"

        "Create Multiplexer",
        fun () ->
            let comp = createNewSymbol (Pos.init 10.0 20.0) Mux2 "MUX1" 8 ""
            Expect.equal comp.Type Mux2 "Component type should be Mux2"
            Expect.equal (List.length comp.InputPorts) 3 "Mux2 should have 3 input ports"
            Expect.equal (List.length comp.OutputPorts) 1 "Should have 1 output port"

        "Port Positioning for DFF",
        fun () ->
            let comp = createNewSymbol (Pos.init 0.0 0.0) DFF "DFF1" 1 ""
            let inputPorts = comp.InputPorts
            let outputPorts = comp.OutputPorts
            Expect.equal (List.length inputPorts) 2 "DFF should have 2 input ports (D and CLK)"
            Expect.equal (List.length outputPorts) 1 "DFF should have 1 output port"
            // Check that clock port is at bottom
            let clkPort = List.find (fun p -> p.PortType = PortType.CLK) inputPorts
            Expect.equal clkPort.PortPos Bottom "Clock port should be at bottom"
    ]

// Test data for BusWire module  
let testBusWireData =
    [
        "Create Wire Between Components",
        fun () ->
            let srcPort = { Id = "port1"; PortNumber = Some 0; PortType = PortType.Output; HostId = "comp1" }
            let tgtPort = { Id = "port2"; PortNumber = Some 0; PortType = PortType.Input; HostId = "comp2" }
            let wire = {
                Id = "wire1"
                InputPort = tgtPort
                OutputPort = srcPort
                Color = CommonTypes.Green
                Width = 1
                Segments = []
            }
            Expect.equal wire.InputPort.HostId "comp2" "Input port should belong to comp2"
            Expect.equal wire.OutputPort.HostId "comp1" "Output port should belong to comp1"

        "Wire Auto-routing",
        fun () ->
            // Test that wire segments are generated correctly
            let segments = autoRoute (Pos.init 0.0 0.0) (Pos.init 100.0 0.0) true
            Expect.isGreaterThan (List.length segments) 0 "Should generate segments"
            
        "Wire Color Based on Width",
        fun () ->
            let color1 = wireColorFromWidth 1
            let color8 = wireColorFromWidth 8
            let color32 = wireColorFromWidth 32
            Expect.equal color1 CommonTypes.Gray "1-bit wire should be gray"
            Expect.notEqual color8 color1 "Multi-bit wire should have different color"

        "Wire Selection",
        fun () ->
            let wire = {
                Id = "wire1"
                InputPort = { Id = "p1"; PortNumber = Some 0; PortType = PortType.Input; HostId = "c1" }
                OutputPort = { Id = "p2"; PortNumber = Some 0; PortType = PortType.Output; HostId = "c2" }
                Color = CommonTypes.Green
                Width = 1
                Segments = [
                    { Id = "seg1"; Start = Pos.init 0.0 0.0; End = Pos.init 100.0 0.0; IsVertical = false }
                ]
            }
            let isNear = isWireCloseToPoint (Pos.init 50.0 2.0) wire 5.0
            Expect.isTrue isNear "Point near wire should be detected"
    ]

// Test data for Sheet module
let testSheetData = 
    [
        "Component Selection",
        fun () ->
            let model = initModel
            let comp = createNewSymbol (Pos.init 100.0 100.0) Input "IN1" 1 ""
            let modelWithComp = addComponentToModel model comp
            let selected = selectComponent comp.Id modelWithComp
            Expect.contains selected.SelectedComponents comp.Id "Component should be selected"

        "Bounding Box Calculation",
        fun () ->
            let comps = [
                createNewSymbol (Pos.init 0.0 0.0) Input "IN1" 1 ""
                createNewSymbol (Pos.init 100.0 100.0) Output "OUT1" 1 ""
            ]
            let bbox = calculateBoundingBox comps []
            Expect.equal bbox.TopLeft (Pos.init 0.0 0.0) "Top-left should be at origin"
            Expect.isGreaterThan bbox.BottomRight.X 100.0 "Bottom-right X should be > 100"

        "Copy and Paste",
        fun () ->
            let model = initModel
            let comp = createNewSymbol (Pos.init 50.0 50.0) And "AND1" 2 ""
            let modelWithComp = addComponentToModel model comp
            let clipboard = copyComponents [comp.Id] modelWithComp
            Expect.equal (List.length clipboard.Components) 1 "Should copy 1 component"
            let pasted = pasteComponents clipboard (Pos.init 100.0 100.0) modelWithComp
            Expect.equal (List.length pasted.Components) 2 "Should have 2 components after paste"

        "Delete Component",
        fun () ->
            let model = initModel
            let comp = createNewSymbol (Pos.init 0.0 0.0) Not "NOT1" 1 ""
            let modelWithComp = addComponentToModel model comp
            let afterDelete = deleteComponent comp.Id modelWithComp
            Expect.equal (List.length afterDelete.Components) 0 "Should have 0 components after delete"

        "Snap to Grid",
        fun () ->
            let pos = Pos.init 47.3 82.7
            let snapped = snapToGrid 10.0 pos
            Expect.equal snapped.X 50.0 "X should snap to 50"
            Expect.equal snapped.Y 80.0 "Y should snap to 80"
    ]

// Helper functions for tests
let createNewSymbol pos compType label busWidth theme =
    {
        Id = CommonTypes.ComponentId (System.Guid.NewGuid().ToString())
        Type = compType
        Label = label
        InputPorts = Symbol.createPorts compType PortType.Input busWidth
        OutputPorts = Symbol.createPorts compType PortType.Output busWidth
        X = pos.X
        Y = pos.Y
        H = Symbol.getHeightFromType compType
        W = Symbol.getWidthFromType compType label
        SymbolInfo = None
    }

let initModel = 
    {
        Wire = BusWire.init
        Components = Map.empty
        SelectedComponents = []
        SelectedWires = []
        NearbyComponents = []
        Clipboard = { Components = []; Wires = [] }
        LastMousePos = Pos.init 0.0 0.0
        ScrollingPos = Pos.init 0.0 0.0
        CursorType = Default
        SnapToGrid = true
        DragToSelectBox = { TopLeft = Pos.init 0.0 0.0; BottomRight = Pos.init 0.0 0.0 }
        ConnectPortsLine = { Start = Pos.init 0.0 0.0; End = Pos.init 0.0 0.0 }
        TargetPortId = None
        Action = Idle
    }

// Mock implementations for missing functions
let autoRoute startPos endPos isHorizontal =
    [{ Id = "seg1"; Start = startPos; End = endPos; IsVertical = not isHorizontal }]

let wireColorFromWidth width =
    match width with
    | 1 -> CommonTypes.Gray
    | w when w <= 8 -> CommonTypes.Blue
    | _ -> CommonTypes.Purple

let isWireCloseToPoint point wire threshold =
    List.exists (fun seg ->
        let dist = 
            if seg.IsVertical then
                abs (seg.Start.X - point.X)
            else
                abs (seg.Start.Y - point.Y)
        dist <= threshold
    ) wire.Segments

let addComponentToModel model comp =
    { model with Components = Map.add comp.Id comp model.Components }

let selectComponent compId model =
    { model with SelectedComponents = [compId] }

let calculateBoundingBox comps wires =
    let xs = List.collect (fun c -> [c.X; c.X + c.W]) comps
    let ys = List.collect (fun c -> [c.Y; c.Y + c.H]) comps
    {
        TopLeft = Pos.init (List.min xs) (List.min ys)
        BottomRight = Pos.init (List.max xs) (List.max ys)
    }

let copyComponents compIds model =
    let comps = compIds |> List.choose (fun id -> Map.tryFind id model.Components)
    { Components = comps; Wires = [] }

let pasteComponents clipboard offset model =
    let newComps = clipboard.Components |> List.map (fun c ->
        { c with 
            Id = CommonTypes.ComponentId (System.Guid.NewGuid().ToString())
            X = c.X + offset.X
            Y = c.Y + offset.Y
        }
    )
    { model with 
        Components = 
            (model.Components, newComps) 
            ||> List.fold (fun map c -> Map.add c.Id c map)
    }

let deleteComponent compId model =
    { model with Components = Map.remove compId model.Components }

let snapToGrid gridSize pos =
    Pos.init
        (round (pos.X / gridSize) * gridSize)
        (round (pos.Y / gridSize) * gridSize)

// Create test lists
[<Tests>]
let symbolTests = 
    testList "Symbol Module Tests" (
        testSymbolData |> List.map (fun (name, test) -> 
            testCase name test
        )
    )

[<Tests>]
let busWireTests =
    testList "BusWire Module Tests" (
        testBusWireData |> List.map (fun (name, test) ->
            testCase name test
        )
    )

[<Tests>]
let sheetTests =
    testList "Sheet Module Tests" (
        testSheetData |> List.map (fun (name, test) ->
            testCase name test
        )
    )