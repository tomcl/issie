module TestDrawBlockD3
open SheetBeautifyHelpers //use for flip and rotate
open TestDrawBlock
open GenerateData
open TestDrawBlock.HLPTick3
open CommonTypes
open ModelType
open DrawModelType
open TestDrawBlock.TestLib
open TestDrawBlock.HLPTick3.Builder
open Elmish

//--------------------------------------------------------------------------------------------------//
//----------------------------------------D3 Testing Helper functions-------------------------------//
//--------------------------------------------------------------------------------------------------//

    type componentInfo = {
        Label: string
        CompType: ComponentType
    }

    type connectionInfo = {
        SourceCompLabel: string
        SourceCompPort: int
        TargetCompLabel: string
        TargetCompPort: int
    }

    let genXYPos lim =
           let coords =
               fromList [-lim..20..lim]
               |> map (fun n -> float n)
           product (fun x y -> {X=x; Y=y}) coords coords

    let makePositions = genXYPos 100

    ///Generate samples for sheetWireLabelSymbol (D3) Easy Test
    let makeSamplesD3Easy =
        let rotations = fromList [Some Rotation.Degree90; Some Rotation.Degree270; None]
        let flips = fromList [Some SymbolT.FlipType.FlipHorizontal; None]
        genXYPos 100
        |> product (fun a b -> (a,b)) (genXYPos 100)
        |> product (fun a b -> (a,b)) rotations
        |> product (fun a b -> (a,b)) flips
        |> map (fun (flipMux,(rotMux,(demuxPos, muxPos))) ->
            {|
                FlipMux = flipMux;
                RotMux = rotMux;
                DemuxPos = demuxPos;
                Mux1Pos = muxPos;
                Mux2Pos = muxPos
            |})

    ///Generate samples for sheetWireLabelSymbol (D3) Hard Test
    let makeSamplesD3Hard =
        let rotations = fromList [Some Rotation.Degree90; Some Rotation.Degree270; None]
        let flips = fromList [Some SymbolT.FlipType.FlipHorizontal; None]
        genXYPos 100
        |> product (fun a b -> (a,b)) (genXYPos 100)
        |> product (fun a b -> (a,b)) rotations
        |> product (fun a b -> (a,b)) flips
        |> map (fun (flipMux,(rotMux,(orPos, muxPos))) ->
            {|
                FlipMux = flipMux;
                RotMux = rotMux;
                AndPos = muxPos;
                OrPos = orPos;
                XorPos = orPos;
                MuxPos = muxPos
            |})

    ///Generates position for a component in circuit such that it's threshold distance away from another component
    let generatePosition (model: SheetT.Model) (threshold:float) : XYPos =
        let initPosition = middleOfSheet

        let getExistingPositions =
            model.Wire.Symbol.Symbols
            |> Map.toList
            |> List.map (fun (_, sym) -> sym.Pos)

        let rec findPosition (position: XYPos) =
            if getExistingPositions |> List.exists (fun existingPos ->
                euclideanDistance existingPos position <= threshold) then
                if random.Next(0,2) = 0 then
                    findPosition { position with X = position.X + threshold }
                else
                    findPosition { position with Y = position.Y + threshold }
            else
                position

        findPosition initPosition

    let placeComponentsOnModel (components: componentInfo list) (threshold: float) (genSamples: XYPos) : SheetT.Model =
        components
        |> List.fold (fun currModel comp ->
            let compPosition = generatePosition currModel threshold 
            placeSymbol comp.Label comp.CompType (compPosition + genSamples) currModel
            |> getOkOrFail) initSheetModel

    ///Constructs a circuit using given list of compoennts and randomly connects components at threshold distance apart
    let randomConnectCircuitGen (components: componentInfo list) (threshold: float) (genSamples: XYPos) =
        components
        |> List.toArray
        |> shuffleA
        //could do pairwise and then connect the ports of the 1st element in the tuple with the 2nd
        |> Array.pairwise
        |> Array.fold (fun currModel (comp1, comp2) ->
            placeWire (portOf comp1.Label 0) (portOf comp2.Label 0) currModel
            |> Result.bind (placeWire (portOf comp1.Label 0) (portOf comp2.Label 1))
            |> getOkOrFail
            |> separateAllWires
        ) (placeComponentsOnModel components threshold genSamples)

    ///Constructs a circuit by connecting a given list of components based on given list of connections
    let fixedConnectCircuitGen (components: componentInfo list) (connections: connectionInfo list) (threshold: float) (genSamples: XYPos)=
        connections
        |> List.fold (fun currModel connect ->
            placeWire (portOf connect.SourceCompLabel connect.SourceCompPort) (portOf connect.TargetCompLabel connect.TargetCompPort) currModel
            |> getOkOrFail
            |> separateAllWires
        ) (placeComponentsOnModel components threshold genSamples)

    ///Counts number of overlapping symbols, number of wires intersecting symbols and number of wire bends in a given sheet
    let countMetrics (model: SheetT.Model) =
        //Metric 1: count number of symbols overlapping
        let boxes =
            model.BoundingBoxes
            |> Map.toList

        let numOfSymbolOverlaps =
            List.allPairs boxes boxes
            |> List.filter (fun ((n1, box1), (n2, box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
            |> List.length

        //Metric 2: count number of wires intersecting symbols
        let wireModel = model.Wire
        
        let allWires = wireModel.Wires
                        |> Map.toList

        let numOfWireIntersectSym =
            allWires
            |> List.fold (fun totalCount (_, wire) -> 
                totalCount + (BusWireRoute.findWireSymbolIntersections wireModel wire |> List.length)
            ) 0

        //Metric 3: Number of wire bends
        let numOfWireBends =
            T5 model
   
        {|SymbolOverlaps = numOfSymbolOverlaps; WireIntersectSym = numOfWireIntersectSym; WireBends = numOfWireBends|}

    ///Runs tests through testFunction and prints result metrics for each test
    let collectMetricsForTests (samples: Gen<'a>) (sheetMaker: 'a -> SheetT.Model) (testFunction) =
        [0..samples.Size-1]
        |> List.map (fun n -> 
            let sample = samples.Data n
            let sheetModel = sheetMaker sample

            let result = testFunction sheetModel

            let metrics = countMetrics result
            (n, metrics))

        |> List.iter (fun (n, m) ->
            printfn "Sample %d Metrics:" n
            printfn "Symbol overlaps: %d" m.SymbolOverlaps
            printfn "Wire intersect symbols: %d" m.WireIntersectSym
            printfn "Wire bends: %d" m.WireBends)

//--------------------------------------------------------------------------------------------------//
//-------------------------Example Test Circuits using Gen<'a> samples testing D3 Function----------//
//--------------------------------------------------------------------------------------------------//

    ///Generate easy/likely-to-pass tests for sheetWireLabelSymbol (D3)
    let makeTestD3Easy (threshold: float) (
                            sample: {|
                                FlipMux: SymbolT.FlipType option;
                                RotMux: Rotation option;
                                DemuxPos: XYPos;
                                Mux1Pos: XYPos;
                                Mux2Pos: XYPos
                            |}) : SheetT.Model = 
        let s = sample
        initSheetModel
        |> placeSymbol "DM1" (Demux4) (middleOfSheet - {X=threshold; Y=0.} + s.DemuxPos)
        |> Result.bind (placeSymbol "MUX1" (Mux4) (middleOfSheet + s.Mux1Pos))
        |> match s.RotMux with
            | Some rot -> Result.map (rotateSymbol "MUX1" rot)
            | None -> id
        |> match s.FlipMux with
            | Some flip -> Result.map (flipSymbol "MUX1" flip)
            | None -> id
        |> Result.bind (placeWire (portOf "DM1" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "DM1" 1) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "DM1" 2) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "DM1" 3) (portOf "MUX1" 3))
        |> Result.bind (placeSymbol "MUX2" (Mux4) (middleOfSheet + {X=0.; Y=threshold} + s.Mux2Pos))
        |> Result.bind (placeWire (portOf "DM1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "DM1" 1) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "DM1" 2) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "DM1" 3) (portOf "MUX2" 3))
        |> getOkOrFail
        |> separateAllWires

    ///Generate hard/likely-to-fail tests for sheetWireLabelSymbol (D3)
    let makeTestD3Hard (threshold: float) (
                            sample: {|
                                FlipMux: SymbolT.FlipType option;
                                RotMux: Rotation option;
                                AndPos: XYPos;
                                OrPos: XYPos;
                                XorPos: XYPos;
                                MuxPos: XYPos
                            |}) : SheetT.Model =    
        let s = sample
        initSheetModel
        |> placeSymbol "MUX1" (Mux2) (middleOfSheet - {X=threshold; Y=0.} + s.MuxPos)
        |> match s.RotMux with
            | Some rot -> Result.map (rotateSymbol "MUX1" rot)
            | None -> id
        |> match s.FlipMux with
            | Some flip -> Result.map (flipSymbol "MUX1" flip)
            | None -> id
        |> Result.bind (placeSymbol "OR1" (GateN(Or, 2)) (middleOfSheet + s.OrPos))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "OR1" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "OR1" 1))
        |> Result.bind (placeSymbol "AND1" (GateN(And, 2)) (middleOfSheet + {X=threshold; Y=threshold} + s.AndPos))
        |> Result.bind (placeWire (portOf "OR1" 0) (portOf "AND1" 0))
        |> Result.bind (placeWire (portOf "OR1" 0) (portOf "AND1" 1))
        |> Result.bind (placeWire (portOf "OR1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "AND1" 0) (portOf "MUX1" 1))
        |> Result.bind (placeSymbol "XOR1" (GateN(Xor, 2)) (middleOfSheet + {X=threshold; Y=0.} + s.XorPos))
        |> Result.bind (placeWire (portOf "AND1" 0) (portOf "XOR1" 0))
        |> Result.bind (placeWire (portOf "AND1" 0) (portOf "XOR1" 1))
        |> Result.bind (placeWire (portOf "XOR1" 0) (portOf "MUX1" 0))
        |> getOkOrFail
        |> separateAllWires

    ///Generate tests using circuit generation DSL with given components and connections list
    let makeTestFixedConnectCircuitGen (threshold: float) (genSamples : XYPos) : SheetT.Model =
        let components = [
            { Label = "AND1"; CompType = GateN(And, 2) };
            { Label = "OR1"; CompType = GateN(Or, 2) };
            { Label = "XOR1"; CompType = GateN(Xor, 2) };
            { Label = "MUX1"; CompType = Mux2 };
        ]

        let connections = [
            { SourceCompLabel = "AND1"; SourceCompPort = 0; TargetCompLabel = "OR1"; TargetCompPort = 0 };
            { SourceCompLabel = "OR1"; SourceCompPort = 0; TargetCompLabel = "XOR1"; TargetCompPort = 1 };
            { SourceCompLabel = "XOR1"; SourceCompPort = 0; TargetCompLabel = "MUX1"; TargetCompPort = 1 };
            { SourceCompLabel = "MUX1"; SourceCompPort = 0; TargetCompLabel = "AND1"; TargetCompPort = 1 };
        ]

        fixedConnectCircuitGen components connections threshold genSamples

    ///Generate tests using circuit generation DSL with only given components list
    let makeTestRandomConnectCircuitGen (threshold: float) (genSamples : XYPos) : SheetT.Model =
        let components = [
            { Label = "AND1"; CompType = GateN(And, 2) };
            { Label = "OR1"; CompType = GateN(Or, 2) };
            { Label = "XOR1"; CompType = GateN(Xor, 2) };
            { Label = "MUX1"; CompType = Mux2 };
        ]

        randomConnectCircuitGen components threshold genSamples

//------------------------------------------------------------------------------------------------//
//-------------------------D3 assertions used to test sheets--------------------------------------//
//------------------------------------------------------------------------------------------------//

    module Asserts =

        (* Each assertion function from this module has as inputs the sample number of the current test and the corresponding schematic sheet.
           It returns a boolean indicating (true) that the test passes or (false) that the test fails. The sample numbr is included to make it
           easy to document tests and so that any specific sampel schematic can easily be displayed using failOnSampleNumber. *)

        ///Fails when sheet contains two overlapping symbols or a wire intersecting symbol
        let failD3 (sample: int) (sheet: SheetT.Model) =
            let metrics = countMetrics sheet

            if metrics.SymbolOverlaps > 0 || metrics.WireIntersectSym > 0 then
                Some ($"Test failed on sample {sample}: {metrics.SymbolOverlaps} symbol overlaps and {metrics.WireIntersectSym} wire symbol intersections.")
            else
                None

//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests to test D3 Function----------------------------//
//---------------------------------------------------------------------------------------//

    module Tests =

        /// Allow test errors to be viewed in sequence by recording the current error
        /// in the Issie Model (field DrawblockTestState). This contains all Issie persistent state.
        let recordPositionInTest (testNumber: int) (dispatch: Dispatch<Msg>) (result: TestResult<'a>) =
            dispatch <| UpdateDrawBlockTestState(fun _ ->
                match result.TestErrors with
                | [] ->
                    printf "Test finished"
                    None
                | (numb, _) :: _ ->
                    printf $"Sample {numb}"
                    Some { LastTestNumber=testNumber; LastTestSampleIndex= numb})
            
        let testD3Easy testNum firstSample dispatch =
            let threshold = 200.0
            runTestOnSheets
                "2 Mux4 and 1 DeMux threshold distance apart: fail on overlapping symbols or symbol wire intersect"
                firstSample
                makeSamplesD3Easy
                (makeTestD3Easy threshold)
                Asserts.failD3
                dispatch
            |> recordPositionInTest testNum dispatch
            (collectMetricsForTests makeSamplesD3Easy (makeTestD3Easy threshold) id)

        let testD3Hard testNum firstSample dispatch =
            let threshold = 200.0
            runTestOnSheets
                "Mux2, AND, OR, XOR threshold distance apart: fail on overlapping symbols or symbol wire intersect"
                firstSample
                makeSamplesD3Hard
                (makeTestD3Hard threshold)
                Asserts.failD3
                dispatch
            |> recordPositionInTest testNum dispatch

        let testFixedConnectCircuitGen testNum firstSample dispatch =
            let threshold = 200.0
            runTestOnSheets
                "Circuit made of components and connections list, threshold distance apart: fail on overlapping symbols or symbol wire intersect"
                firstSample
                makePositions
                (makeTestFixedConnectCircuitGen threshold)
                Asserts.failD3
                dispatch
            |> recordPositionInTest testNum dispatch

        let testRandomConnectCircuitGen testNum firstSample dispatch =
            let threshold = 200.0
            runTestOnSheets
                "Circuit made of components list and randomly connected components, threshold distance apart: fail on overlapping symbols or symbol wire intersect"
                firstSample
                makePositions
                (makeTestRandomConnectCircuitGen threshold)
                Asserts.failD3
                dispatch
            |> recordPositionInTest testNum dispatch

        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [
                "Test1", testD3Easy // example
                "Test2", testD3Hard // example
                "Test3", testFixedConnectCircuitGen // example
                "Test4", testRandomConnectCircuitGen 
                "Test5", fun _ _ _ -> printf "Test5" // dummy test - delete line or replace by real test as needed
                "Test6", fun _ _ _ -> printf "Test6"
                "Test7", fun _ _ _ -> printf "Test7"
                "Test8", fun _ _ _ -> printf "Test8"
                "Next Test Error", fun _ _ _ -> printf "Next Error:" // Go to the nexterror in a test

            ]
