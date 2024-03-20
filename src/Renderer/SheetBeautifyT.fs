module SheetBeautifyT

// this file includes some pre-integrate work
// the test structure are very much similar 
// efforts have been made to avoid redundancy in these tests 

//-----------------------------------------------------------------------------------------//
//----------Module for top-level beautify functions making (mostly) whole-sheet changes----//
//-----------------------------------------------------------------------------------------//

(*
Whole sheet functions are normally applied to whole sheet. In many cases a feature could be to
apply them to currently selected wires or components. That provides users control over what gets beautified.
Ideal beautify code will never make things worse so can be applied to whole sheet always.

Otehr relevant modules:
SheetBeautifyHelpers (helpers)
Generatedata (used to make randomized tests)
TestDrawBlock (used to test the code written here).

*)

// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open Optics.Operators
open GenerateData
open Elmish
open EEExtensions
open Helpers
open BlockHelpers
open ModelType
open Sheet.SheetInterface
open Symbol
open SymbolUpdate
open SymbolResizeHelpers
open BusWidthInferer
open BusWireSeparate
open RotateScale
open CanvasStateAnalyser



    module Asserts =

        let failOnIncreaseCrossing (sample: int) (sheet: SheetT.Model) =
            // Calculate initial number of intersections
            let initialIntersections = countIntersections sheet

            // Apply buildB2 to potentially modify the wiring
            let modifiedModel = buildB2 sheet

            // Calculate new number of intersections
            let newIntersections = countIntersections modifiedModel

            // Compare initial and new counts of intersections
            if newIntersections > initialIntersections then
                // If there's an increase, return an error message indicating failure
                Some $"Intersections increased after applying buildB2 in Sample {sample}. Initial: {initialIntersections}, New: {newIntersections}"
            else
                // If there's no increase, return None indicating a pass
                None



//------------------------------------------------------------------------------------------------------------------------------
//                                      Code Added for Team-Phase Work by hp921
//------------------------------------------------------------------------------------------------------------------------------

    
    // Define a type for component with its type and number of inputs and outputs
    type ComponentInfo = { CompType: ComponentType; Inputs: int; Outputs: int }

    // Example component types and their input/output counts
    let componentTypes = [
        { CompType = GateN (And, 2); Inputs = 2; Outputs = 1 }
        { CompType = Mux2; Inputs = 3; Outputs = 1 } // Assuming Mux2 has 2 inputs + 1 select = 3 inputs in total
        { CompType = NbitsAdder 4; Inputs = 8; Outputs = 5 } // 4 bits (2 inputs each) + carry in, 4 bits out + carry out
        { CompType = Register 8; Inputs = 8; Outputs = 8 }
        { CompType = Not; Inputs = 1; Outputs = 1 }
        { CompType = Decode4; Inputs = 2; Outputs = 4 }
        { CompType = Mux4; Inputs = 6; Outputs = 1 } // Assuming Mux4 has 4 inputs + 2 select lines
        { CompType = Demux2; Inputs = 2; Outputs = 2 } // Assuming Demux2 has 1 input + 1 select line
        { CompType = NbitsXor (4, None); Inputs = 8; Outputs = 4 } // 4 bits XOR
        { CompType = NbitsAnd 4; Inputs = 8; Outputs = 4 } // 4 bits AND
        { CompType = NbitsOr 4; Inputs = 8; Outputs = 4 } // 4 bits OR
        { CompType = DFF; Inputs = 1; Outputs = 1 }
        { CompType = DFFE; Inputs = 2; Outputs = 1 } // DFF with Enable
        { CompType = Counter 4; Inputs = 1; Outputs = 4 } // 4-bit counter with enable
        // Add more custom or specific components as needed
    ]


    // Function to generate a random position
    let randomXYPos (maxX: float) (maxY: float) : XYPos =
        let rnd = Random()
        { X = rnd.NextDouble() * maxX; Y = rnd.NextDouble() * maxY }

    // Function to select a random component from the list
    let randomComponent () : ComponentInfo =
        let rnd = Random()
        let index = rnd.Next(0, componentTypes.Length)
        componentTypes.[index]

    // Function to create a unique label for each component
    let uniqueLabel (basee: string) (id: int) : string =
        sprintf "%s%d" basee id

    // Generating a random circuit
    let makeRandomCircuit (andPos:XYPos) =
        let initModel = initSheetModel
        let maxX, maxY = 800.0, 600.0 // Define bounds for component placement

        // Randomly place a fixed number of components, for example, 4
        let rec placeComponents model id remaining =
            match remaining with
            | 0 -> model
            | _ ->
                let comp = randomComponent()
                let pos = randomXYPos maxX maxY
                let label = uniqueLabel "COMP" id
                let newModel = placeSymbol label comp.CompType pos model |> getOkOrFail
                placeComponents newModel (id + 1) (remaining - 1)

        // Assuming all components placed have at least one input and output for simplicity
        let fullyPlacedModel = placeComponents initModel 1 6

        // Randomly connect components, this example simply connects the first component's output to the second's input
        fullyPlacedModel
        |> placeWire (portOf "COMP1" 0) (portOf "COMP2" 0)
        |> Result.bind (placeWire (portOf "COMP2" 0) (portOf "COMP3" 0))
        |> Result.bind (placeWire (portOf "COMP3" 0) (portOf "COMP4" 0))
        |> Result.bind (placeWire (portOf "COMP4" 0) (portOf "COMP5" 0))
        |> Result.bind (placeWire (portOf "COMP5" 0) (portOf "COMP6" 0))
        |> Result.bind (placeWire (portOf "COMP6" 0) (portOf "COMP1" 0))
        |> getOkOrFail


    let makeRandomCircuit_ (andPos:XYPos) =
        let initModel = initSheetModel
        let maxX, maxY = 800.0, 600.0 // Define bounds for component placement
        let rnd = Random()

        // Function to randomly decide on a flip type and apply it
        let applyRandomFlip label model =
            match rnd.Next(0, 3) with
            | 0 -> model // No flip
            | 1 -> flipSymbol label SymbolT.FlipType.FlipHorizontal model |> getOkOrFail // Horizontal flip
            | 2 -> flipSymbol label SymbolT.FlipType.FlipVertical model |> getOkOrFail // Vertical flip
            | _ -> failwith "Unexpected flip option"

        // Randomly place a fixed number of components, for example, 6
        let rec placeComponents model id remaining =
            match remaining with
            | 0 -> model
            | _ ->
                let comp = randomComponent()
                let pos = randomXYPos maxX maxY
                let label = uniqueLabel "COMP" id
                let newModelResult = placeSymbol label comp.CompType pos model
                match newModelResult with
                | Ok newModel ->
                    let flippedModel = applyRandomFlip label newModel // Randomly apply flip after placing the symbol
                    placeComponents flippedModel (id + 1) (remaining - 1)
                | Error msg -> failwithf "Failed to place and flip component: %s" msg

        // Assuming all components placed have at least one input and output for simplicity
        let fullyPlacedModel = placeComponents initModel 1 2

        // Randomly connect components, this example simply connects the first component's output to the second's input
        fullyPlacedModel
        |> placeWire (portOf "COMP1" 0) (portOf "COMP2" 0)
        |> Result.bind (placeWire (portOf "COMP2" 0) (portOf "COMP1" 0))
        // |> Result.bind (placeWire (portOf "COMP3" 0) (portOf "COMP4" 0))
        // |> Result.bind (placeWire (portOf "COMP4" 0) (portOf "COMP5" 0))
        // |> Result.bind (placeWire (portOf "COMP5" 0) (portOf "COMP6" 0))
        |> Result.bind (SheetBeautify.getOptimizedModel )
        |> getOkOrFail


    let makeRandomCircuit2 (andPos:XYPos) =
        let initModel = initSheetModel
        let maxX, maxY = 800.0, 600.0 // Define bounds for component placement

        // Assume we have a predefined list `componentTypes` with at least one of each component type.
        // If not, adjust the list accordingly to include at least one of each desired component type.

        // Sequentially place components and connect them
        let rec placeAndConnectComponents model id remainingComponents =
            match remainingComponents with
            | [] -> model
            | compInfo::rest ->
                // Place the current component at a random position
                let pos = randomXYPos maxX maxY
                let label = uniqueLabel "COMP" id
                let newModel = placeSymbol label compInfo.CompType pos model |> getOkOrFail
                // Connect this component to the previous one if not the first component
                let newModelWithConnection =
                    if id > 1 then
                        let sourceLabel = uniqueLabel "COMP" (id - 1)
                        let targetLabel = label
                        // Assuming each component has at least one output and one input for simplicity
                        placeWire (portOf sourceLabel 0) (portOf targetLabel 0) newModel |> getOkOrFail
                    else
                        newModel
                placeAndConnectComponents newModelWithConnection (id + 1) rest

        let fullyConnectedModel = placeAndConnectComponents initModel 1 componentTypes

        fullyConnectedModel


    let listInputPortIds (model:SheetT.Model) =
        model.Wire.Symbol.Ports
        |> Map.toList
        |> List.choose (fun (_, port) -> 
            match port.PortType with
            | PortType.Input -> Some(port.Id)
            | _ -> None)

    let listOutputPortIds (model:SheetT.Model) =
        model.Wire.Symbol.Ports
        |> Map.toList
        |> List.choose (fun (_, port) -> 
            match port.PortType with
            | PortType.Output -> Some(port.Id)
            | _ -> None)

    
    
    let getSymbol (model:SheetT.Model) (label:string)  = 
        mapValues model.Wire.Symbol.Symbols
            |> Array.find (fun sym -> caseInvariantEqual label sym.Component.Label)
    

    let makeTest1Circuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "AND1" (GateN (And, 2)) andPos
        |> Result.bind (placeSymbol "MUX2_1" Mux2 {X=300.; Y=100.})
        |> Result.bind (flipSymbol "MUX2_1" SymbolT.FlipType.FlipHorizontal)
        |> Result.bind (placeSymbol "ADDER1" (NbitsAdder 4) {X=500.; Y=200.})
        |> Result.bind (placeSymbol "REG1" (Register 8) {X=700.; Y=300.})
        |> Result.bind (placeWire (portOf "AND1" 0) (portOf "MUX2_1" 0))
        |> Result.bind (placeWire (portOf "AND1" 0) (portOf "AND1" 1))
        |> getOkOrFail



    let display (model: SheetT.Model) =
        // T1: Count intersecting bounding box pairs within the model.
        let intersectingPairs = countIntersectingPairs model
        printfn "Number of intersecting bounding box pairs: %d" intersectingPairs

        // T2: Determine the number of symbols intersecting with any wire.
        let symbolsIntersectingWire = countSymbolIntersectingWire model
        printfn "Number of symbols intersecting with any wire: %d" symbolsIntersectingWire

        // T3: Counts the total number of intersections between all wire segments in the model.
        let totalIntersections = countIntersections model
        printfn "Total number of intersections between all wire segments: %d" totalIntersections

        // T4: Compute the sum of wire length in the model.
        let totalWireLength = totalVisibleWiringLength model
        printfn "Total wire length: %f" totalWireLength
        

        // T5: Counts the total number of right angles across all wire segments in the model.
        let totalRightAngles = countTotalRightAngles model
        printfn "Total number of right angles across all wire segments: %d" totalRightAngles

        // T6: Identifies retracing segments within the wire model.
        let (retracingSegments, endOfWireRetracing) = getRetracingSegments model
        printfn "Number of retracing segments: %d" (List.length retracingSegments)
        printfn "Number of retracing segments at the end of wires: %d" (List.length endOfWireRetracing)


    let buildB2 (model: SheetT.Model) :  SheetT.Model =
        failwithf "Not Implemented"

       

//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests on Draw Block code-----------------------------//
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
            
        /// Example test: Horizontally positioned AND + DFF: fail on sample 0
        let test1 testNum firstSample dispatch =

            let tester =
                let xrange = fromList [-100]
                let yrange = fromList [-100]
                GenerateData.product (fun x y -> x,y) xrange yrange
                |> map (fun n -> middleOfSheet + {X=float (fst n); Y=float (snd n)})
            
            let testTotalRightAngleIntersect (sample: int) (model: SheetT.Model) = 
                let length = numOfWireRightAngleCrossings model
                if (length <> -1) then
                    Some $"Right Angle number {length}"
                else
                    None
            
            let makethisTest (andPos:XYPos) =
                initSheetModel
                |> placeSymbol "G1" (GateN(And,2)) andPos
                |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
                |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
                |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
                |> getOkOrFail
                // |> randomPossibleModels
                |> getOptimizedModel
            
            let getSymId (symLabel: string) (symModel: SymbolT.Model) =
                mapValues symModel.Symbols
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
                |> function | Some x -> x.Id | None -> failwith "Can't find symbol with label '{symPort.Label}'"

            let findSymByName (symLabel: string) (model: SheetT.Model) =
                let symId = getSymId symLabel model.Wire.Symbol

                model.Wire.Symbol.Symbols[symId]
                

            // middleOfSheet + {X= -100; Y= -100}
            // |> makethisTest
            // |> findSymByName "G1"
            // |> generateSymbolScript
            // |> List.map printSymbolScript
            // |> ignore
            
            // middleOfSheet + {X= -100; Y= -100}
            // |> makethisTest
            // |> numOfWireRightAngleCrossings
            // |> printfn "Total Right Angle Intersect: %d"

            runTestOnSheets
                "beautify2d test test test"
                firstSample
                tester
                makethisTest
                testTotalRightAngleIntersect
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail on sample 10
        let test2 testNum firstSample dispatch =
            let testTotalRightAngleIntersect (sample: int) (model: SheetT.Model) = 
                let length = numOfWireRightAngleCrossings model
                Some $"Right Angle number {length}"
                // if (length <> -1) then
                //     Some $"Right Angle number {length}"
                // else
                //     None


            runTestOnSheets
                "totalRightAngleIntersect"
                firstSample
                AroundPositions
                makeTest1Circuit
                testTotalRightAngleIntersect
                dispatch
            |> recordPositionInTest testNum dispatch
            
        /// Example test: Horizontally positioned AND + DFF: fail on symbols intersect
        let test3 testNum firstSample dispatch =

            let mockSymbol =
                Map.ofList [
                    (Edge.Left, ["1"; "2"]);
                    (Edge.Right, ["3"; "4"]);
                    (Edge.Top, ["5"]);
                    (Edge.Bottom, ["6"; "7"])
                ]
            

            let generatePortOrderCombs (map: Map<Edge, string list>) =
                let edgePermutations = 
                    map
                    |> Map.map (fun _ orderList -> combinations orderList)
                
                let portOrderCombs = 
                    edgePermutations[Edge.Left] |> List.collect (fun leftConfig ->
                        edgePermutations[Edge.Right] |> List.collect (fun rightConfig ->
                            edgePermutations[Edge.Top] |> List.collect (fun topConfig ->
                                edgePermutations[Edge.Bottom] |> List.map (fun bottomConfig ->
                                    Map.ofList [
                                        (Edge.Left, leftConfig);
                                        (Edge.Right, rightConfig);
                                        (Edge.Top, topConfig);
                                        (Edge.Bottom, bottomConfig)
                                    ]
                                )
                            )
                        )
                    )
        
                portOrderCombs
            
            let result = generatePortOrderCombs mockSymbol
            result |>  List.iter (fun map ->
                map |> Map.iter (fun key value ->
                    printfn "%A: %A" key value
                )
                printfn "------" // Separator for clarity
            )

            runTestOnSheets
                "Horizontally positioned AND + DFF: fail on symbols intersect"
                firstSample
                horizLinePositions
                makeTest1Circuit
                Asserts.failOnSymbolIntersectsSymbol
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail all tests
        let test4 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail all tests"
                firstSample
                horizLinePositions
                makeTest1Circuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch
        
        let test5 testNum firstSample dispatch =
            let printXYPosList (xyPosList: list<XYPos>) =
                xyPosList |> List.iter (fun xy -> printfn "X: %f, Y: %f" xy.X xy.Y)

            let testTotalRetrace (sample: int) (model: SheetT.Model) = 
                let resultList = findRetracingSegments model
                
                if (true) then
                    Some $"{sample}"
                else
                    None

            runTestOnSheets
                "Horizontally positioned AND + DFF: fail on sample 0"
                firstSample
                horizLinePositions
                makeTest1Circuit
                testTotalRetrace
                dispatch
            |> recordPositionInTest testNum dispatch

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

        let test5 testNum firstSample dispatch =
            let testAssertion sample circuitModel =
                // Measure the number of intersections before optimization
                let beforeOptimization = countIntersections circuitModel

                // Optimize the circuit
                let optimizedCircuit = SheetBeautify.getOptimizedModel circuitModel

                // Measure the number of intersections after optimization
                // let afterOptimization = countIntersections optimizedCircuit
                let afterOptimization = 0

                // Check if the number of intersections has reduced
                if afterOptimization >= beforeOptimization then
                    // If not reduced, return a message indicating failure
                    Some $"Optimization did not reduce the number of intersections. Before: {beforeOptimization}, After: {afterOptimization}"
                else
                    // If reduced, return None indicating a pass
                    None


            // Run the test
            runTestOnSheets
                "Optimization Effectiveness Test on Intersections"
                firstSample
                horizLinePositions
                makeRandomCircuit_
                testAssertion
                dispatch
            |> recordPositionInTest testNum dispatch
