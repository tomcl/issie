module TestDrawBlockD2
open Elmish

// goal of D2 Tests: check to see if wire crossings have been reduced.

// Test metrics (starters):
// 2a (complete): number of wires straightened. Test before and after as Assertation. "should-never-happen" test (if increase), must not fail (functional requirment)
// 2b (complete): number of wires crossing. Test before and after as Assertation. "should-never-happen" test, must not fail

// Test metrics:(Option) 
// 5a (complete): components do not overlap. Need to filter out position components that are overlapped
// 5b: wires are not “squashed” between components too close together, 
// 5c (complete): wire routing does not become much longer 
// Plus 2a and b

// TODO:
// wire squishing


open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open CommonTypes
open BlockHelpers
open SymbolPortHelpers
open Symbol
open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open ModelType
open BusWire
open RotateScale
open SheetBeautifyHelpers
open GenerateData
open System

module D2TestLib =
    let getOkOrFail (res: Result<'a,string>) =
        match res with
        | Ok x -> x
        | Error mess ->
            failwithf "%s" mess


    type TestStatus =
        | Fail of string
        | Exception of string

    type Test<'a> = {
        Name: string
        Samples: Gen<'a> 
        StartFrom: int
        /// The 1st argument is the test number: allows assertions that fail on a specific sample
        /// to display just one sample.
        /// The return value is None if test passes, or Some message if it fails.
        Assertion: int -> 'a -> string option
        }

    type TestResult<'a> = {
        TestName: string
        TestData: Gen<'a> // need to change
        FirstSampleTested: int
        TestErrors: (int * TestStatus) list
    }

    let catchException name func arg =
        try
            Ok (func arg)
        with
            | e ->
                Error ($"Exception when running {name}\n" + e.StackTrace)

    /// for testing
    type Sample = {
        AndPos: XYPos;
        Mux1Pos: XYPos;
        Mux2Pos: XYPos;
        Input1Pos: XYPos;
        Input2Pos: XYPos
    }

    let runD2Tests (test: Test<'a>) : TestResult<'a>  =
        [test.StartFrom..test.Samples.Size - 1]
        |> List.map (fun n ->
                catchException $"generating test {n} from {test.Name}" test.Samples.Data n
                |> (fun res -> n,res)
           )           
        |> List.collect (function
                            | n, Error mess -> [n, Exception mess]
                            | n, Ok sample ->
                                match catchException $"'test.Assertion' on test {n} from 'runTests'" (test.Assertion n) sample with
                                | Ok None -> []
                                | Ok (Some failure) -> [n,Fail failure]
                                | Error (mess) -> [n,Exception mess])
        |> (fun resL ->                
                {
                    TestName = test.Name
                    FirstSampleTested = test.StartFrom
                    TestData = test.Samples
                    TestErrors =  resL
                })
    
module TestD2 = 
    open SheetBeautify
    open D2TestLib
    open EEExtensions
    open Sheet.SheetInterface
    open SymbolResizeHelpers

    // create an initial empty Sheet Model 
    let initSheetModel = DiagramMainView.init().Sheet

    /// Optic to access SheetT.Model from Issie Model
    let sheetModel_ = sheet_

    /// Optic to access BusWireT.Model from SheetT.Model
    let busWireModel_ = SheetT.wire_

    /// Optic to access SymbolT.Model from SheetT.Model
    let symbolModel_ = SheetT.symbol_

    /// allowed max X or y coord of svg canvas
    let maxSheetCoord = Sheet.Constants.defaultCanvasSize
    let middleOfSheet = {X=maxSheetCoord/2.;Y=maxSheetCoord/2.}

    let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2
    
    type SymbolPort = { Label: string; PortNumber: int }

    /// convenience function to make SymbolPorts
    let portOf (label:string) (number: int) =
        {Label=label; PortNumber = number}

    module Builder = 
        let placeSymbol (symLabel: string) (compType: ComponentType) (position: XYPos) (model: SheetT.Model) : Result<SheetT.Model, string> =
            let symLabel = String.toUpper symLabel // make label into its standard casing
            let symModel, symId = SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
            let sym = symModel.Symbols[symId]
            match position + sym.getScaledDiagonal with
            | {X=x;Y=y} when x > maxSheetCoord || y > maxSheetCoord ->
                Error $"symbol '{symLabel}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
            | _ ->
                model
                |> Optic.set symbolModel_ symModel
                |> SheetUpdateHelpers.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
                |> Ok
        
        let placeCustomSymbol
                (symLabel: string)
                (ccSheetName: string)
                (project: Project)
                (scale: XYPos)
                (position: XYPos)
                (model: SheetT.Model)
                    : Result<SheetT.Model, string> =
           let symbolMap = model.Wire.Symbol.Symbols
           if caseInvariantEqual ccSheetName project.OpenFileName then
                Error "Can't create custom component with name same as current opened sheet"        
            elif not <| List.exists (fun (ldc: LoadedComponent) -> caseInvariantEqual ldc.Name ccSheetName) project.LoadedComponents then
                Error "Can't create custom component unless a sheet already exists with smae name as ccSheetName"
            elif symbolMap |> Map.exists (fun _ sym ->  caseInvariantEqual sym.Component.Label symLabel) then
                Error "Can't create custom component with duplicate Label"
            else
                let canvas = model.GetCanvasState()
                let ccType: CustomComponentType =
                    {
                        Name = ccSheetName
                        InputLabels = Extractor.getOrderedCompLabels (Input1 (0, None)) canvas
                        OutputLabels = Extractor.getOrderedCompLabels (Output 0) canvas
                        Form = None
                        Description = None
                    }
                placeSymbol symLabel (Custom ccType) position model
        let placeWire (source: SymbolPort) (target: SymbolPort) (model: SheetT.Model) : Result<SheetT.Model,string> =
            let symbols = model.Wire.Symbol.Symbols
            let getPortId (portType:PortType) symPort =
                mapValues symbols
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symPort.Label)
                |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"
                |> Result.bind (fun sym ->
                    match portType with
                    | PortType.Input -> List.tryItem symPort.PortNumber sym.Component.InputPorts
                    | PortType.Output -> List.tryItem symPort.PortNumber sym.Component.OutputPorts
                    |> function | Some port -> Ok port.Id
                                | None -> Error $"Can't find {portType} port {symPort.PortNumber} on component {symPort.Label}")
            
            match getPortId PortType.Input target, getPortId PortType.Output source with
            | Error e, _ | _, Error e -> Error e
            | Ok inPort, Ok outPort ->
                let newWire = BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
                if model.Wire.Wires |> Map.exists (fun wid wire -> wire.InputPort=newWire.InputPort && wire.OutputPort = newWire.OutputPort) then
                        // wire already exists
                        Error "Can't create wire from {source} to {target} because a wire already exists between those ports"
                else
                     model
                     |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
                     |> Ok
            

        /// Run the global wire separation algorithm (should be after all wires have been placed and routed)
        let separateAllWires (model: SheetT.Model) : SheetT.Model =
            model
            |> Optic.map busWireModel_ (BusWireSeparate.updateWireSegmentJumpsAndSeparations (model.Wire.Wires.Keys |> Seq.toList))

        /// Copy testModel into the main Issie Sheet making its contents visible
        let showSheetInIssieSchematic (testModel: SheetT.Model) (dispatch: Dispatch<Msg>) =
            let sheetDispatch sMsg = dispatch (Sheet sMsg)
            dispatch <| UpdateModel (Optic.set sheet_ testModel) // set the Sheet component of the Issie model to make a new schematic.
            sheetDispatch <| SheetT.KeyPress SheetT.CtrlW // Centre & scale the schematic to make all components viewable.
        
        /// 1. Create a set of circuits from Sample samples by applying sheetMaker to each sample.
        /// 2. Check each ciruit with sheetChecker.
        /// 3. Return a TestResult record with errors those samples for which sheetChecker returns false,
        /// or where there is an exception.
        /// If there are any test errors display the first in Issie, and its error message on the console.
        /// sheetMaker: generates a SheetT.model from the random sample
        /// sheetChecker n model: n is sample number, model is the genrated model. Return false if test fails.
        let runD2TestOnSheets
            (name: string)
            (sampleToStartFrom: int)
            (samples : Gen<'a>)
            (sheetMaker: 'a -> SheetT.Model)
            (sheetChecker: int -> SheetT.Model -> string option)
            (dispatch: Dispatch<Msg>)
                : TestResult<'a> =
            let generateAndCheckSheet n = sheetMaker >> sheetChecker n
            let result =
                {
                    Name=name;
                    Samples=samples;
                    StartFrom = sampleToStartFrom
                    Assertion = generateAndCheckSheet
                }
                |> runD2Tests
            match result.TestErrors with
            | [] -> // no errors
                printf $"Test {result.TestName} has PASSED."
            | (n,first):: _ -> // display in Issie editor and print out first error
                printf $"Test {result.TestName} has FAILED on sample {n} with error message:\n{first}"
                match catchException "" sheetMaker (samples.Data n) with
                | Ok sheet -> showSheetInIssieSchematic sheet dispatch
                | Error mess -> ()
            result
    
    open Builder
    /// for Test metric 2a: number of wires straightened. Really praying visibleSegments is goated. Not using this atm
    let findStraightWireSeg (model: SheetT.Model) (wire: BusWireT.Wire) : bool =
        let segments = SegmentHelpers.visibleSegments wire.WId model
        List.length segments = 1 // I'm not sure this will work as well as expected, cuz segments are weird 

    /// Returns the number of straight wires in a sheet   . Not using this atm
        /// Returns the number of straight wires in a sheet   
    let numOfStraightWires (sheet: SheetT.Model) : int =
            let wModel = sheet.Wire
            let allWires = sheet.Wire.Wires |> Map.values

            let straightWiresCount =
                allWires
                |> Seq.filter(fun wire ->
                    match findStraightWireSeg sheet wire with
                    | true -> true
                    | _ -> false)
                |> Seq.length
            straightWiresCount

    // for random (not yet implemented, could use shuffleA) pos in Option tests 5.  generates Sample Data. Do not need this yet
    // let samplePositions =
    //     let height = fromList [-100..10..100]
    //     let width = fromList [-200..10..200]

    //     product (fun x y -> x,y) width height
    //     |> map (fun (x,y)->middleOfSheet + {X=float x; Y=float y})
    // let pickRandomElements n list =
    //     let rnd = Random().Next(0,Li)
        
    //     // // Generate a list of unique random indices
    //     // let uniqueIndices =
    //     //     Seq.initInfinite (fun _ -> rnd.Next(0, List.length list))
    //     //     |> Seq.distinct
    //     //     |> Seq.take n

    //     // Map indices to corresponding elements in the list
    //     Seq.map (fun i -> List.item i list) rnd
    //     |> Seq.toList
    
    // filtering overlaps is a necessary preprocessor for testing
    // let filterOverlap
    
    // Fixes the FlipVertical 0 180 error
    let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : (SheetT.Model) =
        let symbolsMap = model.Wire.Symbol.Symbols
        let getSymbol =
            mapValues symbolsMap
            |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
            |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"

        match getSymbol with
        | Ok symbol ->
            let flippedSymbol = 
                match flip with
                | FlipHorizontal -> SymbolResizeHelpers.flipSymbol flip symbol
                | FlipVertical -> SymbolResizeHelpers.flipSymbol FlipHorizontal symbol
                                    |> rotateAntiClockByAng Degree180 
            let updatedSymbolsMap = Map.add symbol.Id flippedSymbol symbolsMap
            { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

        | _ -> model

    let randomFlipSymbol (symLabel: string) = 
        let randomFlipType = 
            let rnd = Random().Next(0, 4) // says exclusive upperbound, but match statement seems to think it could be an impossible value
            match rnd with
            // | 1 -> Some FlipHorizontal
            | 3 -> Some FlipVertical
            | _ -> None
        match randomFlipType with
            | Some f -> Result.map (flipSymbol symLabel f)
            | None -> id
    let normalFlipSymbol (symLabel: string) (flip: SymbolT.FlipType)= 
        Result.map (flipSymbol symLabel flip)

    let reverseInputPorts (symLabel: string) (reverse: bool option) (model: SheetT.Model) = 
        let symbolsMap = model.Wire.Symbol.Symbols
        let getSymbol =
            mapValues symbolsMap
            |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
            |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"

        match getSymbol with
        | Ok symbol ->
            let reversedSymbol = Optic.set reversedInputPorts_ reverse symbol
            let updatedSymbolsMap = Map.add symbol.Id reversedSymbol symbolsMap
            { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

        | _ -> model
    let randomReverseInputPorts (symLabel: string) =
        let reverse = 
            let rnd = Random().Next(0, 2) 
            match rnd with
            | 0 -> Some true
            | 1 -> Some false
            | _ -> None
        Result.map (reverseInputPorts symLabel reverse)

//=======================================For testing samples===============================

    /// Circuit where gates and MUXes are randomly flipped. 
    /// Can be reimplemented with list as input rather than record if making random collection of components
    /// unsafe. Should return a Result (idk why lol)
    /// TODO: flips and reverses should be input params to be iterated over 
    
    /// default circuit positions.
    let andPos = middleOfSheet + {X = 200.0; Y = -100.0}
    let mux1Pos = middleOfSheet + {X = -100.0; Y = -100.0}
    let mux2Pos = middleOfSheet + {X = 20.0; Y = 20.0}
    let input1Pos = middleOfSheet + {X = -200.0; Y = 50.0}
    let input2Pos = middleOfSheet + {X = -200.0; Y = 100.0}

    // sample is currently arbitrary. Hope to add random positions to it. 
    // Currently just runs through the circuit 20 (21?) times with random flips and rotations
    // TODO: make rotations and reverse input ports into samples... 
    // wait it's already working I think lol. Just usually fails at 1 for D1 atm because usually fails w/o beautify
    let arbitrarySamples = 
        fromList [0..1..20]
    
    // let arbitrarySamples = 
    //     /// default circuit positions.
    //     let andPos = middleOfSheet + {X = 200.0; Y = -100.0}
    //     let mux1Pos = middleOfSheet + {X = -100.0; Y = -100.0}
    //     let mux2Pos = middleOfSheet + {X = 20.0; Y = 20.0}
    //     let input1Pos = middleOfSheet + {X = -200.0; Y = 50.0}
    //     let input2Pos = middleOfSheet + {X = -200.0; Y = 100.0}
    //     [|0..20|] 
    //     |> map (fun n ->  
    //         [andPos; mux1Pos; mux2Pos; input1Pos; input2Pos])

    // produces random pos not in lst. Use List.tryFind

    let randomPos lst = 
        let rndX = Random().Next(-20, 20)*10
        let rndY = Random().Next(-10, 10)*10
        let pos = middleOfSheet + {X = rndX; Y = rndY}
        pos
    
    // add an element to a list

    let samplePositions coord =
        let x,y = coord
        let height = fromList [-y..30..y]
        let width = fromList [-x..30..x]

        product (fun x y -> x,y) width height
        |> map (fun (x,y)->middleOfSheet + {X=float x; Y=float y})

    // with fixed positions
    let makeRandomFlipCircuit (sample)  = 
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> randomFlipSymbol "G1"
        |> randomReverseInputPorts "G1"
        |> Result.bind (placeSymbol "MUX1" Mux2 mux1Pos)
        |> randomFlipSymbol "MUX1"
        |> randomReverseInputPorts "MUX1"
        |> Result.bind (placeSymbol "MUX2" Mux2 mux2Pos)
        |> randomFlipSymbol "MUX2"
        |> randomReverseInputPorts "MUX2"
        |> Result.bind (placeSymbol "S1" (Input1(1, None)) input1Pos) // why called Input1 if need int 1?
        |> randomFlipSymbol "S1"
        |> Result.bind (placeSymbol "S2" (Input1(1, None)) input2Pos)
        |> randomFlipSymbol "S2"
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2)) // how is port number determined
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 0))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 1))
        |> getOkOrFail

    let makeRandomFlipandPosCircuit (sample)  = 
        let andPos = List.head sample    
        let sample = List.tail sample
        let mux1Pos = List.head sample
        let sample = List.tail sample
        let mux2Pos = List.head sample
        let sample = List.tail sample
        let input1Pos = List.head sample
        let sample = List.tail sample
        let input2Pos = List.head sample

        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> randomFlipSymbol "G1"
        |> randomReverseInputPorts "G1"
        |> Result.bind (placeSymbol "MUX1" Mux2 mux1Pos)
        |> randomFlipSymbol "MUX1"
        |> randomReverseInputPorts "MUX1"
        |> Result.bind (placeSymbol "MUX2" Mux2 mux2Pos)
        |> randomFlipSymbol "MUX2"
        |> randomReverseInputPorts "MUX2"
        |> Result.bind (placeSymbol "S1" (Input1(1, None)) input1Pos) // why called Input1 if need int 1?
        |> randomFlipSymbol "S1"
        |> Result.bind (placeSymbol "S2" (Input1(1, None)) input2Pos)
        |> randomFlipSymbol "S2"
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2)) // how is port number determined
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 0))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 1))
        |> getOkOrFail
    // TODO: get this working
    let gridMaker dimensions =
        let height,width = dimensions
        let randomCompPos = samplePositions (height,width)
                            |> toArray 
                            |> shuffleA
                            |> Array.toList
                            |> List.truncate 5 // only 5 components atm
        let samples size = 
             [|0..size|] 
             |> Array.map (fun n -> randomCompPos)
             |> fromArray
        // unsafe, should return default pos if empty after filter
        // num of samples could be a param for gridMaker? Rather than setting to 20 atm
        let filteredPos = 
            samples 20
            |> filter (fun pos ->
                let numOfOverlaps = makeRandomFlipCircuit pos 
                                    |> numOfIntersectedSymPairs
                numOfOverlaps > 0
            )

        /// default circuit positions.
        let yee (x:Gen<'a>) = 0
        let andPos = middleOfSheet + {X = 200.0; Y = -100.0}
        let mux1Pos = middleOfSheet + {X = -100.0; Y = -100.0}
        let mux2Pos = middleOfSheet + {X = 20.0; Y = 20.0}
        let input1Pos = middleOfSheet + {X = -200.0; Y = 50.0}
        let input2Pos = middleOfSheet + {X = -200.0; Y = 100.0}
        // prevents empty after filter
        match (toList filteredPos) with
        | [] -> 
            let defaultPos = [[andPos; mux1Pos; mux2Pos; input1Pos; input2Pos]]
            fromList defaultPos
        | _ -> filteredPos

    module Asserts =
        let randomFail (sample: int) (sheet: SheetT.Model) = 
            let rnd = random.Next(0,1)
            match rnd with
            | 0 -> 
                Some $"Random Fail (test for testing) in Sample {sample}"
            | _ -> 
                let printSuccess x = printf $"Random fail test success in Sample {sample}" ; x
                printSuccess None
        /// The following should not fail if sheetOrderFlip works properly (unless edge cases).
        let failOnMoreWireCrossings (sample: int) (sheet: SheetT.Model) = 
            let crossingsBefore = numOfWireRightAngleCrossings sheet
            let crossingsAfter = sheetOrderFlip sheet |> numOfWireRightAngleCrossings
            match crossingsBefore, crossingsAfter with
            | _ when crossingsAfter > crossingsBefore -> 
                Some $"Wire crossings increased from {crossingsBefore} to {crossingsAfter} detected in Sample {sample}"
            | _ -> 
                let printSuccess x = printf $"Wire crossings decreased from {crossingsBefore} to {crossingsAfter} in Sample {sample}" ; x
                printSuccess None
        
        let failOnFewerStraightWires (sample:int) (sheet: SheetT.Model) = 
            let oldStraightLinesCount = numOfStraightWires sheet
            let newStraightLinesCount = sheetOrderFlip sheet |> numOfStraightWires
            match oldStraightLinesCount, newStraightLinesCount with
            | _ when newStraightLinesCount < oldStraightLinesCount ->
                Some $"Straight lines decreased. Straight lines before: {oldStraightLinesCount}. Straight lines after: {newStraightLinesCount} detected in Sample {sample}"
            | _ -> 
                let printSuccess x = printf $"Straight lines increased from {oldStraightLinesCount} to {newStraightLinesCount}" ; x
                printSuccess None

        let failOnComponentOverlap (sample: int) (sheet: SheetT.Model) =
            let numOfCompOverlap = sheetOrderFlip sheet |> numOfIntersectedSymPairs 
            match numOfCompOverlap with
            | _ when numOfCompOverlap > 0 -> 
                Some $"{numOfCompOverlap} component intersections detected in Sample {sample}"
            | _ -> 
                let printSuccess x = printf $"No component overlaps in sample {sample}" ; x
                printSuccess None 
        
        let failOnLongerWireRouting (threshold: float) (sample: int) (sheet: SheetT.Model) = 
            let oldRoutingLength = calcVisWireLength sheet
            let newRoutingLength = sheetOrderFlip sheet |> calcVisWireLength 
            match newRoutingLength, oldRoutingLength with
            | _ when newRoutingLength - oldRoutingLength > threshold -> 
                Some $"Wire routing length exceeded threshold in Sample {sample}. From {oldRoutingLength} to {newRoutingLength} in Sample {sample}"
            | _ -> 
                let printSuccess x = printf $"Routing length went from {oldRoutingLength} to {newRoutingLength} in Sample {sample}" ; x
                printSuccess None
        // TODO: Wire squashing test

    module Tests = 
    // redo runTestsOnSheets
        let recordPositionInTest (testNumber: int) (dispatch: Dispatch<Msg>) (result: TestResult<'a>) =
            dispatch <| UpdateDrawBlockTestState(fun _ ->
                match result.TestErrors with
                | [] ->
                    printf "Test finished"
                    None
                | (numb, _) :: _ ->
                    printf $"Sample {numb}"
                    Some { LastTestNumber=testNumber; LastTestSampleIndex= numb})
        let D2Test0 testNum firstSample dispatch =
            runD2TestOnSheets
                "Random Fail/Success Test"
                firstSample
                arbitrarySamples
                makeRandomFlipCircuit
                Asserts.randomFail
                dispatch
            |> recordPositionInTest testNum dispatch

        let D2Test1 testNum firstSample dispatch =
            runD2TestOnSheets
                "Wire Crossings Test"
                firstSample
                arbitrarySamples
                makeRandomFlipCircuit
                Asserts.failOnMoreWireCrossings
                dispatch
            |> recordPositionInTest testNum dispatch
        
        let D2Test2 testNum firstSample dispatch =
            runD2TestOnSheets
                "Straight lines Test"
                firstSample
                arbitrarySamples
                makeRandomFlipCircuit
                Asserts.failOnFewerStraightWires
                dispatch
            |> recordPositionInTest testNum dispatch
        
        let D2Test3 testNum firstSample dispatch =
            runD2TestOnSheets
                "Component Overlap Test"
                firstSample
                arbitrarySamples
                makeRandomFlipCircuit
                Asserts.failOnComponentOverlap
                dispatch
            |> recordPositionInTest testNum dispatch
        
        let D2Test4 threshold testNum firstSample dispatch =
            runD2TestOnSheets
                "Wire Routing Length Test"
                firstSample
                arbitrarySamples
                makeRandomFlipCircuit
                (Asserts.failOnLongerWireRouting threshold)
                dispatch
            |> recordPositionInTest testNum dispatch

        // TODO: get random pos working
        let D2Test5 testNum firstSample dispatch =
            runD2TestOnSheets
                "Comp overlap Test with Random Pos"
                firstSample
                (gridMaker (200,100))
                makeRandomFlipandPosCircuit
                Asserts.failOnComponentOverlap
                dispatch
            |> recordPositionInTest testNum dispatch
        
        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [
                "D2Test1: Wire crossings", D2Test1 
                "D2Test2: Wire straightened", D2Test2
                "D2Test3: Component overlap", D2Test3
                "D2Test4: Wire routing length", D2Test4 10.0
                "Next Test Error", fun _ _ _ -> printf "Next Error:" // Go to the nexterror in a test
                "DemoTest: sheetOrderFlip", fun _ _ _ -> printf "DemoTest: sheetOrderFlip"
                "D2Test0: Random Fail Test", D2Test0
            ]

        /// Display the next error in a previously started test
        let nextError (testName, testFunc) firstSampleToTest dispatch =
            let testNum =
                testsToRunFromSheetMenu
                |> List.tryFindIndex (fun (name,_) -> name = testName)
                |> Option.defaultValue 0
            testFunc testNum firstSampleToTest dispatch

        /// common function to execute any test.
        /// testIndex: index of test in testsToRunFromSheetMenu
        let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name,func = testsToRunFromSheetMenu[testIndex] 
            printf "%s" name
            match name, model.DrawBlockTestState with
            | "Next Test Error", Some state ->
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) dispatch
            | "Next Test Error", None ->
                printf "Test Finished"
                ()
            | "DemoTest: sheetOrderFlip", Some state ->
                // let printPipe (msg:string) x = printf $"{msg} {x}" ; x
                printf "Starting DemoTest: sheetOrderFlip"
                printf $"DemoTest: Wire Crossings before: {numOfWireRightAngleCrossings model.Sheet}"
                // printf $"Right Angles before: {numOfVisRightAngles model.Sheet}"
                printf $"DemoTest: # of straight lines before: {numOfStraightWires model.Sheet}"
                printf $"DemoTest: Length of Wire Routing before: {calcVisWireLength model.Sheet}"
                printf $"DemoTest: # of overlapping components before: {numOfIntersectedSymPairs model.Sheet}"

                printf "DemoTest: Implement sheetOrderFlip"
                let flippedSheet = SheetBeautify.sheetOrderFlip model.Sheet
                printf $"DemoTest: Wire Crossings after: {numOfWireRightAngleCrossings flippedSheet}"
                // printf $"Right Angles after: {numOfVisRightAngles flippedSheet}"
                printf $"DemoTest: # of straight lines after: {numOfStraightWires flippedSheet}"
                printf $"DemoTest: Length of Wire Routing after: {calcVisWireLength flippedSheet}"
                printf $"DemoTest: # of overlapping components after: {numOfIntersectedSymPairs flippedSheet}"
                showSheetInIssieSchematic (flippedSheet) dispatch
            | _ ->
                func testIndex 0 dispatch