module TestDrawBlockD3
open GenerateData
open Elmish
open EEExtensions
open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open CommonTypes
open ModelType
open DrawModelType
open Sheet.SheetInterface
open GenerateData
open SheetBeautifyD3



module D3TestLib =

    /// convenience unsafe function to extract Ok part of Result or fail if value is Error
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
        TestData: Gen<'a>
        FirstSampleTested: int
        TestErrors: (int * TestStatus) list
    }

    let catchException name func arg =
        try
            Ok (func arg)
        with
            | e ->
                Error ($"Exception when running {name}\n" + e.StackTrace)
            
    /// Run the Test samples from 0 up to test.Size - 1.
    /// The return list contains all failures or exceptions: empty list => everything has passed.
    /// This will always run to completion: use truncate if text.Samples.Size is too large.
    let runTests (test: Test<'a>) : TestResult<'a>  =
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


/// create an initial empty Sheet Model 
let newSheetModel = DiagramMainView.init().Sheet

/// Optic to access SheetT.Model from Issie Model
let sheetModel_ = sheet_

/// Optic to access BusWireT.Model from SheetT.Model
let busWireModel_ = SheetT.wire_

/// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_ = SheetT.symbol_

/// allowed max X or y coord of svg canvas
let maxSheetCoord = Sheet.Constants.defaultCanvasSize
let middleOfSheet = {X=maxSheetCoord/2.;Y=maxSheetCoord/2.}

/// Used throughout to compare labels since these are case invariant "g1" = "G1"
let caseInvariantEqual str1 str2 =
    String.toUpper str1 = String.toUpper str2

type SymbolPort = { Label: string; PortNumber: int }

/// convenience function to make SymbolPorts
let portOf (label:string) (number: int) =
    {Label=label; PortNumber = number}




module D3Builder =
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

    open D3TestLib
    let randomPos lst = 
        let rndX = System.Random().Next(-20, 20)*10
        let rndY = System.Random().Next(-10, 10)*10
        let pos = middleOfSheet + {X = rndX; Y = rndY}
        pos

    let numOfIntersectedSymPairs (sheet: SheetT.Model) =
        let boxes = Map.toList sheet.BoundingBoxes
        List.allPairs boxes boxes
        |> List.sumBy (function | ((id1, _),(id2, _)) when id1 <= id2 -> 0
                                | ((_, box1),(_, box2)) when BlockHelpers.overlap2DBox box1 box2 -> 1
                                | _ -> 0)

    let samplePositions coord =
        let x,y = coord
        let height = fromList [-y..30..y]
        let width = fromList [-x..30..x]

        product (fun x y -> x,y) width height
        |> map (fun (x,y)->middleOfSheet + {X=float x; Y=float y})

    let makeTest1Circuit (sample)  =
        let andPos = List.head sample    
        let sample = List.tail sample
        let mux1Pos = List.head sample
        let sample = List.tail sample
        let mux2Pos = List.head sample
        let sample = List.tail sample
        let input1Pos = List.head sample
        let sample = List.tail sample
        let input2Pos = List.head sample
        newSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "MUX1" Mux2 mux1Pos)
        |> Result.bind (placeSymbol "MUX2" Mux2 mux2Pos)
        |> Result.bind (placeSymbol "S1" (Input1(1, None)) input1Pos)
        |> Result.bind (placeSymbol "S2" (Input1(1, None)) input2Pos)
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2)) // how is port number determined
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 0))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 1))
        |> getOkOrFail


    // add an element to a list
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
        // but I dont want Gen<XYPos>, I want to pass a record into model. Or a list...
        // within the circuit, can use randomPos.toArray and use index locations
        samples 20
        |> filter (fun pos ->
            let sheet = makeTest1Circuit pos
            let numofOverlaps =  makeTest1Circuit pos
                                 |> numOfIntersectedSymPairs
            numofOverlaps > 0)
                          



    /// Copy testModel into the main Issie Sheet making its contents visible
    let showSheetInIssieSchematic (testModel: SheetT.Model) (dispatch: Dispatch<Msg>) =
        let sheetDispatch sMsg = dispatch (Sheet sMsg)
        dispatch <| UpdateModel (Optic.set sheet_ testModel) // set the Sheet component of the Issie model to make a new schematic.
        sheetDispatch <| SheetT.KeyPress SheetT.CtrlW // Centre & scale the schematic to make all components viewable.


    let runTestOnSheets
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
            |> runTests
        match result.TestErrors with
        | [] -> // no errors
            printf $"Test {result.TestName} has PASSED."
        | (n,first):: _ -> // display in Issie editor and print out first error
            printf $"Test {result.TestName} has FAILED on sample {n} with error message:\n{first}"
            match catchException "" sheetMaker (samples.Data n) with
            | Ok sheet -> showSheetInIssieSchematic sheet dispatch
            | Error mess -> ()
        result


module D3Asserts =

    open SheetBeautifyHelpers
    open SheetBeautify

    /// Fails all tests: useful to show in sequence all the sheets generated in a test
    let failOnAllTests (sample: int) (sheet: SheetT.Model) =
        let numOfComponentOverlap = numOfIntersectedSymPairs sheet
        let numOfWireBends = numOfVisRightAngles sheet
        let numOfComponentOverlapAfter = sheet |> SheetBeautify.D4Build.sheetWireLabelSymbol |> numOfIntersectedSymPairs
        let numOfWireBendsAfter = sheet |> SheetBeautify.D4Build.sheetWireLabelSymbol |> numOfVisRightAngles 
        Some $"overlaps before: {numOfComponentOverlap} overlaps after: {numOfComponentOverlapAfter} wire bends before: {numOfWireBends} wire bends after: {numOfWireBendsAfter}"


    /// Fail when sheet contains two symbols which overlap
    let failOnSymbolIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
            let numOfComponentOverlap = numOfIntersectedSymPairs sheet
            match numOfComponentOverlap with
            | _ when numOfComponentOverlap > 0 ->
                Some $"{numOfComponentOverlap} {sample}"
            | _ -> None

module D3Tests =
    open D3TestLib
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

    open D3Builder
    let arbitrarysamples = fromList [0..1..50]
    let test1 testNum firstSample dispatch =
        runTestOnSheets
            "Attempt"
            firstSample
            (gridMaker (200, 100))
            makeTest1Circuit
            (D3Asserts.failOnAllTests)
            dispatch
        |> recordPositionInTest testNum dispatch

