module TestDrawBlock
open GenerateData
open Elmish


//-------------------------------------------------------------------------------------------//
//--------Types to represent tests with (possibly) random data, and results from tests-------//
//-------------------------------------------------------------------------------------------//
module TestLib =

    /// convenience unsafe function to extract Ok part of Result or fail if value is Error
    let getOkOrFail (res: Result<'a,string>) =
        match res with
        | Ok x -> x
        | Error mess ->
            failwithf "%s" mess

    type TestStatus =
        | Fail of string
        | Exception of string

    type Evaluator<'a> = {
        EvalFunc: 'a -> float
        Penalty: float
    }

    type Test<'a> = {
        Name: string
        Samples: Gen<'a>
        StartFrom: int
        /// The 1st argument is the test number: allows assertions that fail on a specific sample
        /// to display just one sample.
        /// The return value is None if test passes, or Some message if it fails.
        Assertion: int -> 'a -> string option
        Evaluation: Evaluator<'a>
    }

    type TestResult<'a> = {
        TestName: string
        TestData: Gen<'a>
        FirstSampleTested: int
        firstTestError: option<int * TestStatus>
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
    let runTestsTillFail (test: Test<'a>) : TestResult<'a> =
        printfn $"[{test.StartFrom}/{test.Samples.Size}]"
        let getTestFail (n, res) : option<int * TestStatus> =
            match n, res with
                | n, Error mess -> Some (n, Exception mess)
                | n, Ok sample ->
                    match catchException $"'test.Assertion' on test {n} from 'runTestsTillFail'" (test.Assertion n) sample with
                    | Ok None -> None
                    | Ok (Some failure) -> Some (n,Fail failure)
                    | Error (mess) -> Some (n,Exception mess)

        [test.StartFrom..test.Samples.Size - 1]
        |> List.map (fun n ->
                catchException $"generating test {n} from {test.Name}" test.Samples.Data n
                |> (fun res -> n,res)
           )
        |> List.tryFind (Option.isSome << getTestFail)
        |> (function 
                | None -> None
                | Some (n,res) -> getTestFail (n,res)) // re-executes test if fail to get message
        |> (fun resL ->
                {
                    TestName = test.Name
                    FirstSampleTested = test.StartFrom
                    TestData = test.Samples
                    firstTestError = resL
                })

    /// Run the Test samples from 0 up to test.Size - 1.
    /// The return list contains all failures or exceptions: empty list => everything has passed.
    /// This will always run to completion: use truncate if text.Samples.Size is too large.
    let calcEval (test: Test<'a>) : float =
        [1..test.Samples.Size - 1]
        |> List.map (fun n ->
                catchException $"generating test {n} from {test.Name}" test.Samples.Data n
                |> (fun res -> n,res)
           )           
        |> List.collect (function
                            | _, Error _ -> [test.Evaluation.Penalty]
                            | n, Ok sample ->
                                match catchException $"'test.Assertion' on test {n} from 'runTests'" (test.Assertion n) sample with
                                | Ok None -> [(test.Evaluation.EvalFunc) sample]
                                | Ok (Some failure) -> [test.Evaluation.Penalty]
                                | Error (mess) -> [test.Evaluation.Penalty])
        // |> (fun x ->
        //             printfn "%A" x
        //             x)
        |> List.average
            
(******************************************************************************************
   This submodule contains a set of functions that enable random data generation
   for property-based testing of Draw Block wire routing functions.
   basic idea.
   1. Generate, in various ways, random circuit layouts
   2. For each layout apply smartautoroute to regenerate all wires
   3. Apply check functions to see if the resulting wire routing obeys "good layout" rules.
   4. Output any layouts with anomalous wire routing
*******************************************************************************************)
module HLPTick3 =
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
    open TestLib
    open PopupHelpers

    /// create an initial empty Model 
    let initModel = DiagramMainView.init()

    /// create an initial empty Sheet Model
    let initSheetModel = initModel.Sheet

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
 

    /// Identify a port from its component label and number.
    /// Usually both an input and output port will mathc this, so
    /// the port is only unique if it is known to be input or output.
    /// used to specify the ends of wires, since tehee are known to be
    /// connected to outputs (source) or inputs (target).
    type SymbolPort = { Label: string; PortNumber: int }

    /// convenience function to make SymbolPorts
    let portOf (label:string) (number: int) =
        {Label=label; PortNumber = number}


    //-----------------------------------------------------------------------------------------------
    // visibleSegments is included here as ahelper for info, and because it is needed in project work
    //-----------------------------------------------------------------------------------------------

    /// The visible segments of a wire, as a list of vectors, from source end to target end.
    /// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed 
    /// which if present causes the two segments on either side of it to coalesce into a single visible segment.
    /// A wire can have any number of visible segments - even 1.
    let visibleSegments (wId: ConnectionId) (model: SheetT.Model): XYPos list =

        let wire = model.Wire.Wires[wId] // get wire from model

        /// helper to match even and off integers in patterns (active pattern)
        let (|IsEven|IsOdd|) (n: int) = match n % 2 with | 0 -> IsEven | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
        let getSegmentVector (index:int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction  of a segment is determined by 
            // its index in the list of wire segments and the wire initial direction
            match index, wire.InitialOrientation with
            | IsEven, BusWireT.Vertical | IsOdd, BusWireT.Horizontal -> {X=0.; Y=seg.Length}
            | IsEven, BusWireT.Horizontal | IsOdd, BusWireT.Vertical -> {X=seg.Length; Y=0.}

        /// Return the list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// wherever this is possible
        let rec coalesce (segVecs: XYPos list)  =
            match List.tryFindIndex (fun segVec -> segVec =~ XYPos.zero) segVecs[1..segVecs.Length-2] with          
            | Some zeroVecIndex -> 
                let index = zeroVecIndex + 1 // base index onto full segVecs
                segVecs[0..index-2] @
                [segVecs[index-1] + segVecs[index+1]] @
                segVecs[index+2..segVecs.Length - 1]
                |> coalesce // recurse as long as more coalescing might be possible
            | None -> segVecs // end when there are no inner zero-length segment vectors
     
        wire.Segments
        |> List.mapi getSegmentVector
        |> coalesce


//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
    module Builder =


                

            



        /// Place a new symbol with label symLabel onto the Sheet with given position.
        /// Return error if symLabel is not unique on sheet, or if position is outside allowed sheet coordinates (0 - maxSheetCoord).
        /// To be safe place components close to (maxSheetCoord/2.0, maxSheetCoord/2.0).
        /// symLabel - the component label, will be uppercased to make a standard label name
        /// compType - the type of the component
        /// position - the top-left corner of the symbol outline.
        /// model - the Sheet model into which the new symbol is added.
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
        


    
        /// Place a new symbol onto the Sheet with given position and scaling (use default scale if this is not specified).
        /// The ports on the new symbol will be determined by the input and output components on some existing sheet in project.
        /// Return error if symLabel is not unique on sheet, or ccSheetName is not the name of some other sheet in project.
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
            
        

        /// Finds the componentID of a symbol with symLabel
        /// returns symbolMap and componentID
        let getSymMapCompId symLabel (model:SheetT.Model) =
            let symbolMap = model.Wire.Symbol.Symbols
            let componentID = Map.findKey (fun _ (sym: SymbolT.Symbol) -> sym.Component.Label = symLabel) symbolMap
            (symbolMap, componentID)

        /// updates model with symbol and reroutes wires
        let updatedModel componentID symModel model =
            let newModel =
                model
                |> Optic.set (SheetT.symbolOf_ componentID) symModel
                |> SheetUpdateHelpers.updateBoundingBoxes
            let reroutedWires = BusWireSeparate.routeAndSeparateSymbolWires newModel.Wire componentID
            {newModel with Wire = reroutedWires}

        /// Rotate a symbol given its label
        let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (SheetT.Model) =
            let symbolMap, componentID = getSymMapCompId symLabel model
            let symModel = SymbolResizeHelpers.rotateAntiClockByAng rotate symbolMap[componentID]
            updatedModel componentID symModel model

        /// Flip a symbol given its label
        let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : (SheetT.Model) =
            let symbolMap, componentID = getSymMapCompId symLabel model
            let symModel = SymbolResizeHelpers.flipSymbol flip symbolMap[componentID]
            updatedModel componentID symModel model

        /// Get PortId from symPort
        let getPortId (symbols: Map<ComponentId, SymbolT.Symbol>) (portType:PortType) symPort  =
                mapValues symbols
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symPort.Label)
                |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"
                |> Result.bind (fun sym ->
                    match portType with
                    | PortType.Input -> List.tryItem symPort.PortNumber sym.Component.InputPorts
                    | PortType.Output -> List.tryItem symPort.PortNumber sym.Component.OutputPorts
                    |> function | Some port -> Ok port.Id
                                | None -> Error $"Can't find {portType} port {symPort.PortNumber} on component {symPort.Label}")
            
        /// Add a (newly routed) wire, source specifies the Output port, target the Input port.
        /// Return an error if either of the two ports specified is invalid, or if the wire duplicates and existing one.
        /// The wire created will be smart routed but not separated from other wires: for a nice schematic
        /// separateAllWires should be run after  all wires are added.
        /// source, target: respectively the output port and input port to which the wire connects.
        let placeWire (source: SymbolPort) (target: SymbolPort) (model: SheetT.Model) : Result<SheetT.Model,string> =
            let symbols = model.Wire.Symbol.Symbols
            let getPortIdInSheet = getPortId symbols

            match getPortIdInSheet PortType.Input target, getPortIdInSheet PortType.Output source with
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

        /// Popup shown after test evaluation
        let evaluationPopup (model:Model) dispatch =
            // let title = sprintf "SCORE" 
            // let beforeInt =
            //     fun _ -> str "How wide should the register be (in bits)?"
            // let intDefault = model.LastUsedDialogWidth
            // let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
            // let buttonText = "Add"
            // let buttonAction = dispatch ClosePopup
            // let isDisabled =
            //     fun (model': Model) -> getInt model'.PopupDialogData < 1
            // dialogPopup title body buttonText buttonAction isDisabled [] dispatch
            confirmationPopup

        /// 1. Create a set of circuits from Gen<'a> samples by applying sheetMaker to each sample.
        /// 2. Check each ciruit with sheetChecker.
        /// 3. Return a TestResult record with errors those samples for which sheetChecker returns false,
        /// or where there is an exception.
        /// If there are any test errors display the first in Issie, and its error message on the console.
        /// sheetMaker: generates a SheetT.model from the random sample
        /// sheetChecker n model: n is sample number, model is the genrated model. Return false if test fails.
        let runTestOnSheets
            (name: string)
            (sampleToStartFrom: int)
            (samples : Gen<'a>)
            (sheetMaker: 'a -> SheetT.Model)
            (sheetChecker: int -> SheetT.Model -> string option)
            (sheetScorer: Evaluator<SheetT.Model>)
            (dispatch: Dispatch<Msg>)
                : TestResult<'a> =
            let generateAndCheckSheet n = sheetMaker >> sheetChecker n
            let generateAndScoreSheet = sheetMaker >> sheetScorer.EvalFunc

            let test =
                {
                    Name=name;
                    Samples=samples;
                    StartFrom = sampleToStartFrom
                    Assertion = generateAndCheckSheet
                    Evaluation = {
                        EvalFunc = generateAndScoreSheet
                        Penalty = sheetScorer.Penalty
                    }
                }
            let result = runTestsTillFail test

            match result.firstTestError with
            | None -> // no errors
                printf $"Test {result.TestName} has PASSED."
                printf "----------"
                printf $" SCORE: {calcEval test}"
                printf "----------"
                // evaluationPopup
            | Some (n,first) -> // display in Issie editor and print out first error
                printf $"Test {result.TestName} has FAILED on sample {n} with error message:\n{first}"
                match catchException "" sheetMaker (samples.Data n) with
                | Ok sheet -> 
                    showSheetInIssieSchematic sheet dispatch
                | Error mess -> ()
            result
//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//

    open Builder
    /// Sample data based on 11 equidistant points on a horizontal line
    let horizLinePositions =
        fromList [-100..20..100]
        |> map (fun n -> middleOfSheet + {X=float n; Y=0.})

    /// demo test circuit consisting of a DFF & And gate
    let makeTest1Circuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> getOkOrFail



//------------------------------------------------------------------------------------------------//
//-------------------------Example assertions used to test sheets---------------------------------//
//------------------------------------------------------------------------------------------------//


    module Asserts =

        (* Each assertion function from this module has as inputs the sample number of the current test and the corresponding schematic sheet.
           It returns a boolean indicating (true) that the test passes or 9false) that the test fails. The sample numbr is included to make it
           easy to document tests and so that any specific sampel schematic can easily be displayed using failOnSampleNumber. *)

        /// Ignore sheet and fail on the specified sample, useful for displaying a given sample
        let failOnSampleNumber (sampleToFail :int) (sample: int) _sheet =
            if sampleToFail = sample then
                Some $"Failing forced on Sample {sampleToFail}."
            else
                None

        /// Fails all tests: useful to show in sequence all the sheets generated in a test
        let failOnAllTests (sample: int) _ =
            Some <| $"Sample {sample}"

        /// Fails no tests: useful for evaluation when only need to compare to pre-changes
        let failOnNoTests (_: int) _ =
            None

        /// Fail when sheet contains a wire segment that overlaps (or goes too close to) a symbol outline  
        let failOnWireIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
            let wireModel = sheet.Wire
            wireModel.Wires
            |> Map.exists (fun _ wire -> BusWireRoute.findWireSymbolIntersections wireModel wire <> [])
            |> (function | true -> Some $"Wire intersects a symbol outline in Sample {sample}"
                         | false -> None)

        /// Fail when sheet contains two symbols which overlap
        let failOnSymbolIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
            let wireModel = sheet.Wire
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n,box)
            List.allPairs boxes boxes 
            |> List.exists (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
            |> (function | true -> Some $"Symbol outline intersects another symbol outline in Sample {sample}"
                         | false -> None)

//---------------------------------------------------------------------------------------//
//-----------------------------Evaluation------------------------------------------------//
//---------------------------------------------------------------------------------------//
// Evaluation of the circuit will be calulated after the sheetChecker is run successfully. 
// Each evaluation metric returns score between [0, 1].
// The larger the score, the more 'beautiful' the beautified sheet is 
// relative to ideal beautification.

    module Evaluations =
        open DrawModelType

        /// Evaluates sheet to 0
        let nullEvaluator : Evaluator<SheetT.Model> =
            {
                EvalFunc = (fun _ -> 1.)
                Penalty = 0
            }

//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

    module Tests =

        /// Allow test errors to be viewed in sequence by recording the current error
        /// in the Issie Model (field DrawblockTestState). This contains all Issie persistent state.
        let recordPositionInTest (testNumber: int) (dispatch: Dispatch<Msg>) (result: TestResult<'a>) =
            dispatch <| UpdateDrawBlockTestState(fun _ ->
                match result.firstTestError with
                | None ->
                    printf "Test finished"
                    None
                | Some (numb, _) ->
                    printf $"Sample {numb}"
                    Some { LastTestNumber=testNumber; LastTestSampleIndex= numb})
            
        /// Example test: Horizontally positioned AND + DFF: fail on sample 0
        let test1 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail on sample 0"
                firstSample
                horizLinePositions
                makeTest1Circuit
                (Asserts.failOnSampleNumber 0)
                Evaluations.nullEvaluator
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail on sample 10
        let test2 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail on sample 10"
                firstSample
                horizLinePositions
                makeTest1Circuit
                (Asserts.failOnSampleNumber 10)
                Evaluations.nullEvaluator
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail on symbols intersect
        let test3 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail on symbols intersect"
                firstSample
                horizLinePositions
                makeTest1Circuit
                Asserts.failOnSymbolIntersectsSymbol
                Evaluations.nullEvaluator
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
                Evaluations.nullEvaluator
                dispatch
            |> recordPositionInTest testNum dispatch

        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [
                "Test1", test1 // example
                "Test2", test2 // example
                "Test3", test3 // example
                "Test4", test4 
                "Test5", fun _ _ _ -> printf "Test5" // dummy test - delete line or replace by real test as needed
                "Test6", fun _ _ _ -> printf "Test6"
                "Test7", fun _ _ _ -> printf "Test7"
                "Test8", fun _ _ _ -> printf "Test8"
                "Next Test Error", fun _ _ _ -> printf "Next Error:" // Go to the nexterror in a test

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
            | _ ->
                func testIndex 0 dispatch
        


    


