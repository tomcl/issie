module TestDrawBlock

//--------------------------------------------------------------------------------------------------//
//---Note to assessor: Any related labels to the questions in Tick3 are marked with hashes #, ------//
//-------------------------- type in CTRL + F or CMD + F to find them.------------------------------//
//--------------------------------------------------------------------------------------------------//

open GenerateData
open Elmish

//-------------------------------------------------------------------------------------------//
//--------Types to represent tests with (possibly) random data, and results from tests-------//
//-------------------------------------------------------------------------------------------//
module TestLib =

    /// convenience unsafe function to extract Ok part of Result or fail if value is Error
    let getOkOrFail (res: Result<'a, string>) =
        match res with
        | Ok x -> x
        | Error mess -> failwithf "%s" mess

    type TestStatus =
        | Fail of string
        | Exception of string

    type Test<'a> =
        {
            Name: string
            Samples: Gen<'a>
            StartFrom: int
            /// The 1st argument is the test number: allows assertions that fail on a specific sample
            /// to display just one sample.
            /// The return value is None if test passes, or Some message if it fails.
            Assertion: int -> 'a -> string option
        }

    type TestResult<'a> =
        { TestName: string
          TestData: Gen<'a>
          FirstSampleTested: int
          TestErrors: (int * TestStatus) list }

    let catchException name func arg =
        try
            Ok(func arg)
        with e ->
            Error($"Exception when running {name}\n" + e.StackTrace)

    /// Run the Test samples from 0 up to test.Size - 1.
    /// The return list contains all failures or exceptions: empty list => everything has passed.
    /// This will always run to completion: use truncate if text.Samples.Size is too large.
    let runTests (test: Test<'a>) : TestResult<'a> =
        [ test.StartFrom .. test.Samples.Size - 1 ]
        |> List.map (fun n ->
            catchException $"generating test {n} from {test.Name}" test.Samples.Data n
            |> (fun res -> n, res))
        |> List.collect (function
            | n, Error mess -> [ n, Exception mess ]
            | n, Ok sample ->
                match catchException $"'test.Assertion' on test {n} from 'runTests'" (test.Assertion n) sample with
                | Ok None -> []
                | Ok(Some failure) -> [ n, Fail failure ]
                | Error(mess) -> [ n, Exception mess ])
        |> (fun resL ->
            { TestName = test.Name
              FirstSampleTested = test.StartFrom
              TestData = test.Samples
              TestErrors = resL })

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

    /// create an initial empty Sheet Model
    let initSheetModel = DiagramMainView.init().Sheet

    /// Optic to access SheetT.Model from Issie Model
    let sheetModel_ = sheet_

    /// Optic to access BusWireT.Model from SheetT.Model
    let busWireModel_ = SheetT.wire_

    /// Optic to access SymbolT.Model from SheetT.Model
    let symbolModel_ = SheetT.symbol_

    /// allowed max X or y coord of svg canvas
    let maxSheetCoord = Sheet.Constants.defaultCanvasSize
    let middleOfSheet = { X = maxSheetCoord / 2.; Y = maxSheetCoord / 2. }

    /// Used throughout to compare labels since these are case invariant "g1" = "G1"
    let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2

    /// Identify a port from its component label and number.
    /// Usually both an input and output port will match this, so
    /// the port is only unique if it is known to be input or output.
    /// used to specify the ends of wires, since these are known to be
    /// connected to outputs (source) or inputs (target).
    type SymbolPort = { Label: string; PortNumber: int }

    /// convenience function to make SymbolPorts
    let portOf (label: string) (number: int) = { Label = label; PortNumber = number }

    //-----------------------------------------------------------------------------------------------
    // visibleSegments is included here as ahelper for info, and because it is needed in project work
    //-----------------------------------------------------------------------------------------------

    /// The visible segments of a wire, as a list of vectors, from source end to target end.
    /// Note that in a wire with n segments a zero length (invisible) segment at any index [1..n-2] is allowed
    /// which if present causes the two segments on either side of it to coalesce into a single visible segment.
    /// A wire can have any number of visible segments - even 1.
    let visibleSegments (wId: ConnectionId) (model: SheetT.Model) : XYPos list =

        let wire = model.Wire.Wires[wId] // get wire from model

        /// helper to match even and odd integers in patterns (active pattern)
        let (|IsEven|IsOdd|) (n: int) =
            match n % 2 with
            | 0 -> IsEven
            | _ -> IsOdd

        /// Convert seg into its XY Vector (from start to end of segment).
        /// index must be the index of seg in its containing wire.
        let getSegmentVector (index: int) (seg: BusWireT.Segment) =
            // The implicit horizontal or vertical direction  of a segment is determined by
            // its index in the list of wire segments and the wire initial direction
            match index, wire.InitialOrientation with
            | IsEven, BusWireT.Vertical
            | IsOdd, BusWireT.Horizontal -> { X = 0.; Y = seg.Length }
            | IsEven, BusWireT.Horizontal
            | IsOdd, BusWireT.Vertical -> { X = seg.Length; Y = 0. }

        /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// if this is possible, otherwise return segVecs unchanged.
        /// Index must be in range 1..segVecs
        let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int) =
            if segVecs[index] =~ XYPos.zero then
                segVecs[0 .. index - 2]
                @ [ segVecs[index - 1] + segVecs[index + 1] ]
                @ segVecs[index + 2 .. segVecs.Length - 1]
            else
                segVecs

        wire.Segments
        |> List.mapi getSegmentVector
        |> (fun segVecs ->
            (segVecs, [ 1 .. segVecs.Length - 2 ])
            ||> List.fold tryCoalesceAboutIndex)

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
        let placeSymbol
            (symLabel: string)
            (compType: ComponentType)
            (position: XYPos)
            (model: SheetT.Model)
            : Result<SheetT.Model, string>
            =
            let symLabel = String.toUpper symLabel // make label into its standard casing
            let symModel, symId =
                SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
            let sym = symModel.Symbols[symId]
            match position + sym.getScaledDiagonal with
            | { X = x; Y = y } when x > maxSheetCoord || y > maxSheetCoord ->
                Error
                    $"symbol '{symLabel}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
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
            : Result<SheetT.Model, string>
            =
            let symbolMap = model.Wire.Symbol.Symbols
            if caseInvariantEqual ccSheetName project.OpenFileName then
                Error "Can't create custom component with name same as current opened sheet"
            elif
                not
                <| List.exists
                    (fun (ldc: LoadedComponent) -> caseInvariantEqual ldc.Name ccSheetName)
                    project.LoadedComponents
            then
                Error "Can't create custom component unless a sheet already exists with smae name as ccSheetName"
            elif
                symbolMap
                |> Map.exists (fun _ sym -> caseInvariantEqual sym.Component.Label symLabel)
            then
                Error "Can't create custom component with duplicate Label"
            else
                let canvas = model.GetCanvasState()
                let ccType: CustomComponentType =
                    { Name = ccSheetName
                      InputLabels = Extractor.getOrderedCompLabels (Input1(0, None)) canvas
                      OutputLabels = Extractor.getOrderedCompLabels (Output 0) canvas
                      Form = None
                      Description = None }
                placeSymbol symLabel (Custom ccType) position model

        //--------------------------------------------------------------------------------------------------//
        //-----------------------------# Question 10 Rotation and Flip Components---------------------------//
        //--------------------------------------------------------------------------------------------------//
        // open DrawModelType.SheetT
        //  I can't import everything, seems to throw other errors
        //  Type mismatch. Expecting a'Dispatch<Msg>'but given a 'Dispatch<SheetT.Msg>'

        // Lenses for fields of SheetT.Model
        // Note how this differs from DrawModelType.SheetT, as its wire_ is type Lens<Model, BusWireT.Model>
        // Here we have wire_ : Lens<SheetT.Model, BusWireT.Model>
        // This is because rotateSymbol and flipSymbol are specified to take in model: SheetT.Model

        let wire_: Lens<SheetT.Model, BusWireT.Model> =
            Lens.create (fun m -> m.Wire) (fun w m -> { m with Wire = w })
        let symbol_: Lens<BusWireT.Model, SymbolT.Model> =
            Lens.create (fun m -> m.Symbol) (fun w m -> { m with Symbol = w })
        let boundingBoxes_: Lens<SheetT.Model, Map<ComponentId, BoundingBox>> =
            Lens.create (fun m -> m.BoundingBoxes) (fun bb m -> { m with BoundingBoxes = bb })

        let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (SheetT.Model) =
            // we need to look up a component ID for symLabel.
            // the issue is, we need to check symbolMap, for caseInvariantEqual-ity with symLabel
            // I would've preferred rotateSymbol be changed to an option type, so it follows in nicely with
            // the Result.bind for placeSymbol. Unfortuantely, we must stick with the spec

            // get ahold of ComponentID
            let symbolMap = model.Wire.Symbol.Symbols
            let findComponentID map predicate =
                map
                |> Map.filter predicate
                |> Map.toSeq
                |> Seq.tryHead
                |> function
                    | Some(key, _) -> key
                    | None -> failwith "ComponentID not found for given symLabel"

            let componentID =
                findComponentID symbolMap (fun _ sym -> caseInvariantEqual sym.Component.Label symLabel)

            // let compList = (List.ofArray (Map.keys model.Wire.Symbol.Symbols))
            let compList = [ componentID ]

            let rotatedSymbol = RotateScale.rotateBlock compList model.Wire.Symbol rotate
            model
            |> Optic.set (wire_ >-> symbol_) rotatedSymbol
            |> Optic.set boundingBoxes_ (Symbol.getBoundingBoxes rotatedSymbol)

        let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : (SheetT.Model) =
            // same comments from rotateSymbol apply here

            let symbolMap = model.Wire.Symbol.Symbols
            // get ahold of ComponentID
            let findComponentID map predicate =
                map
                |> Map.filter predicate
                |> Map.toSeq
                |> Seq.tryHead
                |> function
                    | Some(key, _) -> key
                    | None -> failwith "Key not found"

            let componentID =
                findComponentID symbolMap (fun _ sym -> caseInvariantEqual sym.Component.Label symLabel)

            // let compList = (List.ofArray (Map.keys model.Wire.Symbol.Symbols))
            let compList = [ componentID ]
            let flippedSymbol = RotateScale.flipBlock compList model.Wire.Symbol flip
            model
            |> Optic.set (wire_ >-> symbol_) flippedSymbol
            |> Optic.set boundingBoxes_ (Symbol.getBoundingBoxes flippedSymbol)

        /// Add a (newly routed) wire, source specifies the Output port, target the Input port.
        /// Return an error if either of the two ports specified is invalid, or if the wire duplicates and existing one.
        /// The wire created will be smart routed but not separated from other wires: for a nice schematic
        /// separateAllWires should be run after  all wires are added.
        /// source, target: respectively the output port and input port to which the wire connects.
        let placeWire (source: SymbolPort) (target: SymbolPort) (model: SheetT.Model) : Result<SheetT.Model, string> =
            let symbols = model.Wire.Symbol.Symbols
            let getPortId (portType: PortType) symPort =
                mapValues symbols
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symPort.Label)
                |> function
                    | Some x -> Ok x
                    | None -> Error "Can't find symbol with label '{symPort.Label}'"
                |> Result.bind (fun sym ->
                    match portType with
                    | PortType.Input -> List.tryItem symPort.PortNumber sym.Component.InputPorts
                    | PortType.Output -> List.tryItem symPort.PortNumber sym.Component.OutputPorts
                    |> function
                        | Some port -> Ok port.Id
                        | None -> Error $"Can't find {portType} port {symPort.PortNumber} on component {symPort.Label}")

            match getPortId PortType.Input target, getPortId PortType.Output source with
            | Error e, _
            | _, Error e -> Error e
            | Ok inPort, Ok outPort ->
                let newWire =
                    BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
                if
                    model.Wire.Wires
                    |> Map.exists (fun wid wire ->
                        wire.InputPort = newWire.InputPort
                        && wire.OutputPort = newWire.OutputPort)
                then
                    // wire already exists
                    Error
                        "Can't create wire from {source} to {target} because a wire already exists between those ports"
                else
                    model
                    |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
                    |> Ok

        /// Run the global wire separation algorithm (should be after all wires have been placed and routed)
        let separateAllWires (model: SheetT.Model) : SheetT.Model =
            model
            |> Optic.map
                busWireModel_
                (BusWireSeparate.updateWireSegmentJumpsAndSeparations (model.Wire.Wires.Keys |> Seq.toList))

        /// Copy testModel into the main Issie Sheet making its contents visible
        let showSheetInIssieSchematic (testModel: SheetT.Model) (dispatch: Dispatch<Msg>) =
            let sheetDispatch sMsg = dispatch (Sheet sMsg)
            dispatch
            <| UpdateModel(Optic.set sheet_ testModel) // set the Sheet component of the Issie model to make a new schematic.
            sheetDispatch <| SheetT.KeyPress SheetT.CtrlW // Centre & scale the schematic to make all components viewable.

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
            (samples: Gen<'a>)
            (sheetMaker: 'a -> SheetT.Model)
            (sheetChecker: int -> SheetT.Model -> string option)
            (dispatch: Dispatch<Msg>)
            : TestResult<'a>
            =
            let generateAndCheckSheet n = sheetMaker >> sheetChecker n
            let result =
                { Name = name
                  Samples = samples
                  StartFrom = sampleToStartFrom
                  Assertion = generateAndCheckSheet }
                |> runTests
            match result.TestErrors with
            | [] -> // no errors
                printf $"Test {result.TestName} has PASSED."
            | (n, first) :: _ -> // display in Issie editor and print out first error
                printf
                    // # modified to output samples.Data n for Errors so we can find out a pattern when the symbols intersect for part 7i
                    $"Test {result.TestName} has FAILED on sample {n} = {samples.Data n} with error message:\n{first}"
                match catchException "" sheetMaker (samples.Data n) with
                | Ok sheet -> showSheetInIssieSchematic sheet dispatch
                | Error mess -> ()
            result
    //--------------------------------------------------------------------------------------------------//
    //----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
    //--------------------------------------------------------------------------------------------------//

    open Builder
    /// Sample data based on 11 equidistant points on a horizontal line
    let horizLinePositions =
        fromList [ -100..20..100 ]
        |> map (fun n -> middleOfSheet + { X = float n; Y = 0. })

    /// demo test circuit consisting of a DFF & And gate
    let makeTest1Circuit (andPos: XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And, 2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0))
        |> getOkOrFail

    type XYPosRFlip = { X: float; Y: float; Rotation: Rotation; Flip: SymbolT.FlipType }
    let makeTest2Circuit (andParameters: XYPosRFlip) = //(andPos: XYPos) (andRotate: Rotation) (andFlip : SymbolT.FlipType) =
        let andPos = { X = andParameters.X; Y = andParameters.Y }
        let andRotate = andParameters.Rotation
        let andFlip = andParameters.Flip

        initSheetModel
        |> placeSymbol "G1" (GateN(And, 2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0))
        |> getOkOrFail
        |> rotateSymbol "G1" andRotate
        |> flipSymbol "G1" andFlip
    // |> separateAllWires does not make a difference in testing

    let vertLinePositions: Gen<XYPos> =
        fromList [ -100..20..100 ]
        |> map (fun n -> middleOfSheet + { X = 0.; Y = float n })

    //--------------------------------------------------------------------------------------------------//
    //--- # 7 Varying both horizontal and vertical line positions, using GenerateData.product, ---------//
    //--- GenerateData.filter, GenereateData.map, and GenereateData.fromList to create a Gen<XYPos> ----//
    //--------------------------------------------------------------------------------------------------//

    let bounds = 120
    let step = 30
    let horizVertLinePositions: Gen<XYPos> =
        let horizLinePositionsSparse =
            fromList [ -bounds .. step .. bounds ]
            |> map (fun n -> { X = float n; Y = 0. })
        let vertLinePositionsSparse =
            fromList [ -bounds .. step .. bounds ]
            |> map (fun n -> { X = 0.; Y = float n })
        product (fun x y -> x + y) horizLinePositionsSparse vertLinePositionsSparse
        |> filter (fun pos -> not (abs pos.X <= 60 && abs pos.Y <= 60))
        // after measuring, keep the absolute distances less than 60 together
        |> map (fun pos -> middleOfSheet + pos)

    let hVLinePosFlipRotate: Gen<XYPosRFlip> =
        let rotateList = fromList [ Degree0; Degree90; Degree180; Degree270 ]
        let flipList = fromList [ SymbolT.FlipHorizontal; SymbolT.FlipVertical ]

        let horizLinePositionsSparse =
            fromList [ -bounds .. step .. bounds ]
            |> map (fun n -> { X = float n; Y = 0. })
        let vertLinePositionsSparse =
            fromList [ -bounds .. step .. bounds ]
            |> map (fun n -> { X = 0.; Y = float n })

        let hVData =
            product (fun x y -> x + y) horizLinePositionsSparse vertLinePositionsSparse
            |> filter (fun pos -> not (abs pos.X <= 60 && abs pos.Y <= 60))
            // after measuring, keep the absolute distances less than 60 together
            |> map (fun pos -> middleOfSheet + pos)

        map3
            (fun (pos: XYPos) flip rot -> { X = pos.X; Y = pos.Y; Rotation = rot; Flip = flip })
            hVData
            flipList
            rotateList

    //------------------------------------------------------------------------------------------------//
    //-------------------------Example assertions used to test sheets---------------------------------//
    //------------------------------------------------------------------------------------------------//

    module Asserts =

        (* Each assertion function from this module has as inputs the sample number of the current test and the corresponding schematic sheet.
           It returns a boolean indicating (true) that the test passes or 9false) that the test fails. The sample numbr is included to make it
           easy to document tests and so that any specific sampel schematic can easily be displayed using failOnSampleNumber. *)

        /// Ignore sheet and fail on the specified sample, useful for displaying a given sample
        let failOnSampleNumber (sampleToFail: int) (sample: int) _sheet =
            if sampleToFail = sample then
                Some $"Failing forced on Sample {sampleToFail}."
            else
                None

        /// Fails all tests: useful to show in sequence all the sheets generated in a test
        let failOnAllTests (sample: int) _ = Some <| $"Sample {sample}"

        /// Fail when sheet contains a wire segment that overlaps (or goes too close to) a symbol outline
        let failOnWireIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
            let wireModel = sheet.Wire
            wireModel.Wires
            |> Map.exists (fun _ wire ->
                BusWireRoute.findWireSymbolIntersections wireModel wire
                <> [])
            |> (function
            | true -> Some $"Wire intersects a symbol outline in Sample {sample}"
            | false -> None)

        /// Fail when sheet contains two symbols which overlap
        let failOnSymbolIntersectsSymbol (sample: int) (sheet: SheetT.Model) =
            let wireModel = sheet.Wire
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n, box)
            List.allPairs boxes boxes
            |> List.exists (fun ((n1, box1), (n2, box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
            |> (function
            | true -> Some $"Symbol outline intersects another symbol outline in Sample {sample} under "
            | false -> None)

    //---------------------------------------------------------------------------------------//
    //-----------------------------Demo tests on Draw Block code-----------------------------//
    //---------------------------------------------------------------------------------------//

    module Tests =

        /// Allow test errors to be viewed in sequence by recording the current error
        /// in the Issie Model (field DrawblockTestState). This contains all Issie persistent state.
        let recordPositionInTest (testNumber: int) (dispatch: Dispatch<Msg>) (result: TestResult<'a>) =
            dispatch
            <| UpdateDrawBlockTestState(fun _ ->
                match result.TestErrors with
                | [] ->
                    printf "Test finished"
                    None
                | (numb, _) :: _ ->
                    printf $"Sample {numb}"
                    Some { LastTestNumber = testNumber; LastTestSampleIndex = numb })

        /// Example test: Horizontally positioned AND + DFF: fail on sample 0
        let test1 testNum firstSample dispatch =
            printf "Test1"
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail on sample 0"
                firstSample
                horizLinePositions
                makeTest1Circuit
                (Asserts.failOnSampleNumber 0)
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

        //--------------------------------------------------------------------------------------------------//
        //----------------------- # A selection of tests for horizVertLinePositions ------------------------//
        //--------------------------------------------------------------------------------------------------//

        // test5 should pass all!
        let test5 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally and Vertically positioned AND + DFF: fail on symbol intersects symbol"
                firstSample
                horizVertLinePositions
                makeTest1Circuit
                Asserts.failOnSymbolIntersectsSymbol
                dispatch
            |> recordPositionInTest testNum dispatch

        //  test6 is what we use for tick 3 part 7
        let test6 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally and Vertically positioned AND + DFF: fail on symbol intersects wire"
                firstSample
                horizVertLinePositions
                makeTest1Circuit
                Asserts.failOnWireIntersectsSymbol
                dispatch
            |> recordPositionInTest testNum dispatch

        //  test7 fails on all for us to check
        let test7 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally and Vertically positioned AND + DFF: fail on all"
                firstSample
                horizVertLinePositions
                makeTest1Circuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        let test8 testNum firstSample dispatch =

            runTestOnSheets
                "Horizontally and Vertically positioned, rotated and flipped AND + DFF: fail on wire intersects symbol"
                firstSample
                hVLinePosFlipRotate
                makeTest2Circuit
                Asserts.failOnWireIntersectsSymbol
                dispatch
            |> recordPositionInTest testNum dispatch

        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testsToRunFromSheetMenu: (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [ "Test1", test1 // example
              "Test2", test2 // example
              "Test3", test3 // example
              "Test4", test4
              "Test5", test5
              "Test6", test6
              "Test7", test7
              "Test8", test8
              //   fun _ _ _ -> printf "Test8" // example
              "Next Test Error",
              fun _ _ _ -> printf "Next Error:" // Go to the nexterror in a test

              ]

        /// Display the next error in a previously started test
        let nextError (testName, testFunc) firstSampleToTest dispatch =
            let testNum =
                testsToRunFromSheetMenu
                |> List.tryFindIndex (fun (name, _) -> name = testName)
                |> Option.defaultValue 0
            testFunc testNum firstSampleToTest dispatch

        /// common function to execute any test.
        /// testIndex: index of test in testsToRunFromSheetMenu
        let testMenuFunc (testIndex: int) (dispatch: Dispatch<Msg>) (model: Model) =
            let name, func = testsToRunFromSheetMenu[testIndex]
            printf "%s" name
            match name, model.DrawBlockTestState with
            | "Next Test Error", Some state ->
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex + 1) dispatch
            | "Next Test Error", None ->
                printf "Test Finished"
                ()
            | _ -> func testIndex 0 dispatch

// Written Questions

//--------------------------------------------------------------------------------------------------//
//------------------------------------------# Question 6--------------------------------------------//
//--------------------------------------------------------------------------------------------------//

// What is the sample data?
// The sample data is a set of 11 equidistant points on a horizontal line, in bounds -100 to 100 spaced apart by 20.
// This is represented by the variable horizLinePositions.

// How is that converted into an Issie design sheet (e.g. what change to the sheet components does the sample data parametrise)?
// The sample data is converted into an Issie design sheet by placing a new symbol with label "G1" (And gate) and "FF1" (DFF) onto the Sheet with given position.

// What is the assertion used in the test (1-3)?
// Tests1-2 fail on a given sample – this is just for a check
// Test3 fails on symbols intersect assertion

//--------------------------------------------------------------------------------------------------//
//------------------------------------------# Question 8--------------------------------------------//
//--------------------------------------------------------------------------------------------------//

// Trace how the start of Test 1 is executed in Issie from the menu command in Renderer.filemenu by listing in order
// every function that is executed between a key being pressed and the TestDrawBlock.HlpTick3.Tests.test1 function
// starting. Considering the Fsharp source file compile order, and the fact that cross-module forward references are not
//  allowed, explain the use of functions as data in this code.

// We start from teststoRunFromSheetMenu, which is a list of tests available which can be run from the Issie File Menu.
// This is called in Renderer.filemenu:

// makeDebugItem name (Some $"CmdOrCtrl+{accelNumber + 1}") (fun _ ->
//       dispatch (MenuAction(MenuDrawBlockTest(TestDrawBlock.HLPTick3.Tests.testMenuFunc, accelNumber), dispatch)))
//
// ...
// makeMenuGen (debugLevel > 0) false "Tick3 Tests" (
//             TestDrawBlock.HLPTick3.Tests.testsToRunFromSheetMenu // make a submenu from this list
//             |> List.truncate 10 // allow max 10 items accelerated by keys Ctrl-0 .. Ctrl-9.
//                                 // Remove accelerator if keys are needed for other purposes
//             |> List.mapi (fun n (name,_) -> (makeTestItem name n)))
// ...
// let makeMenuGen (visible: bool) (topLevel: bool) (name : string) (table : MenuItemConstructorOptions list) =
//    let subMenu = createEmpty<MenuItemConstructorOptions>
//    subMenu.``type`` <- Some (if topLevel then MenuItemType.Normal else MenuItemType.Submenu)
//    subMenu.label <-Some name
//    subMenu.submenu <- Some (U2.Case1 (table |> ResizeArray))
//    subMenu.visible <-  Some visible
//    subMenu
// ...
// let makeDebugItem label accelerator option =
//   makeCondItem (JSHelpers.debugLevel <> 0) label accelerator option
// ...
// let makeCondItem cond label accelerator action =
//     let item = makeItem label accelerator action
//     item.visible <- Some cond
//     item

//  makeCondItem makes a conditional menu item from a condition, name, opt key to trigger, and action
//  makeDebugItem: A menu item which is visible only if in debug mode (run dev or command line -D on binaries) and on windows. Also handles the accelerator keypress
//  makeMenuGen: Make a new menu from a list of menu items (javascript)

//--------------------------------------------------------------------------------------------------//
//------------------------------------------# Question 11-------------------------------------------//
//--------------------------------------------------------------------------------------------------//

// It is interesting that some blocks, once rotated or flipped, will fail to auto-route their wires.
// This can be done by running Test 8 from the Issie File Menu. This test will fail on wire intersects symbol.
// Right-clicking and choosing "Reroute All Wires" does not change the wire routing – which means in the last line of
// smartAutoRoute, we have gone with the default vcalue snappedToNetWire

// let smartAutoroute (model: Model) (wire: Wire) : Wire =

//     let initialWire = (autoroute model wire)

//     // Snapping to Net only if model.SnapToNet toggled to be true
//     let snappedToNetWire =
//         match model.SnapToNet with
//         | _ -> initialWire // do not snap
//     //| true -> snapToNet model initialWire

//     let intersectedBoxes = findWireSymbolIntersections model snappedToNetWire

//     match intersectedBoxes.Length with
//     | 0 -> snappedToNetWire
//     | _ ->
//         tryShiftVerticalSeg model intersectedBoxes snappedToNetWire
//         |> Option.orElse (tryShiftHorizontalSeg maxCallsToShiftHorizontalSeg model intersectedBoxes snappedToNetWire)
//         |> Option.defaultValue snappedToNetWire

// However, immediately after the rotation or flip, if the user nudges a block a tiny bit away and then back to its original position,
// The wires immediately snaps back correctly

// Naive idea 1: Could this be automated? Could we dispatch a message to nudge the selected/last rotated block
// a tiny bit away and then back to its original position?

// Naive idea 2: Do we even need to physically move the block? We could perform calculations as normal as if the block was moved,
// and then snap the wires back to their correct positions.

// Naive idea 3: I don't have time, perhaps there is a way to reset all the segments of a wire to an orignal 2-segment
// position, one for horizontal travel and one for vertical travel. Then reroute again
