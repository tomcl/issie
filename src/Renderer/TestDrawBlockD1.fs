module TestDrawBlockD1

open GenerateData
open Elmish

(*
Purpose of the TestDrawBlockD1:
More focused on programmatically generating circuits, running a function on the sheet,
followed by basic tests e.g. wire-wire, symbol-symbol, wire-symbol intersection tests.
Mostly for wire routing tests (from Tick3) and to be used for a majority of D1T.

TestSheetFunctions on the other hand is more focused on testing heuristics, helper functions
or sub-functions that might not take in the whole sheet, but symbols or wires instead.
TestSheetFunctions is more for testing RotateScale, D1B functions, BeautifySheetHelpers
*)

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
    open SheetUpdateHelpers
    open SheetBeautifyHelpers

    /// create an initial empty Sheet Model
    let initSheetModel = DiagramMainView.init().Sheet

    /// Optic to access SheetT.Model from Issie Model. Also used by TestSheetFunctions
    let sheetModel_ = sheet_

    /// Optic to access UndoList from SheetT.Model
    let UndoList_: Lens<SheetT.Model, SheetT.Model List> =
        Lens.create (fun m -> m.UndoList) (fun w m -> { m with UndoList = w })

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

        let wire_: Lens<SheetT.Model, BusWireT.Model> =
            Lens.create (fun m -> m.Wire) (fun w m -> { m with Wire = w })
        let symbol_: Lens<BusWireT.Model, SymbolT.Model> =
            Lens.create (fun m -> m.Symbol) (fun w m -> { m with Symbol = w })
        let boundingBoxes_: Lens<SheetT.Model, Map<ComponentId, BoundingBox>> =
            Lens.create (fun m -> m.BoundingBoxes) (fun bb m -> { m with BoundingBoxes = bb })

        // let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (SheetT.Model) =
        //     // we need to look up a component ID for symLabel.
        //     // the issue is, we need to check symbolMap, for caseInvariantEqual-ity with symLabel
        //     // I would've preferred rotateSymbol be changed to an option type, so it follows in nicely with
        //     // the Result.bind for placeSymbol. Unfortuantely, we must stick with the spec

        //     // get ahold of ComponentID
        //     let symbolMap = model.Wire.Symbol.Symbols
        //     let findComponentID map predicate =
        //         map
        //         |> Map.filter predicate
        //         |> Map.toSeq
        //         |> Seq.tryHead
        //         |> function
        //             | Some(key, _) -> key
        //             | None -> failwith "ComponentID not found for given symLabel"

        //     let componentID =
        //         findComponentID symbolMap (fun _ sym -> caseInvariantEqual sym.Component.Label symLabel)

        //     // let compList = (List.ofArray (Map.keys model.Wire.Symbol.Symbols))
        //     let compList = [ componentID ]

        //     let rotatedSymbol = RotateScale.rotateBlock compList model.Wire.Symbol rotate
        //     model
        //     |> Optic.set (wire_ >-> symbol_) rotatedSymbol
        //     |> Optic.set boundingBoxes_ (Symbol.getBoundingBoxes rotatedSymbol)

        /// Helper to get symbol ID from symbol label and symbol model
        let getSymId (symLabel: string) (symModel: SymbolT.Model) : ComponentId =
            mapValues symModel.Symbols
            |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
            |> function
                | Some x -> x.Id
                | _ -> failwithf "TestDrawBlock.rotateSymbol: symLabel (%A) not found" symLabel

        /// Run the global wire separation algorithm (should be after all wires have been placed and routed)
        let separateAllWires (model: SheetT.Model) : SheetT.Model =
            model
            |> Optic.map
                busWireModel_
                (BusWireSeparate.updateWireSegmentJumpsAndSeparations (model.Wire.Wires.Keys |> Seq.toList))

        /// Helper to rotate a symbol by given number of degrees
        let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (SheetT.Model) =
            let rotateAntiClockwiseBy90 (model: SheetT.Model) : SheetT.Model =
                let symModel = Optic.get symbolModel_ model
                let symId = getSymId symLabel symModel
                let updatedSymModel =
                    SymbolUpdate.updateSymbol
                        (SymbolResizeHelpers.rotateSymbol Degree90)
                        (symId)
                        (Optic.get symbolModel_ model)
                Optic.set symbolModel_ updatedSymModel model

            match rotate with
            | Degree0 -> model
            | Degree90 -> model |> rotateAntiClockwiseBy90
            | Degree180 ->
                model
                |> rotateAntiClockwiseBy90
                |> rotateAntiClockwiseBy90
            | Degree270 ->
                model
                |> rotateAntiClockwiseBy90
                |> rotateAntiClockwiseBy90
                |> rotateAntiClockwiseBy90
            |> separateAllWires
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

        let flipSymbolD2
            (symLabel: string)
            (flip: SymbolT.FlipType)
            (model: SheetT.Model)
            : Result<SheetT.Model, string>
            =
            let symModel = (fst symbolModel_) model
            let symId =
                symModel.Symbols
                |> Map.toList
                |> List.tryFind (fun (id, sym) -> sym.Component.Label = symLabel)
                |> Option.map (fun (id, sym) -> id)
            match symId with
            | None -> Error $"symbol '{symLabel}' could not be found"
            | Some symId ->
                let transform = SymbolResizeHelpers.flipSymbol flip
                let symModelNew = SymbolUpdate.updateSymbol transform symId symModel
                model
                |> Optic.set symbolModel_ symModelNew
                |> SheetUpdate.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
                |> Ok

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

        /// Copy testModel into the main Issie Sheet making its contents visible
        let showSheetInIssieSchematic (testModel: SheetT.Model) (dispatch: Dispatch<Msg>) =
            let sheetDispatch sMsg = dispatch (Sheet sMsg)
            dispatch
            <| UpdateModel(fun (model) ->
                let newUndoList = appendUndoList model.Sheet.UndoList model.Sheet

                model
                |> Optic.set sheet_ testModel
                |> Optic.set (ModelType.sheet_ >-> UndoList_) (newUndoList)

            ) // set the Sheet component of the Issie model to make a new schematic.
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

    /// /// Generates a sequence of triplets containing random horizontal or vertical flip positions for three elements, based on random integers.
    let flipPositions =
        let toFlip =
            function
            | n when n % 2 = 0 -> SymbolT.FlipHorizontal
            | n when n % 2 = 1 -> SymbolT.FlipVertical
            | _ -> failwithf "Flip not possible" // Shouldn't be possible
        map3
            (fun f1 f2 f3 -> (toFlip f1, toFlip f2, toFlip f3))
            (randomInt 0 1 10)
            (randomInt 0 1 10)
            (randomInt 0 1 10)

    type XYPosRFlip = { X: float; Y: float; Rotation: Rotation; Flip: SymbolT.FlipType }
    let bounds1 = 120
    let bounds2 = 660
    let step = 30
    let horizVertLinePositions1: Gen<XYPos> =
        let horizLinePositionsSparse =
            fromList [ -bounds1 .. step .. bounds1 ]
            |> map (fun n -> { X = float n; Y = 0. })
        let vertLinePositionsSparse =
            fromList [ -bounds1 .. step .. bounds1 ]
            |> map (fun n -> { X = 0.; Y = float n })
        product (fun x y -> x + y) horizLinePositionsSparse vertLinePositionsSparse
        |> filter (fun pos -> not (abs pos.X <= 60 && abs pos.Y <= 60))
        // after measuring, keep the absolute distances less than 360 together
        |> map (fun pos -> middleOfSheet + pos)

    let horizVertLinePositions2: Gen<XYPos> =
        let horizLinePositionsSparse =
            fromList [ -bounds2 .. step .. bounds2 ]
            |> map (fun n -> { X = float n; Y = 0. })
        let vertLinePositionsSparse =
            fromList [ -bounds2 .. step .. bounds2 ]
            |> map (fun n -> { X = 0.; Y = float n })
        product (fun x y -> x + y) horizLinePositionsSparse vertLinePositionsSparse
        |> filter (fun pos -> not (abs pos.X <= 300 && abs pos.Y <= 360))
        // after measuring, keep the absolute distances less than 60 together
        |> map (fun pos -> middleOfSheet + pos)

    let hVLinePosFlipRotate1: Gen<XYPosRFlip> =
        let rotateList = fromList [ Degree0; Degree90; Degree180; Degree270 ]
        let flipList = fromList [ SymbolT.FlipHorizontal; SymbolT.FlipVertical ]

        let horizLinePositionsSparse =
            fromList [ -bounds2 .. step .. bounds2 ]
            |> map (fun n -> { X = float n; Y = 0. })
        let vertLinePositionsSparse =
            fromList [ -bounds2 .. step .. bounds2 ]
            |> map (fun n -> { X = 0.; Y = float n })

        let hVData =
            product (fun x y -> x + y) horizLinePositionsSparse vertLinePositionsSparse
            |> filter (fun pos -> not (abs pos.X <= 255 && abs pos.Y <= 255))
            // after measuring, keep the absolute distances less than 60 together
            |> map (fun pos -> middleOfSheet + pos)

        // let shuffledHVData = shuffleA hVData
        map3
            (fun (pos: XYPos) flip rot -> { X = pos.X; Y = pos.Y; Rotation = rot; Flip = flip })
            hVData
            flipList
            rotateList

    // /// demo test circuit consisting of a DFF & And gate
    let makeTest1Circuit (andPos: XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And, 2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0))
        |> getOkOrFail

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

    //--------------------------------D1T Functions for Testing-----------------------------------//
    // Have to provide XYPos with Component since the unplaced component does not have dimensions

    /// <summary>
    /// Defines a sample list of components that can be used to generate circuits.
    /// </summary>
    let test3SampleComponents: (string * ComponentType * XYPos) list =
        [ "AND", GateN(And, 2), { X = 45.0; Y = 45.0 }
          "NOT", Not, { X = 30.0; Y = 30.0 }
          "MUX2", Mux2, { X = 90.0; Y = 60.0 }
          "DFF", DFF, { X = 75.0; Y = 75.0 }
          "ADDER", NbitsAdder(1), { X = 90.0; Y = 120.0 }
          "REG", Register(1), { X = 60.0; Y = 120.0 } ]

    /// <summary>
    /// Defines a sample list of custom components that can be used to generate circuits.
    /// </summary>
    let test3SampleCustomComponents: (string * ComponentType * XYPos) list =
        [ "CUSTOM",
          Custom
              { Name = "CustomComponentTest"
                InputLabels = [ ("A", 1); ("A", 1); ("A", 1) ]
                OutputLabels = [ ("C", 1); ("A", 1); ("A", 1) ]
                Form = None
                Description = None },
          { X = 301.0; Y = 100.0 } ]

    let inputSample = [ "INPUT", Input1(1, Some 1), { X = 30.0; Y = 60.0 } ]
    let outputSample = [ "OUTPUT", Output 1, { X = 30.0; Y = 60.0 } ]

    /// <summary>
    /// Generates a custom component with a specified number of input and output labels.
    /// </summary>
    /// <param name="inputLabelsCount">The count of input labels for the custom component.</param>
    /// <param name="outputLabelsCount">The count of output labels for the custom component.</param>
    /// <returns>A list containing the custom component definition.</returns>
    let generateCustomComponent (inputLabelsCount: int) (outputLabelsCount: int) =
        let inputLabels =
            [ 1..inputLabelsCount ]
            |> List.map (fun i -> ("A", i))
        let outputLabels =
            [ 1..outputLabelsCount ]
            |> List.map (fun i -> ("B", i))
        [ "CUSTOM",
          Custom
              { Name = "CustomComponentTest"
                InputLabels = inputLabels
                OutputLabels = outputLabels
                Form = None
                Description = None },
          //   width = 30 + max(inputLabelsCount, outputLabelsCount) * 20
          { X =
              30.0
              + ((float (max inputLabelsCount outputLabelsCount))
                 * 20.)
            Y = 301.0 } ]

    // function to sample a random uniform number from -10, 10. This is to unstraighten wires
    let perturbationNoiseUnstraighten =
        let rnd = System.Random()
        fun () -> rnd.Next(-10, 10)

    /// <summary>
    /// Generates a sequence of circuits with randomly positioned components.
    /// </summary>
    /// <param name="count">The number of components to generate in the circuit.</param>
    /// <param name="startPos">The starting XY position for the first component.</param>
    /// <param name="sampleComponentsList">The list of sample components to choose from.</param>
    /// <returns>A list of components with their labels, types, and positions.</returns>
    let generateCircuitSequence
        (count: int)
        (startPos: XYPos)
        (sampleComponentsList: (string * ComponentType * XYPos) list)
        =
        // pick a n random components from the list, (repeats allowed) where n = count
        // for each component, calculate its position by adding the dimensions to the startpos + a perturbation

        let rnd = System.Random()
        let randomSampleComponents: (string * ComponentType * XYPos) list =
            [ 1..count ]
            |> List.mapi (fun i _ ->
                let compName, compType, pos =
                    sampleComponentsList[rnd.Next(sampleComponentsList.Length)]
                ((compName + "_" + i.ToString()), compType, pos))

        let componentsWithArrangedPos =
            randomSampleComponents
            |> List.fold
                (fun (accList, currentPos) (compLabel, compType, componentDimensions) ->
                    let offset =
                        { X = (componentDimensions.X + 70.0)
                          Y = float (perturbationNoiseUnstraighten ()) }
                    let newList =
                        accList
                        |> List.append [ (compLabel, compType, currentPos) ]
                    let newPos = currentPos + offset

                    newList, newPos)
                ([], startPos)
        componentsWithArrangedPos

    let increasingPositions = fromList [ 1..9 ]

    /// <summary>
    /// Generates a test circuit with a specified starting position.
    /// </summary>
    /// <param name="startPos">The starting XY position for the circuit.</param>
    /// <returns>A sheet model containing the placed components and wires.</returns>
    let makeTest3Circuit (startPos: XYPos) =
        let componentsWithArrangedPos, _ =
            generateCircuitSequence 5 startPos test3SampleComponents

        let sheetModelResult =
            componentsWithArrangedPos
            |> List.fold
                (fun result (compLabel, compType, pos) ->
                    result
                    |> Result.bind (fun model -> placeSymbol (string compLabel) compType pos model))
                (Ok initSheetModel)

        sheetModelResult
        |> Result.bind (fun model ->
            let componentLabels =
                componentsWithArrangedPos
                |> List.map (fun (label, _, _) -> label)
            let connections =
                componentLabels
                |> List.pairwise
                |> List.map (fun (sourceLabel, destLabel) -> portOf destLabel 0, portOf sourceLabel 0)

            connections
            |> List.fold
                (fun currentResult (sourcePort, destPort) ->
                    currentResult
                    |> Result.bind (fun currentModel -> placeWire sourcePort destPort currentModel))
                (Ok model))
        |> getOkOrFail

    /// <summary>
    /// Generates a test circuit with custom components and a specified starting position.
    /// </summary>
    /// <param name="startPos">The starting XY position for the circuit.</param>
    /// <returns>A sheet model containing the placed custom components and wires.</returns>
    let makeTest4Circuit (startPos: XYPos) =
        let componentsWithArrangedPos, _ =
            generateCircuitSequence 5 startPos test3SampleCustomComponents

        let sheetModelResult =
            componentsWithArrangedPos
            |> List.fold
                (fun result (compLabel, compType, pos) ->
                    result
                    |> Result.bind (fun model -> placeSymbol (string compLabel) compType pos model))
                (Ok initSheetModel)

        sheetModelResult
        |> Result.bind (fun model ->
            let componentLabels =
                componentsWithArrangedPos
                |> List.map (fun (label, _, _) -> label)
            let connections =
                componentLabels
                |> List.pairwise
                |> List.map (fun (sourceLabel, destLabel) -> portOf destLabel 0, portOf sourceLabel 0)

            connections
            |> List.fold
                (fun currentResult (sourcePort, destPort) ->
                    currentResult
                    |> Result.bind (fun currentModel -> placeWire sourcePort destPort currentModel))
                (Ok model))
        |> getOkOrFail

    //-------------------D3T Deliverable----------------//
    // Demux4 and Mux4 test circuit configuration
    let makeD3Test1Circuit (andPos: XYPos) =
        initSheetModel
        |> placeSymbol "DM1" Demux4 andPos
        |> Result.bind (placeSymbol "MUX1" Mux4 middleOfSheet)
        |> Result.bind (placeWire (portOf "DM1" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "DM1" 1) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "DM1" 2) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "DM1" 3) (portOf "MUX1" 3))

        |> getOkOrFail

    //-------------------D3T Deliverable----------------//
    // flips and rotations of the MUX4, + DEMUX4
    let makeD3Test2Circuit (andParameters: XYPosRFlip) =
        let andPos = { X = andParameters.X; Y = andParameters.Y }
        let andRotation = andParameters.Rotation
        let andFlip = andParameters.Flip

        initSheetModel
        |> placeSymbol "DM1" Demux4 andPos
        |> Result.bind (placeSymbol "MUX1" Mux4 middleOfSheet)
        |> Result.bind (placeWire (portOf "DM1" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "DM1" 1) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "DM1" 2) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "DM1" 3) (portOf "MUX1" 3))

        |> getOkOrFail

        |> rotateSymbol "MUX1" andRotation
        |> flipSymbol "MUX1" andFlip

    //-------------------D3T Deliverable----------------//
    // Test circuit of SplitN and MergeN for testing bit legends as seen in Figure C2
    let makeD3Test3Circuit (andParameters: XYPosRFlip) =
        let andPos = { X = andParameters.X; Y = andParameters.Y }
        let andRotation = andParameters.Rotation
        let andFlip = andParameters.Flip

        initSheetModel
        |> placeSymbol "SN1" (SplitN(3, [ 4; 4; 4 ], [ 0; 1; 2 ])) andPos
        |> Result.bind (placeSymbol "MN1" (MergeN(3)) middleOfSheet)
        |> Result.bind (placeWire (portOf "SN1" 0) (portOf "MN1" 0))
        |> Result.bind (placeWire (portOf "SN1" 1) (portOf "MN1" 1))
        |> Result.bind (placeWire (portOf "SN1" 2) (portOf "MN1" 2))
        |> getOkOrFail
        |> rotateSymbol "MN1" andRotation
        |> flipSymbol "MN1" andFlip

    let vertLinePositions: Gen<XYPos> =
        fromList [ -100..20..100 ]
        |> map (fun n -> middleOfSheet + { X = 0.; Y = float n })

    //--------------------------------------------------------------------------------------------------//
    //--- # 7 Varying both horizontal and vertical line positions, using GenerateData.product, ---------//
    //--- GenerateData.filter, GenereateData.map, and GenereateData.fromList to create a Gen<XYPos> ----//
    //--------------------------------------------------------------------------------------------------//

    let bounds = 120

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

    //--------------------------------------------------------------------------------------------------//
    //------------------------------------Circuit for D2 deliverable------------------------------------//
    //--------------------------------------------------------------------------------------------------//
    /// Creates a digital circuit with gates and Muxes. Muxes (2-Mux) are randomly flipped or swapped input ports.
    let makeD2Circuit (flip1: SymbolT.FlipType, flip2: SymbolT.FlipType, flip3: SymbolT.FlipType) =
        let model =
            initSheetModel
            |> placeSymbol "G1" (GateN(And, 2)) { X = 1668; Y = 1888 }
            |> Result.bind (placeSymbol "s1" (Input1(1, None)) { X = 1666.5; Y = 1959.5 })
            |> Result.bind (placeSymbol "G2" (GateN(And, 2)) { X = 1778.5; Y = 1995 })
            |> Result.bind (placeSymbol "MUX1" Mux2 middleOfSheet)
            |> Result.bind (placeSymbol "MUX2" Mux2 { X = 1630; Y = 1698 })
            |> Result.bind (placeSymbol "MUX3" Mux2 { X = 1898.5; Y = 1864.5 })
            |> Result.bind (flipSymbolD2 "MUX1" flip1)
            |> Result.bind (flipSymbolD2 "MUX2" flip2)
            |> Result.bind (flipSymbolD2 "MUX3" flip3)
            |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "MUX1" 0))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX3" 0))
            |> Result.bind (placeWire (portOf "G1" 0) (portOf "MUX1" 1))
            |> Result.bind (placeWire (portOf "s1" 0) (portOf "MUX1" 2))
            |> Result.bind (placeWire (portOf "G2" 0) (portOf "MUX3" 1))
            |> getOkOrFail
        printfn "count crossings: %A" (countVisibleSegsPerpendicularCrossings model)
        model
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

        //---------------------------D3T Deliverable--------------------------//
        // Helper function for counting total wire bends
        let totalWireBends (sample: int) (sheet: SheetT.Model) =
            let totalWireBendNum = countVisibleRAngles sheet
            totalWireBendNum

        //---------------------------D3T Deliverable--------------------------//
        // Also counts the number of wire bends in a sheet
        let failOnSymbolIntersectsSymbolD3 (sample: int) (sheet: SheetT.Model) =
            let sumWireBends = totalWireBends sample sheet

            let boundingBoxes = sheet.BoundingBoxes
            let boxes =
                mapValues boundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n, box)
            List.allPairs boxes boxes
            |> List.exists (fun ((n1, box1), (n2, box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
            |> (function
            | true ->
                Some
                    $"Symbol outline intersects another symbol outline in Sample {sample}. Total instances of bends in the current sheet are {sumWireBends}."
            | false -> Some $"No symbol overlaps. Total instances of bends in the current sheet are {sumWireBends}.")

        let failOnWireBends (sample: int) (sheet: SheetT.Model) =
            let totalWiredBendNum = totalWireBends sample sheet
            if totalWiredBendNum > 0 then
                Some $"Total instances of wire bends are {totalWiredBendNum}."
            else
                Some $"No instances of wire bends"

        //--------------------------D3T Deliverable--------------------------//
        // Better incorporates failOnSymIntersectsSymbol with failOnWireBends into a single func

        let failWireBendSymIntersectSym (sample: int) (sheet: SheetT.Model) =
            let totalWireBendNum = totalWireBends sample sheet

            let boundingBoxes = sheet.BoundingBoxes
            let boxes =
                mapValues boundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n, box)
            let symbolIntersectSymbol =
                List.allPairs boxes boxes
                |> List.exists (fun ((n1, box1), (n2, box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)

            let wireBendMsg =
                if totalWireBendNum > 0 then
                    Some $"Total instances of wire bends are {totalWireBendNum}."
                else
                    Some $"No instances of wire bends"

            match symbolIntersectSymbol, wireBendMsg with
            | true, Some msg -> Some $"Symbol outline intersects another symbol outline in Sample {sample}. {msg}"
            | false, Some msg -> Some $"No symbol overlaps. {msg}"
            | _, _ -> None

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
                "Singly Connected Component"
                firstSample
                horizLinePositions
                makeTest3Circuit
                (Asserts.failOnAllTests)
                dispatch
            |> recordPositionInTest testNum dispatch

        let test2 testNum firstSample dispatch =
            printf "Test1"
            runTestOnSheets
                "Custom Component Array"
                firstSample
                horizLinePositions
                makeTest4Circuit
                (Asserts.failOnAllTests)
                dispatch
            |> recordPositionInTest testNum dispatch

        // let test1 testNum firstSample dispatch =
        //     printf "Test1"
        //     runTestOnSheets
        //         "Horizontally positioned AND + DFF: fail on sample 0"
        //         firstSample
        //         horizLinePositions
        //         makeTest1Circuit
        //         (Asserts.failOnSampleNumber 0)
        //         dispatch
        //     |> recordPositionInTest testNum dispatch

        // Example test: Horizontally positioned AND + DFF: fail on sample 10
        // let test2 testNum firstSample dispatch =
        //     runTestOnSheets
        //         "Horizontally positioned AND + DFF: fail on sample 10"
        //         firstSample
        //         horizLinePositions
        //         makeTest1Circuit
        //         (Asserts.failOnSampleNumber 10)
        //         dispatch
        //     |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail on symbols intersect
        let test3 testNum firstSample dispatch =
            runTestOnSheets
                "D2Test"
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
                flipPositions
                makeD2Circuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        //--------------------- D3Tests -------------------- //

        // D3Test1 - Randomly positioned DEMUX4 + MUX4 testing for failWireBendSymIntersectSym
        let D3Test1 testNum firstSample dispatch =
            runTestOnSheets
                "Randomly positioned MUX4 + DEMUX4"
                firstSample
                horizVertLinePositions2
                makeD3Test1Circuit
                Asserts.failWireBendSymIntersectSym
                dispatch
            |> recordPositionInTest testNum dispatch

        // D3Test2 - Random positioned DEMUX4 and MUX4 tests for FailWireBendSymIntersectSym
        let D3Test2 testNum firstSample dispatch =
            runTestOnSheets
                "Randomly positioned, rotated and flipped MUX4, + DEMUX4"
                firstSample
                hVLinePosFlipRotate1
                makeD3Test2Circuit
                Asserts.failWireBendSymIntersectSym
                dispatch
            |> recordPositionInTest testNum dispatch

        // D3Test3 - "Randomly positioned, rotated and flipped MN1, + SN1" for failWireBendSymIntersectSym
        let D3Test3 testNum firstSample dispatch =
            runTestOnSheets
                "Randomly positioned, rotated and flipped MN1, + SN1"
                firstSample
                hVLinePosFlipRotate1
                makeD3Test3Circuit
                Asserts.failWireBendSymIntersectSym
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
              "D3Test1", D3Test1
              "D3Test2", D3Test2
              "D3Test3", D3Test3
              "Test8",
              fun _ _ _ -> printf "Test8" // example
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
