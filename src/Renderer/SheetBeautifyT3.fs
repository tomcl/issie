module SheetBeautifyT3

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

/// constants used by SheetBeautify
module Constants = 
    // () // dummy to make skeleton type check - remove when other content exists
    let wireLabelThreshold = 100 

// Copied from TestDrawBlock.fs
/// 1. Types to represent tests with (possibly) random data, and results from tests; 
/// 2. Helper functions to manipulate sheet and component data; 
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

        /// Return a list of segment vectors with 3 vectors coalesced into one visible equivalent
        /// if this is possible, otherwise return segVecs unchanged.
        /// Index must be in range 1..segVecs
        let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
            if segVecs[index] =~ XYPos.zero
            then
                segVecs[0..index-2] @
                [segVecs[index-1] + segVecs[index+1]] @
                segVecs[index+2..segVecs.Length - 1]
            else
                segVecs

        wire.Segments
        |> List.mapi getSegmentVector
        |> (fun segVecs ->
                (segVecs,[1..segVecs.Length-2])
                ||> List.fold tryCoalesceAboutIndex)

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

        /// Add a (newly routed) wire, source specifies the Output port, target the Input port.
        /// Return an error if either of the two ports specified is invalid, or if the wire duplicates and existing one.
        /// The wire created will be smart routed but not separated from other wires: for a nice schematic
        /// separateAllWires should be run after  all wires are added.
        /// source, target: respectively the output port and input port to which the wire connects.
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


// ------------------------------------ Team work ------------------------------------------
(* 
    This part of the code aims to test the correct usage of labels as described in D3. 
    See https://github.com/dyu18/hlp24-project-issie-team7/tree/indiv-az1821/README-Indiv-notes.md for more documentation. 
*)

// dummy function to be tested (to avoid error for now)
let sheetWireLabelSymbol (model : SheetT.Model) = 
    Ok (model) // returns the same model, no change in labels

module T3 =
    open TestLib
    open TestLib.Builder
    open Constants

    // -------------------------- Test data generation -------------------------------------------

    let makeTestCircuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "MUX1" Mux2  andPos
        |> Result.bind (placeSymbol "I0" IOLabel (andPos+{X=60.;Y=60.}))
        |> Result.bind (placeSymbol "FF1" DFF (middleOfSheet-{X=0.;Y=100.}))
        |> Result.bind (placeSymbol "FF2" DFF (middleOfSheet))
        |> Result.bind (placeSymbol "FF3" DFF (middleOfSheet+{X=0.;Y=100.}))
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "I0" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "FF2" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "FF3" 0))
        |> Result.bind sheetWireLabelSymbol
        |> getOkOrFail

    // ------------------------------------ Assertions -----------------------------------------
    /// Assert functions to use for the testing of D3 task
    /// 0. The dataset used in this test must pass all the assertion in TestDrawBlocks.fs 
    /// 1. Wire label placement when wire lengths > threshold.
    /// 2. Wire label removal when wire lengths < threshold.
    /// 3. Wire label correct connection between component ports. 
    /// 4. Wire label positioning adjustment to avoid overlaps.
    module Asserts = 

        /// Fails on test number: show certain test case
        let failOnSampleNumber (sampleToFail : int) (sample: int) _sheet =
            if sampleToFail = sample then
                Some $"Failing forced on Sample {sampleToFail}."
            else
                None

        /// Fails all tests: show all test cases
        let failOnAllTests (sample: int) _ =
            Some $"Sample {sample}"

        /// 0. simply count number of wire/labels
        let failOnWireLabels (sample: int) (model: SheetT.Model) =
            let originalWireCount = Map.countBy (fun _ wire -> wire |> WireT.wireOf).Wires
            let updatedmodel = sheetWireLabelSymbol model 
            let updatedWireCount = Map.countBy (fun _ wire -> wire |> WireT.wireOf).Wires

            if originalWireCount <> updatedWireCount then
                Some $"Wire labels were not added or removed correctly in Sample {sample}."
            else
                None

        /// 1. check wire -> label placement
        let failOnLabelNotPlaced (sample: int) (model: SheetT.Model) =
            let wireLabels = Map.toSeq model.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type = IOLabel)
            
            let misplacedLabels =
                wireLabels
                |> Seq.filter (fun (_, label) ->
                    let wireLength = getWireLength label
                    let expectedPlacement = if wireLength > wireLabelThreshold then WireLabelPlacement.InPlace else WireLabelPlacement.Removed
                    match expectedPlacement with
                    | WireLabelPlacement.InPlace -> label.Pos = expectedPositionForLabel label
                    | WireLabelPlacement.Removed -> not (Map.exists (fun _ wire -> wire.Label = label.Component.Label) sheet.Wire.Wires)
                )
            match Seq.isEmpty misplacedLabels with
            | true -> None
            | false -> Some $"Wire labels are misplaced in Sample {sample}."

        /// 2. check label -> wire removal
        let failOnLabelNotRemoved (sample: int) (model: SheetT.Model) =
            let wireLabels = Map.toSeq model.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type = IOLabel)
            
            let removedLabels =
                wireLabels
                |> Seq.filter (fun (_, label) ->
                    let wireLength = getWireLength label
                    wireLength <= wireLabelThreshold && not (Map.exists (fun _ wire -> wire.Label = label.Component.Label) sheet.Wire.Wires)
                )
            match Seq.isEmpty removedLabels with
            | true -> None
            | false -> Some $"Wire labels are not removed correctly in Sample {sample}."

        /// whether wire labels are named correctly based on component and port names
        let assertWireLabelNaming (sample: int) (model: SheetT.Model) =
            failwithf "Not Implemented"

        // /// whether wire labels are correctly positioned to avoid overlaps with symbols
        // let assertWireLabelPositionAdjustment (sample: int) (sheet: SheetT.Model) =
        //     let wireLabels = Map.toSeq sheet.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type = IOLabel)
        //     let symbols = Map.toSeq sheet.Wire.Symbol.Symbols |> Seq.filter (fun (_, sym) -> sym.Component.Type <> IOLabel)
        //     let misplacedLabels =
        //         wireLabels
        //         |> Seq.filter (fun (_, label) ->
        //             symbols |> Seq.exists (fun (_, symbol) -> overlap2DBox (getSymBoundingBox symbol) (getSymBoundingBox label))
        //         )
        //     match Seq.isEmpty misplacedLabels with
        //     | true -> None
        //     | false -> Some $"Wire labels are misplaced due to overlap with symbols in Sample {sample}."

    // ----------------------------------- Test driver -----------------------------------
    /// this is a similar test menu as tick 3
    module Tests = 
        let testSheetWireLabelSymbol testNum firstSample dispatch = 
            // runTestOnSheets
            //     "Test sheetWireLabelSymbol function"
            //     firstSample
            //     sampleSheet // Replace sampleSheet with actual sheet
            //     sheetWireLabelSymbol 
            //     Asserts.failOnWireLabels
            //     dispatch
            // |> recordPositionInTest testNum dispatch
            failwithf "Not Implemented"

        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> unit)) list =
            [
                "SheetWireLabelSymbolTest", testSheetWireLabelSymbol
                // more test cases can be added here
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

