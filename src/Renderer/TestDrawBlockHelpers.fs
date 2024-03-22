module TestDrawBlockHelpers
open GenerateData
open SheetBeautifyHelpers
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
    (******************************************************************************************
    This submodule contains a set of functions that enable random data generation
    for property-based testing of Draw Block wire routing functions.
    basic idea.
    1. Generate, in various ways, random circuit layouts
    2. For each layout apply smartautoroute to regenerate all wires
    3. Apply check functions to see if the resulting wire routing obeys "good layout" rules.
    4. Output any layouts with anomalous wire routing
    *******************************************************************************************)

//------------------------------------------------------------------------------------------------------------------------//
//------------------------------additional metrics to assess improvements-------------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
module Metrics =
    // ----------------------Statistics----------------------

    /// number of visible wire segments counted over whole sheet
    let countVisibleSegments (sheetModel : SheetT.Model) : int =
        // this gives a simple indicator of how readable the schematic is
        let wireModel = sheetModel.Wire

        wireModel.Wires
        |> Map.toList
        |> List.map (fun (wid, wire) -> SegmentHelpers.visibleSegments wid sheetModel)
        |> List.map (fun segs -> segs.Length)
        |> List.sum

    // There is already a function to count segment intersections in SheetBeautifyHelpers
    // Additional Metrics
    let crossingPerSegment (sheetModel: SheetT.Model): float =
        let nIntersect = numOfWireRightAngleCrossings sheetModel
        let nSeg = 
            sheetModel.Wire.Wires 
            |> Map.toList 
            |> List.map (fun (_,wire) -> wire.Segments.Length)
            |> List.sum
        float nIntersect / float nSeg

    // wires having right angles are a necessary condition for intersection to exist
    let rightAnglePerWire (sheetModel: SheetT.Model): float =
        float (numOfVisRightAngles sheetModel) / float (Map.count sheetModel.Wire.Wires)

    // the proportion of visible wiring length to total wiring length
    let visibleToTotalLengthRatio (sheetModel: SheetT.Model): float =
        let totalLength = 
            sheetModel.Wire.Wires 
            |> Map.toList 
            |> List.map (fun (_,wire) -> wire.Segments |> List.fold (fun acc seg -> acc + abs seg.Length) 0.0)
            |> List.sum
        let visibleLength = calcVisWireLength sheetModel
        visibleLength / totalLength
    
    // ----------------------Metrics----------------------

    // difference for integer metrics
    let computeMetricDifference (sheetAfter : SheetT.Model) (sheetBefore : SheetT.Model) (metric : SheetT.Model->int)  =
        metric sheetAfter - metric sheetBefore, metric sheetAfter, metric sheetBefore
    
    // percentage difference for float metrics
    let computeMetricPercentageChange (sheetAfter : SheetT.Model) (sheetBefore : SheetT.Model) (metric : SheetT.Model->float)  =
        float (metric sheetAfter - metric sheetBefore)/float (metric sheetBefore)*100.0, metric sheetAfter, metric sheetBefore

    /// number of segments straightened
    let diffNWireRightAngles (sheetAfter : SheetT.Model) (sheetBefore : SheetT.Model) =
        computeMetricDifference sheetAfter sheetBefore numOfVisRightAngles

    let diffNComponentOverlap (sheetAfter : SheetT.Model) (sheetBefore : SheetT.Model) =
        computeMetricDifference sheetAfter sheetBefore numOfIntersectedSymPairs

    let diffNCrossingsReduced (sheetAfter : SheetT.Model) (sheetBefore : SheetT.Model) =
        computeMetricDifference sheetAfter sheetBefore numOfWireRightAngleCrossings

module Displays =
        open Metrics
        /// <summary> AUTHOR hn621 - Display information or calculated metrics on the sheet model. This will be called automatically on failed tests. </summary>
        /// <param name="displayFunc"> A list of display functions </param>
        /// <param name="sheet"> The sheet model to display </param>
        let display (displayFuncs: (SheetT.Model -> string) list) (sheet: SheetT.Model) =
            let displays = 
                displayFuncs
                |> List.map (fun f -> f sheet)
                |> String.concat "\n"
            printfn $"== Display: \n{displays}"

        let metricDisplay (displayFuncs: (SheetT.Model -> SheetT.Model -> string) list) (sheetAfter: SheetT.Model) (sheetBefore: SheetT.Model)  =
            let displays = 
                displayFuncs
                |> List.map (fun f -> f sheetAfter sheetBefore)
                |> String.concat "\n"
            printfn $"== Display: \n{displays}"
        
        /// <summary> Display information on all symbols' position and transform state </summary>
        let displayComponents (sheet: SheetT.Model) : string =
            sheet.Wire.Symbol.Symbols
            |> Map.toList
            |> List.map (fun (id, sym) -> sprintf "> Symbol %s\n| %A\n| %A" sym.Component.Label sym.Pos sym.STransform)
            |> String.concat "\n"

        let displayer (name: string) (f: SheetT.Model -> 'a) (sheet: SheetT.Model) =
            sheet |> f |> sprintf "> %s = %A" name

        let metricDisplayer (name: string) (f: SheetT.Model -> SheetT.Model -> 'a) (sheetAfter: SheetT.Model) (sheetBefore: SheetT.Model) =
            (sheetAfter, sheetBefore) ||> f |> sprintf "> %s = %A" name

        // Diplay Functions: Statistics
        let displayCountSymIntersectSym (sheet: SheetT.Model) : string =
            displayer "n_sym_intersect_sym" numOfIntersectedSymPairs sheet

        let displayCountSegIntersectSym (sheet: SheetT.Model) : string =
            displayer "n_seg_intersect_sym" numOfIntersectSegSym sheet

        let displaySegmentCrossing (sheet: SheetT.Model) : string =
            displayer "n_crossings" numOfWireRightAngleCrossings sheet
        
        let displayVisibleSegments (sheet: SheetT.Model) : string =
            displayer "n_visible_segments" countVisibleSegments sheet

        let displayVisRightAngles (sheet: SheetT.Model) : string =
            displayer "n_vis_right_angles" numOfVisRightAngles sheet

        let displayVisibleWiringLength (sheet: SheetT.Model) : string =
            displayer "visible_wiring_length" calcVisWireLength sheet

        let displayRetracingSegments (sheet: SheetT.Model) : string =
            displayer "n_retracing_segments" findRetracingSegments sheet

        // Diplay Functions: Metrics
        let displayDiffNCrossingsReduced sheetAfter sheetBefore =
            metricDisplayer "diff_n_sym_intersect_sym" diffNCrossingsReduced  sheetAfter sheetBefore

        let displayDiffNWireRightAngles sheetAfter sheetBefore=
            metricDisplayer "diff_n_wire_right_angles" diffNWireRightAngles  sheetAfter sheetBefore

        let displayIntersectPerSegment =
            displayer "intersection/seg" crossingPerSegment

        let displayRightAnglesPerWire =
            displayer "right_angle/wire" rightAnglePerWire

        let displayMetrics: (SheetT.Model -> SheetT.Model -> string) list = [
            displayDiffNCrossingsReduced
            displayDiffNWireRightAngles
        ]

        let displayAll: (SheetT.Model -> string) list = [
            displayComponents; 
            displayCountSegIntersectSym;
            displaySegmentCrossing; 
            displayVisRightAngles; 
            displayVisibleSegments;
            displayVisibleWiringLength; 
            // displayRetracingSegments;
            ]

//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
module Builder =

    open TestLib
    open Displays

    let getSymId (symLabel: string) (symModel: SymbolT.Model): ComponentId = 
            mapValues symModel.Symbols
            |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
            |> function
                | Some x -> x.Id
                | _ -> failwithf "TestDrawBlock.getSymId: symLabel (%A) not found" symLabel

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
                (ccSheet : SheetT.Model)
                (ccSheetName: string)
                (model: Model)
                (scale: XYPos)
                (position: XYPos)
                (sheetModel: SheetT.Model)
                    : Result<SheetT.Model, string> =
           let symbolMap = sheetModel.Wire.Symbol.Symbols
           let project = Option.get model.CurrentProj
           if caseInvariantEqual ccSheetName project.OpenFileName then
                Error "Can't create custom component with name same as current opened sheet"        
            elif not <| List.exists (fun (ldc: LoadedComponent) -> caseInvariantEqual ldc.Name ccSheetName) project.LoadedComponents then
                Error "Can't create custom component unless a sheet already exists with smae name as ccSheetName"
            elif symbolMap |> Map.exists (fun _ sym ->  caseInvariantEqual sym.Component.Label symLabel) then
                Error "Can't create custom component with duplicate Label"
            else
                // MOD sheetModel -> model, TODO: change this to the custom sheet's model
                let canvas = ccSheet.GetCanvasState()
                let ccType: CustomComponentType =
                    {
                        Name = ccSheetName
                        InputLabels = Extractor.getOrderedCompLabels (Input1 (0, None)) canvas
                        OutputLabels = Extractor.getOrderedCompLabels (Output 0) canvas
                        Form = None
                        Description = None
                    }
                placeSymbol symLabel (Custom ccType) position sheetModel
        
    
    // Rotate a symbol
    let rotateSymbol (symLabel: string) (rotate: Rotation option) (model: SheetT.Model) : (SheetT.Model) =
        let symId = getSymId symLabel model.Wire.Symbol
        let rotateSymbol' = 
            match rotate with
            | Some degree -> SymbolResizeHelpers.rotateAntiClockByAng degree
            | None -> id

        let symModel: SymbolT.Model = 
            SymbolUpdate.updateSymbol rotateSymbol' symId model.Wire.Symbol

        model
        |> Optic.set symbolModel_ symModel
        |> SheetUpdateHelpers.updateBoundingBoxes

    let flipSymbol (symLabel: string) (flip: SymbolT.FlipType option) (model: SheetT.Model) : (SheetT.Model) =
            let symId = getSymId symLabel model.Wire.Symbol
            let flipSymbol' = 
                match flip with
                | Some SymbolT.FlipType.FlipHorizontal -> 
                    SymbolResizeHelpers.flipSymbol SymbolT.FlipType.FlipHorizontal
                | Some SymbolT.FlipType.FlipVertical ->
                    SymbolResizeHelpers.flipSymbol SymbolT.FlipType.FlipHorizontal 
                    >> SymbolResizeHelpers.rotateAntiClockByAng Degree180 
                | None -> id
            let symModel: SymbolT.Model = 
                SymbolUpdate.updateSymbol flipSymbol' symId model.Wire.Symbol

            model
            |> Optic.set symbolModel_ symModel 
            |> SheetUpdateHelpers.updateBoundingBoxes

    /// Add a (newly routed) wire, source specifies the Output port, target the Input port.
    /// Return an error if either of the two ports specified is invalid, or if the wire duplicates and existing one.
    /// The wire created will be smart routed but not separated from other wires: for a nice schematic
    /// separateAllWires should be run after  all wires are added.
    /// source, target: respectively the output port and input port to which the wire connects.
    let placeWire (source: SymbolPort) (target: SymbolPort) (model: SheetT.Model) : (Result<SheetT.Model,string>) =
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
    /// HERE: using dispatch to update higher Issie model
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
            (displayOnFail)
                : TestResult<'a> =
            let generateAndCheckSheet n = sheetMaker >> sheetChecker n
            let result =
                {
                    Name=name;
                    Samples=samples;
                    StartFrom = sampleToStartFrom
                    Assertion = generateAndCheckSheet
                }
                |> runTests // retuns result with added field of TestErrors
            match result.TestErrors with
            | [] -> // no errors
                printf $"Test {result.TestName} has PASSED."
            | (n,first):: _ -> // display in Issie editor and print out first error
                printf $"Test {result.TestName} has FAILED on sample {n} with error message:\n{first}"
                match catchException "" sheetMaker (samples.Data n) with
                | Ok sheet -> 
                    display displayOnFail sheet
                    showSheetInIssieSchematic sheet dispatch
                | Error mess -> ()
            result // return the entire result


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

    let failOnAllTestsPrintWireSeg (sample: int) (sheet: SheetT.Model) =
        let wireModel: BusWireT.Model = sheet.Wire
        let listOfWires = wireModel.Wires |> Map.toList |> List.map (fun (wid: ConnectionId,wire) -> wire.InitialOrientation,wire.StartPos)
        let listOfSegments = 
            wireModel.Wires 
            |> Map.toList 
            |> List.collect (fun (wid,wire) -> wire.Segments)
            |> List.map (fun seg -> seg.Index, seg.Length)

        let wireLengths = 
            wireModel.Wires 
            |> Map.toList 
            |> List.map (fun elm -> snd elm |> BlockHelpers.getWireLength) 

        Some $"======== Wires: {listOfWires} ========= Segments: {listOfSegments} ======= {wireLengths} ========="





