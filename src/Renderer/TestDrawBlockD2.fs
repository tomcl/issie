module TestDrawBlockD2

open Elmish
open GenerateData
open TestDrawBlock
open TestDrawBlock.TestLib
open SheetBeautifyHelpers

module D2Test =
    open EEExtensions
    open Optics
    open Optics.Operators
    open DrawHelpers
    open Helpers
    open CommonTypes
    open ModelType
    open DrawModelType
    open Sheet.SheetInterface

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

//------------------------------------------------------------------------------------------------------------------------//
//------------------------------additional metrics to assess improvements-------------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
    module Metrics =
        // difference for integer metrics
        let computeMetricDifference (sheetAfter : SheetT.Model) (sheetBefore : SheetT.Model) (metric : SheetT.Model->int)  =
            metric sheetAfter - metric sheetBefore, metric sheetAfter, metric sheetBefore
        
        // percentage difference for float metrics
        let computeMetricPercentageChange (sheetAfter : SheetT.Model) (sheetBefore : SheetT.Model) (metric : SheetT.Model->float)  =
            float (metric sheetAfter - metric sheetBefore)/float (metric sheetBefore)*100.0, metric sheetAfter, metric sheetBefore
        
        let numberOfWireStraightened (sheetAfter : SheetT.Model) (sheetBefore : SheetT.Model) =
            computeMetricDifference sheetAfter sheetBefore countWireRightAngles

        let numberOfIntersectionReduced (sheetAfter : SheetT.Model) (sheetBefore : SheetT.Model) =
            computeMetricDifference sheetAfter sheetBefore countSegmentIntersections

        /// number of visible wire segments counted over whole sheet
        let countVisibleSegments (sheetModel : SheetT.Model) : int =
            // this gives a simple indicator of how readable the schematic is
            let wireModel = sheetModel.Wire

            wireModel.Wires
            |> Map.toList
            |> List.map (fun (wid, wire) -> visibleSegments wid sheetModel)
            |> List.map (fun segs -> segs.Length)
            |> List.sum

        // There is already a function to count segment intersections in SheetBeautifyHelpers
        // Additional Metrics
        let intersectionPerSegment (sheetModel: SheetT.Model): float =
            let nIntersect = countSegmentIntersections sheetModel
            let nSeg = 
                sheetModel.Wire.Wires 
                |> Map.toList 
                |> List.map (fun (_,wire) -> wire.Segments.Length)
                |> List.sum
            float nIntersect / float nSeg

        // wires having right angles are a necessary condition for intersection to exist
        let rightAnglePerWire (sheetModel: SheetT.Model): float =
            float (countWireRightAngles sheetModel) / float (Map.count sheetModel.Wire.Wires)

        // the proportion of visible wiring length to total wiring length
        let visibleToTotalLengthRatio (sheetModel: SheetT.Model): float =
            let totalLength = 
                sheetModel.Wire.Wires 
                |> Map.toList 
                |> List.map (fun (_,wire) -> wire.Segments |> List.fold (fun acc seg -> acc + abs seg.Length) 0.0)
                |> List.sum
            let visibleLength = calcVisibleWiringLength sheetModel
            visibleLength / totalLength



//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to display information of sheet model-------------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
    module Displays =
        open Metrics
        /// <summary> Display information or calculated metrics on the sheet model. This will be called automatically on failed tests. </summary>
        /// <param name="displayFunc"> A list of display functions </param>
        /// <param name="sheet"> The sheet model to display </param>
        let display (displayFuncs: (SheetT.Model -> string) list) (sheet: SheetT.Model) =
            let displays = 
                displayFuncs
                |> List.map (fun f -> f sheet)
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

        // Diplay Functions: Statistics
        let displayCountSymIntersectSym (sheet: SheetT.Model) : string =
            displayer "n_sym_intersect_sym" countSymIntersectSym sheet

        let displayCountSegIntersectSym (sheet: SheetT.Model) : string =
            displayer "n_seg_intersect_sym" countSegIntersectSym sheet

        let displaySegmentIntersections (sheet: SheetT.Model) : string =
            displayer "n_intersections" countSegmentIntersections sheet
        
        let displayVisibleSegments (sheet: SheetT.Model) : string =
            displayer "n_visible_segments" countVisibleSegments sheet

        let displayWireRightAngles (sheet: SheetT.Model) : string =
            displayer "n_wire_right_angles" countWireRightAngles sheet

        let displayVisibleWiringLength (sheet: SheetT.Model) : string =
            displayer "visible_wiring_length" calcVisibleWiringLength sheet

        let displayRetracingSegments (sheet: SheetT.Model) : string =
            displayer "n_retracing_segments" getRetracingSegment sheet

        // Diplay Functions: Metrics
        let displayIntersectPerSegment =
            displayer "intersection/seg" intersectionPerSegment

        let displayRightAnglesPerWire =
            displayer "right_angle/wire" rightAnglePerWire

        let displayMetric = [
            displayIntersectPerSegment;
            displayRightAnglesPerWire;
        ]

        let displayAll = [
            displayComponents; 
            displayCountSegIntersectSym;
            displaySegmentIntersections; 
            displayWireRightAngles; 
            displayVisibleSegments;
            displayVisibleWiringLength; 
            displayRetracingSegments;
            ]

//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
    module Builder =
        open Displays
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
            
        let getSymId (symLabel: string) (symModel: SymbolT.Model): ComponentId = 
            mapValues symModel.Symbols
            |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
            |> function
                | Some x -> x.Id
                | _ -> failwithf "TestDrawBlock.getSymId: symLabel (%A) not found" symLabel

        // Rotate a symbol
        let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (SheetT.Model) =
            let symId = getSymId symLabel model.Wire.Symbol
            let rotateSymbol' = SymbolResizeHelpers.rotateSymbol rotate
            let symModel: SymbolT.Model = 
                SymbolUpdate.updateSymbol rotateSymbol' symId model.Wire.Symbol

            model
            |> Optic.set symbolModel_ symModel
            |> SheetUpdateHelpers.updateBoundingBoxes

        // Flip a symbol
        let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : (SheetT.Model) =
            let symId = getSymId symLabel model.Wire.Symbol
            let flipSymbol' = 
                match flip with
                | SymbolT.FlipType.FlipHorizontal -> 
                    SymbolResizeHelpers.flipSymbol SymbolT.FlipType.FlipHorizontal
                | SymbolT.FlipType.FlipVertical ->
                    SymbolResizeHelpers.flipSymbol SymbolT.FlipType.FlipHorizontal 
                    >> SymbolResizeHelpers.rotateAntiClockByAng Degree180 
            let symModel: SymbolT.Model = 
                SymbolUpdate.updateSymbol flipSymbol' symId model.Wire.Symbol

            model
            |> Optic.set symbolModel_ symModel 
            |> SheetUpdateHelpers.updateBoundingBoxes

        let randomFlipping () =
            let randGen = System.Random()
            randGen.Next(0,2)
            |> function
                | 0 -> Ok SymbolT.FlipHorizontal
                | _ -> Error SymbolT.FlipVertical // SymbolT.FlipVertical <= this doesn't work and raise exception!!
            
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
            
//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//
    open Builder
    /// Sample data based on 11 equidistant points on a horizontal line
    let horizLinePositions =
        fromList [-100..20..100]
        |> map (fun n -> middleOfSheet + {X=float n; Y=0.})

    // ====================== Positions Generator ======================
    let gridPositions n : Gen<XYPos> =
        let genX = fromList [-n..20..n]
        let genY = fromList [-n..20..n]
        (genX, genY)
        ||> product (fun x y -> middleOfSheet + {X=float x; Y=float y})

    let filteredGridPositions (sheetMaker) (pos: int) =
        let existOverlapWithBoxes (pos: XYPos) =
            let sheet: SheetT.Model = sheetMaker pos
            let boxes: (int * BoundingBox) list = // list<(int * bounding box)>
                sheet.BoundingBoxes
                |> mapValues
                |> Array.toList
                |> List.mapi (fun n (box: BoundingBox) -> n,box)

            boxes
            |> List.allPairs boxes
            |> List.exists (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)

        GenerateData.filter (existOverlapWithBoxes >> not) (gridPositions 100)

    let makeTuple a b = (a, b)

    /// Gen samples incorporating two sets of rotations, and two of flips
    let flipOnlySamples =
        let flips = fromList [Some SymbolT.FlipType.FlipHorizontal; Some SymbolT.FlipType.FlipVertical; None]
        gridPositions 100
        |> product makeTuple flips  
        |> product makeTuple flips
        // This final stage makes the output more readable in an anonymous record
        |> map (fun (flipDFF, (flipAnd, (andPos)))->
            {|
                FlipDFF=flipDFF;
                FlipAnd=flipAnd;
                AndPos=andPos
             |})

    /// From Ed: Fischer-Yates shuffle algorithm
    /// Returns a random shuffled array without changing the input array
    let shuffleAGen (rng: System.Random) arrayToShuffle: 'a array =
        let tmpA = Array.copy arrayToShuffle
        for i = 0 to tmpA.Length - 1 do 
            let r = rng.Next(i, tmpA.Length);
            (tmpA[i],tmpA[r])
            |> fun (iv, rv) -> tmpA[r] <- iv;  tmpA[i]  <- rv
        tmpA

    // ====================== Fixed Circuit Generator ======================
    let makeTest1Circuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) {X=middleOfSheet.X+100.;Y=middleOfSheet.Y-100.}
        |> Result.bind (placeSymbol "S1" (Input1(1, None)) {X=middleOfSheet.X-150.;Y=middleOfSheet.Y})
        |> Result.bind (placeSymbol "S2" (Input1(1, None)) {X=middleOfSheet.X-150.;Y=middleOfSheet.Y+100.})
        |> Result.bind (placeSymbol "MUX1" Mux2 {X=middleOfSheet.X-100.;Y=middleOfSheet.Y-100.})
        |> Result.bind (placeSymbol "MUX2" Mux2 middleOfSheet)
        |> Result.map (flipSymbol "MUX2" SymbolT.FlipType.FlipVertical)
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0) )
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1) )
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 0) )
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 1) )
        |> getOkOrFail

    let makeTest2Circuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "MUX1" Mux2 middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 0) )
        |> getOkOrFail

    let makeTest3Circuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 1) )
        |> getOkOrFail

    // ====================== Random Flip Generator ======================
    let makeTest4Circuit (sample: {|
        FlipDFF: SymbolT.FlipType option;
        FlipAnd: SymbolT.FlipType option;
        AndPos: XYPos
    |}) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) sample.AndPos
        |> match sample.FlipAnd with
            | Some f -> Result.map (flipSymbol "G1" SymbolT.FlipType.FlipHorizontal)
            | None -> id
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> match sample.FlipDFF with
            | Some f -> Result.map (flipSymbol "G1" SymbolT.FlipType.FlipHorizontal)
            | None -> id
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
    
//-------------------------------------------------------------------------------------//
//-----------------------------D2 Tests on Draw Block code-----------------------------//
//-------------------------------------------------------------------------------------//

    module Tests =
        open Displays
        open System
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
            
        let test1 testNum firstSample dispatch =
            let displayOnFail = displayAll
            let generator = filteredGridPositions makeTest1Circuit 100
            runTestOnSheets
                "DisplayAll: DFF+AND 2 Wires Different Net"
                firstSample
                generator
                makeTest1Circuit
                Asserts.failOnAllTests
                dispatch
                displayOnFail
            |> recordPositionInTest testNum dispatch

        let test2 testNum firstSample dispatch =
            let displayOnFail = displayAll
            let generator = filteredGridPositions makeTest2Circuit 100
            runTestOnSheets
                "DisplayAll: DFF+AND 2 Wires Same Net"
                firstSample
                generator
                makeTest2Circuit
                Asserts.failOnAllTests
                dispatch
                displayOnFail
            |> recordPositionInTest testNum dispatch

        let test3 testNum firstSample dispatch =
            let displayOnFail = displayMetric
            let generator = filteredGridPositions makeTest3Circuit 100
            runTestOnSheets
                "DisplayAll: DFF+AND 2 Wires Same Net + 1 Different Net"
                firstSample
                generator
                makeTest3Circuit
                Asserts.failOnAllTests
                dispatch
                displayOnFail
            |> recordPositionInTest testNum dispatch

        let test4 testNum firstSample dispatch =
            let displayOnFail = displayMetric
            let generator = flipOnlySamples
            let myRandomSample = shuffleAGen <| Random(1)
            runTestOnSheets
                "DisplayAll: Random Flip DFF+AND"
                firstSample
                generator
                makeTest4Circuit
                Asserts.failOnAllTests
                dispatch
                displayOnFail
            |> recordPositionInTest testNum dispatch
        
        // let test9 testNum firstSample dispatch =
        //     let displayOnFail = [displayComponents; displaySegmentIntersections; displayWireRightAngles; displayVisibleWiringLength; displayVisibleSegments]
        //     display displayOnFail (makeTest1Circuit middleOfSheet)

        // ====================== Tick3 Q7+Q10 Test ======================
        // Randomly rotate and flip the symbols in the circuit maker
        let randomFlip =
            let flip = randomFlipping()
            match flip with
            | Ok flip -> 
                flipSymbol "G1" flip
            | Error flip -> 
                // a fix for vertical flip, which uses rotateSymbol with Degree180 that doesn't work
                flipSymbol "G1" flip
                >> rotateSymbol "G1" Degree90
                >> rotateSymbol "G1" Degree90

        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [
                "Test1: 2 diff net", test1
                "Test2: 2 same net", test2
                "Test3: 2 same + 1 diff", test3
                "Test4: Random Flip", test4
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