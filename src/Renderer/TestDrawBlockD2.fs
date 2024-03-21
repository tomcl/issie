module TestDrawBlockD2

open Elmish
open TimeHelpers
open GenerateData
open TestDrawBlock
open TestDrawBlock.TestLib
open TestDrawBlock.HLPTick3
open TestDrawBlock.HLPTick3.Tests
open TestDrawBlock.HLPTick3.Asserts
open TestDrawBlock.HLPTick3.Builder
open SheetBeautifyHelpers
open SheetBeautifyD1
open SheetBeautifyD2

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
    open SheetBeautifyD3


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


//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to display information of sheet model-------------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
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
        open Displays

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

        // ----------------------Circuit Builders Helper----------------------
        let getSymId (symLabel: string) (symModel: SymbolT.Model): ComponentId = 
            mapValues symModel.Symbols
            |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
            |> function
                | Some x -> x.Id
                | _ -> failwithf "TestDrawBlock.getSymId: symLabel (%A) not found" symLabel

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

        // Flip a symbol
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

        // ----------------------Circuit Builders DSL----------------------
        type TestCompType = 
        | TAnd
        | TOr
        | TNot
        | TMux2
        | TMux4
        | TInput
        | TOutput
        | TCustomComp

        /// adapted from EdStem: minimal DSL for placing symbols on the sheet
        let minimalDSL (initSheetModel: SheetT.Model) (placers: (SheetT.Model -> Result<SheetT.Model,'a>) list) : Result<SheetT.Model,'a> = 
            (Ok initSheetModel, placers)
            ||> List.fold (fun circuitRes placer -> Result.bind placer circuitRes)

        let initCCSheet =
            [
                placeSymbol "S1" (Input1(1, None)) {X=middleOfSheet.X-150.; Y=middleOfSheet.Y};
                placeSymbol "S2" (Input1(1, None)) {X=middleOfSheet.X-150.; Y=middleOfSheet.Y+100.};
                placeSymbol "S3" (Input1(1, None)) {X=middleOfSheet.X-150.; Y=middleOfSheet.Y+200.};
                placeSymbol "S4" (Input1(1, None)) {X=middleOfSheet.X-150.; Y=middleOfSheet.Y+300.};
                placeSymbol "X1" (Output(1)) {X=middleOfSheet.X+150.; Y=middleOfSheet.Y};
                placeSymbol "X2" (Output(1)) {X=middleOfSheet.X+150.; Y=middleOfSheet.Y+10.};
                placeSymbol "X3" (Output(1)) {X=middleOfSheet.X+150.; Y=middleOfSheet.Y+20.};
                placeSymbol "X4" (Output(1)) {X=middleOfSheet.X+150.; Y=middleOfSheet.Y+300.};
            ]
            |> minimalDSL initSheetModel
            |> getOkOrFail

        /// DSL for symbol placer, automatically names the symbol using a given index
        let symPlacerDSL (idx: int) (compType: ComponentType) (pos: XYPos) (model: Model) (sheetModel: SheetT.Model) : Result<SheetT.Model, string> =
            match compType with
            | Custom(_) -> placeCustomSymbol (sprintf "S%d" idx) initCCSheet "custom" model {X=1.0; Y=1.0} pos sheetModel
            | comp -> placeSymbol (sprintf "S%d" idx) comp pos sheetModel
            // TODO: input and output symbols have only one of input/output port
            // need an algorithm to conenct them

        let wirePlacerDSL (sourceSymLabel: string) (targetSymLabel: string) (sourceIdx) (targetIdx) (sheetModel: SheetT.Model) : Result<SheetT.Model, string> =
            sheetModel
            |> placeWire (portOf sourceSymLabel sourceIdx) (portOf targetSymLabel targetIdx) 

        
            
//--------------------------------------------------------------------------------------------------//
//---------------------------------------- Predefined Gen<'a> and Helpers---------------------------//
//--------------------------------------------------------------------------------------------------//
    module Generator =
        open System
        open Builder

        // ---------------------- Generator Helpers ----------------------
        let makeTuple a b = (a, b)

        /// From Ed: Fischer-Yates shuffle algorithm
        /// Returns a random shuffled array without changing the input array
        let shuffleAGen (rng: System.Random) arrayToShuffle: 'a array =
            let tmpA = Array.copy arrayToShuffle
            for i = 0 to tmpA.Length - 1 do 
                let r = rng.Next(i, tmpA.Length);
                (tmpA[i],tmpA[r])
                |> fun (iv, rv) -> tmpA[r] <- iv;  tmpA[i]  <- rv
            tmpA

        /// Chunk the generator into smaller arrays of equal lengths
        let chunkShuffledGen (gen: Gen<'a>) (seed: int) (length: int) =
            gen
            |> toArray
            |> shuffleAGen (Random seed) // random by test number
            |> Array.chunkBySize length
            |> fromArray

        // ====================== Positions Generator ======================

        /// Sample data based on 11 equidistant points on a horizontal line
        let horizLinePositions =
            fromList [-100..20..100]
            |> map (fun n -> middleOfSheet + {X=float n; Y=0.})

        let gridPositions n : Gen<XYPos> =
            let genX = fromList [-n..20..n]
            let genY = fromList [-n..20..n]
            (genX, genY)
            ||> product (fun x y -> middleOfSheet + {X=float x; Y=float y})

        let gridPosGen (n: int) (interval: int) : Gen<XYPos> =
            let genX: Gen<int> = fromList [-n..interval..n]
            let genY = fromList [-n..interval..n]
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

        /// Gen samples incorporating two flips
        let flipOnlySamples: Gen<{| AndPos: XYPos; Flip1: SymbolT.FlipType option; Flip2: SymbolT.FlipType option |}> =
            let flips = fromList [Some SymbolT.FlipType.FlipHorizontal; Some SymbolT.FlipType.FlipVertical; None]
            gridPositions 100
            |> product makeTuple flips  
            |> product makeTuple flips
            // This final stage makes the output more readable in an anonymous record
            |> map (fun (flip1, (flip2, (andPos)))->
                {|
                    Flip1=flip1;
                    Flip2=flip2;
                    AndPos=andPos
                |})

        type GenCompStates = {
            Comp: ComponentType; 
            Pos: XYPos; 
            Flip: SymbolT.FlipType option; 
            Rotate: Rotation option
        }

        let cctype = {
                    Name = "custom"
                    InputLabels = []
                    OutputLabels = []
                    Form = None
                    Description = None
                }
        let comps = fromList [GateN(And,2); GateN(Or,2); Not; Mux2; Mux4; Custom(cctype)] // TODO: TInput; TOutput; not yet supported
        let flips = fromList [Some SymbolT.FlipType.FlipHorizontal; Some SymbolT.FlipType.FlipVertical; None]
        let rotates = fromList [Some Degree90; Some Degree180; Some Degree270; None]

        ///<summary> AUTHOR hn621 - Random Gen samples: component, position, flips, rotations</summary>
        let randomComponentSamples : Gen<GenCompStates> =
            rotates
            |> product makeTuple flips
            |> product makeTuple (gridPosGen 300 100)
            |> product makeTuple comps
            |> map (fun (comp, (pos, (flip, rotate))) -> 
                    {
                        Comp=comp;
                        Pos=pos;
                        Flip=flip;
                        Rotate=rotate;
                    }
            )

        ///<summary> AUTHOR hn621 - Random Gen samples: component, position, flips, rotations</summary>
        let randomComponentSamplesNoRotate : Gen<GenCompStates> =
            flips
            |> product makeTuple (gridPosGen 400 100)
            |> product makeTuple comps
            |> map (fun (comp, (pos, flip)) -> 
                    {
                        Comp=comp;
                        Pos=pos;
                        Flip=flip;
                        Rotate=None;
                    }
            )
        
    open Builder
    open Generator
    // ====================== Fixed Circuit Generator ======================

    let makeTest1Circuit (andPos:XYPos) =
        [
            placeSymbol "G1" (GateN(And,2)) {X=middleOfSheet.X+100.;Y=middleOfSheet.Y-100.};
            placeSymbol "S1" (Input1(1, None)) {X=middleOfSheet.X-150.;Y=middleOfSheet.Y};
            placeSymbol "S2" (Input1(1, None)) {X=middleOfSheet.X-150.;Y=middleOfSheet.Y+100.};
            placeSymbol "MUX1" Mux2 {X=middleOfSheet.X-100.;Y=middleOfSheet.Y-100.};
            placeSymbol "MUX2" Mux2 middleOfSheet;
            flipSymbol "MUX2" (Some SymbolT.FlipType.FlipVertical) >> Ok;
            placeWire (portOf "S2" 0) (portOf "MUX2" 2);
            placeWire (portOf "MUX1" 0) (portOf "MUX2" 0);
            placeWire (portOf "S1" 0) (portOf "MUX2" 1);
            placeWire (portOf "MUX2" 0) (portOf "G1" 0);
            placeWire (portOf "MUX1" 0) (portOf "G1" 1);
        ]
        |> minimalDSL initSheetModel
        |> getOkOrFail

    let makeTest2Circuit (model:Model) (andPos:XYPos) = //(dispatch: Dispatch<Msg>)
        // TODO: fix the custom component placing, currently missing the ports
        let project = Option.get model.CurrentProj
        let curSheetName = project.OpenFileName
        let sheetNames = 
            project.LoadedComponents 
            |> List.map (fun ldc -> ldc.Name)
            |> List.filter (fun name -> name <> curSheetName)

        let ccEmptySheet = DiagramMainView.init().Sheet
        let ccSheet: SheetT.Model =
            [
                placeSymbol "S1" (Input1(1, None)) {X=middleOfSheet.X-150.; Y=middleOfSheet.Y};
                placeSymbol "S2" (Input1(1, None)) {X=middleOfSheet.X-150.; Y=middleOfSheet.Y+100.};
                placeSymbol "S3" (Input1(1, None)) {X=middleOfSheet.X-150.; Y=middleOfSheet.Y+200.};
                placeSymbol "S4" (Input1(1, None)) {X=middleOfSheet.X-150.; Y=middleOfSheet.Y+300.};
                placeSymbol "X1" (Output(1)) {X=middleOfSheet.X+150.; Y=middleOfSheet.Y};
                placeSymbol "X2" (Output(1)) {X=middleOfSheet.X+150.; Y=middleOfSheet.Y+10.};
                placeSymbol "X3" (Output(1)) {X=middleOfSheet.X+150.; Y=middleOfSheet.Y+20.};
                placeSymbol "X4" (Output(1)) {X=middleOfSheet.X+150.; Y=middleOfSheet.Y+300.};
            ]
            |> minimalDSL ccEmptySheet
            |> getOkOrFail
        let ccSheetName = "custom" // sheetNames.Head
        
        // CustomCompPorts.printSheetNames model
        // printfn $"{ccSheetName}"

        [
            placeCustomSymbol "CC1" ccSheet ccSheetName  model {X=1.0; Y=1.0} {X=middleOfSheet.X-150.;Y=middleOfSheet.Y} // andPos
            placeCustomSymbol "CC2" ccSheet ccSheetName  model {X=1.0; Y=1.0}  {X=middleOfSheet.X+150.;Y=middleOfSheet.Y}
            placeWire (portOf "CC1" 0) (portOf "CC2" 3);
            placeWire (portOf "CC1" 1) (portOf "CC2" 2);
            placeWire (portOf "CC1" 2) (portOf "CC2" 0);
            placeWire (portOf "CC1" 3) (portOf "CC2" 1);
        ]
        |> minimalDSL initSheetModel
        |> getOkOrFail

    let makeTest5Circuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "MUX1" Mux2 middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 0) )
        |> getOkOrFail
    
    // ====================== Random Sheet Generator ======================
    let makeRandomCircuit (model:Model) (samples: GenCompStates array) =
        let folder acc (sample: GenCompStates) = 
            let (sheetModel, idx) = acc
            let newSheet =
                [
                    // place component and set states
                    symPlacerDSL idx sample.Comp sample.Pos model;
                    flipSymbol (sprintf "S%d" idx) sample.Flip >> Ok;
                    rotateSymbol (sprintf "S%d" idx) sample.Rotate >> Ok;
                    // set wire connections
                    if idx > 0 then 
                        wirePlacerDSL (sprintf "S%d" (idx-1)) (sprintf "S%d" (idx)) 0 0
                    else id >> Ok;
                ]
                |> minimalDSL sheetModel
                |> getOkOrFail
            (newSheet, idx+1)
        
        samples
        |> Array.fold folder (initSheetModel, 0)
        |> fst

    let makeTest4Circuit (sample: {|
        Flip1: SymbolT.FlipType option;
        Flip2: SymbolT.FlipType option;
        AndPos: XYPos
    |}) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) sample.AndPos
        |> match sample.Flip2 with
            | Some f -> Result.map (flipSymbol "G1" (Some SymbolT.FlipType.FlipHorizontal))
            | None -> id
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> match sample.Flip1 with
            | Some f -> Result.map (flipSymbol "G1" (Some SymbolT.FlipType.FlipHorizontal))
            | None -> id
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> getOkOrFail

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
            
        let test1 testNum firstSample dispatch model =
            let displayOnFail = displayAll
            let generator = gridPosGen 1 2
            runTestOnSheets
                "DisplayAll: MUX+AND Unoptimized"
                firstSample
                generator
                makeTest1Circuit
                Asserts.failOnAllTests
                dispatch
                displayOnFail
            |> recordPositionInTest testNum dispatch

        let test1Opt testNum firstSample dispatch model =
            let sheetMaker = makeTest1Circuit >> optimizePortOrder   
            let displayOnFail = displayAll
            let generator = gridPosGen 1 2
            runTestOnSheets
                "DisplayAll: MUX+AND Optimized"
                firstSample
                generator
                sheetMaker
                Asserts.failOnAllTests
                dispatch
                displayOnFail
            |> recordPositionInTest testNum dispatch

        let test2 testNum firstSample dispatch model =
            let sheetMaker = makeTest2Circuit model
            let displayOnFail = displayAll
            let generator = gridPosGen 1 2
            runTestOnSheets
                "DisplayAll: Custom Symbol"
                firstSample
                generator
                sheetMaker
                Asserts.failOnAllTests
                dispatch
                displayOnFail
            |> recordPositionInTest testNum dispatch

        let test2Opt testNum firstSample dispatch model =
            let sheetMaker = makeTest2Circuit model
            let displayOnFail = displayAll
            let generator = gridPosGen 1 2
            runTestOnSheets
                "DisplayAll: Custom Symbol"
                firstSample
                generator
                (sheetMaker >> optimizePortOrder)
                Asserts.failOnAllTests
                dispatch
                displayOnFail
            |> recordPositionInTest testNum dispatch

        let test4 testNum firstSample dispatch model =
            let displayOnFail = displayAll
            let myRandomSample = shuffleAGen <| Random(1)
            let generator = 
                flipOnlySamples
                |> toArray
                |> myRandomSample
                |> fromArray
            
            runTestOnSheets
                "DisplayAll: Random Flip DFF+AND"
                firstSample
                generator
                makeTest4Circuit
                Asserts.failOnAllTests
                dispatch
                displayOnFail
            |> recordPositionInTest testNum dispatch

        let testRandomComp testNum firstSample dispatch model =
            let nComponents = 10 // note that the position generation is fixed, too large nComponents will not have non-overlapping test cases
            let sheetMaker = makeRandomCircuit model
            let displayOnFail = displayAll
            let generator = nComponents |> chunkShuffledGen (randomComponentSamples) 1
            // this assertion fails on all tests without symbol intersection!
            let assertion (sample: int) (sheetModel: SheetT.Model) = 
                sheetModel
                |> Asserts.failOnSymbolIntersectsSymbol sample
                |> function
                    | Some str -> None
                    | None -> Some "Random Component Test Failed"
            
            runTestOnSheets
                "DisplayAll: Random N Components"
                firstSample
                generator
                sheetMaker
                assertion
                dispatch
                displayOnFail
            |> recordPositionInTest testNum dispatch

        let testRandomCompNoRotate testNum firstSample dispatch model =
            let nComponents = 10 // note that the position generation is fixed, too large nComponents will not have non-overlapping test cases
            let sheetMaker = makeRandomCircuit model
            let displayOnFail = displayAll
            let generator = nComponents |> chunkShuffledGen (randomComponentSamplesNoRotate) 1
            // this assertion fails on all tests without symbol intersection!
            let assertion (sample: int) (sheetModel: SheetT.Model) = 
                sheetModel
                |> Asserts.failOnSymbolIntersectsSymbol sample
                |> function
                    | Some str -> None
                    | None -> Some "Random Component Test Failed"
            
            runTestOnSheets
                "DisplayAll: Random N Components No Rotate"
                firstSample
                generator
                sheetMaker
                assertion
                dispatch
                displayOnFail
            |> recordPositionInTest testNum dispatch

        /// <summary>AUTHOR hn621 - Prints out the average time to beautify a sheet of n components, where n is in range [5..5..25]</summary>
        let testBeautifyTimePerformance testNum firstSample dispatch model =
            let sheetMaker = makeRandomCircuit model
            let beautifier = optimizePortOrder
            let nGenerator = chunkShuffledGen (randomComponentSamples) 1
            let nComponents = [5..5..25]

            printfn $"Now testing Beautify Time Performance..."

            nComponents
            |> List.map nGenerator
            |> List.map (fun gen -> gen.Data 0)
            |> List.map sheetMaker // prepare the sheetModel before beautify
            |> List.mapi (
                fun idx elm -> 
                    recordExecutionTimeStats (sprintf "n_component=%d" (nComponents.[idx])) beautifier elm |> ignore
                )
            |> ignore

            executionStats
            |> Map.toList
            |> List.map (fun (key, value) -> (printfn "%s, avg_time=%.3f ms" key value.Av))
            |> ignore

            executionStats <- Map []

        /// This test is to apply beautification and calculate metric
        let applyBeautify beautifyFunc testNum firstSample dispatch model =
            let beforeCount = numOfWireRightAngleCrossings model.Sheet
            let sheetAfter = beautifyFunc model.Sheet
            let afterCount = numOfWireRightAngleCrossings sheetAfter
            
            metricDisplay displayMetrics sheetAfter model.Sheet
            printfn $"after crossings: {afterCount}, before: {beforeCount}"
            showSheetInIssieSchematic sheetAfter dispatch

        /// This test is to display info on the current model as it is in Issie
        let displayCurSheet testNum firstSample dispatch model =
            display displayAll model.Sheet
            ()

        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Model -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [
                "Apply Beautify D1", (applyBeautify (sheetAlignScale 3)) // TODO: add actual function
                "Apply Beautify D2", (applyBeautify optimizePortOrder) 
                "Apply Beautify D3", (applyBeautify wireLabelBeautify)
                "Build: Random Components No Rotate", testRandomCompNoRotate 
                "Build: Random Components", testRandomComp
                "Test: Statistics", testRandomComp // TODO: add actual function
                "Test: Edge Case", testRandomComp // TODO: add actual function
                "Test: Beautify Time Complexity", testBeautifyTimePerformance
                "Next Test Error", fun _ _ _ _ -> printf "Next Error:" // Go to the nexterror in a test
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
                nextError testsToRunFromSheetMenu[state.LastTestNumber] (state.LastTestSampleIndex+1) dispatch model
            | "Next Test Error", None ->
                printf "Test Finished"
                ()
            | _ ->
                func testIndex 0 dispatch model
