module TestDrawBlockD1

open Elmish
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open CommonTypes
open Optics
open Optics.Operators
open Helpers
open ModelType
open SheetBeautifyHelpers
open SheetBeautifyD1
open GenerateData
open System


module D1TestLib =

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
    let runD1Tests (test: Test<'a>) : TestResult<'a>  =
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

module TestD1 =
    open SheetBeautify
    open D1TestLib
    open EEExtensions
    open Sheet.SheetInterface

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
            
        

            // Rotate a symbol
            let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (SheetT.Model) =
                let symLabel = String.toUpper symLabel
                let symbol = model.Wire.Symbol.Symbols
                match mapValues symbol |> Array.tryFind (fun sym -> sym.Component.Label = symLabel) with
                | Some sym ->
                    let rotatedSymbol = SymbolResizeHelpers.rotateSymbol rotate sym
                    model
                    |> Optic.set (symbolModel_ >-> SymbolT.symbolOf_ sym.Id) rotatedSymbol
                    |> SheetUpdateHelpers.updateBoundingBoxes
                | None -> model


            // Flip a symbol
            let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : (SheetT.Model) =
                let symLabel = String.toUpper symLabel
                let symbol = model.Wire.Symbol.Symbols
                match mapValues symbol |> Array.tryFind (fun sym -> sym.Component.Label = symLabel) with
                | Some sym ->
                    let flippedSymbol = SymbolResizeHelpers.flipSymbol flip sym
                    model
                    |> Optic.set (symbolModel_ >-> SymbolT.symbolOf_ sym.Id) flippedSymbol
                    |> SheetUpdateHelpers.updateBoundingBoxes
                | None -> model


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
            let runD1TestOnSheets
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
                    |> runD1Tests
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

    let findStraightWireSeg (model: SheetT.Model) (wire: BusWireT.Wire) : bool =
        let segments = SegmentHelpers.visibleSegments wire.WId model
        List.length segments = 1




 //================================== GENERATE COORDINATES ====================================//

     /// Sample data based on 11 equidistant points on a horizontal line
    let horizLinePositions =
        fromList [-100..20..100]
        |> map (fun n -> middleOfSheet + {X=float n; Y=0.})

    let generateSampleData =
        product (fun x y -> middleOfSheet + {X = float x; Y = float y})
            (fromList [-100..20..100])
            (fromList [-100..20..100])

    let generateSmallDeviations : Gen<XYPos> =
        let rnd = Random()
        let beginningSeq = -40
        let endSeq = beginningSeq * -1
        product (fun x y ->
                    if x = beginningSeq && y = endSeq then
                        { X = 0.0; Y = 0.0 } // Zero case for first set of values
                    else
                    let deviationX = rnd.NextDouble() * 15.0 - 10.0
                    let deviationY = rnd.NextDouble() * 15.0 - 10.0 
                    { X = float x + deviationX; Y = float y + deviationY})
                (fromList [beginningSeq..2..endSeq])
                (fromList [beginningSeq..2..endSeq])
            
    let filterOverlap (pos1: XYPos) =
        // Define the size of the components
        let componentSize = 10.0
        
        let overlapX = abs(pos1.X - middleOfSheet.X) < componentSize
        let overlapY = abs(pos1.Y - middleOfSheet.Y) < componentSize
        
        // Return true if there's no overlap, false otherwise
        not (overlapX && overlapY)

    let filteredSampleData =
        generateSampleData
        |> GenerateData.filter filterOverlap

    let filteredSampleDataWithDeviation =
        generateSmallDeviations
        |> GenerateData.filter filterOverlap

    let rnd = Random()


    //==================================GENERATE COORDINATES ==============================//


    //----------------------- RANDOM CIRCUITS FOR CHECKING ISSIE FUNCTIONALITY ----------------------------------------- //

    /// demo test circuit consisting of a DFF & And gate
    let makeTest1Circuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 1) )
        |> getOkOrFail  

    let makeTest2Circuit (andPos:XYPos) =
        let model =
            initSheetModel
            |> placeSymbol "G1" (GateN(And,2)) andPos
            |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
            |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
            |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 1) )
            |> getOkOrFail

        let alignedModel = Beautify.sheetMultiply model
       
        alignedModel

    let makeTest3Circuit (andPos: XYPos) =
        initSheetModel
        |> placeSymbol "Component1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "Component2" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "Component1" 0) (portOf "Component2" 0))
        |> Result.bind (placeWire (portOf "Component2" 0) (portOf "Component1" 0))
        |> getOkOrFail




//========================================================PROJECT WORK===========================================================================//

//============================= CIRCUIT HELPERS ==============================//

    ///D.U for symbol pos
    type SymbolPositions = {
        APos: XYPos
        BPos: XYPos
        CPos: XYPos
        M2Pos: XYPos
        M3Pos: XYPos
        S1Pos: XYPos
        S2Pos: XYPos
        CC2Pos: XYPos
    }

    /// function to set symbol positions
    let setSymbolPositions (andPos: XYPos) =

        let aPos = middleOfSheet + { X = -100.0; Y = -50.0 } + {X = andPos.X ; Y = andPos.Y}
        let bPos = middleOfSheet + { X = -100.0; Y = 50.0 } + {X = andPos.X; Y = -andPos.Y}
        let cPos = middleOfSheet + { X = 120.0; Y = 0.0 } + {X = -andPos.X; Y = andPos.Y}
        let m2Pos = middleOfSheet + {X = 90.0; Y = 100.0 } 
        let m3Pos = middleOfSheet + {X = 200.0; Y = 120.0} 
        let s1Pos = middleOfSheet + {X = -30.0; Y = 300.0} + { X = andPos.X ; Y = andPos.Y}
        let s2Pos = middleOfSheet + {X = -40.0; Y = 200.0} + { X = andPos.X ; Y = andPos.Y}
        let cc2Pos = middleOfSheet + {X = 30.0; Y = 0.0} + { X = andPos.X ; Y = andPos.Y}
        
        { APos = aPos; BPos = bPos; CPos = cPos; M2Pos = m2Pos; M3Pos = m3Pos; S1Pos = s1Pos; S2Pos = s2Pos; CC2Pos = cc2Pos }

    // Define a sample project
 (*   let customProject = {
        ProjectPath = "C:\Users\talha\HLP_Project\HLP_project"
        OpenFileName = "custcomp"
        WorkingFileName = Some "main"
        LoadedComponents = 
    }  *)

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

    // Referenced from SheetBeutifyHelpers
    let numOfVisRightAngles (model: SheetT.Model) : int =
        let nets = SegmentHelpers.allWireNets model
        let numWires = nets |> List.sumBy (fun (source,wires) -> wires.Length)
        let distinctSegs =
            nets
            |> List.collect (fun (_, net) -> SegmentHelpers.getVisualSegsFromNetWires true model net)
        // every visual segment => right-angle bend except for the first (or last) in a wire
        distinctSegs.Length - numWires

// ======================================= D1 TEST CIRCUITS ============================================== //

    ///Create circuit containing 2 input ports and MUX2
    let makeD1Test1Circuit (andPos: XYPos) =
        let symbolPositions = setSymbolPositions andPos

        let model = 
            initSheetModel
            |> placeSymbol "A" (Input1(1, None)) symbolPositions.APos
            |> Result.bind (placeSymbol "B" (Input1(1, None)) symbolPositions.BPos)
            |> Result.bind (placeSymbol "C" (Output(1)) symbolPositions.CPos) 
            |> Result.bind (placeSymbol "MUX1" Mux2 middleOfSheet) 
            |> Result.bind (placeWire (portOf "A" 0) (portOf "MUX1" 0))
            |> Result.bind (placeWire (portOf "B" 0) (portOf "MUX1" 1))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "C" 0) ) 
            |> getOkOrFail

        model


    ///Create circuit containing 4x Input Ports, 3x MUX2
    let makeD1Test2Circuit (andPos: XYPos) =
        let symbolPositions = setSymbolPositions andPos

        let model = 
            initSheetModel
            |> placeSymbol "A" (Input1(1, None)) symbolPositions.APos
            |> Result.bind (placeSymbol "B" (Input1(1, None)) symbolPositions.BPos)
            |> Result.bind (placeSymbol "MUX2" Mux2 symbolPositions.M2Pos)
            |> Result.bind (placeSymbol "MUX3" Mux2 symbolPositions.M3Pos)
            |> Result.bind (placeSymbol "S1" (Input1(1, None)) symbolPositions.S1Pos)
            |> Result.bind (placeSymbol "S2" (Input1(1, None)) symbolPositions.S2Pos)
            |> Result.bind (placeSymbol "MUX1" Mux2 middleOfSheet) 
            |> Result.bind (placeWire (portOf "A" 0) (portOf "MUX1" 0))
            |> Result.bind (placeWire (portOf "B" 0) (portOf "MUX1" 1))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
            |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "MUX3" 0))
            |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
            |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX3" 1))
            |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX1" 2))
            |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
            |> getOkOrFail

        model

    //Same as above but with rotated S1
    let makeD1Test3Circuit (andPos: XYPos) =
        let symbolPositions = setSymbolPositions andPos

        let model = 
            initSheetModel
            |> placeSymbol "A" (Input1(1, None)) symbolPositions.APos
            |> Result.bind (placeSymbol "B" (Input1(1, None)) symbolPositions.BPos)
            |> Result.bind (placeSymbol "MUX2" Mux2 symbolPositions.M2Pos)
            |> Result.bind (placeSymbol "C" (Output(1)) symbolPositions.CPos)
            |> Result.bind (placeSymbol "S1" (Input1(1, None)) symbolPositions.S1Pos)
            |> Result.map (rotateSymbol "S1" Degree90)
            |> Result.bind (placeSymbol "S2" (Input1(1, None)) symbolPositions.S2Pos)
            |> Result.bind (placeSymbol "MUX1" Mux2 middleOfSheet) 
            |> Result.bind (placeWire (portOf "A" 0) (portOf "MUX1" 0))
            |> Result.bind (placeWire (portOf "B" 0) (portOf "MUX1" 1))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
            |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "C" 0))
            |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX1" 2))
            |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
            |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
            |> getOkOrFail

        model

    // Cannot remember what this is for
    let makeD1TestMainCircuit (andPos: XYPos) =
        let symbolPositions = setSymbolPositions andPos

        let model = 
            initSheetModel
            |> placeSymbol "A" (Input1(1, None)) symbolPositions.APos
            |> Result.bind (placeSymbol "B" (Input1(1, None)) symbolPositions.BPos)
            |> Result.bind (placeSymbol "MUX2" Mux2 symbolPositions.M2Pos)
            |> Result.bind (placeSymbol "MUX3" Mux2 symbolPositions.M3Pos)
            |> Result.bind (placeSymbol "S1" (Input1(1, None)) symbolPositions.S1Pos)
            |> Result.bind (placeSymbol "S2" (Input1(1, None)) symbolPositions.S2Pos)
            |> Result.bind (placeSymbol "MUX1" Mux2 middleOfSheet) 
            |> Result.bind (placeWire (portOf "A" 0) (portOf "MUX1" 0))
            |> Result.bind (placeWire (portOf "B" 0) (portOf "MUX1" 1))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
            |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "MUX3" 0))
            |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
            |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX3" 1))
            |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX1" 2))
            |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
            |> getOkOrFail

        model

// ========================================================== ASSERTS ========================================================= //

    module Asserts =

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

        //referenced from indiv-ccc121
        let failOnLongerWireRouting (threshold: float) (sample: int) (sheet: SheetT.Model) = 
            let oldRoutingLength = calcVisWireLength sheet
            let newRoutingLength = Beautify.sheetAlignScale sheet |> calcVisWireLength 
            match newRoutingLength, oldRoutingLength with
            | _ when newRoutingLength - oldRoutingLength > threshold -> 
                Some $"Wire routing length exceeded threshold in Sample {sample}. From {oldRoutingLength} to {newRoutingLength}"
            | _ -> 
                let printSuccess x = printf $"Routing length went from {oldRoutingLength} to {newRoutingLength}" ; x
                printSuccess None

        //referenced from indiv-ccc121
        let failOnComponentOverlap (sample: int) (sheet: SheetT.Model) =
            let numOfCompOverlap = Beautify.sheetAlignScale sheet |> numOfIntersectedSymPairs 
            match numOfCompOverlap with
            | _ when numOfCompOverlap > 0 -> 
                Some $"{numOfCompOverlap} component intersections detected in Sample {sample}"
            | _ -> None

        //referenced from indiv-ccc121
        let failOnFewerStraightWires (sample:int) (sheet: SheetT.Model) = 
            let oldStraightLinesCount = numOfStraightWires sheet
            let newStraightLinesCount = Beautify.sheetAlignScale sheet |> numOfStraightWires
            match oldStraightLinesCount, newStraightLinesCount with
            | _ when newStraightLinesCount < oldStraightLinesCount ->
                Some $"Straight lines decreased. Straight lines before: {oldStraightLinesCount}. Straight lines after: {newStraightLinesCount} detected in Sample {sample}"
            | _ -> 
                let printSuccess x = printf $"Straight lines increased from {oldStraightLinesCount} to {newStraightLinesCount}" ; x
                printSuccess None

// ========================================================== ASSERTS ========================================================= //


// ========================================================== TESTS ========================================================= //

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
            runD1TestOnSheets
                "Horizontally positioned AND + DFF: fail on sample 0"
                firstSample
                horizLinePositions
                makeTest1Circuit
                (Asserts.failOnSampleNumber 0)
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail on sample 10
        let test2 testNum firstSample dispatch =
            runD1TestOnSheets
                "Horizontally positioned AND + DFF: fail on sample 10"
                firstSample
                horizLinePositions
                makeTest2Circuit
                (Asserts.failOnSampleNumber 10)
                dispatch
            |> recordPositionInTest testNum dispatch

        let test3 testNum firstSample dispatch =
            runD1TestOnSheets
                "Test wire routing between two components"
                firstSample
                filteredSampleData
                makeTest3Circuit
                Asserts.failOnWireIntersectsSymbol
                dispatch
            |> recordPositionInTest testNum dispatch

// --------------------------- D1 TESTS ------------------------------------ //

        let D1Test1 testNum firstSample dispatch =
            runD1TestOnSheets
                "Test Near Straight Wire"
                firstSample
                filteredSampleDataWithDeviation
                makeD1Test1Circuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        let D1Test2 testNum firstSample dispatch =
            runD1TestOnSheets
                "Test Multiply"
                firstSample
                filteredSampleDataWithDeviation
                makeD1Test2Circuit
                Asserts.failOnComponentOverlap
                dispatch
            |> recordPositionInTest testNum dispatch

        let D1Test3 testNum firstSample dispatch =
            runD1TestOnSheets
                "Test Multiply"
                firstSample
                filteredSampleDataWithDeviation
                makeD1Test3Circuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Main Test for singly and multiply
        let D1TestMain testNum firstSample dispatch =
            runD1TestOnSheets
                "D1 Test with both singly and multiply connected components"
                firstSample
                filteredSampleDataWithDeviation
                makeD1TestMainCircuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch
