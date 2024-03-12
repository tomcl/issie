module TestDrawBlockSimpleSymbol
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
 
 
            
(******************************************************************************************
   This submodule contains a set of functions that enable random data generation
   for property-based testing of Draw Block wire routing functions.
   basic idea.
   1. Generate, in various ways, random circuit layouts
   2. For each layout apply smartautoroute to regenerate all wires
   3. Apply check functions to see if the resulting wire routing obeys "good layout" rules.
   4. Output any layouts with anomalous wire routing
*******************************************************************************************)
module SimpleSymbolTesting =
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
    open SheetBeautifyHelpers

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
    

    let flipPortMaps (sym: SymbolT.Symbol) : SymbolT.PortMaps = 
        let portOrientation = 
            sym.PortMaps.Orientation |> Map.map (fun id side -> SymbolResizeHelpers.flipSideHorizontal side)

        let flipPortList currPortOrder side =
            currPortOrder |> Map.add (SymbolResizeHelpers.flipSideHorizontal side ) sym.PortMaps.Order[side]

        let portOrder = 
            (Map.empty, [Top; Left; Bottom; Right]) ||> List.fold flipPortList
            |> Map.map (fun edge order -> List.rev order)  

        {Order=portOrder;Orientation=portOrientation}   
        

    /// Identify a port from its component label and number.
    /// Usually both an input and output port will mathc this, so
    /// the port is only unique if it is known to be input or output.
    /// used to specify the ends of wires, since tehee are known to be
    /// connected to outputs (source) or inputs (target).
    type SymbolPort = { Label: string; PortNumber: int }

    /// convenience function to make SymbolPorts
    let portOf (label:string) (number: int) =
        {Label=label; PortNumber = number}

    type SimpleSymbol = {
        SymLabel : string
        CompType : ComponentType
        Position: XYPos
        STransform: STransform
    }

    type SimpleConnection = {
        Source: SymbolPort
        Target: SymbolPort
    }

    type TestModel = {
        SimpleSymbols : SimpleSymbol List
        Connections : SimpleConnection List
    }


    let createSimpleSymbol' (label: string) (compType: ComponentType) (position: XYPos) (sTransform: STransform) =
        { SymLabel = label;
        CompType = compType;
        Position = position;
        STransform = sTransform }

        
    let getSimSymbolMap (model: SheetT.Model) : Map<ComponentId, SimpleSymbol> = 
        let extractValues (label: string) (symbol: SymbolT.Symbol) : SimpleSymbol= 
            { SymLabel = label 
              CompType = symbol.Component.Type
              Position = { X = symbol.Pos.X + float symbol.Component.W / 2.0; Y = symbol.Pos.Y + float symbol.Component.H / 2.0 }
              STransform = symbol.STransform }

        Optic.get SheetT.symbols_ model
        |> Map.toList
        |> List.mapi (fun index symbolMap -> // have access to index here so can add to symLabel if desired
                let symbol = snd symbolMap
                (fst symbolMap, extractValues (symbol.Component.Label) symbol))
        |> Map.ofList
    

    let getSimpleConnections (model: SheetT.Model) (symbolMap: Map<ComponentId, SimpleSymbol>) = 
        let getSymLabel (hostId: ComponentId) =
            symbolMap
            |> Map.find hostId
            |> fun sym -> sym.SymLabel
            
        let getPortIndex (port: Port) (portList: List<Port>) = 
            portList
            |> List.findIndex (fun elm -> port.Id = elm.Id)
        
        let getSymbolPort (portType: PortType) (port: Port) =
            let compId = ComponentId port.HostId
            let Symbol = Optic.get (SheetT.symbolOf_ compId) model

            let portNum = 
                match portType with
                    | PortType.Input -> getPortIndex port Symbol.Component.InputPorts
                    | PortType.Output -> getPortIndex port Symbol.Component.OutputPorts

            { Label = getSymLabel compId
              PortNumber = portNum }

        BusWire.extractConnections model.Wire
        |> List.map (fun conn ->
             { Source = getSymbolPort PortType.Output conn.Source
               Target = getSymbolPort PortType.Input conn.Target })
    
    
    let getTestModel (model: SheetT.Model) = 
        let simpleSymbolMap = getSimSymbolMap model

        { SimpleSymbols = Map.values simpleSymbolMap |> Array.toList
          Connections = getSimpleConnections model simpleSymbolMap  }

//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
    module Builder =

        /// <summary>
        /// Output a sheet model with a SimpleSymbol added to it.
        /// </summary>
        /// <param name="model">The Sheet model into which the new symbol is added.</param>
        /// <param name="simSymbol">The SimpleSymbol to be added to the model.</param>
        let placeSimpleSymbol (simSymbol: SimpleSymbol) (model: SheetT.Model) : Result<SheetT.Model, string> =
            let symLabel = String.toUpper simSymbol.SymLabel // make label into its standard casing
            let symModel, symId = SymbolUpdate.addSymbol [] (model.Wire.Symbol) simSymbol.Position simSymbol.CompType symLabel
            let sym = symModel.Symbols[symId]

            let portMaps' = 
                if simSymbol.STransform.Flipped 
                then
                    flipPortMaps sym
                    |> SymbolResizeHelpers.rotatePortInfo simSymbol.STransform.Rotation
                else
                    SymbolResizeHelpers.rotatePortInfo simSymbol.STransform.Rotation sym.PortMaps
            
            let sym' = sym
                      |> Optic.set symbol_flipped_ simSymbol.STransform.Flipped
                      |> Optic.set symbol_rotation_ simSymbol.STransform.Rotation
                      |> Optic.set SymbolT.portMaps_ portMaps'

            let symModel' = Optic.set (SymbolT.symbolOf_ symId) sym' symModel
            
            match simSymbol.Position with
            | {X=x;Y=y} when x > maxSheetCoord || y > maxSheetCoord ->
                Error $"symbol '{symLabel}' position {simSymbol.Position + sym.getScaledDiagonal} lies outside allowed coordinates"
            | _ ->
                model
                |> Optic.set symbolModel_ symModel'
                |> SheetUpdateHelpers.updateBoundingBoxes
                |> Ok
        

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
                     |> Optic.map busWireModel_ (BusWireSeparate.updateWireSegmentJumpsAndSeparations [newWire.WId])  
                     |> Ok


        /// <summary>
        /// Fold over a list of simple symbols and output a sheet model with all of them added.
        /// </summary>
        /// <param name="model">The Sheet model into which the new symbols are added.</param>
        /// <param name="simSymbolList">The list of SimpleSymbols to be added to the model.</param>
        let placeSimSymbolList (simSymbolList: List<SimpleSymbol>) (model: SheetT.Model) = 
            (Ok model,simSymbolList)
            ||> List.fold (fun curModel curSimSymbol -> Result.bind (placeSimpleSymbol curSimSymbol) curModel)
                    // match placeSimpleSymbol curModel curSimSymbol with
                    // | Ok model -> model
                    // | Error e -> curModel) // Failed to draw symbol so pass previous model
            

        let placeConnections (conns: List<SimpleConnection>) (model: SheetT.Model) =
            (Ok model,conns)
            ||> List.fold (fun curModel curConn -> Result.bind (placeWire curConn.Source curConn.Target) curModel) 
        

        /// Run the global wire separation algorithm (should be after all wires have been placed and routed)
        let separateAllWires (model: SheetT.Model) : SheetT.Model =
            model
            |> Optic.map busWireModel_ (BusWireSeparate.updateWireSegmentJumpsAndSeparations (model.Wire.Wires.Keys |> Seq.toList))

        let placeTestModel (testModel: TestModel) = 
            initSheetModel
            |> placeSimSymbolList testModel.SimpleSymbols
            |> Result.bind (placeConnections testModel.Connections)
            |> Result.map separateAllWires
            |> getOkOrFail
      

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
        let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (Result<SheetT.Model,string>) =
            let symbolMap = model.Wire.Symbol.Symbols
            let symbol =    
                mapValues symbolMap
                |> Array.find (fun sym -> caseInvariantEqual sym.Component.Label symLabel)

            let rotatedSymbol = SymbolResizeHelpers.rotateAntiClockByAng rotate symbol
            
            let updatedSymbolMap = 
                symbolMap
                |> Map.change rotatedSymbol.Id (fun x ->
                    match x with
                    | Some sym -> Some rotatedSymbol
                    | None -> None )

            // Optic.set SheetT.symbols_ symLabel
            model
            |> Optic.set SheetT.symbols_ updatedSymbolMap
            |> Ok 
        

        // Flip a symbol
        let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : (Result<SheetT.Model,string>) =
            let symbolMap = model.Wire.Symbol.Symbols
            let symbol =    
                mapValues symbolMap
                |> Array.find (fun sym -> caseInvariantEqual sym.Component.Label symLabel)

            let flippedSymbol = SymbolResizeHelpers.flipSymbol flip symbol
            
            let updatedSymbolMap = 
                symbolMap
                |> Map.change flippedSymbol.Id (fun x ->
                    match x with
                    | Some sym -> Some flippedSymbol
                    | None -> None )

            // Optic.set SheetT.symbols_ symLabel
            model
            |> Optic.set SheetT.symbols_ updatedSymbolMap
            |> Ok 
        

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


//------------------------------------------------------------------------------------------------//
//-------------------------Example assertions used to test sheets---------------------------------//
//------------------------------------------------------------------------------------------------//


    module Asserts =

        (* Each assertion function from this module has as inputs the sample number of the current test and the corresponding schematic sheet.
           It returns a boolean indicating (true) that the test passes or (false) that the test fails. The sample numbr is included to make it
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
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n,box)
            List.allPairs boxes boxes 
            |> List.exists (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
            |> (function | true -> Some $"Symbol outline intersects another symbol outline in Sample {sample}"
                         | false -> None)
        

        /// <summary>
        /// Assertion that takes 2 sheet models, one before and after beautify.
        /// If the number of right angles increased by beautification, this is considered a failure.
        /// If it's the same, it's not considered a failure; other metrics will be used here.
        /// </summary>
        /// <param name="sample">The sample number.</param>
        /// <param name="sheetBeforeBeautify">The sheet model before beautification.</param>
        /// <param name="sheetAfterBeautify">The sheet model after beautification.</param>
        /// <returns>
        /// A failure message if the number of right angles increased in the sample;
        /// otherwise, None.
        /// </returns>
        let failOnBeautifyIncreasesRightAngles (sample: int) (sheetBeforeBeautify: SheetT.Model) (sheetAfterBeautify: SheetT.Model) =
            let rAnglesBefore = numOfVisRightAngles sheetBeforeBeautify
            let rAnglesAfter = numOfVisRightAngles sheetAfterBeautify

            match rAnglesAfter > rAnglesBefore with
                | true -> Some $"Beautify increased no. right angles in Sample {sample}"
                | false -> None


//--------------------------------------------------------------------------------------------------//
//-------------------------------Example Test Circuits using Gen<'a> samples------------------------//
//--------------------------------------------------------------------------------------------------//

    open Builder
    /// Sample data based on 11 equidistant points on a horizontal line
    let horizLinePositions =
        fromList [-100..20..100]
        |> map (fun n -> middleOfSheet + {X=float n; Y=0.})
    
    let vertLinePositions =
        fromList [-100..20..100]
        |> map (fun n -> middleOfSheet + {X=0.; Y=float n})
    
    let randomRotation _ =
        random.Next(3)
        |> function
            | 0 -> Degree0
            | 1 -> Degree90
            | 2 -> Degree180
            | _ -> Degree270 
    
    let randomflip _ =
        random.Next(1)
        |> function
            | 0 -> SymbolT.FlipHorizontal
            | _ -> SymbolT.FlipVertical

    /// demo test circuit consisting of a DFF & And gate
    let makeTest1Circuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> getOkOrFail
    
    let makeRotatedCircuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (rotateSymbol "FF1" Degree180)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> getOkOrFail

    let makeFlippedCircuit (andPos:XYPos) =
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) andPos
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (flipSymbol "FF1" SymbolT.FlipVertical)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> getOkOrFail
    
    let makeRotatedAndFlippedCircuit (posData: XYPos * Rotation * SymbolT.FlipType) =
        let (pos, rotation, flip) = posData
        initSheetModel
        |> placeSymbol "G1" (GateN(And,2)) (pos)
        |> Result.bind (rotateSymbol "G1" rotation)
        |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (flipSymbol "FF1" flip)
        |> Result.bind (rotateSymbol "FF1" rotation)
        |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
        |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
        |> getOkOrFail
    
    
    let generateXYPosFromInts (X:int) (Y:int) =
        middleOfSheet + {X=float X; Y=float Y}

    let checkNoSymbolOverlap (andPos:XYPos) =
        makeTest1Circuit andPos
        |>  Asserts.failOnSymbolIntersectsSymbol 1 
        |>  function
            | None -> true
            | Some _ -> false
    
    let checkNoSymbolOverlapFlipRotate (posData: XYPos * Rotation * SymbolT.FlipType) =
        makeRotatedAndFlippedCircuit posData
        |>  Asserts.failOnSymbolIntersectsSymbol 1 
        |>  function
            | None -> true
            | Some _ -> false

    let gridPositions = 
        let coordRange = fromList [-100..19..100]
        let initGrid = product generateXYPosFromInts coordRange coordRange

        filter checkNoSymbolOverlap initGrid

    let gridPositionsWithFlipAndRotation = 
        let coordRange = fromList [-100..19..100]
        let initGrid = product generateXYPosFromInts coordRange coordRange
        let gridPlusRotation =  initGrid
                                |> toList
                                |> List.map (fun x -> (x,randomRotation "", randomflip ""))
                                |> fromList

        filter checkNoSymbolOverlapFlipRotate gridPlusRotation
    

//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

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
        
        let routingTest testNum firstSample dispatch =
            runTestOnSheets
                "Routing Test (Grid Positioned AND + DFF): fail on wire intersects symbol"
                firstSample
                gridPositions
                makeTest1Circuit
                Asserts.failOnWireIntersectsSymbol
                dispatch
            |> recordPositionInTest testNum dispatch

        let rotateTest testNum firstSample dispatch =
            runTestOnSheets
                "Rotate 180 Degrees Test: fail every test"
                firstSample
                gridPositions
                makeRotatedCircuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch
        
        let flipTest testNum firstSample dispatch =
            runTestOnSheets
                "Flip Vertically Test: fail every test"
                firstSample
                gridPositions
                makeFlippedCircuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        let rotateAndFlipRouting testNum firstSample dispatch =
            runTestOnSheets
                "Rotate and Flip Routing: fail on wire intersects symbol"
                firstSample
                gridPositionsWithFlipAndRotation
                makeRotatedAndFlippedCircuit
                Asserts.failOnWireIntersectsSymbol
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
                "Part 7: Routing Test", routingTest // Test for part 7 - auto wire routing
                "Rotate Test", rotateTest // test 6
                "Flip Test", flipTest // test 7
                "Part 10: Rotate and Flip Routing", rotateAndFlipRouting
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
        
        let testModelGen (model: Model) (dispatch: Dispatch<Msg>) =
            printfn $"{getTestModel model.Sheet}"

            let test2 : TestModel = 
                { SimpleSymbols =
                    [ { SymLabel = "G1"
                        CompType = GateN(And, 2)
                        Position = { X = 1638.105; Y = 1671.75 }
                        STransform = { Rotation = Degree0; Flipped = false } }
                      { SymLabel = "G2"
                        CompType = GateN(And, 2)
                        Position = { X = 1874.605; Y = 1858.25 }
                        STransform = { Rotation = Degree0; Flipped = false } }
                      { SymLabel = "MUX1"
                        CompType = Mux2
                        Position = { X = 1632.895; Y = 1780.25 }
                        STransform = { Rotation = Degree0; Flipped = false } } ]
                  Connections =
                    [ { Source = { Label = "MUX1"; PortNumber = 0 }
                        Target = { Label = "G2"; PortNumber = 0 } }
                      { Source = { Label = "G1"; PortNumber = 0 }
                        Target = { Label = "G2"; PortNumber = 1 } } ] }

            let test3 = 
                { SimpleSymbols =
                    [ { SymLabel = "MUX1"
                        CompType = Mux2
                        Position = { X = 1595.5774438476562; Y = 1801.0249633789062 }
                        STransform = { Rotation = Degree0; Flipped = false } }
                      { SymLabel = "G2"
                        CompType = GateN(And, 2)
                        Position = { X = 1834.7874438476563; Y = 1886.0249633789062 }
                        STransform = { Rotation = Degree0; Flipped = false } }
                      { SymLabel = "ADD1"
                        CompType = NbitsAdder 3
                        Position = { X = 1844.4225561523438; Y = 1666.4750366210938 }
                        STransform = { Rotation = Degree0; Flipped = false } }
                      { SymLabel = "G1"
                        CompType = GateN(And, 2)
                        Position = { X = 1600.7874438476563; Y = 1692.5249633789062 }
                        STransform = { Rotation = Degree0; Flipped = false } } ]
                  Connections =
                    [ { Source = { Label = "ADD1"; PortNumber = 1 }
                        Target = { Label = "MUX1"; PortNumber = 2 } }
                      { Source = { Label = "MUX1"; PortNumber = 0 }
                        Target = { Label = "ADD1"; PortNumber = 1 } }
                      { Source = { Label = "MUX1"; PortNumber = 0 }
                        Target = { Label = "G2"; PortNumber = 0 } }
                      { Source = { Label = "G1"; PortNumber = 0 }
                        Target = { Label = "G2"; PortNumber = 1 } } ] }

            let test = getTestModel model.Sheet

            let sheet = placeTestModel test3

            showSheetInIssieSchematic sheet dispatch

        


    

