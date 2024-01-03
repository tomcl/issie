module TestDrawBlock
open GenerateData
open Elmish

/// This module contains types to represent tests with (possibly) random data, and results from tests.
module Tests =

    type TestStatus =
            | Fail
            | Exception of string

    type Test<'a> = {
        Name: string
        Samples: Gen<'a>
        Assertion: 'a -> bool
        }

    type TestResult<'a> = {
        TestName: string
        TestData: Gen<'a>
        TestErrors: ('a * TestStatus) list
    }

    let catchException func arg =
        try
            Ok (func arg)
        with
            | e -> Error e.Message
            
    /// Run the Test samples from 0 up to test.Size - 1.
    /// The return list contains all failures or exceptions: empty lits => everything has passed.
    /// This will always run to completion: use truncate if text.Samples.Size is too large.
    let runTests (test: Test<'a>) : TestResult<'a> =
        [0..test.Samples.Size - 1]
        |> List.map test.Samples.Data
        |> List.collect (fun dat ->
                            match catchException test.Assertion dat with
                            | Ok true -> []
                            | Ok false -> [dat,Fail]
                            | Error mess -> [dat,Exception mess])
        |> (fun resL ->                
                {
                    TestName = test.Name
                    TestData = test.Samples
                    TestErrors =  resL
                }
            )
        
            
/// This submodule contains a set of functions that enable random data generation
/// for property-based testing of Draw Block wire routing functions.
/// basic idea.
/// 1. Generate, in various ways, random circuit layouts
/// 2. For each layout apply smartautoroute to regenerate all wires
/// 3. Apply check functions to see if the resulting wire routing obeys "good layout" rules.
/// 4. Output any layouts with anomalous wire routing
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
    open Tests

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

    /// Used throughout to compare labels since these are case invariant "g1" = "G1"
    let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2
 

    /// Identify a port from its component label and number.
    /// Usually both an input and output port will mathc this, so
    /// the port is only unique if it is known to be input or output.
    /// used to specify the ends of wires, since tehee are known to be
    /// connected to outputs (source) or inputs (target).
    type SymbolPort = { Label: string; PortNumber: int }

                        



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
            |> SheetUpdate.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
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
        failwithf "Not Implemented"

    // Flip a symbol
    let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : (SheetT.Model) =
        failwithf "Not Implemented"

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
    let runTestOnSheets
        (name: string)
        (samples : Gen<'a>)
        (sheetMaker: 'a -> SheetT.Model)
        (sheetChecker: SheetT.Model -> bool)
        (dispatch: Dispatch<Msg>)
            : TestResult<'a> =
        let generateAndCheckSheet = sheetMaker >> sheetChecker
        let result =
            {
                Name=name;
                Samples=samples;
                Assertion = generateAndCheckSheet
            }
            |> runTests
        match result.TestErrors with
        | [] -> // no errors
            printf $"Test {result.TestName} has PASSED."
        | first:: _ -> // display in Issie editor and print out first error
            printf $"Test {result.TestName} has FAILED\n{snd first}"
            showSheetInIssieSchematic (sheetMaker (fst first)) dispatch
        result
