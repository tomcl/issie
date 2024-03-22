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
                let index = zeroVecIndex + 1 // base index as it should be on full segVecs
                segVecs[0..index-2] @
                [segVecs[index-1] + segVecs[index+1]] @
                segVecs[index+2..segVecs.Length - 1]
                |> coalesce
            | None -> segVecs
     
        wire.Segments
        |> List.mapi getSegmentVector
        |> coalesce
                


//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//
    module Builder =


                

            



        type FailureType =
            | WiresNotStraightened of string
            | SymbolIntersections of string
            | WireSymbolIntersections of string


        type TestResult = | Success of string | Failure of FailureType


        type SheetAlignCircuitType =
            | UnstraightenedSimpleMUX of SheetT.Model
            | MultipleConnections of SheetT.Model
            | GateScaling of SheetT.Model
            | MultipleMUX of SheetT.Model
            | OrderFlip of SheetT.Model
            | CustomComponents of SheetT.Model
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
        let rotateSymbol (rotation: Rotation) (label: string) (sheet: SheetT.Model): SheetT.Model =
            let symMap = sheet.Wire.Symbol.Symbols
            let symbol =
                (Map.toList >> List.map snd) symMap
                |> List.find (fun sym -> sym.Component.Label = label)

            let symCentre = Symbol.getRotatedSymbolCentre symbol // If not rotated, just returns the correct centre, but accounts for rotated case.
            let symId = ComponentId symbol.Component.Id

            let rotatedSymbol =
                symMap
                |> Map.tryFind symId
                |> Option.map (fun sym -> RotateScale.rotateSymbolInBlock rotation symCentre sym) // The symbol is the only one in the block

            match rotatedSymbol with
            | Some (sym) ->
                let newSymbols =
                    (symId, sym)
                    |> symMap.Add
                Optic.set SheetT.symbols_ newSymbols sheet
                |> SheetUpdateHelpers.updateBoundingBoxes // Need to recalculate bounding boxes because rotation changes them
            | None ->
                printf $"Given symbol {symbol.Component.Label} does not exist on the sheet. Returning sheet before change."
                sheet


        /// Given a symbol on a sheet and its flip orientation, flips the symbol.
        ///
        /// symbol - The symbol the be flipped
        ///
        /// flip - The new flip orientation
        ///
        /// sheet - The sheet model on which symbol flip state needs to be changed.
        let flipSymbol (label: string) (flip: SymbolT.FlipType) (sheet: SheetT.Model): SheetT.Model =
            let symMap = sheet.Wire.Symbol.Symbols
            let symbol =
                (Map.toList >> List.map snd) symMap
                |> List.find (fun sym -> sym.Component.Label = label)

            let symCentre = Symbol.getRotatedSymbolCentre symbol // If not rotated, just returns the correct centre, but accounts for rotated case.
            let symId = ComponentId symbol.Component.Id

            let flippedSymbol =
                symMap
                |> Map.tryFind symId
                |> Option.map (fun sym -> RotateScale.flipSymbolInBlock flip symCentre sym) // The symbol is the only one in the block

            match flippedSymbol with
            | Some (sym) ->
                let newSymbols =
                    (symId, sym)
                    |> symMap.Add
                Optic.set SheetT.symbols_ newSymbols sheet
            | None ->
                printf $"Given symbol {symbol.Component.Label} does not exist on the sheet. Returning sheet before change."
                sheet

        /// Implements an arbitrary flip or rotate on both components in the sheet
        /// and gets a random flip and rotate value.
        let arbitraryFlipRotate =
            let rSeed = randomInt 0 1 3
            let fSeed = randomInt 0 1 2
            let rotate =
                match rSeed.Data 0 with
                | 0 -> None // According to https://edstem.org/us/courses/51379/discussion/4281774: Rotation needs to be patched. Please patch before testing!!
                | 1 -> Some Rotation.Degree90
                | 2 -> Some Rotation.Degree180 // Patch of using pos in adjustPosForRotation did not seem to work.
                | 3 -> Some Rotation.Degree270
                | errVal -> failwithf $"Seed ranges from 0-3, no value outside this range should be generated: {errVal}"
            let flip =
                match fSeed.Data 0 with
                | 0 -> None
                | 1 -> Some SymbolT.FlipHorizontal
                | errVal -> failwithf $"Seed ranges from 0-3, no value outside this range should be generated: {errVal}"
            printfn $"Rotation: {rotate}, Flip: {flip}"
            {|rotateSeed = rotate; flipSeed = flip|}


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

        /// Given a model, applies the completed sheetAlignScale and judges if visual quality
        /// has improved or not
        ///
        /// model - The sheet to be beautified and checked.
        let evaluateBeautification (model: SheetT.Model) =
            let straightWires (model: SheetT.Model) =
                model.Wire.Wires
                |> Map.toList
                |> List.map fst
                |> List.fold (fun wires wId -> if List.length (visibleSegments wId model) = 1 then wires + 1 else wires) 0

            let beautifiedModel = SheetBeautifyD1.sheetAlignScale model

            let initialStraightWires = straightWires model
            let finalStraightWires = straightWires beautifiedModel

            let percentWiresStraightened = int (100.0 * float (finalStraightWires - initialStraightWires) / float initialStraightWires)
            let symbolIntersectionCount = T1.intersectingSymbolPairs beautifiedModel
            let wireSymbolIntersectionCount = T2.visSegsIntersectSymbol beautifiedModel

            match (percentWiresStraightened, symbolIntersectionCount, wireSymbolIntersectionCount) with
            | (wires, _, _) when wires < 0 ->
                $">> sheetAlignScale failed - Initial Straight Wires: {initialStraightWires} | Final Straight Wires: {finalStraightWires}"
                |> WiresNotStraightened
                |> Failure
            | (_, count, _) when count > 0 ->
                $">> sheetAlignScale failed - Pairs of intersecting symbols: {count}"
                |> SymbolIntersections
                |> Failure
            | (_, _, count) when count > 0 ->
                $">> sheetAlignScale failed - Wires intersecting symbols: {count}"
                |> WireSymbolIntersections
                |> Failure
            | _ -> Success ($">> {percentWiresStraightened}" + "% wires straightened")
    
        /// Given a sheet, deconstructs all the information present in the circuit.
        ///
        /// model - Sheet to be replicated.
        let deconstructCircuit (model: SheetT.Model) =

            /// Finds mapping between portId and symbol for all ports on the sheet.
            let portSymbolMap =
                (Map.toList >> List.map snd) model.Wire.Symbol.Symbols
                |> List.collect (fun sym ->
                    sym.PortMaps.Orientation
                    |> Map.toList
                    |> List.map (fun (portId, _) -> portId, sym))
                |> Map.ofList

            /// For all symbols on the sheet, breaks them down into label, type and position.
            let symbolsInfo =
                // Similar to the functions above, this function can be fed into Builder.placeSymbol to
                // conveniently place symbols on the sheet.
                model.Wire.Symbol.Symbols
                |> (Map.toList >> List.map snd)
                |> List.map (fun sym ->
                    let actualPos = {X = sym.Pos.X + float sym.Component.W / 2.0; Y = sym.Pos.Y + float sym.Component.H / 2.0}
                    // Need to get the actual X and Y position back. See Symbol.createNewSymbol for why.
                    sym.Component.Label, sym.Component.Type, actualPos)

            /// For a given port ID and port type, returns the label of the symbol
            /// the port is attached to, and the index associated with the port on
            /// that symbol.
            let findPortSymbolIndex (portId: string) (portType: PortType) =
                // This function is crucial in generating information about the connections on the sheet.
                // The output of this function can be directly fed to the Builder.placeWire function (label, port index)
                // This function does not perform error handling when trying to find the symbol attached to a particular
                // port ID, because it is assumed said port ID will always exist (since this function is used in conjunction)
                // with the port ID:symbol mapping produced earlier.
                let sym = Map.find portId portSymbolMap

                match portType with
                    | PortType.Input -> sym.Component.InputPorts
                    | PortType.Output -> sym.Component.OutputPorts
                |> List.tryFindIndex (fun (port:Port) -> port.Id = portId)
                |> Option.defaultWith (fun _ ->
                                        printfn $"The given port ID: {portId} does not exist on the sheet"
                                        0)
                |> (fun portNum -> sym.Component.Label, portNum)

            /// Generates a list of all of the symbol(port) <-> (port)symbol connections on the sheet.
            let connectionsInfo =
                // This function uses findPortSymbolIndex to construct a list of all of the said connections
                // on the sheet.
                model.Wire.Wires
                |> (Map.toList >> List.map snd)
                |> List.map (fun wire -> match wire.InputPort, wire.OutputPort with
                                                | InputPortId iPort, OutputPortId oPort ->
                                                    findPortSymbolIndex oPort PortType.Output,
                                                    // Output port needs to be first - this serves as the source port for the wire.
                                                    findPortSymbolIndex iPort PortType.Input)
                |> List.map (fun (symInfo1, symInfo2) -> symInfo1 ||> portOf, symInfo2 ||> portOf)

            printfn $"Symbols: {symbolsInfo}"
            printfn $"Connections: {connectionsInfo}"

            {| Symbols = symbolsInfo; Connections = connectionsInfo |} // Anonymous record.


        /// Given data from a deconstructed circuit, reconstructs it and returns the sheet.
        let reconstructCircuit
            (data: {| Symbols: List<string * ComponentType * XYPos>; Connections: List<SymbolPort * SymbolPort> |}) =

            (Ok initSheetModel, data.Symbols)
            ||> List.fold (fun sheet (label, compType, pos) ->
                                Result.bind (placeSymbol label compType pos) sheet)
            |> (fun connections sheet ->
                    (sheet, connections)
                    ||> List.fold (fun sheet (sourceSymbolPort, targetSymbolPort) ->
                                        Result.bind(placeWire sourceSymbolPort targetSymbolPort) sheet)) data.Connections
            |> TestLib.getOkOrFail


        let verticalLinePositions =
            fromList [-42.0..7.0..42.0]
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

    module sheetAlignScaleTests = 
        let twoMuxTest (_: XYPos) = 
            initSheetModel
            |> Builder.placeSymbol "AND" (GateN(And, 2)) middleOfSheet
            |> Result.bind(Builder.placeSymbol "I1" (Mux2) {X = middleOfSheet.X - 150.0; Y = middleOfSheet.Y - 75.0})
            |> Result.bind(Builder.placeSymbol "I2" (Mux2) {X = middleOfSheet.X - 150.0; Y = middleOfSheet.Y + 75.0})
            |> Result.bind(Builder.placeWire (portOf "I1" 0) (portOf "AND" 0))
            |> Result.bind(Builder.placeWire (portOf "I2" 0) (portOf "AND" 1))
            |> getOkOrFail
            |> SheetBeautifyD1.sheetAlignScale

        let andGateTest (_: XYPos) = 
            initSheetModel
            |> Builder.placeSymbol "AND" Mux2 middleOfSheet
            |> Result.bind(Builder.placeSymbol "I1" (Input1(1, None)) {X = middleOfSheet.X - 150.0; Y = middleOfSheet.Y - 75.0})
            |> Result.bind(Builder.placeSymbol "I2" (Input1(1, None)) {X = middleOfSheet.X - 150.0; Y = middleOfSheet.Y + 75.0})
            |> getOkOrFail
            |> rotateSymbol Rotation.Degree90 "I1"
            |> rotateSymbol Rotation.Degree90 "I1"
            |> Builder.placeWire (portOf "I1" 0) (portOf "AND" 0)
            |> Result.bind(Builder.placeWire (portOf "I2" 0) (portOf "AND" 1))
            |> getOkOrFail
            |> SheetBeautifyD1.alignOnePortSymbolsPhase

        // Circuit demonstrating enough degrees of freedom to straighten non singly constrained components.
        let multipleMUXTest (_: XYPos) =
            initSheetModel
            |> Builder.placeSymbol "MUX2" Mux2 middleOfSheet
            |> Result.bind (Builder.placeSymbol "MUX1" Mux2 {X = middleOfSheet.X - 175.0; Y = middleOfSheet.Y - 20.0})
            |> Result.bind (Builder.placeSymbol "A" (Input1(1, None)) {X = middleOfSheet.X - 300.0; Y = middleOfSheet.Y - 56.0})
            |> Result.bind (Builder.placeSymbol "B" (Input1 (1, None)) {X = middleOfSheet.X - 300.0; Y = middleOfSheet.Y})
            |> Result.bind (Builder.placeSymbol "S2" (Input1 (1, None)) {X = middleOfSheet.X - 250.0; Y = middleOfSheet.Y + 100.0})
            |> Result.bind (Builder.placeSymbol "C" (Output(1)) {X = middleOfSheet.X + 150.0; Y = middleOfSheet.Y + 10.0})
            |> Result.bind (Builder.placeWire (portOf "A" 0) (portOf "MUX1" 0))
            |> Result.bind (Builder.placeWire (portOf "B" 0) (portOf "MUX1" 1))
            |> Result.bind (Builder.placeWire (portOf "S2" 0) (portOf "MUX2" 2))
            |> Result.bind (Builder.placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
            |> Result.bind (Builder.placeWire (portOf "MUX2" 0) (portOf "C" 0))
            |> TestLib.getOkOrFail
            |> SheetBeautifyD1.sheetAlignScale


        // Defined custom component
        let customMain =
            {
                Name = "MAIN";
                InputLabels = [("A", 1); ("B", 1); ("S2", 1); ("S1", 1)];
                OutputLabels = [("E", 1); ("F", 1); ("G", 1)];
                Form = None
                Description = None
            }


        // Aligning and scaling of circuits with custom components
        let customComponentScaling (_: XYPos) =
            initSheetModel
            |> Builder.placeSymbol "MAIN1" (Custom(customMain)) middleOfSheet
            |> Result.bind (Builder.placeSymbol "MAIN2" (Custom(customMain)) {X = middleOfSheet.X + 200.0; Y = middleOfSheet.Y - 100.0})
            |> Result.bind (Builder.placeWire (portOf "MAIN1" 0) (portOf "MAIN2" 0))
            |> Result.bind (Builder.placeWire (portOf "MAIN1" 1) (portOf "MAIN2" 1))
            |> Result.bind (Builder.placeWire (portOf "MAIN1" 2) (portOf "MAIN2" 2))
            |> TestLib.getOkOrFail
            |> SheetBeautifyD1.scaleSymbolAlignPhase
    
        let testTripleMUX (_: XYPos) = 
            initSheetModel
            |> Builder.placeSymbol "MUX2" Mux2 middleOfSheet
            |> Result.bind (Builder.placeSymbol "MUX1" Mux2 {X = middleOfSheet.X - 175.0; Y = middleOfSheet.Y - 40.0})
            |> Result.bind (Builder.placeSymbol "A" (Input1(1, None)) {X = middleOfSheet.X - 300.0; Y = middleOfSheet.Y - 68.4})
            |> Result.bind (Builder.placeSymbol "B" (Input1 (1, None)) {X = middleOfSheet.X - 300.0; Y = middleOfSheet.Y})
            |> Result.bind (Builder.placeSymbol "S2" (Input1 (1, None)) {X = middleOfSheet.X - 300.0; Y = middleOfSheet.Y + 100.0})
            |> Result.bind (Builder.placeSymbol "MUX3" Mux2 {X = middleOfSheet.X + 150.0; Y = middleOfSheet.Y + 125.0})
            |> Result.bind(Builder.placeSymbol "S1" (Input1(1, None)) {X = middleOfSheet.X - 250.0; Y = middleOfSheet.Y + 200.0})
            |> Result.bind (Builder.placeWire (portOf "A" 0) (portOf "MUX1" 0))
            |> Result.bind (Builder.placeWire (portOf "B" 0) (portOf "MUX1" 1))
            |> Result.bind (Builder.placeWire (portOf "S2" 0) (portOf "MUX2" 2))
            |> Result.bind (Builder.placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
            |> Result.bind (Builder.placeWire (portOf "MUX2" 0) (portOf "MUX3" 0))
            |> Result.bind (Builder.placeWire (portOf "S1" 0) (portOf "MUX1" 2))
            |> Result.bind (Builder.placeWire (portOf "S1" 0) (portOf "MUX2" 1))
            |> Result.bind (Builder.placeWire (portOf "S1" 0) (portOf "MUX3" 1))
            |> TestLib.getOkOrFail
            |> SheetBeautifyD1.sheetAlignScale


        // Circuit emulating results from orderFlip, for compatibility with the other beautify algorithms.
        let orderFlipCompatibilityTest (_: XYPos) =
            initSheetModel
            |> Builder.placeSymbol "S1" (Input1(1, None)) {X = middleOfSheet.X - 275.0; Y = middleOfSheet.Y - 40.0}
            |> Result.bind (Builder.placeSymbol "S2" (Input1(1, None)) {X = middleOfSheet.X - 275.0; Y = middleOfSheet.Y + 60.0})
            |> Result.bind (Builder.placeSymbol "MUX2" (Mux2) middleOfSheet)
            |> Result.bind (Builder.placeSymbol "MUX1" (Mux2) {X = middleOfSheet.X - 150.0; Y = middleOfSheet.Y - 150.0})
            |> Result.bind (Builder.placeSymbol "G1" (GateN(And, 2)) {X = middleOfSheet.X + 150.0; Y = middleOfSheet.Y - 160.0})
            |> Result.bind (Builder.placeWire (portOf "S1" 0) (portOf "MUX2" 1))
            |> Result.bind (Builder.placeWire (portOf "S2" 0) (portOf "MUX2" 2))
            |> Result.bind (Builder.placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
            |> Result.bind (Builder.placeWire (portOf "MUX1" 0) (portOf "G1" 0))
            |> Result.bind (Builder.placeWire (portOf "MUX2" 0) (portOf "G1" 1))
            |> TestLib.getOkOrFail
            |> SheetBeautifyD1.sheetAlignScale

        // Circuit with multiple connections between the same two components
        let scalingMultipleConnections (_: XYPos) =
            initSheetModel
            |> Builder.placeSymbol "DEMUX" Demux4 middleOfSheet
            |> Result.bind (Builder.placeSymbol "AND" (GateN(And, 3)) {middleOfSheet with X = middleOfSheet.X + 200.0})
            |> Result.bind (Builder.placeWire (portOf "DEMUX" 0) (portOf "AND" 0))
            |> Result.bind (Builder.placeWire (portOf "DEMUX" 1) (portOf "AND" 1))
            |> TestLib.getOkOrFail
            |> SheetBeautifyD1.sheetAlignScale

    



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
                sheetAlignScaleTests.scalingMultipleConnections
                (Asserts.failOnSampleNumber 0)
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail all tests
        let test4 testNum firstSample dispatch =
            runTestOnSheets
                "Horizontally positioned AND + DFF: fail all tests"
                firstSample
                horizLinePositions
                sheetAlignScaleTests.testTripleMUX
                (Asserts.failOnSampleNumber 0)
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
        


    


