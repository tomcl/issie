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


        /// Rotate the symbol given by symLabel by an amount rotate.
        /// Takes in a symbol label, a rotate fixed amount, and a sheet containing the symbol.
        /// Return the sheet with the rotated symbol.
        let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : (SheetT.Model) =

            let symbolsMap = model.Wire.Symbol.Symbols
            let getSymbol =
                mapValues symbolsMap
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
                |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"

            match getSymbol with
            | Ok symbol ->
                let rotatedSymbol = SymbolResizeHelpers.rotateSymbol rotate symbol
                let updatedSymbolsMap = Map.add symbol.Id rotatedSymbol symbolsMap
                { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

            | _ -> model

        /// Flip the symbol given by symLabel by an amount flip.
        /// Takes in a symbol label, a flip fixed amount, and a sheet containing the symbol.
        /// Return the sheet with the flipped symbol.
        let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : (SheetT.Model) =

            let symbolsMap = model.Wire.Symbol.Symbols
            let getSymbol =
                mapValues symbolsMap
                |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
                |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"

            match getSymbol with
            | Ok symbol ->
                let flippedSymbol = SymbolResizeHelpers.flipSymbol flip symbol
                let updatedSymbolsMap = Map.add symbol.Id flippedSymbol symbolsMap
                { model with Wire = { model.Wire with Symbol = { model.Wire.Symbol with Symbols = updatedSymbolsMap } } }

            | _ -> model

        /// Implements an arbitrary flip or rotate on both components in the sheet
        /// and gets a random flip and rotate value.
        //let arbitraryFlipRotate =
        //    let rSeed = randomInt 0 1 3
        //    let fSeed = randomInt 0 1 2
        //    let rotate =
        //        match rSeed.Data 0 with
        //        | 0 -> None // According to https://edstem.org/us/courses/51379/discussion/4281774: Rotation needs to be patched. Please patch before testing!!
        //        | 1 -> Some Rotation.Degree90
        //        | 2 -> Some Rotation.Degree180 // Patch of using pos in adjustPosForRotation did not seem to work.
        //        | 3 -> Some Rotation.Degree270
        //        | errVal -> failwithf $"Seed ranges from 0-3, no value outside this range should be generated: {errVal}"
        //    let flip =
        //        match fSeed.Data 0 with
        //        | 0 -> None
        //        | 1 -> Some SymbolT.FlipHorizontal
        //        | errVal -> failwithf $"Seed ranges from 0-3, no value outside this range should be generated: {errVal}"
        //    printfn $"Rotation: {rotate}, Flip: {flip}"
        //    {|rotateSeed = rotate; flipSeed = flip|}


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
        let evaluateBeautificationD1 (model: SheetT.Model) =
            let straightWires (model: SheetT.Model) =
                model.Wire.Wires
                |> Map.toList
                |> List.map fst
                |> List.fold (fun wires wId -> if List.length (visibleSegments wId model) = 1 then wires + 1 else wires) 0

            let beautifiedModel = SheetBeautifyD1.sheetAlignScale model

            let initialStraightWires = straightWires model
            let finalStraightWires = straightWires beautifiedModel

            let percentWiresStraightened = int (100.0 * float (finalStraightWires - initialStraightWires) / float initialStraightWires)
            let symbolIntersectionCount = SheetBeautifyHelpers.numOfIntersectedSymPairs beautifiedModel
            let wireSymbolIntersectionCount = SheetBeautifyHelpers.numOfIntersectSegSym beautifiedModel

            printfn $"sheetAlignScale metrics: Initial Straight Wires: {initialStraightWires} | Final Straight Wires: {finalStraightWires}
            | {percentWiresStraightened} percent wires straightened \n Pairs of intersecting symbols: {symbolIntersectionCount} \n
            Wire symbols intersections: {wireSymbolIntersectionCount}"

            beautifiedModel

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
//----------------------------------------D3 Testing Helper functions-------------------------------//
//--------------------------------------------------------------------------------------------------//

        ///componentInfo type used in circuit maker DSL.
        ///Contains component label and type of component.
        type componentInfo = {
            Label: string
            CompType: ComponentType
        }

        ///connectionInfo type used in circuit maker DSL.
        ///Contains connection between source component SourceCompLabel, and its port SourceCompPort
        ///to target component TargetCompLabel, and its port TargetCompPort.
        type connectionInfo = {
            SourceCompLabel: string
            SourceCompPort: int
            TargetCompLabel: string
            TargetCompPort: int
        }

        ///Generate XYPos samples for tests.
        ///Takes in the amount of samples to create.
        let genXYPos lim =
               let coords =
                   fromList [-lim..20..lim] //was 20
                   |> map (fun n -> float n)
               product (fun x y -> {X=x*10.0; Y=y*10.0}) coords coords

        let makePositions = genXYPos 10

        ///Generate samples for sheetWireLabelSymbol (D3) Easy Test.
        ///Return a Gen of flip, rotations and positions.
        let makeSamplesD3Easy =
            let rotations = fromList [Rotation.Degree90; Rotation.Degree270]
            let flips = fromList [Some SymbolT.FlipType.FlipHorizontal; None]
            let applyD3 = fromList [ false; true ]
            genXYPos 10
            |> product (fun a b -> (a,b)) (genXYPos 10)
            |> product (fun a b -> (a,b)) rotations
            |> product (fun a b -> (a,b)) flips
            |> product (fun a b -> (a,b)) applyD3
            |> map (fun (applyBeautify, (flipMux,(rotMux,(demuxPos, muxPos)))) ->
                {|
                    ApplyBeautify = applyBeautify;
                    FlipMux = flipMux;
                    RotMux = rotMux;
                    DemuxPos = (demuxPos);
                    Mux1Pos = (muxPos);
                    Mux2Pos = (muxPos);
                |})

        ///Generate samples for sheetWireLabelSymbol (D3) Hard Test.
        ///Return a Gen of flip, rotations and positions.
        let makeSamplesD3Hard =
            let rotations = fromList [Rotation.Degree90; Rotation.Degree270]
            let flips = fromList [Some SymbolT.FlipType.FlipHorizontal; None]
            let applyD3 = fromList [ false; true ]
            genXYPos 10
            |> product (fun a b -> (a,b)) (genXYPos 10)
            |> product (fun a b -> (a,b)) rotations
            |> product (fun a b -> (a,b)) flips
            |> product (fun a b -> (a,b)) applyD3
            |> map (fun (applyBeautify, (flipMux,(rotMux,(orPos, muxPos)))) ->
                {|
                    ApplyBeautify = applyBeautify;
                    FlipMux = flipMux;
                    RotMux = rotMux;
                    AndPos = muxPos;
                    OrPos = orPos;
                    XorPos = orPos;
                    MuxPos = muxPos
                |})

        ///Generates position for a component in circuit such that it's threshold distance away from another component.
        ///Takes the sheet and the required threshold distance between components.
        ///Returns X,Y coordinate position of the component in the sheet.
        let generatePosition (model: SheetT.Model) (threshold:float) : XYPos =
            let initPosition = middleOfSheet

            let getExistingPositions =
                model.Wire.Symbol.Symbols
                |> Map.toList
                |> List.map (fun (_, sym) -> sym.Pos)

            let rec findPosition (position: XYPos) =
                if getExistingPositions |> List.exists (fun existingPos ->
                    euclideanDistance existingPos position <= threshold) then
                    if random.Next(0,2) = 0 then
                        findPosition { position with X = position.X + threshold }
                    else
                        findPosition { position with Y = position.Y + threshold }
                else
                    position

            findPosition initPosition

        ///Places a list of components on a sheet such that they're all threshold distance apart with small random changes in position.
        ///Takes a list of components, threshold distance and set of X,Y coordinates to apply small changes in position.
        ///Returns a sheet with the components placed.
        let placeComponentsOnModel (components: componentInfo list) (threshold: float) (genSamples: XYPos) : SheetT.Model =
            components
            |> List.fold (fun currModel comp ->
                let compPosition = generatePosition currModel threshold
                placeSymbol comp.Label comp.CompType (compPosition + genSamples) currModel
                |> getOkOrFail) initSheetModel

        ///Constructs a circuit using a list of components and randomly connects components at threshold distance apart.
        ///Takes a list of components, threshold distance and set of X,Y coordinates.
        ///Returns a sheet with components placed and wires routed.
        let randomConnectCircuitGen (components: componentInfo list) (threshold: float) (genSamples: XYPos) =
            components
            |> List.toArray
            |> shuffleA
            |> Array.pairwise
            |> Array.fold (fun currModel (comp1, comp2) ->
                placeWire (portOf comp1.Label 0) (portOf comp2.Label 0) currModel
                |> Result.bind (placeWire (portOf comp1.Label 0) (portOf comp2.Label 1))
                |> getOkOrFail
                |> separateAllWires
            ) (placeComponentsOnModel components threshold genSamples)

        ///Constructs a circuit by connecting a given list of components based on given list of connections at threshold distance apart.
        ///Takes a list of components, list of connections, threshold distance and set of X,Y coordinates.
        ///Returns a sheet with the components placed and wires routed.
        let fixedConnectCircuitGen (components: componentInfo list) (connections: connectionInfo list) (threshold: float) (genSamples: XYPos)=
            connections
            |> List.fold (fun currModel connect ->
                placeWire (portOf connect.SourceCompLabel connect.SourceCompPort) (portOf connect.TargetCompLabel connect.TargetCompPort) currModel
                |> getOkOrFail
                |> separateAllWires
            ) (placeComponentsOnModel components threshold genSamples)

        ///Counts number of overlapping symbols, number of wires intersecting symbols, number of total wire bends and total wire crossings in the sheet.
        ///Takes in a sheet.
        ///Returns a record with the number of overlapping symbols, wires intersecting symbols, total wire bends, total wire crossings.
        let countMetrics (model: SheetT.Model) =
            //Metric 1: count number of symbols overlapping
            let boxes =
                model.BoundingBoxes
                |> Map.toList

            let numOfSymbolOverlaps =
                List.allPairs boxes boxes
                |> List.filter (fun ((n1, box1), (n2, box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
                |> List.length

            //Metric 2: count number of wires intersecting symbols
            let wireModel = model.Wire

            let allWires = wireModel.Wires
                            |> Map.toList

            let numOfWireIntersectSym =
                allWires
                |> List.fold (fun totalCount (_, wire) ->
                    totalCount + (BusWireRoute.findWireSymbolIntersections wireModel wire |> List.length)
                ) 0

            //Metric 3: Total number of right angles in model (right angles in wires of the model) - doesn't include right angles formed by crossings (but this is in crossings count)
            let totalWireBends =
                allWires
                |> List.fold (fun totalCount (_, wire) ->
                    totalCount + (SheetBeautifyD3.countRightAngleBendsInWire model wire)) 0

            //Metric 4: Total number of wire crossings in the model
            let totalWireCrossings =
                allWires
                |> List.fold (fun totalCount (_, wire) ->
                    totalCount + (SheetBeautifyD3.countCrossingsOnWire model wire)) 0

            {|SymbolOverlaps = numOfSymbolOverlaps; WireIntersectSym = numOfWireIntersectSym; TotalWireBends = totalWireBends; TotalWireCrossings = totalWireCrossings|}

        ///Prints metrics for each test.
        ///Takes in the set of tests, a test/circuit maker function.
        ///Prints the number of overlapping symbols, wires intersecting symbols, total wire bends, total wire crossings, test fail/pass status.
        let collectMetricsOfTests (samples: Gen<'a>) (sheetMaker: 'a -> SheetT.Model)=
            [0..samples.Size-1]
            |> List.map (fun n ->
                let sample = samples.Data n
                let sheetModel = sheetMaker sample
                let metrics = countMetrics sheetModel
                (n, metrics))

            |> List.iter (fun (n, m) ->
                printfn "Sample %d Metrics:" n
                // if (n % 2 = 1) then
                printfn "Symbol overlaps: %d" m.SymbolOverlaps
                printfn "Wire intersect symbols: %d" m.WireIntersectSym
                printfn "Total wire bends: %d" m.TotalWireBends
                printfn "Total wire crossings: %d" m.TotalWireCrossings)
                // if m.SymbolOverlaps > 1 || m.WireIntersectSym > 0 || m.TotalWireBends > 8 || m.TotalWireCrossings > 1 && (n % 2 = 1) then
                //     printfn "Fail test!"
                // else
                //     printfn "Pass test!")
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

    let beautifyIncrementally (increments: int) (sheet: SheetT.Model) =
        match increments with
        | 1 -> SheetBeautifyD2.autoRouteAllWires sheet
        | 2 -> sheet |> SheetBeautifyD2.autoRouteAllWires |> evaluateBeautificationD1
        | 3 -> sheet |>  SheetBeautifyD2.autoRouteAllWires |> evaluateBeautificationD1 |> SheetBeautifyD3.sheetWireLabelSymbol
        | _ -> sheet

    module D3Tests =

        ///-------SINGLE FUNCTIONALITY TESTS------///

        ///Generate a single unchanged test to test D3 handling of bit legends overlapping with wires.
        ///Returns a sheet with the circuit placed on it.
        let makeTestForBitsOverlap (beautify: bool) =
            initSheetModel
            |> placeSymbol "MUX1" (Mux2) (middleOfSheet + { X = -700; Y = -500 })
            |> Result.bind (placeSymbol "MUX2" (Mux2) (middleOfSheet))
            |> Result.bind (placeSymbol "S1" (Input1 (1,None) ) (middleOfSheet + { X = -500; Y = -10 }))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 1))
            |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 0))
            |> (fun res -> if beautify then Result.map SheetBeautifyD3.sheetWireLabelSymbol res else res)
            |> Result.map SheetBeautifyD2.autoRouteAllWires
            |> getOkOrFail

        ///Generate a single unchanged test to test D3 handling of wire overlapping.
        ///Returns a sheet with the circuit placed on it.
        let makeTestForWireOverlaps (increments: int) =
            initSheetModel
            |> placeSymbol "MUX1" (Mux2) (middleOfSheet + { X = -700; Y = -300 })
            |> Result.bind (placeSymbol "MUX2" (Mux2) (middleOfSheet))
            |> Result.bind (placeSymbol "DEMUX2" (Demux2) (middleOfSheet + { X = -600; Y = -150 }))
            |> Result.bind (placeSymbol "G1" (GateN (And,2)) (middleOfSheet + { X = 400; Y = -100}))
            |> Result.bind (placeSymbol "G2" (GateN (Or,2)) (middleOfSheet + { X = 300; Y = 120}))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 1))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 1))
            |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 0))
            |> Result.bind (placeWire (portOf "DEMUX2" 0) (portOf "MUX2" 2))
            |> Result.bind (placeWire (portOf "DEMUX2" 1) (portOf "MUX2" 0))
            |> Result.bind (placeWire (portOf "DEMUX2" 0) (portOf "G2" 0))
            |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G2" 1))
            //|> (fun res -> if beautify then Result.map SheetBeautifyD1.sheetAlignScale res else res)
            |> (fun res -> match increments with
                            | 2 -> Result.map SheetBeautifyD2.sheetOrderFlip res
                                   |> Result.map SheetBeautifyD1.sheetAlignScale
                            | 3 -> Result.map SheetBeautifyD2.sheetOrderFlip res
                                    |> Result.map SheetBeautifyD3.sheetWireLabelSymbol
                            | 1 -> Result.map SheetBeautifyD2.sheetOrderFlip res
                            | _ -> res)
            // |> (fun res -> if beautify then Result.map SheetBeautifyD3.sheetWireLabelSymbol res else res)
            |> Result.map SheetBeautifyD2.autoRouteAllWires
            |> getOkOrFail
    ///-------SINGLE FUNCTIONALITY TESTS------///

    //--------------------------------------------------------------------------------------------------//
    //-------------------------Example Test Circuits using Gen<'a> samples testing D3 Function----------//
    //--------------------------------------------------------------------------------------------------//

        ///Generate easy/likely-to-pass tests for SheetBeautifyD3.sheetWireLabelSymbol (D3) with all components being threshold distance apart.
        ///Takes in a threshold distance between components. A sample test value for compoent positions, rotations, flips.
        ///Returns a sheet with the circuit placed on it.
        let makeTestD3Easy (threshold: float) (sample: {|ApplyBeautify: bool; FlipMux: SymbolT.FlipType option; RotMux: Rotation; DemuxPos: XYPos; Mux1Pos: XYPos; Mux2Pos: XYPos|}) : SheetT.Model =
            let s = sample
            initSheetModel
            |> placeSymbol "DM1" (Demux4) (middleOfSheet - {X=threshold; Y=0.} + s.DemuxPos)
            |> Result.bind (placeSymbol "MUX1" (Mux4) (middleOfSheet + s.Mux1Pos))
            |> Result.map (rotateSymbol "MUX1" s.RotMux)
            |> match s.FlipMux with
                | Some f -> Result.map (flipSymbol "MUX1" f)
                | None -> id
            |> Result.bind (placeWire (portOf "DM1" 0) (portOf "MUX1" 0))
            |> Result.bind (placeWire (portOf "DM1" 1) (portOf "MUX1" 1))
            |> Result.bind (placeWire (portOf "DM1" 2) (portOf "MUX1" 2))
            |> Result.bind (placeWire (portOf "DM1" 3) (portOf "MUX1" 3))
            |> Result.bind (placeSymbol "MUX2" (Mux4) (middleOfSheet + {X=0.; Y=threshold} + s.Mux2Pos))
            |> Result.bind (placeWire (portOf "DM1" 0) (portOf "MUX2" 0))
            |> Result.bind (placeWire (portOf "DM1" 1) (portOf "MUX2" 1))
            |> Result.bind (placeWire (portOf "DM1" 2) (portOf "MUX2" 2))
            |> Result.bind (placeWire (portOf "DM1" 3) (portOf "MUX2" 3))
            |> (fun res -> if s.ApplyBeautify then Result.map SheetBeautifyD3.sheetWireLabelSymbol res else res)
            |> Result.map SheetBeautifyD2.autoRouteAllWires
            |> getOkOrFail
            //|> separateAllWires

        ///Generate a spaced out circuit set of easy/likely-to-pass tests for SheetBeautifyD3.sheetWireLabelSymbol (D3) with all components being threshold distance apart.
        ///Takes in a threshold distance between components. A sample test value for compoent positions, rotations, flips.
        ///Returns a sheet with the circuit placed on it.
        let makeTestD3SpacedOut (threshold: float) (sample: {|ApplyBeautify: bool; FlipMux: SymbolT.FlipType option; RotMux: Rotation; DemuxPos: XYPos; Mux1Pos: XYPos; Mux2Pos: XYPos|}) : SheetT.Model =
            let s = sample
            initSheetModel
            |> placeSymbol "MUX1" (Mux2) (middleOfSheet + { X = -700; Y = -300 } + s.Mux1Pos)
            |> match s.FlipMux with
                | Some f -> Result.map (flipSymbol "MUX1" f) //so if flip is not none hten do result.map else do identity function to pass model as is without flip
                | None -> id
            |> Result.bind (placeSymbol "MUX2" (Mux2) (middleOfSheet + s.Mux2Pos))
            |> Result.map (rotateSymbol "MUX2" s.RotMux)
            |> Result.bind (placeSymbol "DEMUX2" (Demux2) (middleOfSheet + { X = -600; Y = -150 } + s.DemuxPos))
            |> Result.bind (placeSymbol "G1" (GateN (And,2)) (middleOfSheet + { X = 400; Y = -100}))
            |> Result.bind (placeSymbol "G2" (GateN (Or,2)) (middleOfSheet + { X = 300; Y = 120}))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 1))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 1))
            |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 0))
            |> Result.bind (placeWire (portOf "DEMUX2" 0) (portOf "MUX2" 2))
            |> Result.bind (placeWire (portOf "DEMUX2" 1) (portOf "MUX2" 0))
            |> Result.bind (placeWire (portOf "DEMUX2" 0) (portOf "G2" 0))
            |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G2" 1))
            |> (fun res -> if s.ApplyBeautify then Result.map SheetBeautifyD3.sheetWireLabelSymbol res else res)
            |> Result.map SheetBeautifyD2.autoRouteAllWires
            |> getOkOrFail

        ///Generate a compressed circuit for SheetBeautifyD3.sheetWireLabelSymbol (D3) with all components being threshold distance apart.
        ///Takes in a threshold distance between components. A sample test value for compoent positions, rotations, flips.
        ///Returns a sheet with the circuit placed on it.
        let makeTestD3Compressed (threshold: float) (sample: {|ApplyBeautify: bool; FlipMux: SymbolT.FlipType option; RotMux: Rotation; DemuxPos: XYPos; Mux1Pos: XYPos; Mux2Pos: XYPos|}) : SheetT.Model =
            let s = sample
            initSheetModel
            |> placeSymbol "MUX1" (Mux2) (middleOfSheet + { X = 170; Y = -100 } + s.Mux1Pos)
            |> match s.FlipMux with
                | Some f -> Result.map (flipSymbol "MUX1" f) //so if flip is not none hten do result.map else do identity function to pass model as is without flip
                | None -> id
            |> Result.bind (placeSymbol "MUX2" (Mux2) (middleOfSheet + { X = -50; Y = -200 } + s.Mux2Pos))
            |> Result.map (rotateSymbol "MUX2" s.RotMux)
            |> Result.bind (placeSymbol "DEMUX2" (Demux2) (middleOfSheet + { X = -240; Y = -15 } + s.DemuxPos))
            |> Result.bind (placeSymbol "G1" (GateN (And,2)) (middleOfSheet + { X = 140; Y = 10}))
            |> Result.bind (placeSymbol "G2" (GateN (Or,2)) (middleOfSheet + { X = 80; Y = -80}))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 1))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "G1" 1))
            |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G1" 0))
            |> Result.bind (placeWire (portOf "DEMUX2" 0) (portOf "MUX2" 2))
            |> Result.bind (placeWire (portOf "DEMUX2" 1) (portOf "MUX2" 0))
            |> Result.bind (placeWire (portOf "DEMUX2" 0) (portOf "G2" 0))
            |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "G2" 1))
            |> (fun res -> if s.ApplyBeautify then Result.map SheetBeautifyD3.sheetWireLabelSymbol res else res)
            |> Result.map SheetBeautifyD2.autoRouteAllWires
            |> getOkOrFail

        ///Generate hard/likely-to-fail tests for SheetBeautifyD3.sheetWireLabelSymbol (D3) with all components being threshold distance apart.
        ///Takes in a threshold distance between components
        ///Returns a sheet with the circuit placed on it.
        let makeTestD3Hard (threshold: float) (sample: {|ApplyBeautify: bool; FlipMux: SymbolT.FlipType option; RotMux: Rotation; AndPos: XYPos; OrPos: XYPos; XorPos: XYPos; MuxPos: XYPos|}) : SheetT.Model =
            let s = sample
            initSheetModel
            |> placeSymbol "MUX1" (Mux2) (middleOfSheet - {X=threshold; Y=0.} + s.MuxPos)
            |> Result.bind (placeSymbol "OR1" (GateN(Or, 2)) (middleOfSheet - {X=0.0; Y=threshold} + s.OrPos))
            |> Result.map (rotateSymbol "MUX1" s.RotMux)
            |> match s.FlipMux with
                | Some f -> Result.map (flipSymbol "MUX1" f)
                | None -> id
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "OR1" 0))
            |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "OR1" 1))
            |> Result.bind (placeSymbol "AND1" (GateN(And, 2)) (middleOfSheet + {X=threshold; Y=threshold} + s.AndPos))
            |> Result.bind (placeWire (portOf "OR1" 0) (portOf "AND1" 0))
            |> Result.bind (placeWire (portOf "OR1" 0) (portOf "AND1" 1))
            |> Result.map (rotateSymbol "OR1" s.RotMux)
            |> match s.FlipMux with
                | Some f -> Result.map (flipSymbol "OR1" f)
                | None -> id
            |> Result.bind (placeWire (portOf "OR1" 0) (portOf "MUX1" 2))
            |> Result.bind (placeWire (portOf "AND1" 0) (portOf "MUX1" 1))
            |> Result.bind (placeSymbol "XOR1" (GateN(Xor, 2)) (middleOfSheet + {X=threshold; Y=0.} + s.XorPos))
            |> Result.bind (placeWire (portOf "AND1" 0) (portOf "XOR1" 0))
            |> Result.bind (placeWire (portOf "AND1" 0) (portOf "XOR1" 1))
            |> Result.bind (placeWire (portOf "XOR1" 0) (portOf "MUX1" 0))
            |> Result.bind (placeSymbol "AND2" (GateN (Nand,2)) (middleOfSheet + { X = -250; Y = 250}))
            |> Result.bind (placeSymbol "AND3" (GateN (And,2)) (middleOfSheet + { X = -150; Y = 180}))
            |> Result.bind (placeWire (portOf "AND3" 0) (portOf "AND2" 1))
            |> Result.bind (placeWire (portOf "AND2" 0) (portOf "AND3" 1))
            |> (fun res -> if s.ApplyBeautify then Result.map SheetBeautifyD3.sheetWireLabelSymbol res else res)
            // |> Result.map SheetBeautifyD2.autoRouteAllWires
            |> (fun res -> if s.ApplyBeautify then Result.map SheetBeautifyD2.sheetOrderFlip res else res)
            |> (fun res -> if s.ApplyBeautify then Result.map SheetBeautifyD1.sheetAlignScale res else res)
            |> getOkOrFail

        ///Generate tests using circuit generation DSL with given components and connections list
        ///Takes in a threshold distance between components and X,Y position.
        ///Returns a sheet with the circuit placed on it.
        let makeTestFixedConnectCircuitGen (threshold: float) (genSamples : XYPos) : SheetT.Model =
            let components = [
                { Label = "AND1"; CompType = GateN(And, 2) };
                { Label = "OR1"; CompType = GateN(Or, 2) };
                { Label = "XOR1"; CompType = GateN(Xor, 2) };
                { Label = "MUX1"; CompType = Mux2 };
            ]

            let connections = [
                { SourceCompLabel = "AND1"; SourceCompPort = 0; TargetCompLabel = "OR1"; TargetCompPort = 0 };
                { SourceCompLabel = "OR1"; SourceCompPort = 0; TargetCompLabel = "XOR1"; TargetCompPort = 1 };
                { SourceCompLabel = "XOR1"; SourceCompPort = 0; TargetCompLabel = "MUX1"; TargetCompPort = 1 };
                { SourceCompLabel = "MUX1"; SourceCompPort = 0; TargetCompLabel = "AND1"; TargetCompPort = 1 };
            ]

            fixedConnectCircuitGen components connections threshold genSamples

        ///Generate tests using circuit generation DSL with only given components list
        ///Takes in a threshold distance between components and X,Y position.
        ///Returns a sheet with the circuit placed on it.
        let makeTestRandomConnectCircuitGen (threshold: float) (genSamples : XYPos) : SheetT.Model =
            let components = [
                { Label = "AND1"; CompType = GateN(And, 2) };
                { Label = "OR1"; CompType = GateN(Or, 2) };
                { Label = "XOR1"; CompType = GateN(Xor, 2) };
                { Label = "MUX1"; CompType = Mux2 };
            ]

            randomConnectCircuitGen components threshold genSamples

    module sheetAlignScaleTests =
        let twoMuxTest (increments: int) =
            initSheetModel
            |> Builder.placeSymbol "AND" (GateN(And, 2)) middleOfSheet
            |> Result.bind(Builder.placeSymbol "I1" (Mux2) {X = middleOfSheet.X - 150.0; Y = middleOfSheet.Y - 75.0})
            |> Result.bind(Builder.placeSymbol "I2" (Mux2) {X = middleOfSheet.X - 150.0; Y = middleOfSheet.Y + 75.0})
            |> Result.bind(Builder.placeWire (portOf "I1" 0) (portOf "AND" 0))
            |> Result.bind(Builder.placeWire (portOf "I2" 0) (portOf "AND" 1))
            |> getOkOrFail
            |> beautifyIncrementally increments

        let andGateTest (increments: int) =
            initSheetModel
            |> Builder.placeSymbol "AND" Mux2 middleOfSheet
            |> Result.bind(Builder.placeSymbol "I1" (Input1(1, None)) {X = middleOfSheet.X - 150.0; Y = middleOfSheet.Y - 75.0})
            |> Result.bind(Builder.placeSymbol "I2" (Input1(1, None)) {X = middleOfSheet.X - 150.0; Y = middleOfSheet.Y + 75.0})
            |> getOkOrFail
            |> rotateSymbol "I1" Rotation.Degree90
            |> rotateSymbol "I1" Rotation.Degree90
            |> Builder.placeWire (portOf "I1" 0) (portOf "AND" 0)
            |> Result.bind(Builder.placeWire (portOf "I2" 0) (portOf "AND" 1))
            |> getOkOrFail
            |> beautifyIncrementally increments

        // Circuit demonstrating enough degrees of freedom to straighten non singly constrained components.
        let multipleMUXTest (increments: int) =
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
            |> beautifyIncrementally increments


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
        let customComponentScaling (increments: int) =
            initSheetModel
            |> Builder.placeSymbol "MAIN1" (Custom(customMain)) middleOfSheet
            |> Result.bind (Builder.placeSymbol "MAIN2" (Custom(customMain)) {X = middleOfSheet.X + 200.0; Y = middleOfSheet.Y - 100.0})
            |> Result.bind (Builder.placeWire (portOf "MAIN1" 0) (portOf "MAIN2" 0))
            |> Result.bind (Builder.placeWire (portOf "MAIN1" 1) (portOf "MAIN2" 1))
            |> Result.bind (Builder.placeWire (portOf "MAIN1" 2) (portOf "MAIN2" 2))
            |> TestLib.getOkOrFail
            |> beautifyIncrementally increments

        let testTripleMUX (increments: int) =
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
            |> beautifyIncrementally increments


        // Circuit emulating results from orderFlip, for compatibility with the other beautify algorithms.
        let orderFlipCompatibilityTest (increments: int) =
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
            |> beautifyIncrementally increments

        // Circuit with multiple connections between the same two components
        let scalingMultipleConnections (increments: int) =
            initSheetModel
            |> Builder.placeSymbol "DEMUX" Demux4 middleOfSheet
            |> Result.bind (Builder.placeSymbol "AND" (GateN(And, 3)) {middleOfSheet with X = middleOfSheet.X + 200.0})
            |> Result.bind (Builder.placeWire (portOf "DEMUX" 0) (portOf "AND" 0))
            |> Result.bind (Builder.placeWire (portOf "DEMUX" 1) (portOf "AND" 1))
            |> TestLib.getOkOrFail
            |> beautifyIncrementally increments





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

        ///Fails when sheet contains any overlapping symbols, any wire intersecting symbols, more than 4 total wire bends, more than 3 wire crossings
        ///Takes in a sample number for the current test and a sheet.
        ///Returns a string option, either: Some (error message) or None.
        let failD3 (sample: int) (sheet: SheetT.Model) =
            let metrics = countMetrics sheet
            if (metrics.SymbolOverlaps > 1 || metrics.WireIntersectSym > 0 || metrics.TotalWireBends > 15 || metrics.TotalWireCrossings > 1) && sample % 2 = 1 then
                Some ($"Test failed on sample {sample}: {metrics.SymbolOverlaps} symbol overlaps and {metrics.WireIntersectSym} wire symbol intersections.")
            else
                None



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

        /// singleTestForWireOverlaps test: A single test sheet to specifically test D3 function handling of wire overlapping.
        let singleTestForWireOverlaps testNum firstSample dispatch =
            runTestOnSheets
                "Multiple muxes and gates circuit: fail on wire overlapping "
                firstSample
                (fromList [0;1;2;3])
                D3Tests.makeTestForWireOverlaps
                // Asserts.failD3
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        /// singleTestForBitsOverlap test: A single test sheet to specifically test D3 function handling of bit legends overlapping wires.
        let singleTestForBitsOverlap testNum firstSample dispatch =
            runTestOnSheets
                "Bit legends and gates circuit: fail on wire overlapping with bit legend"
                firstSample
                (fromList [ false; true ])
                D3Tests.makeTestForBitsOverlap
                // Asserts.failD3
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        /// testD3Easy test: Checks if D3 works for sets of easy test cases with multiple wire overlaps, components.
        /// Test user can modify threshold variable to change the threshold distance between all components in the circuit.
        /// Runs count metrics on each test.
        let testD3Easy testNum firstSample dispatch =
            let threshold = 400.0
            runTestOnSheets
                "2 Mux4 and 1 DeMux threshold distance apart: fail on overlapping symbols or symbol wire intersect"
                firstSample
                makeSamplesD3Easy
                (D3Tests.makeTestD3Easy threshold)
                Asserts.failD3
                //Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch
            (collectMetricsOfTests makeSamplesD3Easy (D3Tests.makeTestD3Easy threshold))

        /// testD3Hard test: Checks if D3 works for sets of hard test cases with multiple wire overlaps, components.
        /// Test user can modify threshold variable to change the threshold distance between all components in the circuit.
        /// Runs count metrics on each test.
        let testD3Hard testNum firstSample dispatch =
            let threshold = 400.0
            runTestOnSheets
                "Mux2, AND, OR, XOR threshold distance apart: fail on overlapping symbols or symbol wire intersect"
                firstSample
                makeSamplesD3Hard
                (D3Tests.makeTestD3Hard threshold)
                // Asserts.failD3
                Asserts.failOnAllTests //allows iterating through all tests
                dispatch
            |> recordPositionInTest testNum dispatch
            (collectMetricsOfTests makeSamplesD3Hard (D3Tests.makeTestD3Hard threshold))

        /// testD3SpacedOut test: Checks if D3 works for test with spaced out components with multiple wire overlaps, wire intersections.
        /// Test user can modify threshold variable to change the threshold distance between all components in the circuit.
        /// Runs count metrics on each test.
        let testD3SpacedOut testNum firstSample dispatch =
            let threshold = 400.0
            runTestOnSheets
                "2 Mux4 and 1 DeMux threshold distance apart: fail on overlapping symbols or symbol wire intersect - 2nd Easy Case"
                firstSample
                makeSamplesD3Easy
                (D3Tests.makeTestD3SpacedOut threshold)
                Asserts.failD3
                //Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch
            (collectMetricsOfTests makeSamplesD3Easy (D3Tests.makeTestD3SpacedOut threshold))

        /// testD3Compressed test: Checks if D3 works for test with compressed or very close components with multiple wire overlaps, wire intersections.
        /// Test user can modify threshold variable to change the threshold distance between all components in the circuit.
        /// Runs count metrics on each test.
        let testD3Compressed testNum firstSample dispatch =
            let threshold = 1000.0
            runTestOnSheets
                "2 Mux4 and 1 DeMux threshold distance apart: fail on overlapping symbols or symbol wire intersect"
                firstSample
                makeSamplesD3Easy
                (D3Tests.makeTestD3Compressed threshold)
                //Asserts.failD3
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch
            (collectMetricsOfTests makeSamplesD3Easy (D3Tests.makeTestD3Compressed threshold))

        /// testFixedConnectCircuitGen test: Test created using DSL. Checks if D3 works for test circuit made from components and connections list.
        /// Test user can modify threshold variable to change the threshold distance between all components in the circuit.
        /// Runs count metrics on each test.
        let testFixedConnectCircuitGen testNum firstSample dispatch =
            let threshold = 400.0
            runTestOnSheets
                "Circuit made of components and connections list, threshold distance apart: fail on overlapping symbols or symbol wire intersect"
                firstSample
                makePositions
                (D3Tests.makeTestFixedConnectCircuitGen threshold)
                Asserts.failD3
                //Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        /// testRandomConnectCircuitGen test: Test created using DSL. Checks if D3 works for test circuit made from only from components list that are randomly connected.
        /// Test user can modify threshold variable to change the threshold distance between all components in the circuit.
        /// Runs count metrics on each test.
        let testRandomConnectCircuitGen testNum firstSample dispatch =
            let threshold = 400.0
            runTestOnSheets
                "Circuit made of components list and randomly connected components, threshold distance apart: fail on overlapping symbols or symbol wire intersect"
                firstSample
                makePositions
                (D3Tests.makeTestRandomConnectCircuitGen threshold)
                Asserts.failD3
                dispatch
            |> recordPositionInTest testNum dispatch

        /// testD3EasyIndiv test: Checks if D3 works for sets of easy test cases and iterates through each test case and shows circuit before and after D3 function applied.
        /// Test user can modify threshold variable to change the threshold distance between all components in the circuit.
        /// Runs count metrics on each test.
        let testD3EasyIndiv testNum firstSample dispatch =
            let threshold = 400.0
            runTestOnSheets
                "2 Mux4 and 1 DeMux threshold distance apart: fail on overlapping symbols or symbol wire intersect"
                firstSample
                makeSamplesD3Easy
                (D3Tests.makeTestD3Easy threshold)
                //Asserts.failD3
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch
            (collectMetricsOfTests makeSamplesD3Easy (D3Tests.makeTestD3Easy threshold))

        /// Example test: Horizontally positioned AND + DFF: fail on symbols intersect
        let multiConnect testNum firstSample dispatch =
            runTestOnSheets
                "Demux and 3 input and gate showcasing capabilities of D1 to scale"
                firstSample
                (fromList [0;1;2;3])
                sheetAlignScaleTests.scalingMultipleConnections
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Example test: Horizontally positioned AND + DFF: fail all tests
        let tripleMUX testNum firstSample dispatch =
            runTestOnSheets
                "Complicated triple mux circuit with multiple degrees of freedom"
                firstSample
                (fromList [0;1;2;3])
                sheetAlignScaleTests.testTripleMUX
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        let customComponents testNum firstSample dispatch =
            runTestOnSheets
                "Classic custom component scaling test"
                firstSample
                (fromList [0;1;2;3])
                sheetAlignScaleTests.customComponentScaling
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch
        let andGate testNum firstSample dispatch =
            runTestOnSheets
                "Rotated and gate"
                firstSample
                (fromList [0;1;2;3])
                sheetAlignScaleTests.andGateTest
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch


        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            // Change names and test functions as required
            // delete unused tests from list
            [
                "Test1", singleTestForWireOverlaps // single test to evaluate D3 handling wire overlaps
                "Test2", testD3EasyIndiv // set of tests to evaluate D3 designed using DSL with given components and connections list
                "Test3", testD3Easy // set of easy tests to evaluate D3
                "Test4", testD3Hard // set of hard tests to evaluate D3
                "Test5", multiConnect // set of test with more spaced out components to evaluate D3
                "Test6", tripleMUX // set of test with more compressed components to evaluate D3
                "Test7", customComponents
                "Test8", andGate  // set of tests to evaluate D3 designed using DSL with only components list, randomly connect components
                "Next Test Error", fun _ _ _ -> printf "Next Error:"
            ]
            // Easy, EasyIndiv, testD3Hard

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






