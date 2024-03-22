module TestDrawBlock
open GenerateData
open SheetBeautifyHelpers
open Elmish
open Helpers


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
    open SheetBeautifyD2
    open SheetBeautify

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
        /// Index must be in range >= 1
        let tryCoalesceAboutIndex (segVecs: XYPos list) (index: int)  =
            if index < segVecs.Length - 1 && segVecs[index] =~ XYPos.zero
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

    /// Helper function to count the number of right angles in a wire's visible segments
    let countNonStraightenedWires (model: SheetT.Model) : int =
        let numWireRightAngles (_, segs: XYPos list) =
            List.pairwise segs
            |> List.filter (fun (seg1, seg2) -> seg1.X <> seg2.X && seg1.Y <> seg2.Y)
            |> List.length // Number of right angles in the wire
        SegmentHelpers.allVisibleSegments model
        |> List.map numWireRightAngles // List of number of right angles for each wire
        |> List.filter ((>) 1) // List of number of right angles for each wire with more than 1 right angle (1 right-angle is allowed)
        |> List.length // Number of wires with non-zero right angle counts

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
        
        /// Place a new symbol with label symLabel onto the Sheet with given position and rotation.
        let placeRotatedSymbol
            (symLabel: string)
            (compType: ComponentType)
            (position: XYPos)
            (rotation: Rotation)
            (model: SheetT.Model)
            : Result<SheetT.Model, string>
            =
            let symLabel = String.toUpper symLabel // make label into its standard casing
            let symModel, symId =
                SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
            let rotSymbol symToRot =
                RotateScale.rotateSymbolInBlock rotation ((RotateScale.getBlock [ symToRot ]).Centre()) symToRot
            let rotSymModel = SymbolUpdate.updateSymbol rotSymbol symId symModel
            let rotSym = rotSymModel.Symbols[symId]
            match position + rotSym.getScaledDiagonal with
            | { X = x; Y = y } when x > maxSheetCoord || y > maxSheetCoord ->
                Error
                    $"symbol '{symLabel}' position {position + rotSym.getScaledDiagonal} and rotation {rotation} lies outside allowed coordinates"
            | _ ->
                model
                |> Optic.set symbolModel_ rotSymModel
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
            
        
        /// Rotate a symbol by a rotation amount, given its label. Returns the rotated model
        let rotateSymbol (symLabel: string) (rotate: Rotation) (model: SheetT.Model) : Result<SheetT.Model,string> =
            let componentList =
                mapValues model.Wire.Symbol.Symbols
                |> Array.find (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
                |> (fun x -> [x.Id])

            let rotmodel = 
                {model with Wire = {model.Wire with Symbol = (RotateScale.rotateBlock componentList model.Wire.Symbol rotate)}
                            TmpModel = Some model
                            UndoList = SheetUpdateHelpers.appendUndoList model.UndoList model}

            let newModel = {rotmodel with BoundingBoxes = Symbol.getBoundingBoxes rotmodel.Wire.Symbol}
         
            let errorComponents =
                newModel.SelectedComponents
                |> List.filter (fun sId -> not (Sheet.notIntersectingComponents newModel newModel.BoundingBoxes[sId] sId))

            {newModel with ErrorComponents = errorComponents; Action = SheetT.CurrentAction.DragAndDrop}|> Ok

        /// Flip a symbol along the axis 'flip' given its symbol label. Returns the flipped model.
        let flipSymbol (symLabel: string) (flip: SymbolT.FlipType) (model: SheetT.Model) : Result<SheetT.Model,string> =
            let componentList =
                mapValues model.Wire.Symbol.Symbols
                |> Array.find (fun sym -> caseInvariantEqual sym.Component.Label symLabel)
                |> (fun x -> [x.Id])

            let flipmodel = 
                {model with Wire = {model.Wire with Symbol = (RotateScale.flipBlock componentList model.Wire.Symbol flip)}
                            TmpModel = Some model
                            UndoList = SheetUpdateHelpers.appendUndoList model.UndoList model}

            let newModel = {flipmodel with BoundingBoxes = Symbol.getBoundingBoxes flipmodel.Wire.Symbol}

            let errorComponents =
                newModel.SelectedComponents
                |> List.filter (fun sId -> not (Sheet.notIntersectingComponents newModel newModel.BoundingBoxes[sId] sId))

            let nextAction = 
                match errorComponents with
                    | [] -> 
                        newModel.Action
                    | _ ->
                        SheetT.CurrentAction.DragAndDrop

            {newModel with ErrorComponents = errorComponents; Action = nextAction}|> Ok

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

//--------------------------------------------------------------------------------------------------//
//---------------------------Example Test Circuits using Gen<'a> samples----------------------------//
//--------------------------------------------------------------------------------------------------//

    open Builder

    /// return flipped or rotated model or a model with no change (rotate 0 degree) for a symbol based on a random number
    /// for rotating only , provide value between 0 - 100, for flipping only, provide value between 100-200,
    /// for flipping and rotating, provide value betwenn 0 - 200.
    let flipRot (randomization:int) (symbolName:string) =
            if randomization >= 100 then
                if randomization >=133 then
                    if randomization >=166
                    then flipSymbol symbolName SymbolT.FlipType.FlipHorizontal
                    else flipSymbol symbolName SymbolT.FlipType.FlipVertical
                else rotateSymbol symbolName Rotation.Degree0
            elif randomization < 100 then
                if randomization >=50 then
                    if randomization >=75 then rotateSymbol symbolName Rotation.Degree90
                    else rotateSymbol symbolName Rotation.Degree180
                else
                    if randomization >=25 then rotateSymbol symbolName Rotation.Degree270
                    else rotateSymbol symbolName Rotation.Degree0
            else rotateSymbol symbolName Rotation.Degree0

    /// Places wires from two input components to random ports of muxName. 
    let connectRandomMuxPorts (randomization:int) (muxName:string) (source1CompLabel:string) (source2CompLabel:string) =
            if randomization > 100 then
                (placeWire (portOf source2CompLabel 0) (portOf muxName 0), placeWire (portOf source1CompLabel 0) (portOf muxName 1))
            else
                (placeWire (portOf source2CompLabel 0) (portOf muxName 1), placeWire (portOf source1CompLabel 0) (portOf muxName 0))

    /// Places wires from sourceComp to a random port of targetComp. 
    let connectRandomCompPort (randomization:int) (targetCompLabel:string) (sourceCompLabel:string) =
        if randomization > 100 then
            placeWire (portOf sourceCompLabel 0) (portOf targetCompLabel 0)
        else
            placeWire (portOf sourceCompLabel 0) (portOf targetCompLabel 1)        
    
    /// Make a circuit for overlap testing.
    let makeOverlapTestingCircuit (posRots:XYPos list * int list) =
        initSheetModel
        |> placeSymbol "S1" (Input1(1,None)) ((fst posRots)[0])
        |> Result.bind (placeSymbol "S2" (Input1(1,None)) ((fst posRots)[1]))
        |> Result.bind (placeSymbol "MUX1" Mux2 ((fst posRots)[2]))
        |> Result.bind (placeSymbol "MUX2" Mux2 ((fst posRots)[3]))
        |> Result.bind (placeSymbol "G1" (GateN(And,2)) ((fst posRots)[4]))
        |> Result.bind (flipRot ((snd posRots)[0]) "S1")
        |> Result.bind (flipRot ((snd posRots)[1]) "S2")
        |> Result.bind (flipRot ((snd posRots)[2]) "MUX1")
        |> Result.bind (flipRot ((snd posRots)[3]) "MUX2")
        |> Result.bind (flipRot ((snd posRots)[4]) "G1")
        |> getOkOrFail

    /// Returns true if symbol intersects symbol on sheet.
    let symbolIntersectsSymbol (sheet: SheetT.Model) =
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n,box)
            List.allPairs boxes boxes 
            |> List.exists (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)

    /// Returns true if the given set of positions contains overlapping symbols
    let overlapAnySymbol (posRots:XYPos list * int list) =
            makeOverlapTestingCircuit posRots
            |>symbolIntersectsSymbol


    /// Get a Gen of random positions (in list of pos on one axis for different components)
    /// and randomizer values (in list of int for different components).
    let randomCompPosVal (componentCount: int) (testCount: int) (posRange:int*int) (randomizerRange: int*int)=
        /// Converts random x and y values to an XYPos type
        let coordinateBatch (initialPos: XYPos) (xCoords: int list) (yCoords: int list): XYPos list =
            (xCoords,yCoords)
            ||> List.map2 (fun x y -> initialPos + {X= float x; Y= float y})
        
        map3 (fun n m k -> (coordinateBatch middleOfSheet n m,
                            k))
                            (randomIntList (fst posRange) (snd posRange) testCount componentCount)
                            (randomIntList (fst posRange) (snd posRange) testCount componentCount)
                            (randomIntList (fst randomizerRange) (snd randomizerRange) testCount componentCount)
        |> filter (fun s -> not (overlapAnySymbol s))

    /// Get a Gen of small random deviations (in list of pos on one axis for different components)
    /// and randomizer values (in list of int for different components).
    let minorRandomCompPosVal (componentCount: int) (testCount: int) (posRange:int*int) (randomizerRange: int*int)=
        /// Converts random x and y values to an XYPos type
        let coordinateBatch (initialPos: XYPos) (xCoords: int list) (yCoords: int list): XYPos list =
            (xCoords,yCoords)
            ||> List.map2 (fun x y -> {X= float x; Y= float y})
        
        map3 (fun n m k -> (coordinateBatch middleOfSheet n m,
                            k))
                            (randomIntList (fst posRange) (snd posRange) testCount componentCount)
                            (randomIntList (fst posRange) (snd posRange) testCount componentCount)
                            (randomIntList (fst randomizerRange) (snd randomizerRange) testCount componentCount)

    /// Make test circuit for D1. Places 2 Muxes, 4 inputs, and 1 output. Along with wires in between.
    /// Small random deviations from a perfectly straightened circuit, such that the D1 algorithm only has to make small corrections.
    let makeD1CircuitA2 (posRots:XYPos list * int list)  =
        
        initSheetModel
        |> placeSymbol "MUX1" Mux2 middleOfSheet
        |> Result.bind (
            placeSymbol
                "A"
                (Input1(1, None))
                (middleOfSheet
                 - { X = (150.); Y = (-28. +  ((fst posRots)[0]).X) })
        )
        |> Result.bind (placeSymbol "B" (Input1(1, None)) (middleOfSheet - { X = 150.; Y = 28. - ((fst posRots)[1]).Y}))
        |> Result.bind (
            placeRotatedSymbol "S1" (Input1(1, None)) (middleOfSheet - { X = 0. + ((fst posRots)[2]).X; Y = (-200.) }) Degree90
        )
        |> Result.bind (placeSymbol "MUX2" Mux2 (middleOfSheet - { X = (-100.); Y = (-50.) }))
        |> Result.bind (
            placeSymbol
                "S2"
                (Input1(1, None))
                (middleOfSheet
                 - { X = (100. + ((fst posRots)[2]).X); Y = (-150.) })
        )
        |> Result.bind (
            placeSymbol
                "C"
                (Output 1)
                (middleOfSheet
                 - { X = (-250.); Y = (-100. + ((fst posRots)[1]).Y) })
        )
        |> Result.bind (placeWire (portOf "A" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "B" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "C" 0))
        |> getOkOrFail
        |> beautifySheet


    /// Makes a non-beautified circuit with 3 muxes, 6 inputs, 1 output and a set of IO labels. All with random minor deviations from straight wires
    /// for comparison to Test4 which is a beautified version
    let makeTest2Circuit (posRots:XYPos list * int list) =

        initSheetModel
        |> placeSymbol "S1" (Input1(1, None)) ({X = 1511.38; Y = 1760.53}+(fst posRots)[0])
        |> Result.bind (placeSymbol "IN3" (Input1(1, None)) ({X = 1386.75; Y = 1605.23}+(fst posRots)[1]))
        |> Result.bind (placeSymbol "IN4" (Input1(1, None)) ({X = 1386.75; Y = 1661.48}+(fst posRots)[2]))
        |> Result.bind (placeSymbol "MUX1" Mux2 ({X = 1496.38; Y = 1633.36}+(fst posRots)[3]))
        |> Result.bind (placeSymbol "MUX3" Mux2 ({X = 1960.15; Y = 1577.11}+(fst posRots)[4]))
        |> Result.bind (placeSymbol "IN1" (Input1(1, None)) ({X = 1590.37; Y = 1577.11}+(fst posRots)[5]))
        |> Result.bind (placeSymbol "S3" (Input1(1, None)) ({X = 1775.15; Y = 1679.97}+(fst posRots)[6]))
        |> Result.bind (placeSymbol "MUX2" Mux2 ({X = 1702.15; Y = 1605.23}+(fst posRots)[7]))
        |> Result.bind (placeSymbol "IN2" (Input1(1, None)) ({X = 1822.09; Y = 1548.98}+(fst posRots)[8]))
        |> Result.bind (placeSymbol "OUT1" (Output(1)) ({X = 2150.15; Y = 1577.11}+(fst posRots)[9]))
        |> Result.bind (placeSymbol "IO1" (IOLabel) ({X = 2150.15; Y = 1477.11}+(fst posRots)[9]))
        |> Result.bind (placeWire (portOf "IN3" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "IN4" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "IN1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "IN2" 0) (portOf "MUX3" 0))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "MUX3" 1))
        |> Result.bind (placeWire (portOf "S3" 0) (portOf "MUX3" 2))
        //|> Result.bind (placeWire (portOf "MUX3" 0) (portOf "OUT1" 0))
        |> Result.bind (placeWire (portOf "MUX3" 0) (portOf "IO1" 0))
        |> Result.bind (placeSymbol "IO1" (IOLabel) ({X = 2150.15; Y = 1677.11}+(fst posRots)[9]))
        |> Result.bind (placeWire (portOf "IO1" 0) (portOf "OUT1" 0))
        |> getOkOrFail
        |> beautifySheet


    /// Makes a non-beautified circuit with 3 muxes, 6 inputs and 1 output all with random minor deviations from straight wires
    /// for comparison to Test4 which is a beautified version
    let makeTest3Circuit (posRots:XYPos list * int list) =

        initSheetModel
        |> placeSymbol "S1" (Input1(1, None)) ({X = 1511.38; Y = 1760.53}+(fst posRots)[0])
        |> Result.bind (placeSymbol "IN3" (Input1(1, None)) ({X = 1386.75; Y = 1605.23}+(fst posRots)[1]))
        |> Result.bind (placeSymbol "IN4" (Input1(1, None)) ({X = 1386.75; Y = 1661.48}+(fst posRots)[2]))
        |> Result.bind (placeSymbol "MUX1" Mux2 ({X = 1496.38; Y = 1633.36}+(fst posRots)[3]))
        |> Result.bind (placeSymbol "MUX3" Mux2 ({X = 1960.15; Y = 1577.11}+(fst posRots)[4]))
        |> Result.bind (placeSymbol "IN1" (Input1(1, None)) ({X = 1590.37; Y = 1577.11}+(fst posRots)[5]))
        |> Result.bind (placeSymbol "S3" (Input1(1, None)) ({X = 1775.15; Y = 1679.97}+(fst posRots)[6]))
        |> Result.bind (placeSymbol "MUX2" Mux2 ({X = 1702.15; Y = 1605.23}+(fst posRots)[7]))
        |> Result.bind (placeSymbol "IN2" (Input1(1, None)) ({X = 1822.09; Y = 1548.98}+(fst posRots)[8]))
        |> Result.bind (placeSymbol "OUT1" (Output(1)) ({X = 2150.15; Y = 1577.11}+(fst posRots)[9]))
        |> Result.bind (placeWire (portOf "IN3" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "IN4" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "IN1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "IN2" 0) (portOf "MUX3" 0))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "MUX3" 1))
        |> Result.bind (placeWire (portOf "S3" 0) (portOf "MUX3" 2))
        |> Result.bind (placeWire (portOf "MUX3" 0) (portOf "OUT1" 0))
        |> getOkOrFail

    /// Makes a beautified circuit with 3 muxes, 6 inputs and 1 output all with random minor deviations from straight wires
    /// in order to test sheetAlignScale on a crowded circuit, with restrictive possible target positions for the Muxes.
    let makeTest4Circuit (posRots:XYPos list * int list) =

        initSheetModel
        |> placeSymbol "S1" (Input1(1, None)) ({X = 1511.38; Y = 1760.53}+(fst posRots)[0])
        |> Result.bind (placeSymbol "IN3" (Input1(1, None)) ({X = 1386.75; Y = 1605.23}+(fst posRots)[1]))
        |> Result.bind (placeSymbol "IN4" (Input1(1, None)) ({X = 1386.75; Y = 1661.48}+(fst posRots)[2]))
        |> Result.bind (placeSymbol "MUX1" Mux2 ({X = 1496.38; Y = 1633.36}+(fst posRots)[3]))
        |> Result.bind (placeSymbol "MUX3" Mux2 ({X = 1960.15; Y = 1577.11}+(fst posRots)[4]))
        |> Result.bind (placeSymbol "IN1" (Input1(1, None)) ({X = 1590.37; Y = 1577.11}+(fst posRots)[5]))
        |> Result.bind (placeSymbol "S3" (Input1(1, None)) ({X = 1775.15; Y = 1679.97}+(fst posRots)[6]))
        |> Result.bind (placeSymbol "MUX2" Mux2 ({X = 1702.15; Y = 1605.23}+(fst posRots)[7]))
        |> Result.bind (placeSymbol "IN2" (Input1(1, None)) ({X = 1822.09; Y = 1548.98}+(fst posRots)[8]))
        |> Result.bind (placeSymbol "OUT1" (Output(1)) ({X = 2150.15; Y = 1577.11}+(fst posRots)[9]))
        |> Result.bind (placeWire (portOf "IN3" 0) (portOf "MUX1" 0))
        |> Result.bind (placeWire (portOf "IN4" 0) (portOf "MUX1" 1))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX1" 2))
        |> Result.bind (placeWire (portOf "IN1" 0) (portOf "MUX2" 0))
        |> Result.bind (placeWire (portOf "MUX1" 0) (portOf "MUX2" 1))
        |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 2))
        |> Result.bind (placeWire (portOf "IN2" 0) (portOf "MUX3" 0))
        |> Result.bind (placeWire (portOf "MUX2" 0) (portOf "MUX3" 1))
        |> Result.bind (placeWire (portOf "S3" 0) (portOf "MUX3" 2))
        |> Result.bind (placeWire (portOf "MUX3" 0) (portOf "OUT1" 0))
        |> getOkOrFail
        |> beautifySheet

    /// Makes a circuit with 2 inputs, 2 muxes and an AND gate where positions have small random deviations
    /// For testing both D1 and D2
    let makeTest5Circuit (posRots:XYPos list * int list) =

        // Placing wires between components with no randomisation
        let MUX2Connection1, MUX2Connection2 = connectRandomMuxPorts (0) "MUX2" "S1" "MUX1"
        let ANDConnection1, ANDConnection2 = connectRandomMuxPorts (0) "G1" "MUX2" "MUX1"


        initSheetModel
        |> placeSymbol "MUX1" Mux2 ({X = 1482.91; Y = 1711.89}+(fst posRots)[0])
        |> Result.bind (placeSymbol "S2" (Input1(1, None)) ({X = 1650.06; Y = 1906.01}+(fst posRots)[1]))
        |> Result.bind (placeSymbol "S1" (Input1(1, None)) ({X = 1474.5; Y = 1595.86}+(fst posRots)[2]))
        |> Result.bind (placeSymbol "MUX2" Mux2 ({X = 1717.43; Y = 1593.99}+(fst posRots)[3]))
        |> Result.bind (placeSymbol "G1" (GateN(And, 2)) ({X = 1973.56; Y = 1630.55}+(fst posRots)[4]))
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
        |> Result.bind (MUX2Connection1)
        |> Result.bind (MUX2Connection2)
        |> Result.bind (ANDConnection1)
        |> Result.bind (ANDConnection2)
        |> getOkOrFail
                

    /// Makes a beautified circuit with 2 inputs, 2 muxes and an AND gate where positions originally had small random deviations
    /// For testing both D1 and D2
    let makeTest6Circuit (posRots:XYPos list * int list) =

        // Placing wires between components with no randomisation
        let MUX2Connection1, MUX2Connection2 = connectRandomMuxPorts (0) "MUX2" "S1" "MUX1"
        let ANDConnection1, ANDConnection2 = connectRandomMuxPorts (0) "G1" "MUX2" "MUX1"


        initSheetModel
        |> placeSymbol "MUX1" Mux2 ({X = 1482.91; Y = 1711.89}+(fst posRots)[0])
        |> Result.bind (placeSymbol "S2" (Input1(1, None)) ({X = 1650.06; Y = 1906.01}+(fst posRots)[1]))
        |> Result.bind (placeSymbol "S1" (Input1(1, None)) ({X = 1474.5; Y = 1595.86}+(fst posRots)[2]))
        |> Result.bind (placeSymbol "MUX2" Mux2 ({X = 1717.43; Y = 1593.99}+(fst posRots)[3]))
        |> Result.bind (placeSymbol "G1" (GateN(And, 2)) ({X = 1973.56; Y = 1630.55}+(fst posRots)[4]))
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
        |> Result.bind (MUX2Connection1)
        |> Result.bind (MUX2Connection2)
        |> Result.bind (ANDConnection1)
        |> Result.bind (ANDConnection2)
        |> getOkOrFail
        |> beautifySheet


    /// Creates a non-beautified circuit of two inputs, two muxes and an AND gate with random positions, random flip state, random rotations and random inversion of MUX input ports
    /// Compare to test 8 which is the beautified version
    let makeTest7Circuit (posRots:XYPos list * int list) =
        let MUX2Connection1, MUX2Connection2 = connectRandomMuxPorts ((snd posRots)[3]) "MUX2" "S1" "MUX1"
        let ANDConnection1, ANDConnection2 = connectRandomMuxPorts ((snd posRots)[4]) "G1" "MUX2" "MUX1"

        initSheetModel
        |> placeSymbol "S1" (Input1(1,None)) ((fst posRots)[0])
        |> Result.bind (placeSymbol "S2" (Input1(1,None)) ((fst posRots)[1]))
        |> Result.bind (placeSymbol "MUX1" Mux2 ((fst posRots)[2]))
        |> Result.bind (placeSymbol "MUX2" Mux2 ((fst posRots)[3]))
        |> Result.bind (placeSymbol "G1" (GateN(And,2)) ((fst posRots)[4]))
        |> Result.bind (flipRot ((snd posRots)[0]) "S1")
        |> Result.bind (flipRot ((snd posRots)[1]) "S2")
        |> Result.bind (flipRot ((snd posRots)[2]) "MUX1")
        |> Result.bind (flipRot ((snd posRots)[3]) "MUX2")
        |> Result.bind (flipRot ((snd posRots)[4]) "G1")
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
        |> Result.bind (MUX2Connection1)
        |> Result.bind (MUX2Connection2)
        |> Result.bind (ANDConnection1)
        |> Result.bind (ANDConnection2)
        |> getOkOrFail
        |> beautifySheet

    /// Creates a beautified circuit of two inputs, two muxes and an AND gate with random positions, random flip state, random rotations and random inversion of MUX input ports
    /// Compare to test 8 which is the beautified version
    /// Used to test the D2 algorithm
    let makeTest8Circuit (posRots:XYPos list * int list) =
        let MUX2Connection1, MUX2Connection2 = connectRandomMuxPorts ((snd posRots)[3]) "MUX2" "S1" "MUX1"
        let ANDConnection1, ANDConnection2 = connectRandomMuxPorts ((snd posRots)[4]) "G1" "MUX2" "MUX1"

        initSheetModel
        |> placeSymbol "S1" (Input1(1,None)) ((fst posRots)[0])
        |> Result.bind (placeSymbol "S2" (Input1(1,None)) ((fst posRots)[1]))
        |> Result.bind (placeSymbol "MUX1" Mux2 ((fst posRots)[2]))
        |> Result.bind (placeSymbol "MUX2" Mux2 ((fst posRots)[3]))
        |> Result.bind (placeSymbol "G1" (GateN(And,2)) ((fst posRots)[4]))
        |> Result.bind (flipRot ((snd posRots)[0]) "S1")
        |> Result.bind (flipRot ((snd posRots)[1]) "S2")
        |> Result.bind (flipRot ((snd posRots)[2]) "MUX1")
        |> Result.bind (flipRot ((snd posRots)[3]) "MUX2")
        |> Result.bind (flipRot ((snd posRots)[4]) "G1")
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
        |> Result.bind (MUX2Connection1)
        |> Result.bind (MUX2Connection2)
        |> Result.bind (ANDConnection1)
        |> Result.bind (ANDConnection2)
        |> Result.bind (optimalEdgeOrder) // Call D2 algorithm to beautify model
        |> getOkOrFail
        // |> alignSymbols // Call D1 algorithm to beautify model by straightening wires

    /// <summary>HLP 24: AUTHOR kv221 - Circuit maker that outputs a model containing a random number of 
    /// randomly chosen ISSIE components from a provided list. Components have random positions, rotations and flip state.<para></para> 
    /// Places wires between each component <para></para>
    /// Random circuit each time <para></para>
    /// Helpful for comparison to makeD4TestCircuitRandomAligned which is a beautified version.</summary>
    /// <param name="posRots"> Tuple of: a list of random positions (XYPos), and a list of random rotation states (int)</param>
    /// <param name="possibleComps"> List of component types (ComponentType list) that can be randomly selected for placement</param>
    /// <returns>SheetT.model containing the randomly generated circuit</returns>
    let makeD4TestCircuitRandom (posRots:XYPos list * int list) = // (possibleComps: ComponentType list) (maxNumSymbols: int) =
        // Parameters. TODO: Make these provided as arguments to the function, and passed as Gens to the testrunner
        let maxNumSymbols = 5
        // let possibleComps = [Mux2; Mux4; Demux2; GateN(And,2); GateN(Or,2); GateN(Xor,2); GateN(Nand,2); GateN(Nor,2); GateN(Xnor,2); Input1(1,None); Output 1;]
        let possibleComps = [Mux2; Mux4; Demux2; GateN(And,2); GateN(Or,2); GateN(Xor,2); GateN(Nand,2); GateN(Nor,2); GateN(Xnor,2);]
        
        // Random number of components
        let numComps = fromList (List.init maxNumSymbols (fun _ -> 0))

        // Generate a list of random symbols of length maxNumSymbols
        let random = System.Random()
        let randomComponents = List.init maxNumSymbols (fun _ -> List.item (random.Next(possibleComps.Length)) possibleComps)

        // Place all symbols
        let model: Result<SheetT.Model,string> = 
            randomComponents
            |> List.mapi (fun index comp -> placeSymbol (index.ToString()) comp ((fst posRots).[index])) // for the list of random components, create a function to place them. TODO: make labels relevant to their component
            |> List.fold (fun (acc: (Result<SheetT.Model, string>)) (symbolPlacer: (SheetT.Model -> Result<SheetT.Model,string>)) -> 
                Result.bind symbolPlacer acc) (Ok initSheetModel) // place each symbol in the sheet and return the final model
            
        // Randomly place wires between random components
        let placeRandomWiresOnModel (model: SheetT.Model) =
            List.init (maxNumSymbols - 1) (fun i -> (i, i + 1)) // component pairs [(0,1); (1,2); ...]
            |> List.mapi (fun index pair  -> connectRandomCompPort ((snd posRots)[index]) ((snd pair).ToString()) ((fst pair).ToString()))
            |> List.fold (fun (acc: (Result<SheetT.Model,string>)) (wirePlacer: (SheetT.Model -> Result<SheetT.Model,string>)) -> 
                Result.bind wirePlacer acc) (Ok model)

        model
        |> Result.bind placeRandomWiresOnModel
        |> getOkOrFail
    
    /// <summary>HLP 24: AUTHOR kv221 - Outputs a beautified version of a model containing a random number of 
    /// randomly chosen ISSIE components from a provided list. Components have random positions, rotations and flip state.<para></para> 
    /// Places wires between each component <para></para>
    /// Helpful for generating testcases for D4 (SheetBeautify) verification and demonstration.</summary>
    /// Random circuit each time <para></para>
    /// <param name="posRots"> Tuple of: a list of random positions (XYPos), and a list of random rotation states (int)</param>
    /// <param name="possibleComps"> List of component types (ComponentType list) that can be randomly selected for placement</param>
    /// <returns>SheetT.model containing the randomly generated circuit</returns>
    let makeD4TestCircuitRandomAligned (posRots:XYPos list * int list) = // (possibleComps: ComponentType list) (maxNumSymbols: int) =
        // Parameters. TODO: Make these provided as arguments to the function, and passed as Gens to the testrunner
        let maxNumSymbols = 5
        let possibleComps = [Mux2; Mux4; Demux2; GateN(And,2); GateN(Or,2); GateN(Xor,2); GateN(Nand,2); GateN(Nor,2); GateN(Xnor,2);]

        // Generate a list of random symbols of length maxNumSymbols
        let random = System.Random()
        let randomComponents = List.init maxNumSymbols (fun _ -> List.item (random.Next(possibleComps.Length)) possibleComps)

        // Place all symbols
        let model: Result<SheetT.Model,string> = 
            randomComponents
            |> List.mapi (fun index comp -> placeSymbol (index.ToString()) comp ((fst posRots).[index])) // for the list of random components, create a function to place them. TODO: make labels relevant to their component
            |> List.fold (fun (acc: (Result<SheetT.Model, string>)) (symbolPlacer: (SheetT.Model -> Result<SheetT.Model,string>)) -> 
                Result.bind symbolPlacer acc) (Ok initSheetModel) // place each symbol in the sheet and return the final model
            
        // Randomly place wires between random components
        let placeRandomWiresOnModel (model: SheetT.Model) =
            List.init (maxNumSymbols - 1) (fun i -> (i, i + 1)) // component pairs [(0,1); (1,2); ...]
            |> List.mapi (fun index pair  -> connectRandomCompPort ((snd posRots)[index]) ((snd pair).ToString()) ((fst pair).ToString()))
            |> List.fold (fun (acc: (Result<SheetT.Model,string>)) (wirePlacer: (SheetT.Model -> Result<SheetT.Model,string>)) -> 
                Result.bind wirePlacer acc) (Ok model)

        model
        |> Result.bind placeRandomWiresOnModel
        |> getOkOrFail
        |> beautifySheet

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

        /// Fail on all tests, return the average segents a wire has in the sheet to evaluate 
        /// the effect of straightening up wires.
        let countWireSegs (sample: int) (sheet: SheetT.Model) =
            let rightAngles = numOfVisRightAngles sheet
            let wireCount = Map.count sheet.Wire.Wires
            let rightAngs = countWireRightAngles sheet
            let avgSegsPerWire = float rightAngs/float wireCount
            Some <| $"There are {rightAngles} right angles,\n A wire consists of average {avgSegsPerWire} segments \n"

        /// Fail on all tests, return the number of crossing wires (with each other) in the sheet.
        let countWireCrossing (sample: int) (sheet: SheetT.Model) =
            let crossingCount = numOfWireRightAngleCrossings sheet
            Some <| $"The sheet contains {crossingCount} wire crossings"
        /// Fail on all tests, return both the average segments per wire and number of crossing wires (with each other and components).
        let countWireCrossingAndSegs (sample: int) (sheet: SheetT.Model) =
            let rightAngles = numOfVisRightAngles sheet
            let segsCount = countWireRightAngles sheet
            let wireCount = Map.count sheet.Wire.Wires
            let avgSegsPerWire = float segsCount/float wireCount
            let compCrossingCount = numOfIntersectSegSym sheet
            let segCrossingCount = numOfWireRightAngleCrossings sheet
            Some <| $"The sheet contains:\n    {segCrossingCount} wire-wire crossings,\n    {compCrossingCount} wire-component crossings,\n    A wire contains:\n    average {avgSegsPerWire} segments\n    Total {rightAngles} right angles"

        /// Fail on all tests to show the number of right angles in the wires
        let showNumRightAngles (sample: int) (sheet: SheetT.Model)=
            let cnt = numOfVisRightAngles sheet
            Some <| $"Total number of right angles in sheet wires: {cnt} in Sample {sample}"
        
        /// Fail if there is a non-straightened wire, and show the number of non-straightened wires
        let showNumNonStraightenedWires (sample: int) (sheet: SheetT.Model) =
            let cnt = countNonStraightenedWires sheet
            match cnt with
            | n when n > 0 -> Some <| $"Total number of non-straightened wires in sheet: {cnt} in Sample {sample}"
            | _ -> None

        /// Fail on component overlapping another component, and show the number of overlaps
        let showNumComponentOverlaps (sample: int) (sheet: SheetT.Model) =
            let wireModel = sheet.Wire
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n, box)
            let numOverlaps =
                List.allPairs boxes boxes
                |> List.filter (fun ((n1, box1), (n2, box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
                |> List.length
            match numOverlaps with
            | n when n > 0 -> Some <| $"Number of components overlapping another component in Sample {sample}: {numOverlaps}"
            | _ -> None

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
                    // New for D1 testing

        /// Create the Figure A2 reference circuit for testing the D1 algorithm
        /// Assert that prints the number of right angles in the circuit, for every sample
        /// Not currently enabled as a test circuit 
        let D1Test1 testNum firstSample dispatch =
            runTestOnSheets
                "D1 Test 1. Show number of right angles in wires." 
                firstSample
                (randomCompPosVal 5 20 (-200,200) (100,200))
                makeD1CircuitA2
                Asserts.showNumRightAngles 
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Create the Figure A2 reference circuit for testing the D1 algorithm
        /// Assert that prints the number of component overlaps in the circuit, for every sample
        /// Not currently enabled as a test circuit 
        let D1Test2 testNum firstSample dispatch =
            runTestOnSheets
                "D1 Test 3. Show number of non-straightened wires." 
                firstSample
                (randomCompPosVal 5 20 (-200,200) (100,200))
                makeD1CircuitA2
                Asserts.showNumComponentOverlaps 
                dispatch
            |> recordPositionInTest testNum dispatch
        
        /// Create the Figure A2 reference circuit for testing the D1 algorithm
        /// Assert that prints the number of non-straightened wires in the circuit, for every sample
        /// Not currently enabled as a test circuit 
        let D1Test3 testNum firstSample dispatch =
            runTestOnSheets
                "D1 Test 3. Show number of non-straightened wires." 
                firstSample
                (randomCompPosVal 5 20 (-200,200) (100,200))
                makeD1CircuitA2
                Asserts.showNumNonStraightenedWires 
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Create a completely random circuit (see makeD4TestCircuitRandom)
        /// Fail on all tests to show the unaligned circuit
        let D4Test1 testNum firstSample dispatch =
            runTestOnSheets
                "D4 Test 1. Fail on all tests." 
                firstSample
                (randomCompPosVal 5 20 (-300,300) (100,200))
                makeD4TestCircuitRandom
                Asserts.failOnAllTests 
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Create a completely random circuit (see makeD4TestCircuitRandom)
        /// that the D4 beautify function is called on
        /// Fail on all tests to show the beautified circuits    
        let D4Test2 testNum firstSample dispatch =
            runTestOnSheets
                "D4 Test 1. Fail on all tests." 
                firstSample
                (randomCompPosVal 5 20 (-300,300) (100,200))
                makeD4TestCircuitRandomAligned
                Asserts.failOnAllTests 
                dispatch
            |> recordPositionInTest testNum dispatch
        
        /// Create a random deviated circuit based off makeTest2Circuit
        /// Fail on all tests and show metrics for evaluation
        let test2 random testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                random
                makeTest2Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Create a random deviated circuit based off makeTest3Circuit
        /// Fail on all tests and show metrics for evaluation
        let test3 random testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                random
                makeTest3Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Create a random deviated circuit based off makeTest4Circuit
        /// Fail on all tests and show metrics for evaluation
        let test4 random testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                random
                makeTest4Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Create a random deviated circuit based off makeTest5Circuit
        /// Fail on all tests and show metrics for evaluation
        let test5 random testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                random
                makeTest5Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Create a random deviated circuit based off makeTest6Circuit
        /// Fail on all tests and show metrics for evaluation
        let test6 random testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                random
                makeTest6Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Create a random deviated circuit based off makeTest7Circuit
        /// Fail on all tests and show metrics for evaluation
        let test7 random testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                random
                makeTest7Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

        /// Create a random deviated circuit based off makeTest8Circuit
        /// Fail on all tests and show metrics for evaluation
        let test8 random testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                random
                makeTest8Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

        /// List of tests available which can be run ftom Issie File Menu.
        /// The first 9 tests can also be run via Ctrl-n accelerator keys as shown on menu
        let testsToRunFromSheetMenu : (string * (int -> int -> Dispatch<Msg> -> Unit)) list =
            let randomized = (randomCompPosVal 5 10 (-200,200) (0,200))
            let minorDiviationRandomized = (minorRandomCompPosVal 5 40 (-20,20) (0,200))
            let minorDiviationRandomizedTen = (minorRandomCompPosVal 10 40 (-20,20) (0,200))
            // Change names and test functions as required
            // delete unused tests from list
            [
                // "Test1", 
                "Test2", D4Test2
                "Test3", (fun a b c -> test3 minorDiviationRandomizedTen a b c)
                "Test4", (fun a b c -> test4 minorDiviationRandomizedTen a b c)
                "Test5", (fun a b c -> test5 minorDiviationRandomized a b c)
                "Test6", (fun a b c -> test6 minorDiviationRandomized a b c)
                "Test7", (fun a b c -> test7 randomized a b c)
                "Test8", (fun a b c -> test8 randomized a b c)
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
        


    


