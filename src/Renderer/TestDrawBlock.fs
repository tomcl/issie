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

        // Flip a symbol
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


        let runTestOnSheets2
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
            result
        
//--------------------------------------------------------------------------------------------------//
//---------------------------Example Test Circuits using Gen<'a> samples----------------------------//
//--------------------------------------------------------------------------------------------------//

    open Builder
    //helper functions
    ///return flipped or rotated model or a model with no change (rotate 0 degree) for a symbol based on a random number
    ///for rotating only , provide value between 0 - 100, for flipping only, provide value between 100-200,
    ///for flipping and rotating, provide value betwenn 0 - 200.
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

    ///Return swapped or not swapped MUX wiring models based on a random number
    let swap (randomization:int) (muxName:string) (input1:string) (input2:string) =
            if randomization > 100 then
                (placeWire (portOf input2 0) (portOf muxName 0), placeWire (portOf input1 0) (portOf muxName 1))
            else
                (placeWire (portOf input2 0) (portOf muxName 1), placeWire (portOf input1 0) (portOf muxName 0))
    ///Make a circuit for overlap testing.
    //TODO: it is such a horrible way of doing this. improve
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
    ///Returns true if symbol intersects symbol on sheet.
    //TODO: again it is such a horrible way of doing this. improve
    let symbolIntersectsSymbol (sheet: SheetT.Model) =
            let boxes =
                mapValues sheet.BoundingBoxes
                |> Array.toList
                |> List.mapi (fun n box -> n,box)
            List.allPairs boxes boxes 
            |> List.exists (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    ///Returns true if the given set of positions contains overlapping symbols
    let overlapAnySymbol (posRots:XYPos list * int list) =
            makeOverlapTestingCircuit posRots
            |>symbolIntersectsSymbol
    //position functions
    /// Sample data based on 11 equidistant points on a horizontal line
    let horizLinePositions =
        fromList [1]
        |> map (fun n -> middleOfSheet + {X=float n; Y=0.})

    let rectangularPositions =
        let andNotOverlap (andPos:XYPos) =
            let marginDFF = {X=30.; Y=30.}
            let marginAND = {X=25.; Y=25.}
            not (BlockHelpers.overlap2D (middleOfSheet+marginDFF,middleOfSheet-marginDFF) (andPos + marginAND,andPos - marginAND))

        product (fun a b -> middleOfSheet + {X=float a; Y=float b}) (fromList [-70..5..70]) (fromList [-70..5..70])
        |> filter andNotOverlap

    /// Get a Gen of random positions (in list of pos on one axis for different components)
    /// and randomizer values (in list of int for different components).
    let randomCompPosVal (componentCount: int) (testCount: int) (posRange:int*int) (randomizerRange: int*int)=
        let coordinateBatch (initialPos: XYPos) (xCoords: int list) (yCoords: int list): XYPos list =
            (xCoords,yCoords)
            ||> List.map2 (fun x y -> initialPos + {X= float x; Y= float y})
        
        map3 (fun n m k -> (coordinateBatch middleOfSheet n m,
                            k))
                            (randomIntList (fst posRange) (snd posRange) testCount componentCount)
                            (randomIntList (fst posRange) (snd posRange) testCount componentCount)
                            (randomIntList (fst randomizerRange) (snd randomizerRange) testCount componentCount)
        |> filter (fun s -> not (overlapAnySymbol s))

    /// Get a Gen of random deviations (in list of pos on one axis for different components)
    /// and randomizer values (in list of int for different components).
    let minorRandomCompPosVal (componentCount: int) (testCount: int) (posRange:int*int) (randomizerRange: int*int)=
        let coordinateBatch (initialPos: XYPos) (xCoords: int list) (yCoords: int list): XYPos list =
            (xCoords,yCoords)
            ||> List.map2 (fun x y -> {X= float x; Y= float y})
        
        map3 (fun n m k -> (coordinateBatch middleOfSheet n m,
                            k))
                            (randomIntList (fst posRange) (snd posRange) testCount componentCount)
                            (randomIntList (fst posRange) (snd posRange) testCount componentCount)
                            (randomIntList (fst randomizerRange) (snd randomizerRange) testCount componentCount)
        
    /// Get a Gen of manual positions (in list for different components)
    /// and randomizer values (in list for different components).
    let randomCompVal (componentCount: int) (testCount: int) (posRange:int*int) (randomizerRange: int*int)=
        
        map3 (fun n m k -> ([middleOfSheet + {X= float n; Y= float m};
                             middleOfSheet + {X= float n; Y= float m - 80.0};
                             middleOfSheet + {X= float n + 160.0; Y= float m};
                             middleOfSheet + {X= float n + 80.0; Y= float m - 160.0};
                             middleOfSheet + {X= float n + 200.0; Y= float m - 160.0}
                            ],
                            k))
                            (fromList (List.init testCount (fun _ -> 0)))
                            (fromList (List.init testCount (fun _ -> 0)))// fixed position
                            (randomIntList (fst randomizerRange) (snd randomizerRange) testCount componentCount)
        |> filter (fun s -> not (overlapAnySymbol s))

    //TODO: make automatic circuit creation
    ///Suggested circuit in appendix b (test starter 1.)
    ///Components include 2 inputs, 2 muxes, and 1 and gate (see FigureB1).
    ///Swap and flip is done randomly.
    let makeTest1Circuit (posRots:XYPos list * int list) =
        let MUX2Connection1, MUX2Connection2 = swap ((snd posRots)[3]) "MUX2" "S1" "MUX1"
        let ANDConnection1, ANDConnection2 = swap ((snd posRots)[4]) "G1" "MUX2" "MUX1"

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
    ///Chaotic circuit of 2 inputs, 2 muxes, and 1 and gate (see FigureB1).
    ///Rotation, flip, swap is done randomly.
    let makeTest2Circuit (posRots:XYPos list * int list) =
        let MUX2Connection1, MUX2Connection2 = swap ((snd posRots)[3]) "MUX2" "S1" "MUX1"
        let ANDConnection1, ANDConnection2 = swap ((snd posRots)[4]) "G1" "MUX2" "MUX1"

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

    let makeCircuit =
         initSheetModel
          |> placeSymbol "S1" (Input1(1, None)) {X = 1862.66; Y = 1841.49}
          |> Result.bind (placeSymbol "MUX1" Mux2 {X = 1572.66; Y = 1671.49})
          |> Result.bind (placeSymbol "S2" (Input1(1, None)) {X = 1752.66; Y = 1651.49})
          |> Result.bind (placeSymbol "MUX2" Mux2 {X = 1627.66; Y = 1776.49})
          |> Result.bind (placeSymbol "G1" (GateN(And, 2)) {X = 1735.16; Y = 1708.99})
          |> getOkOrFail

    let makeTest7Circuit (posRots:XYPos list * int list) =
        let MUX2Connection1, MUX2Connection2 = swap ((snd posRots)[3]) "MUX2" "S1" "MUX1"
        let ANDConnection1, ANDConnection2 = swap ((snd posRots)[4]) "G1" "MUX2" "MUX1"

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

    let makeTest5Circuit (posRots:XYPos list * int list) =
        (*initSheetModel
        |> placeSymbol "S1" (Input1(1,None)) ((fst posRots)[0])
        |> Result.bind (placeSymbol "S2" (Input1(1,None)) ((fst posRots)[1]))
        |> Result.bind (placeSymbol "MUX1" Mux2 ((fst posRots)[2]))
        |> Result.bind (placeSymbol "MUX2" Mux2 ((fst posRots)[3]))
        |> Result.bind (placeSymbol "G1" (GateN(And,2)) ((fst posRots)[4]))
        |> Result.bind (flipRot ((snd posRots)[0]) "S1" )
        |> Result.bind (flipRot ((snd posRots)[1]) "S2")
        |> Result.bind (flipRot ((snd posRots)[2]) "MUX1")
        |> Result.bind (flipRot ((snd posRots)[3]) "MUX2")
        |> Result.bind (flipRot ((snd posRots)[4]) "G1")
        |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
        |> Result.bind (MUX2Connection1)
        |> Result.bind (MUX2Connection2)
        |> Result.bind (ANDConnection1)
        |> Result.bind (ANDConnection2)
        |> getOkOrFail*)
        let MUX2Connection1, MUX2Connection2 = swap (0) "MUX2" "S1" "MUX1"
        let ANDConnection1, ANDConnection2 = swap (0) "G1" "MUX2" "MUX1"


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
                


    let makeTest6Circuit (posRots:XYPos list * int list) =
        let MUX2Connection1, MUX2Connection2 = swap (0) "MUX2" "S1" "MUX1"
        let ANDConnection1, ANDConnection2 = swap (0) "G1" "MUX2" "MUX1"


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
        |> alignSymbols

    let makeTest8Circuit (posRots:XYPos list * int list) =
        let MUX2Connection1, MUX2Connection2 = swap ((snd posRots)[3]) "MUX2" "S1" "MUX1"
        let ANDConnection1, ANDConnection2 = swap ((snd posRots)[4]) "G1" "MUX2" "MUX1"

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
        //|> Result.bind (optimalEdgeOrder)
        |> getOkOrFail
        |> alignSymbols

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
            
        let test1 testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random swap flip"
                firstSample
                (randomCompVal 5 20 (-200,200) (100,200))
                makeTest1Circuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        let test2 testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random swap flip"
                firstSample
                (randomCompVal 5 20 (-200,200) (100,200))
                makeTest1Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

        let test3 testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                (randomCompPosVal 5 200 (-200,200) (0,200))
                makeTest2Circuit
                Asserts.failOnAllTests
                dispatch
            |> recordPositionInTest testNum dispatch

        let test4 testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                (randomCompPosVal 5 200 (-200,200) (0,200))
                makeTest2Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

        let test5 random testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                random
                makeTest5Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

        let test6 random testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                random
                makeTest6Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

        let test7 random testNum firstSample dispatch =
            runTestOnSheets
                "Figure B2: fail on all, random flip rotate"
                firstSample
                random
                makeTest7Circuit
                Asserts.countWireCrossingAndSegs
                dispatch
            |> recordPositionInTest testNum dispatch

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
            // Change names and test functions as required
            // delete unused tests from list
            [
                "Test1", test1
                "Test2", test2
                "Test3", test3
                "Test4", test4
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
        


    


