module TestDrawBlockD1
open GenerateData
open Elmish

(******************************************************************************************
   This submodule contains a set of functions that enable random data generation
   for property-based testing of Draw Block wire routing functions.
   basic idea.
   1. Generate, in various ways, random circuit layouts
   2. For each layout apply smartautoroute to regenerate all wires
   3. Apply check functions to see if the resulting wire routing obeys "good layout" rules.
   4. Output any layouts with anomalous wire routing
*******************************************************************************************)

open TestDrawBlock
open TestLib
open TestDrawBlock.HLPTick3
open TestDrawBlock.HLPTick3.Asserts
open TestDrawBlock.HLPTick3.Builder
open TestDrawBlock.HLPTick3.Tests

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
open SheetBeautifyHelpers
open BlockHelpers

//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//

module Builder =
    // ac2021: Figure A4
    /// custom component for testing with more ports
    let mainCC: ComponentType =
        Custom {
                    Name = "MAIN";
                    InputLabels = [("A", 1); ("B", 1); ("S2", 1); ("S3", 1)];
                    OutputLabels = [("E", 1); ("F", 1); ("G", 1)];
                    Form = None
                    Description = None
               }

    // ac2021: reduced ports allows for testing of alignment when ports line up but different
    /// custom component for testing with fewer ports
    let smallMainCC: ComponentType =
        Custom {
                    Name = "MAIN";
                    InputLabels = [("A", 1); ("B", 1)];
                    OutputLabels = [("E", 1); ("F", 1)];
                    Form = Some ProtectedTopLevel
                    Description = None
               }
    
    // ac2021: Intro Lecture Shape
    let ctrlPathCC: ComponentType =
        Custom {
                    Name = "CONTROLPATH";
                    InputLabels = [("CPEN", 1); ("ND", 1); ("ZD", 1); ("CD", 1); ("VD", 1); ("MEMDATA", 16); ("RA", 16); ("IMMEXT", 16)];
                    OutputLabels = [("FLAGC", 1); ("INS", 16); ("RETADDR", 16); ("MEMADDR", 16)];
                    Form = Some ProtectedTopLevel
                    Description = None
               }

    // ac2021: Intro Lecture Shape
    let dataPathCC: ComponentType =
        Custom {
                    Name = "DATAPATH";
                    InputLabels = [("PCIN", 16); ("INS", 16); ("FLAGCIN", 1); ("DPEN", 1); ("MEMDOUT", 16)];
                    OutputLabels = [("FLAGN", 1); ("FLAGZ", 1); ("FLAGC", 1); ("FLAGV", 1); ("IMMEXT", 16); ("RAOUT", 16); ("MEMADDR", 16); ("MEMDIN", 16); ("MEMWEN", 1)];
                    Form = Some ProtectedTopLevel
                    Description = None
               }

    let exampleMem1 =
        {
            Init = FromData
            AddressWidth = 16
            WordWidth = 16
            Data = Map [(0,0)]
        }

    let exampleAROM = 
        AsyncROM1 exampleMem1

    let exampleARAM = 
        AsyncRAM1 exampleMem1

    /// Count how many segments connected to sym
    let segsConnectedToSym (sheet: SheetT.Model) (sym: SymbolT.Symbol) =
        let countVisSegsInWire (wire: BusWireT.Wire) =
            visibleSegments wire.WId sheet
            |> List.length

        let symPortIds = 
            sym.PortMaps.Order
            |> mapValues
            |> Array.toList
            |> List.concat
        
        sheet.Wire.Wires
        |> Map.filter (fun _ wire -> List.contains (string wire.InputPort) symPortIds || List.contains (string wire.OutputPort) symPortIds)
        |> Map.toList
        |> List.map (fun (_, wire) -> wire)
        |> List.map countVisSegsInWire
        |> List.sum

    /// Returns symbol matching label.
    /// Fails if label doesn't exists
    let getSymFromLbl symLabel (model:SheetT.Model) =
        let symbolMap = model.Wire.Symbol.Symbols
        let componentID = Map.findKey (fun _ (sym: SymbolT.Symbol) -> sym.Component.Label = symLabel) symbolMap
        symbolMap[componentID]

    let scaleSym (lbl: string) (scale: XYPos) (sheet: SheetT.Model) : Result<SheetT.Model, string> =
        let sym = getSymFromLbl lbl sheet
        let dims = getCustomCompDims sym
        let newDims = {X = dims.X * scale.X; Y = dims.Y * scale.Y}
    
        sheet
        |> Optic.map (SheetT.symbolOf_ sym.Id) (putCustomCompDims newDims)
        |> Ok

    let getPortIdFromName lbl portType (sheet: SheetT.Model) portNum =
        let syms = sheet.Wire.Symbol.Symbols
        printfn "getting portId"
        portOf lbl portNum
        |> getPortId syms portType
        |> function
              | Ok result -> result
              | _ -> failwithf "Invalid portNum. PortId not found"


    /// Changes the port order of a symbol based on its label
    let orderSymPorts (symLbl: string) edge orderIn orderOut (sheet: SheetT.Model) : Result<SheetT.Model, string> =
        printfn "started"
        let sym = getSymFromLbl symLbl sheet
        let portIds = 
            List.map (getPortIdFromName symLbl PortType.Input sheet) orderIn
            @ List.map (getPortIdFromName symLbl PortType.Output sheet) orderOut

        // Edit PortMap's Order & Orientation 
        let editSym =
            List.fold (putPortOrientation edge) sym portIds
            |> putPortOrder edge portIds
            |> (fun x ->
                    printfn "%A" x.PortMaps
                    x)
        Optic.set (SheetT.symbolOf_ sym.Id) editSym sheet
        |> Ok
        



    //--------------------------------------------------------------------------------------------------//
    //----------------------------------------DEV-------------------------------------------------------//
    //--------------------------------------------------------------------------------------------------//

    let printId x =
        printfn "%A" x
        x
    
    /// Print info needed for reverse circuit generation from sheet.
    /// Intended for displaying circuit within curried circuit definition
    let printSym (sym: SymbolT.Symbol) (sheet: SheetT.Model) =
        printfn "Input ports: %A" sym.Component.InputPorts
        printfn "Output ports: %A" sym.Component.OutputPorts
        printfn "Pos: %A" sym.Pos
        match sym.Component.Type with
        | Custom c -> 
            printfn "Custom compType: %s" c.Name
            let dims = getCustomCompDims sym
            printfn "Dims: %fx%f" dims.X dims.Y
        | _ -> 
            printfn "compType: %A" sym.Component.Type
            printfn "Dims: %fx%f" sym.Component.H sym.Component.W

    /// Prints symbol in circuit.
    /// Curried so it can be placed within a circuit definition.
    /// TODO: currently prints for all generator values, resulting in verbose output
    let printSymFromLbl lbl sheet =
        let sym = getSymFromLbl lbl sheet
        printfn "Symbol: %s" lbl
        printSym sym sheet
        Ok sheet


//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//

open Builder

/// Options used to create generators
type randomOptions =
    {
        min: float
        step: float
        max: float
    }

/// small offsets in X&Y axis
let offset randOpts =
    randomFloat randOpts.min randOpts.step randOpts.max

let randXY randOpts =
    (offset randOpts, offset randOpts)
    ||> product (fun x y -> {X=x; Y=y})
    |> toArray
    |> shuffleA
    |> fromArray

let DimsAndOffsetXY scaleRandOpts offRandOpts =
    (randXY scaleRandOpts, randXY offRandOpts)
    ||> product (fun s xy -> (s, xy))
    |> toArray
    |> shuffleA
    |> fromArray


//--------------------------------------------------------------------------------------------------//
//----------------------------------------Sheet Building Functions----------------------------------//
//--------------------------------------------------------------------------------------------------//

/// Returns the position in respect to the centre of the sheet
let pos x y = 
    middleOfSheet + {X= x; Y= y}

/// Adds symbol to curried model.
/// x,y are relative to the sheet middle
let addSymToSheet lbl compType x y =
    Result.bind (placeSymbol lbl compType (pos x y))

/// Adds wire to curried model.
let addWireToSheet (lbl1, num1) (lbl2, num2) =
    Result.bind (placeWire (portOf lbl1 num1) (portOf lbl2 num2))

/// Edit dimensions of symbol in curried model
let scaleSymInSheet lbl scale =
    Result.bind (scaleSym lbl scale)


let orderSymPortsInSheet
    lbl
    leftIn leftOut
    bottomIn bottomOut
    rightIn rightOut
    topIn topOut 
    sheet =
    Result.bind (orderSymPorts lbl Left leftIn leftOut) sheet
    |> Result.bind (orderSymPorts lbl Bottom bottomIn bottomOut)
    |> Result.bind (orderSymPorts lbl Right rightIn rightOut)
    |> Result.bind (orderSymPorts lbl Top topIn topOut)

// ac2021: Figure A1 circuit
/// circuit to test alignment with multiple cascading connections
/// 2x MUX2
/// 4x inputs
/// 1x output
let makeA1Circuit _ =
    initSheetModel
    |> placeSymbol "A" (Input1 (1, None)) middleOfSheet
    |> addSymToSheet "B" (Input1 (1, None)) 0 200
    |> addSymToSheet "S2" (Input1 (1, None)) 100 300
    |> addSymToSheet "S1" (Input1 (1, None)) 400 400
    |> addSymToSheet "MUX1" Mux2 400 100
    |> addSymToSheet "MUX2" Mux2 800 200
    |> addSymToSheet "C" (Output 1) 1200 200
    |> addWireToSheet ("A", 0) ("MUX1", 0)
    |> addWireToSheet ("B", 0) ("MUX1", 1)
    |> addWireToSheet ("MUX1", 0) ("MUX2", 0)
    |> addWireToSheet ("MUX2", 0) ("C", 0)
    |> addWireToSheet ("S2", 0) ("MUX2", 2)
    |> addWireToSheet ("S1", 0) ("MUX1", 2)
    |> addWireToSheet ("S1", 0) ("MUX2", 1)
    |> getOkOrFail


// ac2021: Figure A2 circuit
//    |    _____
//    |___|
//        |
//        |
// TODO: Explore circuit like this? (will probably be generated elsewhere)


// ac2021: Figure A3 circuit
/// circuit to test alignment with multiple cascading connections
/// 2x MUX2
/// 4x inputs
/// 1x MUX output
let makeA3Circuit _ =
    initSheetModel
    |> placeSymbol "A" (Input1 (1, None)) middleOfSheet
    |> addSymToSheet "B" (Input1 (1, None)) 0 200
    |> addSymToSheet "S2" (Input1 (1, None)) 100 300
    |> addSymToSheet "S1" (Input1 (1, None)) 400 400
    |> addSymToSheet "MUX1" Mux2 400 100
    |> addSymToSheet "MUX2" Mux2 800 200
    |> addSymToSheet "MUX3" Mux2 1200 200
    |> addWireToSheet ("A", 0) ("MUX1", 0)
    |> addWireToSheet ("B", 0) ("MUX1", 1)
    |> addWireToSheet ("S2", 0) ("MUX2", 2)
    |> addWireToSheet ("S1", 0) ("MUX1", 2)
    |> addWireToSheet ("S1", 0) ("MUX2", 1)
    |> addWireToSheet ("S1", 0) ("MUX3", 1)
    |> addWireToSheet ("MUX1", 0) ("MUX2", 0)
    |> addWireToSheet ("MUX2", 0) ("MUX3", 0)
    |> getOkOrFail

// ac2021: Figure A4 circuit
/// circuit to test alignment between two custom components
/// 1x mainCC at [middle of sheet]
/// 1x mainCC at [middle + 160 ± random offset]
let makeA4Circuit (offsetXY: XYPos) =
    initSheetModel
    |> placeSymbol "MAIN1" mainCC middleOfSheet
    |> addSymToSheet "MAIN2" mainCC (160.+offsetXY.X) (0.+offsetXY.Y)
    |> addWireToSheet ("MAIN1", 0) ("MAIN2", 0)
    |> addWireToSheet ("MAIN1", 1) ("MAIN2", 1)
    |> addWireToSheet ("MAIN1", 2) ("MAIN2", 2)
    |> getOkOrFail


// ac2021: Figure A5 circuit
/// circuit to test alignment between two custom components
/// 1x mainCC at [middle of sheet]
/// 1x mainCC at [middle + 160] with random scaling
let makeA5Circuit (scale: XYPos) =
    initSheetModel
    |> placeSymbol "MAIN1" mainCC middleOfSheet
    |> addSymToSheet "MAIN2" mainCC 160. 0.
    |> scaleSymInSheet "MAIN2" scale
    |> addWireToSheet ("MAIN1", 0) ("MAIN2", 0)
    |> addWireToSheet ("MAIN1", 1) ("MAIN2", 1)
    |> addWireToSheet ("MAIN1", 2) ("MAIN2", 2)
    |> getOkOrFail

// ac2021: Intro lecture circuit for sheetAlignScale
let makeLargeCircuit _ =
    printfn "Build Large Circuit"
    initSheetModel
    |> placeSymbol "CONTROLPATH" ctrlPathCC middleOfSheet
    |> addSymToSheet "DATAPATH" dataPathCC 0 400
    |> addSymToSheet "DATAMEM" exampleARAM 450 400
    |> addSymToSheet "CODEMEM" exampleAROM 450 0
    |> addSymToSheet "C1" (Input1 (1, None)) -450 -20
    |> addSymToSheet "C2" (Input1 (1, None)) -450 400
    |> orderSymPortsInSheet "CONTROLPATH"
                            [0] [0;1;2]
                            [1;2;3;4] []
                            [5;6;7] [3]
                            [] []
    |> orderSymPortsInSheet "DATAPATH"
                        [0;1;2;3] []
                        [] []
                        [4] [8;7;6;5;4]
                        [] [3;2;1;0]
    |> addWireToSheet ("C1", 0) ("CONTROLPATH", 0)
    |> addWireToSheet ("C2", 0) ("DATAPATH", 3)
    |> addWireToSheet ("CONTROLPATH", 0) ("DATAPATH", 2)
    |> addWireToSheet ("CONTROLPATH", 1) ("DATAPATH", 1)
    |> addWireToSheet ("CONTROLPATH", 2) ("DATAPATH", 0)
    |> addWireToSheet ("CONTROLPATH", 3) ("CODEMEM", 0)
    |> addWireToSheet ("CODEMEM", 0) ("CONTROLPATH", 5)
    |> addWireToSheet ("DATAPATH", 0) ("CONTROLPATH", 1)
    |> addWireToSheet ("DATAPATH", 1) ("CONTROLPATH", 2)
    |> addWireToSheet ("DATAPATH", 2) ("CONTROLPATH", 3)
    |> addWireToSheet ("DATAPATH", 3) ("CONTROLPATH", 4)
    |> addWireToSheet ("DATAPATH", 4) ("CONTROLPATH", 7)
    |> addWireToSheet ("DATAPATH", 5) ("CONTROLPATH", 6)
    |> addWireToSheet ("DATAPATH", 6) ("DATAMEM", 0)
    |> addWireToSheet ("DATAPATH", 7) ("DATAMEM", 1)
    |> addWireToSheet ("DATAPATH", 8) ("DATAMEM", 2)
    |> addWireToSheet ("DATAMEM", 0) ("DATAPATH", 4)
    |> getOkOrFail




//------------------------------------------------------------------------------------------------//
//-------------------------Example assertions used to test sheets---------------------------------//
//------------------------------------------------------------------------------------------------//


module Asserts =

    (* Each assertion function from this module has as inputs the sample number of the current test and the corresponding schematic sheet.
        It returns a boolean indicating (true) that the test passes or 9false) that the test fails. The sample numbr is included to make it
        easy to document tests and so that any specific sampel schematic can easily be displayed using failOnSampleNumber. *)

    // ac2021: May not be true as complexity increases, but still could be a useful helper
    /// Fail when there are greater than 4 right angles in a singular wire.
    /// This should be the ideal maximum number of corners in all cases.
    let failOn5WireTurns (sample: int) (sheet: SheetT.Model) =
        let wIds = 
            sheet.Wire.Wires
            |> mapKeys
            |> Array.toList

        let turnsInWire (wId: ConnectionId) =
            visibleSegments wId sheet
            |> List.length
            |> (fun n -> n - 1)

        wIds
        |> List.map turnsInWire
        |> List.tryFind (fun n -> n > 4)
        |> Option.bind (fun _ -> Some $"More wire turns than necessary in sample {sample}")


    // For each pair of nets
    // For each pair of wires from either net
    // count how many crosses
    /// Fail when a wire crosses another wire twice (worst case 1 cross is needed for desired routing)
    let failOnWiresCrossTwice (sample: int) (sheet: SheetT.Model) =
        let wires = 
            sheet.Wire.Wires
            |> mapValues
            |> Array.toList

        let nets = 
            BlockHelpers.partitionWiresIntoNets sheet.Wire
            |> List.map snd
            |> List.map (List.map (fun (_,wire) -> wire))
            |> List.mapi (fun n box -> n,box)

        /// Finds if there are more than 2 or more crosses between each wire in the net
        let crossesBtwnNetsTwice ((net1: BusWireT.Wire list), (net2: BusWireT.Wire list)) : bool =
            failwithf "Not implemented"

        List.allPairs nets nets // pairs of nets
        |> List.filter (fun ((n1,_), (n2,_)) -> n1 <> n2)
        |> List.map (fun ((_,lst1), (_,lst2)) -> (lst1, lst2))
        |> List.tryFind crossesBtwnNetsTwice
        |> function 
            | Some _ -> Some $"Symbol outline intersects another symbol outline in Sample {sample}"
            | None -> None

    /// Combine multiple checks into one for a circuit.
    /// Returns the first error message found.
    let combineSheetChecker (check1: int ->SheetT.Model->option<string>) (check2: int ->SheetT.Model->option<string>) sample sheet =
        check1 sample sheet
        |> Option.orElse (check2 sample sheet)




//---------------------------------------------------------------------------------------//
//-----------------------------Evaluation------------------------------------------------//
//---------------------------------------------------------------------------------------//
// Evaluation of the circuit will be calulated after the sheetChecker is run successfully. 
// Each evaluation metric returns score between [0, 1].
// The larger the score, the more 'beautiful' the beautified sheet is 
// relative to ideal beautification.

module Evaluations =

    /// Calculates the proportion of wire bends compared to the ideal solution.
    /// Same as evaluating the number of visual segments
    let wireBendProp (sheet: SheetT.Model) =
        let wires = mapValues sheet.Wire.Wires
        let symMap = sheet.Wire.Symbol

        // Ideal min turn with no position constraints
        let wireMinTurns (wire: BusWireT.Wire) =
            let inpEdge = getInputPortOrientation symMap wire.InputPort
            let outEdge = getOutputPortOrientation symMap wire.OutputPort
            match inpEdge, outEdge with
            | edge1, edge2 when edge1 = edge2 -> 2
            | Left, Right | Right, Left | Top, Bottom | Bottom, Top -> 0
            | _ -> 1

        let rightAngs = numOfVisRightAngles sheet
        let idealRightAngs =
            wires
            |> Array.map wireMinTurns
            |> Array.sum

        match rightAngs with
        | 0 -> 1.
        | _ -> float idealRightAngs / float rightAngs

    /// Evaluates number of wires compared to number of visual segments
    let visualSegmentProp (sheet: SheetT.Model) =
        failwithf "Not implemented"

    /// Evaluates number of crosses of wires compared to number of wires in sheet
    let wireCrossProp (sheet: SheetT.Model) =
        failwithf "Not implemented"

    /// Evaluates wire squashing between symbols
    let wireSquashProp (sheet: SheetT.Model) =
        failwithf "Not implemented"
        // getWiresInBox

    /// Evaluates length of wires compared to ideal minimum
    let wireLengthProp (sheet: SheetT.Model) =    
        let minWireLen wire =
            BusWireRoute.getWireVertices
        failwithf "Not implemented"

    // For each symbol in sheet
    // evaluates alignment with all other symbols
    // getSymbolPos
    /// Evaluates symbol alignment with all other symbols
    let symCentreAlignmentProp (sheet: SheetT.Model) : float =
        let syms = mapValues sheet.Wire.Symbol.Symbols
        
        /// Scores how aligned two symbols are
        let calcAlignment (symA: SymbolT.Symbol) (symB: SymbolT.Symbol) =
            getSymBoundingBox
            failwithf "not implemented"

        Array.allPairs syms syms
        |> Array.sumBy (function | (symA,symB) when symA.Id <= symB.Id -> calcAlignment symA symB
                                 | _ -> 0.)
        |> (fun x -> x / (float (Array.length syms))) // Scales to lots of symbols

    type ConfigD1 =
        {
            wireBendWeight: float
            wireCrossWeight: float // numOfWireRightAngleCrossings
            wireSquashWeight: float
            wireLengthWeight: float // calcVisWireLength
            failPenalty: float // -1
        }

    let combEval evalA weightA evalB weightB (sheet: SheetT.Model) =
        weightA * (evalA sheet) + weightB * (evalB sheet)

    /// Combines all evaluations into one score
    let evaluateD1 (c: ConfigD1) (sheet: SheetT.Model) : float =
        c.wireBendWeight * (wireBendProp sheet)
        |> (+) (c.wireCrossWeight * (float (numOfWireRightAngleCrossings sheet)))
        |> (+) (c.wireSquashWeight * (float (wireSquashProp sheet)))
        |> (+) (c.wireSquashWeight * (float (wireSquashProp sheet)))






//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

module Tests =
    open Asserts
    open Evaluations

    let testA1 testNum firstSample dispatch =
        runTestOnSheets
            "Figure A1 circuit from hlp2024 brief"
            firstSample
            (randXY {min=(-30); step=3; max=30})
            makeA1Circuit
            failOnAllTests
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum dispatch

    let testA3 testNum firstSample dispatch =
        runTestOnSheets
            "Figure A3 circuit from hlp2024 brief"
            firstSample
            (randXY {min=(-30); step=3; max=30})
            makeA3Circuit
            failOnAllTests
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum dispatch

    let testA4 testNum firstSample dispatch =
        runTestOnSheets
            "two custom components with random offset"
            firstSample
            (randXY {min=(-50); step=5; max=50})
            makeA4Circuit
            failOnAllTests
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum dispatch

    let testA5 testNum firstSample dispatch =
        runTestOnSheets
            "two custom components with random scaling"
            firstSample
            (randXY {min=(0.5); step=0.5; max=3})
            makeA5Circuit
            failOnAllTests
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum dispatch

    let testLargeCircuit testNum firstSample dispatch =
        runTestOnSheets
            "Large circuit"
            firstSample
            (randXY {min=(0.5); step=0.5; max=1})
            makeLargeCircuit
            failOnAllTests
            Evaluations.nullEvaluator
            dispatch
        |> recordPositionInTest testNum dispatch
