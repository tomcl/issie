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

//------------------------------------------------------------------------------------------------------------------------//
//------------------------------functions to build issue schematics programmatically--------------------------------------//
//------------------------------------------------------------------------------------------------------------------------//

module Builder =
    // ac2021: Figure A4
    /// custom component for testing with more ports
    let mainCC: ComponentType =
        Custom {
                    Name = "MAIN";
                    InputLabels = [("A", 0); ("B", 1); ("S2", 2); ("S3", 3)];
                    OutputLabels =  [("E", 4); ("F", 5); ("G", 6)];
                    Form = None
                    Description = None
               }

    // ac2021: reduced ports allows for testing of alignment when ports line up but different
    /// custom component for testing with fewer ports
    let smallMainCC: ComponentType =
        Custom {
                    Name = "MAIN";
                    InputLabels = [("A", 0); ("B", 1)];
                    OutputLabels =  [("E", 4); ("F", 5)];
                    Form = Some ProtectedTopLevel
                    Description = None
               }
    
    // ac2021: Intro Lecture Shape
    let ctrlPathCC: ComponentType =
        Custom {
                    Name = "CONTROLPATH";
                    InputLabels = [("A", 0); ("B", 1)];
                    OutputLabels =  [("E", 4); ("F", 5)];
                    Form = Some ProtectedTopLevel
                    Description = None
               }

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

    let scaleSym (lbl) scale (sheet: SheetT.Model) =
        let sym = getSymFromLbl lbl sheet
        let dims = getCustomCompDims sym
        let newDims = {X = dims.X * scale; Y = dims.Y * scale}
        putCustomCompDims newDims sym

    //--------------------------------------------------------------------------------------------------//
    //----------------------------------------DEBUG-----------------------------------------------------//
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

/// Adds wire to curried model.
/// x,y are relative to the sheet middle
let addSymToSheet lbl compType x y =
    Result.bind (placeSymbol lbl compType (pos x y))

/// Adds wire to curried model.
let addWireToSheet (lbl1, num1) (lbl2, num2) =
    Result.bind (placeWire (portOf lbl1 num1) (portOf lbl2 num2))

// ac2021: Figure A1 circuit
/// circuit to test alignment with multiple cascading connections
/// 2x MUX2
/// 4x inputs
/// 1x output
let makeA1Circuit =
    initSheetModel
    |> placeSymbol "A" (Input1 (1, None)) middleOfSheet
    |> addSymToSheet "B" (Input1 (1, None)) 0 -20
    |> addSymToSheet "S2" (Input1 (1, None)) 10 -30
    |> addSymToSheet "S1" (Input1 (1, None)) 40 -40
    |> addSymToSheet "MUX1" Mux2 40 -10
    |> addSymToSheet "MUX2" Mux2 80 -20
    |> addSymToSheet "C" (Output 1) 120 -20
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
let makeA3Circuit =
    initSheetModel
    |> placeSymbol "A" (Input1 (1, None)) middleOfSheet
    |> addSymToSheet "B" (Input1 (1, None)) 0 -20
    |> addSymToSheet "S2" (Input1 (1, None)) 10 -30
    |> addSymToSheet "S1" (Input1 (1, None)) 40 -40
    |> addSymToSheet "MUX1" Mux2 40 -10
    |> addSymToSheet "MUX2" Mux2 80 -20
    |> addSymToSheet "MUX3" Mux2 120 -20
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
/// 1x mainCC at [middle + 60 ± random offset]
let makeA4Circuit ((offsetXY:XYPos)) =
    initSheetModel
    |> placeSymbol "MAIN1" mainCC middleOfSheet
    |> addSymToSheet "MAIN2" mainCC (160.+offsetXY.X) (0.+offsetXY.Y)
    // |> Result.bind (putCustomCompDims dims (getSymFromLbl "MAIN1"))
    |> addWireToSheet ("MAIN1", 0) ("MAIN2", 0)
    |> addWireToSheet ("MAIN1", 1) ("MAIN2", 1)
    |> addWireToSheet ("MAIN1", 2) ("MAIN2", 2)
    |> getOkOrFail

//------------------------------------------------------------------------------------------------//
//-------------------------Example assertions used to test sheets---------------------------------//
//------------------------------------------------------------------------------------------------//


module Asserts =

    (* Each assertion function from this module has as inputs the sample number of the current test and the corresponding schematic sheet.
        It returns a boolean indicating (true) that the test passes or 9false) that the test fails. The sample numbr is included to make it
        easy to document tests and so that any specific sampel schematic can easily be displayed using failOnSampleNumber. *)

    // ac2021: May not be true as complexity increases, but still could be a useful helper
    /// Fail when there are greater than 4 right angles in a wire.
    /// This should be the ideal maximum number of corners in all cases.
    let failOnTooManyWireTurns (sample: int) (sheet: SheetT.Model) =
        let wIds = 
            sheet.Wire.Wires
            |> mapKeys
            |> Array.toList

        let subOne n = n - 1

        let moreThan4Turns (wId: ConnectionId) =
            visibleSegments wId sheet
            |> List.length
            |> subOne
            |> (fun n -> n > 4)

        wIds
        |> List.tryFind moreThan4Turns
        |> function 
            | Some _ -> Some $"Symbol outline intersects another symbol outline in Sample {sample}"
            | None -> None


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



//---------------------------------------------------------------------------------------//
//-----------------------------Demo tests on Draw Block code-----------------------------//
//---------------------------------------------------------------------------------------//

module Tests =

    let testA4 testNum firstSample dispatch =
        runTestOnSheets
            "two custom components with random offset: fail all tests"
            firstSample
            // (DimsAndOffsetXY {min=0.5; step=0.5; max=2} {min=(-2); step=0.1; max=2})
            (randXY {min=(-30); step=3; max=30})
            makeA4Circuit
            Asserts.failOnAllTests
            dispatch
        |> recordPositionInTest testNum dispatch
