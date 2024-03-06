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
    let (mainCC: ComponentType) =
        Custom {
                    Name = "MAIN";
                    InputLabels = [("A", 0); ("B", 1); ("S2", 2); ("S3", 3)];
                    OutputLabels =  [("E", 4); ("F", 5); ("G", 6)];
                    Form = None
                    Description = None
               }

    // ac2021: reduced ports allows for testing of alignment when ports line up but different
    /// custom component for testing with fewer ports
    let (smallMainCC: ComponentType) =
        Custom {
                    Name = "MAIN";
                    InputLabels = [("A", 0); ("B", 1)];
                    OutputLabels =  [("E", 4); ("F", 5)];
                    Form = None
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


    /// Print info needed for reverse circuit generation from sheet
    let printCircuitBuild (sheet: SheetT.Model) =
        failwithf "Not implemented"


//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//

open Builder

/// small offsets in X&Y axis
let offsetXY =
    let offsetX = randomFloat -2. 0.1 2.
    let offsetY = randomFloat -2. 0.1 2.
    (offsetX, offsetY)
    ||> product (fun (x: float) (y: float) -> {X=x; Y=y})

/// Returns the position in respect to the centre of the sheet
let pos x y = 
    middleOfSheet + {X=float x; Y=float y}

// ac2021: Figure A1 circuit
/// circuit to test alignment with multiple cascading connections
/// 2x MUX2
/// 4x inputs
/// 1x output
let makeA1Circuit =
    initSheetModel
    |> placeSymbol "A" (Input 1) middleOfSheet
    |> Result.bind (placeSymbol "B" (Input 1) (pos 0 -20))
    |> Result.bind (placeSymbol "S2" (Input 1) (pos 10 -30))
    |> Result.bind (placeSymbol "S1" (Input 1) (pos 40 -40))
    |> Result.bind (placeSymbol "MUX1" (Mux2) (pos 40 -10))
    |> Result.bind (placeSymbol "MUX2" (Mux2) (pos 80 -20))
    |> Result.bind (placeSymbol "C" (Output 1) (pos 120 -20))
    |> Result.bind (placeWire (portOf "A" 0) (portOf "MUX1" 0))
    |> Result.bind (placeWire (portOf "B" 0) (portOf "MUX1" 1))
    |> Result.bind (placeWire (portOf "MUX1" 3) (portOf "MUX2" 0))
    |> Result.bind (placeWire (portOf "MUX2" 3) (portOf "C" 0))
    |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX1" 2))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 1))
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
    |> placeSymbol "A" (Input 1) middleOfSheet
    |> Result.bind (placeSymbol "B" (Input 1) (pos 0 -20))
    |> Result.bind (placeSymbol "S2" (Input 1) (pos 10 -30))
    |> Result.bind (placeSymbol "S1" (Input 1) (pos 40 -40))
    |> Result.bind (placeSymbol "MUX1" (Mux2) (pos 40 -10))
    |> Result.bind (placeSymbol "MUX2" (Mux2) (pos 80 -20))
    |> Result.bind (placeSymbol "C" (Output 1) (pos 120 -20))
    |> Result.bind (placeWire (portOf "A" 0) (portOf "MUX1" 0))
    |> Result.bind (placeWire (portOf "B" 0) (portOf "MUX1" 1))
    |> Result.bind (placeWire (portOf "MUX1" 3) (portOf "MUX2" 0))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX1" 3))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX2" 3))
    |> Result.bind (placeWire (portOf "S1" 0) (portOf "MUX3" 1))
    |> Result.bind (placeWire (portOf "S2" 0) (portOf "MUX2" 2))
    |> Result.bind (placeWire (portOf "MUX2" 3) (portOf "MUX3" 0))
    |> getOkOrFail


// ac2021: Figure A4 circuit
/// circuit to test alignment between two custom components
/// 1x mainCC at [middle of sheet]
/// 1x mainCC at [middle + 60 ± random offset]
let makeA4Circuit (offsetXY:XYPos) =
    initSheetModel
    |> placeSymbol "MAIN1" mainCC middleOfSheet
    |> Result.bind (placeSymbol "MAIN2" mainCC ((pos 60 0) + offsetXY))
    |> Result.bind (placeWire (portOf "MAIN1" 4) (portOf "MAIN2" 0))
    |> Result.bind (placeWire (portOf "MAIN1" 5) (portOf "MAIN2" 1))
    |> Result.bind (placeWire (portOf "MAIN1" 6) (portOf "MAIN2" 2))
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

    let test5 testNum firstSample dispatch =
        runTestOnSheets
            "two custom components with random offset: fail all tests"
            firstSample
            offsetXY
            makeA4Circuit
            Asserts.failOnAllTests
            dispatch
        |> recordPositionInTest testNum dispatch