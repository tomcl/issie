// D2 Testing
module TestDrawBlockD2
open GenerateData
open Elmish
open SymbolResizeHelpers
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

// Test D2 using random sample inputs
// Reduction in wire crossings, other quality measures

// TODO: 
// 1. Generate random sample tests for the number of wire crossings
// 2. Test the input before D2 and after D2 is applied
// 3. Output the difference in the number of wire crossings
// 4. Other quality measures: check if wire intersecting symbols, etc.

module groupPhaseWork =
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

    module Builder = 
    // functions to build issue schematics programmatically

//--------------------------------------------------------------------------------------------------//
//----------------------------------------Example Test Circuits using Gen<'a> samples---------------//
//--------------------------------------------------------------------------------------------------//

    open Builder
    /// random sample test circuit
    let makeTestCircuit =
    initSheetModel
    // TODO: Add random sample test circuit

    let makeTest1Circuit (andPos:XYPos) =
    initSheetModel
    |> placeSymbol "G1" (GateN(And,2)) andPos
    |> Result.bind (placeSymbol "FF1" DFF middleOfSheet)
    |> Result.bind (placeWire (portOf "G1" 0) (portOf "FF1" 0))
    |> Result.bind (placeWire (portOf "FF1" 0) (portOf "G1" 0) )
    |> getOkOrFail



//------------------------------------------------------------------------------------------------//
//-------------------------Example assertions used to test sheets---------------------------------//
//------------------------------------------------------------------------------------------------//

// helper functions:
// need to avoid completelt: 
// wire crossing symbols
// reduce for beautify:
// same net crossing, distinct net crossing


    module Asserts =

        let getRightAngleCrossingSegments (sheet: SheetT.Model) =
        let wireList = sheet.Wire.Wires |> Map.toList

        // get the segments of distinct wires that cross each other at right angles
        let distinctWiresPairs =
            wireList
            |> List.allPairs wireList
            |> List.filter (fun (wire1, wire2) -> not (fromSameNet (snd wire1) (snd wire2)))
            |> List.map (fun (wire1, wire2) -> 
                            if (fst wire1) < (fst wire2) then (wire1,wire2) else (wire2, wire1))
            |> List.distinct
            |> List.map (fun (wire1,wire2) -> (fst wire1, fst wire2))
            |> List.collect (fun (wire1, wire2) -> 
                                let seg1 = visibleSegments wire1 sheet
                                let seg2 = visibleSegments wire2 sheet
                                List.allPairs seg1 seg2)

        // get the segments of the wire that share the same input port (on the same net)
        let wiresByInputPort =
            wireList
            |> List.groupBy (fun (_, wire) -> wire.InputPort)
            |> List.map (fun (inputPortID, wireGroup) -> (inputPortID, wireGroup))
            |> List.filter (fun (_, wireGroup) -> List.length wireGroup > 1)
            |> List.map (fun (_, wireGroup) ->
                wireGroup
                |> List.allPairs wireList
                |> List.map (fun (wire1,wire2) -> (fst wire1, fst wire2))
                |> List.collect (fun (wire1, wire2) -> 
                    let seg1 = visibleSegments wire1 sheet
                    let seg2 = visibleSegments wire2 sheet
                    List.allPairs seg1 seg2))

        
        // check if two distinct wires segments cross each other at right angles
        let distinctWiresCrossing (seg1:XYPos*XYPos) (seg2:XYPos*XYPos): bool =
            ((isHorizontal seg1) && (isVertical seg2) 
            || (isVertical seg1) && (isHorizontal seg2)) 
            || checkOverlap seg1 seg2

        // check if two segments on the same net actually cross each other
        let sameNetCrossing (seg1:XYPos*XYPos) (seg2:XYPos*XYPos): bool =
            (distinctWiresCrossing seg1 seg2) && (isSameNetCrossing seg1 seg2)

        let numDistinctCrossing =  
            distinctWiresPairs
            |> List.allPairs distinctWiresPairs
            |> List.map (fun (seg1, seg2) -> distinctWiresCrossing seg1 seg2)
            |> List.fold (fun acc bool -> if bool then acc+1 else acc) 0
            |> (/) 
            <| 2

        let numSameNetCrossing =
            wiresByInputPort
            |> List.map (fun segmentList -> 
                segmentList
                |> List.allPairs segmentList
                |> List.map (fun (seg1, seg2) -> sameNetCrossing seg1 seg2)
                |> List.fold (fun acc bool -> if bool then acc+1 else acc) 0)
            |> List.sum
            |> (/)
            <| 2

        // total number of segments that cross each other at right angles
        numDistinctCrossing + numSameNetCrossing

        let failOnSampleNumber (sampleToFail :int) (sample: int) _sheet =
                if sampleToFail = sample then
                    Some $"Failing forced on Sample {sampleToFail}."
                else
                    None

            /// Fails all tests: useful to show in sequence all the sheets generated in a test
        let failOnAllTests (sample: int) _ =
            Some <| $"Sample {sample}"


    //---------------------------------------------------------------------------------------//
    //-----------------------------Demo tests on Draw Block code-----------------------------//
    //---------------------------------------------------------------------------------------//

    module Tests =

    // similar to the structure in TestDrawBlock
        