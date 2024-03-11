module Renderer.TestDrawBlockD1

open DrawModelType
open Optics
open CommonTypes
open SheetBeautifyHelpers
open TestDrawBlock.TestLib
open TestDrawBlock.HLPTick3
open GenerateData

(*
My part was to do D1T, the test was make in TestDrawBlockD1.fs

    1) Sheet maker -> makeNearStraightCircuit
    2) Testing Assertion -> makeTest1Circuit

    These are starter Contribution to the team, they will be used to test the code written here.
    Specifically, to test D1B written here. Hence D1B will be called to straighten the circuit created by makeNearStraightCircuit.

    These functions are also present in TestDrawBlock.fs, hence they were tested there. They will also be kept there for the team phase.
    (Where they will be combined with D2T, and D3T).
*)


module D1T =

    let deviatedPositions =
        randomInt -100 1 100
        |> map (fun n ->  (float n) / 10.)
        |> map (fun n -> middleOfSheet + {X=200; Y=n})

    // TODO: Make a more interesting circuit, i.e. not just a DFF start using custom components and multiple wires
    // This is just a starter to get the tests working
    let makeNearStraightCircuit (deviated: XYPos) = // D1T
        initSheetModel
        |> Builder.placeSymbol "DFF1" DFF deviated
        |> Result.bind (Builder.placeSymbol "FF1" DFF middleOfSheet)
        |> Result.bind (Builder.placeWire (portOf "FF1" 0) (portOf "DFF1" 0))
        |> getOkOrFail


    // TODO: Add more assertions to test the circuit
    // TODO: Integrate the completed SheetBeautifyHelpers module to test the circuit, Hence it might change in the Team Phase
    let failOnWiresNotStraight (sample: int) (sheet: SheetT.Model) =
        sheet
        // TODO: Call D1B here, assuming that D1B is a SheetT.Model -> SheetT.Model function
        |> Optic.get SheetT.wires_
        |> mapValuesToList
        |> List.map (getVisibleSegmentsOfWire >> List.length)
        |> List.exists (fun n -> n > 1)
        |> (function | true -> Some "Fail ! There is a wire which is not straightened"
                     | false -> None)

