module SheetBeautifyD1

open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open CommonTypes
open BlockHelpers
open SymbolPortHelpers
open Symbol
open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open ModelType
open BusWire
open RotateScale
open SheetBeautifyHelpers
open TestDrawBlock
open GenerateData



module D1TestHelperFunctions =

    let alignSymbols (wireModel: BusWireT.Model) (sourceSymbol: Symbol) (otherSymbol: Symbol) : BusWireT.Model =

            // Only attempt to align symbols if they are connected by ports on parallel edges.
            match getOppEdgePortInfo (wireModel:BusWireT.Model) sourceSymbol otherSymbol with
            | None -> wireModel
            | Some(movePortInfo, otherPortInfo) ->
                let offset = alignPortsOffset movePortInfo otherPortInfo
                let symbol' = moveSymbol offset sourceSymbol
                let model' = Optic.set (symbolOf_ sourceSymbol.Id) symbol' wireModel
                BusWireSeparate.routeAndSeparateSymbolWires model' sourceSymbol.Id

    // create an initial empty Sheet Model 
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


    let horizLinePositions =
        fromList [-100..20..100]
        |> map (fun n -> middleOfSheet + {X=float n; Y=0.})

    let verticLinePositions =
        fromList [-100..20..100]
        |> map (fun n -> middleOfSheet + {X=0.; Y=float n})


    let generateSampleData =
        product (fun x y -> middleOfSheet + {X = float x; Y = float y})
            (fromList [-100..20..100])
            (fromList [-100..20..100])

    let filterOverlap (pos1: XYPos) =
        // Define the size of the components
        let componentSize = 10.0
        
        let overlapX = abs(pos1.X - middleOfSheet.X) < componentSize
        let overlapY = abs(pos1.Y - middleOfSheet.Y) < componentSize
        
        // Return true if there's no overlap, false otherwise
        not (overlapX && overlapY)

    let filteredSampleData =
        generateSampleData
        |> GenerateData.filter filterOverlap

    let makeNearStraightWireCircuit (andPos: XYPos) =
        initSheetModel
        |> HLPTick3.Builder.placeSymbol "A" (Input1(1, None)) andPos
        |> Result.bind (HLPTick3.Builder.placeSymbol "B" (Input1(1, None)) andPos)
        |> Result.bind (HLPTick3.Builder.placeSymbol "MUX1" Mux2 middleOfSheet)
        |> Result.bind (HLPTick3.Builder.placeWire (HLPTick3.portOf "A" 0) (HLPTick3.portOf "MUX1" 0))
        |> Result.bind (HLPTick3.Builder.placeWire (HLPTick3.portOf "MUX1" 0) (HLPTick3.portOf "A" 0))
        |> Result.bind (HLPTick3.Builder.placeWire (HLPTick3.portOf "B" 0) (HLPTick3.portOf "MUX1" 1))
        |> Result.bind (HLPTick3.Builder.placeWire (HLPTick3.portOf "MUX1" 1) (HLPTick3.portOf "B" 0))
        |> TestLib.getOkOrFail



    module Asserts =

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

  (*      // Fail when sheet contains 2 different wires that intersect
        let failOnWireIntersectsWire (sample: int) (sheet: SheetT.Model) =
            let wireModel = sheet.Wire.Wires
            let absSegments =
                Map.toList wireModel
                |> List.map (fun (id, wire) -> id, getAbsSegments wire)

            let intersectingSegments (segs1: ASegment list) (segs2: ASegment list) =
                segs1 |> List.exists (fun seg1 ->
                    segs2 |> List.exists (fun seg2 ->
                        overlap1D (seg1.Start.X, seg1.End.X) (seg2.Start.X, seg2.End.X) &&
                        overlap1D (seg1.Start.Y, seg1.End.Y) (seg2.Start.Y, seg2.End.Y)
                    )
                )
            absSegments
            |> List.allPairs
            |> List.filter (fun ((id1, _), (id2, _)) -> id1 <> id2)
            |> List.exists (fun ((_, segs1), (_, segs2)) ->
                intersectingSegments segs1 segs2
            )
            |> function
            | true -> Some $"Wire intersects another wire in Sample {sample}"
            | false -> None *)

    module TestD1 =

        let testD1_1 testNum firstSample dispatch =
            HLPTick3.Builder.runTestOnSheets
                "Test Near Straight Wire"
                firstSample
                filteredSampleData
                makeNearStraightWireCircuit
                Asserts.failOnSymbolIntersectsSymbol
                dispatch
            |> HLPTick3.Tests.recordPositionInTest testNum dispatch







