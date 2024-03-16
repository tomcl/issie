module SheetBeautifyD2

open BlockHelpers
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open Helpers
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open BusWireUpdate
open BusWireRoute

// --------helpers------------
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

let getVisibleSegOnSheet (sheet: SheetT.Model) = 
    let getSegments (wireModel: ConnectionId * BusWireT.Wire) = 
            let cId = fst wireModel
            let wire = snd wireModel
            visibleSegments cId sheet
            |> List.scan (+) (wire.StartPos)
            |> List.pairwise

    //get visible segments from all wires on sheet with actual position
    
    sheet.Wire.Wires
    |> Map.toList
    |> List.map getSegments
    |> List.concat

let dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

let vectorLength (x, y) = sqrt (x * x + y * y)

let isRightAngle (seg1: XYPos*XYPos) (seg2: XYPos*XYPos) =
    let startPosS1, endPosS1 = seg1
    let startPosS2, endPosS2 = seg2
    let vector1 = ( endPosS1.X -  startPosS1.X, endPosS1.Y -  startPosS1.Y)
    let vector2 = ( endPosS2.X -  startPosS2.X,  endPosS2.Y -  startPosS2.Y)
    let dotProd = dotProduct vector1 vector2
    let mag1 = vectorLength vector1
    let mag2 = vectorLength vector2
    abs(dotProd) < 1e-9 && abs(mag1) > 1e-9 && abs(mag2) > 1e-9
//T3R
///The number of distinct pairs of segments that cross each other at right angles. 
///Does not include 0 length segments or segments on same net intersecting at one end, or
///segments on same net on top of each other. Count over whole sheet.
let numSegmentCrossRightAngle (sheet: SheetT.Model) = 

    let realIntersect1D ((a1, a2): float * float) ((b1, b2): float * float) : bool =
        let a_min, a_max = min a1 a2, max a1 a2
        let b_min, b_max = min b1 b2, max b1 b2
        a_max > b_min && b_max > a_min

    //if the intersection is at the end of one or both of the segments, this is a T junction not counted as an intersection
    let realIntersect2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
        (realIntersect1D (a1.X, a2.X) (b1.X, b2.X)) && (realIntersect1D (a1.Y, a2.Y) (b1.Y, b2.Y))
    
    let checkSegmentCross (seg1: XYPos*XYPos) (seg2: XYPos*XYPos) = 
        (seg1 <> seg2) && (isRightAngle seg1 seg2) && (realIntersect2D seg1 seg2) 

    // filter out 0 length segments using visible segments
    let nonZeroSeg = getVisibleSegOnSheet sheet
 
    nonZeroSeg
    |> List.allPairs nonZeroSeg
    |> List.map (fun (segA, segB) -> checkSegmentCross segA segB)
    |> List.fold ( fun num bool -> 
                                match bool with
                                |true -> num+1
                                |_ -> num ) 0
    |> (fun num -> num/2)  //remove repeated pairs with self



let generateAllStates (symbolCount: int) (stateNum: int): int list list =
    let rec generateStatesForSym (remainingSym: int) : int list list =
        if remainingSym = 0 then
            [[]] // Base case: Empty list for zero elements
        else
            // Generate states for the current element
            let previousStates = generateStatesForSym (remainingSym - 1)
            let currentStates = [0 .. stateNum-1] // possible states
            // Combine current states with previous states
            [ for prevState in previousStates do
                for state in currentStates ->
                    state :: prevState ]
    // Generate all states for all elements
    generateStatesForSym symbolCount



///Team deliverable D2 implementation <az1221>
///Port order on custom components, flip components, flip MUX input order
let sheetOrderFlip (sheet: SheetT.Model) = 
    //1. Flip all MUX inputs and permute gate inputs to reduce wire crossings
    // Do not increase total number of wire bends
    //Algorithm 1. Exhaustive search algorithm. Try every possible combination of flips and swaps: measure wire crossings
    //each time, choose minimum. This is optimal but slow and unscalable.

    // get symbols on sheet
    let symbolList (sheet: SheetT.Model) = 
        sheet.Wire.Symbol.Symbols
        |> mapValues
        |> Array.toList

    ///find all MUXesfrom a list of symbol (MUX2)
    let getMuxList (sheet: SheetT.Model)  = 
        symbolList sheet
        |> List.filter (fun symbol -> 
            let componentType = symbol.Component.Type
            match componentType with 
            |Mux2  -> true // | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 might add later, need to clarify with spec
            | _ -> false
            )

    ///find all gates from a list of symbol
    let getGateList (sheet: SheetT.Model)  = 
        symbolList sheet
        |> List.filter (fun symbol -> 
            let componentType = symbol.Component.Type
            match componentType with 
            | GateN (gateType, numInputs) -> true 
            | _ -> false
            )


    // swap MUX input, return flipped MUX
    let swapMuxInputOrder (mux: SymbolT.Symbol)=
        let edge: Edge option = 
            match mux.Component.Type with
            |Mux2| Mux4| Mux8 -> Some Left
            |Demux2|Demux4|Demux8 -> Some Right
            | _ -> None

        match edge with
        |Some side -> 
            let portOrder = getPortOrder side mux
            let reversePortOrder = List.rev portOrder
            putPortOrder side reversePortOrder mux
        |None -> mux

    // flip a symbol vertically, SymbolResizeHelpers.flip Symbol does not work for flip vertical
    let flipVertical (sym: SymbolT.Symbol) = 
        sym
        |> SymbolResizeHelpers.rotateSymbol Degree180
        |> SymbolResizeHelpers.flipSymbol SymbolT.FlipHorizontal 

    // flip a MUX and swap input
    let flipAndSwapMux (symbol: SymbolT.Symbol) = 
        //let symbolsMap = sheet.Wire.Symbol.Symbols
        symbol 
        |> swapMuxInputOrder
        |> flipVertical
        
        // let updatedSymbolsMap = Map.add symbol.Id transformed symbolsMap
        // { sheet with Wire = { sheet.Wire with Symbol = { sheet.Wire.Symbol with Symbols = updatedSymbolsMap } } }

    //measure wire crossings
    let numWireCrossing (sheet: SheetT.Model) = 
        numSegmentCrossRightAngle sheet
        //numOfWireRightAngleCrossings sheet
    // printf "inital cross number: %A" (numWireCrossing sheet)
    // let updatedSymbolsMap = 
    //     //flip and swap MUX then update Symbol map
    //     let updateMuxSymbolMap = 
    //         getMuxList sheet
    //         |> List.map flipAndSwapMux
    //         |> (fun transformedSym -> 
    //                 let symbolsMap = sheet.Wire.Symbol.Symbols
    //                 (symbolsMap, transformedSym)
    //                 ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap))
    //     // vertical flip the gates and update to the modified symbol map
    //     let updateGatesSymbolMap = 
    //         getGateList sheet
    //         |> List.map flipVertical
    //         |> (fun gateList ->
    //                 (updateMuxSymbolMap, gateList)
    //                 ||> List.fold (fun symMap symToAdd -> 
    //                     //printf "info : %A" symToAdd.Component.SymbolInfo
    //                     Map.add symToAdd.Id symToAdd symMap))
    //     updateGatesSymbolMap

    // 4 possible combinations for a MUX being flipped or having inputs swapped.
    let muxTransform (state:int) (sym: SymbolT.Symbol)= 
        match state with
        | 0 -> sym
        | 1 -> flipVertical sym
        | 2 -> swapMuxInputOrder sym
        | 3 -> flipAndSwapMux sym
        | _ -> failwithf "invalid transform state"
    //  2 possible combinations for a gate being flipped vertical
    let gateTransform (state: int) (sym: SymbolT.Symbol) = 
        match state with
        | 0 -> sym
        | 1 -> flipVertical sym
        | _ -> failwithf "invalid transform state"

    let muxList = getMuxList sheet
    let gateList= getGateList sheet
    let muxTransformStates = generateAllStates (List.length muxList) 4
    let gateTransformStates = generateAllStates (List.length gateList) 2

    let allCombinationList = List.allPairs muxTransformStates gateTransformStates
    let getNumWireCrossingForAllCombination =   
        allCombinationList
        
        |> List.map (fun (combMux, combGate) -> 
            let updateMuxSymbolMap = 
                // (combMux, muxList)
                // |> List.map (fun (state, sym) -> muxTransform state sym)
                List.map2 muxTransform combMux muxList
                |> (fun transformedSym -> 
                    let symbolsMap = sheet.Wire.Symbol.Symbols
                    (symbolsMap, transformedSym)
                    ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap))
                
            let updateGatesSymbolMap = 
                List.map2 gateTransform combGate gateList
                |> (fun transformedSym -> 
                    (updateMuxSymbolMap, transformedSym)
                    ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap))

            let newSheet = { sheet with Wire = { sheet.Wire with Symbol = { sheet.Wire.Symbol with Symbols = updateGatesSymbolMap } } }
            // reroute wires for modified symbols
            let wireMap = 
                newSheet.Wire.Wires
                |> Map.map (fun cid wire -> smartAutoroute newSheet.Wire wire)
            let reRoutedSheet = {newSheet with Wire = {newSheet.Wire with Wires = wireMap} }
            numWireCrossing reRoutedSheet
                )
    let getOptimalCombination = 
        let optimalIndex = 
            //printf "%A" getNumWireCrossingForAllCombination
            getNumWireCrossingForAllCombination
            |> List.mapi (fun i x -> (i, x))
            |> List.minBy snd
            |> fst
        List.item optimalIndex allCombinationList
    //printf "%A" getOptimalCombination
    getOptimalCombination
    |> fun (combMux, combGate) -> 
            let updateMuxSymbolMap = 
                // (combMux, muxList)
                // |> List.map (fun (state, sym) -> muxTransform state sym)
                List.map2 muxTransform combMux muxList
                |> (fun transformedSym -> 
                    let symbolsMap = sheet.Wire.Symbol.Symbols
                    (symbolsMap, transformedSym)
                    ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap))
                
            let updateGatesSymbolMap = 
                List.map2 gateTransform combGate gateList
                |> (fun transformedSym -> 
                    (updateMuxSymbolMap, transformedSym)
                    ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap))
                    
            let newSheet = { sheet with Wire = { sheet.Wire with Symbol = { sheet.Wire.Symbol with Symbols = updateGatesSymbolMap } } }
            // reroute wires for modified symbols
            let wireMap = 
                newSheet.Wire.Wires
                |> Map.map (fun cid wire -> smartAutoroute newSheet.Wire wire)
            {newSheet with Wire = {newSheet.Wire with Wires = wireMap} }









    // printf "number of wire crossing:  %d" (numWireCrossing sheet)
    // let newSheet = { sheet with Wire = { sheet.Wire with Symbol = { sheet.Wire.Symbol with Symbols = updatedSymbolsMap } } }
    // // reroute wires for modified symbols
    // let wireMap = 
    //     newSheet.Wire.Wires
    //     |> Map.map (fun cid wire -> smartAutoroute newSheet.Wire wire)
    // {newSheet with Wire = {newSheet.Wire with Wires = wireMap} }

    


    // for gateTransformList in gateTransformStates do
    //     for muxTransformList in muxTransformStates do 
    //         let updateMuxSymbolMap = 
    //             (muxTransformList, muxList)
    //             ||> List.map2 (fun state sym -> muxTransform state sym)
    //             |> (fun transformedSym -> 
    //                     let symbolsMap = sheet.Wire.Symbol.Symbols
    //                     (symbolsMap, transformedSym)
    //                     ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap))
            
    //         let updateGatesSymbolMap = 
    //             (gateTransformStates, gateList)
    //             ||> List.map2 (fun state sym -> gateTransform state sym)
    //             |> (fun gateList ->
    //                     (updateMuxSymbolMap, gateList)
    //                     ||> List.fold (fun symMap symToAdd -> 
    //                         //printf "info : %A" symToAdd.Component.SymbolInfo
    //                         Map.add symToAdd.Id symToAdd symMap))
    //         let newSheet = { sheet with Wire = { sheet.Wire with Symbol = { sheet.Wire.Symbol with Symbols = updatedSymbolsMap } } }
    //         // reroute wires for modified symbols
    //         let wireMap = 
    //             newSheet.Wire.Wires
    //             |> Map.map (fun cid wire -> smartAutoroute newSheet.Wire wire)
    //         let reRoutedSheet = {newSheet with Wire = {newSheet.Wire with Wires = wireMap} }

            