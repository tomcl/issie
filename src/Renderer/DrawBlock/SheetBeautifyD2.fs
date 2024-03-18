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


// ------------------------helpers---------------------------

///generate all list of elements with possible states
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
// ------------------------helpers---------------------------


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
    
    let checkMux (symbol : Symbol) = 
        let componentType = symbol.Component.Type
        match componentType with
        |Mux2| Mux4 | Mux8 | Demux2 | Demux4 | Demux8  -> true // | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 might add later, need to clarify with spec
        | _ -> false

    let checkGate (symbol : Symbol) = 
        let componentType = symbol.Component.Type
        match componentType with
        |GateN (gateType, numInputs) -> true 
        | _ -> false

    let checkCustom (symbol : Symbol) = 
        let componentType = symbol.Component.Type
        match componentType with
        |Custom customComp -> true // | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 might add later, need to clarify with spec
        | _ -> false

    ///return a list of corresponding component from the symbols on sheet
    ///support all Muxes, Gates and custom components
    let getCompList (comp: string ) (sheet: SheetT.Model) = 
        symbolList sheet
        |> List.filter (fun symbol ->  
            match comp with
            |"Mux" -> checkMux symbol
            |"Gate" -> checkGate symbol
            |"Custom" -> checkCustom symbol
            | _ -> failwithf "not supported yet"
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
        |> SymbolResizeHelpers.flipSymbol SymbolT.FlipVertical

    let rotate90 (sym : SymbolT.Symbol) = 
        sym
        |> SymbolResizeHelpers.rotateSymbol Degree90

    let rotateAnti90 (sym : SymbolT.Symbol) = 
        sym
        |> SymbolResizeHelpers.rotateSymbol Degree270

    // flip a MUX and swap input
    let flipAndSwapMux (symbol: SymbolT.Symbol) = 
        symbol 
        |> swapMuxInputOrder
        |> flipVertical
        
    //measure wire crossings number on sheet
    let numWireCrossing (sheet: SheetT.Model) = 
        numSegmentCrossRightAngle sheet
        //numOfWireRightAngleCrossings sheet
    printf "inital cross number: %A" (numWireCrossing sheet)
    
    // 12 possible combinations for a MUX being flipped, having inputs swapped, rotate 90, rotate270. 12 states
    // flip, swap, rotate 90, rotate-90, flip and swap, flip and rotat90, flip and rotat-90, swap 90, swap -90, flipand swap 
    //and rotate90, flip and swap and rotate-90
    let muxTransform (state:int) (sym: SymbolT.Symbol)= 
        match state with
        | 0 -> sym
        | 1 -> flipVertical sym
        | 2 -> swapMuxInputOrder sym
        | 3 -> flipAndSwapMux sym
        | 4 -> rotate90 sym
        | 5 -> rotateAnti90 sym
        | 6 -> sym |> flipVertical |> rotate90
        | 7 -> sym |> flipVertical |> rotateAnti90
        | 8 -> sym |> swapMuxInputOrder |> rotate90
        | 9 -> sym |> swapMuxInputOrder |> rotateAnti90
        | 10 -> sym |> flipAndSwapMux |> rotate90
        | 11 -> sym |> flipAndSwapMux |> rotateAnti90
        | _ -> failwithf "invalid transform state"

    //  6 possible combinations for a gate being flipped vertical 2 states
    let gateTransform (state: int) (sym: SymbolT.Symbol) = 
        match state with
        | 0 -> sym
        | 1 -> flipVertical sym
        | 2 -> rotate90 sym
        | 3 -> rotateAnti90 sym
        | 4 -> sym |> flipVertical |> rotateAnti90
        | 5 -> sym |> flipVertical |> rotate90
        | _ -> failwithf "invalid transform state"

    let muxList = getCompList "Mux" sheet
    let gateList= getCompList "Gate" sheet
    let customCompList = getCompList "Custom" sheet
    ///list of combination of all Muxes with transform state, e.g 2 MUX, each Mux has 4 transform combination -> total 16 combination 
    let muxTransformStates = generateAllStates (List.length muxList) 12
    ///list of combination of all Gates with transform state, e.g 3 Gates, each Gate has 2 transform combination -> total 8 combination 
    let gateTransformStates = generateAllStates (List.length gateList) 6

    let allCombinationList = List.allPairs muxTransformStates gateTransformStates

    ///get re-routed sheet given a transform state list of MUX and Gates, apply the corresponding transform according to state
    ///for each symbol, update symbol map and rewire
    let reRoutedSheet (combMux:int list) (combGate: int list)  =
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
        {newSheet with Wire = {newSheet.Wire with Wires = wireMap} }  //re-routed sheet

    /// get the number of wire crossing on sheet for all combination of transform
    let getNumWireCrossingForAllCombination =   
        allCombinationList
        
        |> List.map (fun (combMux, combGate) -> 
            let reRoutedSheet = reRoutedSheet combMux combGate
            numWireCrossing reRoutedSheet
            )
    /// get the transform list which number of wire crossing on sheet is minimum
    let getOptimalCombination = 
        printf "%A" (List.length getNumWireCrossingForAllCombination)
        let optimalIndex = 
            //printf "%A" getNumWireCrossingForAllCombination
            getNumWireCrossingForAllCombination
            |> List.mapi (fun i x -> (i, x))
            |> List.minBy snd
            |> fst
        List.item optimalIndex allCombinationList
    //printf "%A" getOptimalCombination

    getOptimalCombination
    |> fun (combMux, combGate) -> reRoutedSheet combMux combGate

   