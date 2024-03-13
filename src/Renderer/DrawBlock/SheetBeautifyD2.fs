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

        // let portOrder = getPortOrder (Option.isSome edge) mux
        // let reversePortOrder = List.rev portOrder
        // putPortOrder edge reversePortOrder mux
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
    let flipAndSwapMux (symbol: Symbol) = 
        let symbolsMap = sheet.Wire.Symbol.Symbols
        symbol 
        |> flipVertical
        |> swapMuxInputOrder
        // let updatedSymbolsMap = Map.add symbol.Id transformed symbolsMap
        // { sheet with Wire = { sheet.Wire with Symbol = { sheet.Wire.Symbol with Symbols = updatedSymbolsMap } } }

    //measure wire crossings
    let numWireCrossing (sheet: SheetT.Model) = 
        numOfWireRightAngleCrossings sheet

    let updatedSymbolsMap = 
        //flip and swap MUX then update Symbol map
        let updateMuxSymbolMap = 
            getMuxList sheet
            |> List.map flipAndSwapMux
            |> (fun transformedSym -> 
                    let symbolsMap = sheet.Wire.Symbol.Symbols
                    (symbolsMap, transformedSym)
                    ||> List.fold (fun symMap symToAdd -> Map.add symToAdd.Id symToAdd symMap))
        // vertical flip the gates and update to the modified symbol map
        let updateGatesSymbolMap = 
            getGateList sheet
            |> List.map flipVertical
            |> (fun gateList ->
                    (updateMuxSymbolMap, gateList)
                    ||> List.fold (fun symMap symToAdd -> 
                        printf "info : %A" symToAdd.Component.SymbolInfo
                        Map.add symToAdd.Id symToAdd symMap))
        updateGatesSymbolMap
        
    printf "number of wire crossing:  %d" (numWireCrossing sheet)
    let newSheet = { sheet with Wire = { sheet.Wire with Symbol = { sheet.Wire.Symbol with Symbols = updatedSymbolsMap } } }
    // reroute wires for modified symbols
    let wireMap = 
        newSheet.Wire.Wires
        |> Map.map (fun cid wire -> smartAutoroute newSheet.Wire wire)
    {newSheet with Wire = {newSheet.Wire with Wires = wireMap} }

    

    
    //2. Re-order any custom component ports along any side to reduce crossings
    //3. Change orientation of gates and MUXes and maybe other components to reduce crossings
    //4. Heuristic to optimise for minimum crossings + not making wiring more complex + not
    //changing orientation of components that will look weird if that is done.
