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

///Team deliverable D2 implementation <az1221>
///Port order on custom components, flip components, flip MUX input order
let sheetOrderFlip (sheet: SheetT.Model) = 
    //1. Flip all MUX inputs and permute gate inputs to reduce wire crossings
    // Do not increase total number of wire bends
    //Algorithm 1. Exhaustive search algorithm. Try every possible combination of flips and swaps: measure wire crossings
    //each time, choose minimum. This is optimal but slow and unscalable.

    // get symbols on sheet
    let symbolLst = 
        sheet.Wire.Symbol.Symbols
        |> mapValues
        |> Array.toList

    //find all MUX component on sheet
    let getMuxList (symbolList: Symbol list) = 
        symbolList
        |> List.filter (fun symbol -> 
            let componentType = symbol.Component.Type
            match componentType with 
            |Mux2  -> true // | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 might add later, need to clarify with spec
            | _ -> false
            )

    // swap MUX input, return flipped MUX
    let swapMuxInput (mux: symbol)=
        let state = getReverseInputMux mux
        setReverseInputMux (~state) mux

    // flip a MUX and swap input
    let flipAndSwapMux (symbol: Symbol) = 
        symbol 
        |> flipSymbol

    //measure wire crossings
    let numWireCrossing (sheet: sheetT.Model) = 
        numSegmentCrossRightAngle sheet
    
    //2. Re-order any custom component ports along any side to reduce crossings
    //3. Change orientation of gates and MUXes and maybe other components to reduce crossings
    //4. Heuristic to optimise for minimum crossings + not making wiring more complex + not
    //changing orientation of components that will look weird if that is done.