module SheetBeautifyD1


// open modules likely to be used
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


/// Team deliverable D1 Build (as part of individual phase work) <ag1421>
/// Custom component scaling. Positioning of all components
let sheetAlignScale (sheet: SheetT.Model) = 
    // 1. Align all singly-connected components to eliminate wire bends in parallel wires
    let connectedSymbols = getConnSyms sheet.Wire
    // find symbols that only have one connection (singly connected symbols)
    let singlyConnected = 
        connectedSymbols
        |> List.collect (fun (sym1, sym2) -> [sym1;sym2])
        |> List.countBy id
        |> List.filter (fun (_, count) -> count=1)
        |> List.map (fun (sym, _) -> sym)
    
    let isSinglyConnected (sym: Symbol) = List.tryFind (fun s -> sym=s) singlyConnected
    // find the singly connected symbol's port and its paired port (input-output or output-input)
    let findSinglePortAndPair sym1 sym2 : Port option*Port option =
        let singlySymbol = 
            match (isSinglyConnected sym1), (isSinglyConnected sym2) with
            | Some sym1, _ -> sym1
            | _, Some sym2 -> sym2
            | _, _ -> sym1 // shouldn't happen

        // get wire connecting both symbols
        let wireConnecting = 
            sheet.Wire.Wires
            |> mapValues
            |> Seq.toList
            |> List.filter (
                fun wire -> 
                    let allSymPorts = 
                        singlySymbol.PortMaps.Orientation
                        |> mapKeys
                        |> Seq.toList
                    let possiblePort = List.tryFind (fun (x: string) -> wire.InputPort=(InputPortId x)) allSymPorts
                    match possiblePort with
                    | None -> false
                    | Some port -> true
            )
            |> List.tryExactlyOne
        match wireConnecting with 
        | None -> None, None
        | Some w -> (Some (getSourcePort sheet.Wire w), Some (getTargetPort sheet.Wire w))



    // Find the singly connected symbol and the symbol it is connected to, and 
    // move the singly connected symbol to the x/y coordinate of the port it is connected to
    connectedSymbols
    |> List.filter (
        fun (sym1, sym2) -> 
            match (isSinglyConnected sym1), (isSinglyConnected sym2) with
            | None, None -> false
            | _, _ -> true
        )
    // ensure singly connected symbol is first
    |> List.map (
        fun (sym1, sym2) -> 
        match (isSinglyConnected sym1, isSinglyConnected sym2) with
        | Some s1, Some s2 -> (s1, s2)
        | Some s1, None-> (s1, sym2)
        | _ , Some s2 -> (s2, sym1)
        | None, None -> (sym1, sym2) // shouldn't happen
    )
    |> List.map (
        fun (sym1, sym2) -> (findSinglePortAndPair sym1 sym2), sym1
    )
    |> List.map (
        fun ((_, portOp2), sym) -> 
        match portOp2 with 
        | Some port -> (findPortPos port.Id sheet), sym
        | None -> None, sym
    )
    |> List.map (
        fun ((newPos), sym) -> 
            match newPos with 
            | Some nP -> setSymbolPosition {X=fst nP; Y=snd nP} sym
            | None -> sym
    )
    |> List.fold (fun cs ci -> Optic.set symbols_ (Map.add ci.Id ci cs.Wire.Symbol.Symbols) cs) sheet
    