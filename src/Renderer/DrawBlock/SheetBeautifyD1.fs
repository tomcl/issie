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
open Optics.Operators // for >-> operator


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
    // Updates the enitre sheet
    let updatedSheet = 
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
            | Some port -> Some (getPortPos port.Id sheet.Wire.Symbol), sym
            | None -> None, sym
        )
        |> List.map (
            fun ((newPos), sym) -> 
                match newPos with 
                | Some nP -> updateSymPos {X=nP.X; Y=nP.Y} sym
                | None -> sym
        )
        |> List.fold (fun cs ci -> Optic.set symbols_ (Map.add ci.Id ci cs.Wire.Symbol.Symbols) cs) sheet
    
    
    // 4. Scale custom symbols to reduce wire bends in parallel wires between two custom components

    // get custom symbols and the symbols they are connected to
    let customSymbols = 
        let test = {
            Name="";
            // Tuples with (label * connection width).
            InputLabels=[];
            OutputLabels=[];
            Form=None;
            Description=None;
        }
        sheet.Wire
        |> getConnSyms
        |> List.filter (
            fun (sym1, sym2) -> 
                match sym1.Component.Type, sym2.Component.Type with
                | Custom _, Custom _ -> true
                | _, _ -> false
        )
    
    // count number of ports on both symbol edges and their current dimensions to find scale factor
    // set vscale correspondingly
    // TODO: same with hscale
    let scaledSyms = 
        customSymbols
        |> List.map (fun (sym1, sym2) -> ((getPortOrder Left sym1).Length, (getCustomCompDims sym1).Y), ((getPortOrder Right sym2).Length, (getCustomCompDims sym2).Y))
        // convert to scaling factor applied to second component
        |> List.map (fun (sym1info, sym2info) -> ((snd sym1info)/(float) (fst sym1info))/((snd sym2info)/(float) (fst sym2info)) )
        |> List.mapi (fun i scale -> {snd customSymbols[i] with VScale=Some scale} )

    let scaledSymSheet = 
        (updatedSheet.Wire.Symbol.Symbols, scaledSyms)
        ||> List.fold (fun acc sym -> Map.add sym.Id sym acc)
        |> Optic.set symbols_
        <| updatedSheet


    // 2/7. Do not overlap components
    // Go through updated sheet and check which symbols overlap
    // if they overlap, move the symbol to its old position in the original sheet

    let newSymbolBoundingBoxes = 
        scaledSymSheet.BoundingBoxes
        |> Map.toList
    
    let overlappingComponents = 
        newSymbolBoundingBoxes
        |> List.allPairs newSymbolBoundingBoxes
        |> List.filter (fun (bb1, bb2) -> bb1<>bb2)
        |> List.filter (fun (bb1, bb2) -> overlap2DBox (snd bb1) (snd bb2))
        |> List.collect (fun tup -> [fst tup; snd tup])
        |> List.map (fun (cid, bb) -> cid)
    
    let noSymbolOverlapSheet = 
        (scaledSymSheet.Wire.Symbol.Symbols, overlappingComponents)
        ||> List.fold (fun acc cid -> Map.add cid sheet.Wire.Symbol.Symbols[cid] acc)
        |> Optic.set symbols_
        <| scaledSymSheet 
    
    noSymbolOverlapSheet


    