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
    let checkSinglyConnected sym = 
        let wiresConnectedToSym = 
            sheet.Wire.Wires
            |> (mapValues >> Seq.toList)
            |> List.filter (
                fun wire -> 
                    (Map.tryFind (string wire.InputPort) sym.PortMaps.Orientation, Map.tryFind (string wire.OutputPort) sym.PortMaps.Orientation)
                    |> function
                    | None, None -> false
                    | _, _ -> true
            )
        
        match wiresConnectedToSym.Length with
        | 1 -> true
        | 2 -> (wiresConnectedToSym[0].Segments.Length%2) <> (wiresConnectedToSym[1].Segments.Length % 2)
        | _ -> false 

    let singlyConnected, multiplyConnected = 
        sheet.Wire.Symbol.Symbols
        |> (mapValues >> Seq.toList)
        |> List.partition checkSinglyConnected
    
    // 3. Where possible align multiply connected components to eliminate wire bends in nearly straight wires
    // This is done first as there are more restrictions on how multiply connected components can move, 
    // so adjusting them first allows us to remove some harder wire bends without affecting the easier ones

    // Four cases:
    // Case 1: 1 output, connected to 1 component (ie only multiply connected on input)
    // Move component to the X or Y position of its output component
    // Case 2: 1 output, connected to several components (A, B, C)
    // Move component to the A's top port X or Y position 
    // Will still have unstraightened parallel wires, but fewer
    // Case 3: Several outputs, all connected to 1 component
    // Move to X or Y position of top port of connected component
    // Case 4: Several outputs, connected to several components (A, B, C)
    // Move to X or Y position of top port of one of the components its connected to (A for example)
    // Smart overlap checker for case 2&4 - if it overlaps with some other component move it to top port of B then C
    // Difficult - get other cases working right first

    // TODO: Done for output ports on right side and vertical positioning (chaning Y value)
    //       Generalise to output ports anywhere and horizontal positioning
    
    // 1 port - 0 port seps down
    // 2 port - 0.5 port sep down
    // 3 port - 1 port sep down
    // 4 port - 1.5 port sep down
    // to align top ports, symbol centre needs to be 0.5x-0.5 separations down from corresponding port y position 

    // aligns ports on Right edge only
    let alignTopPorts (targetPortPos: XYPos) (currentSymbol: Symbol) = 
        let numPorts = (float) (getPortOrder Right currentSymbol).Length
        let portSep = 
            currentSymbol.Component.H/ (numPorts*(Option.defaultValue 1. currentSymbol.VScale)+1.)

        updateSymPos {X=currentSymbol.Component.X; Y=targetPortPos.Y+(0.5*numPorts+0.5)*portSep} currentSymbol
        

    let updateAllComponents targetPorts priorSheet=   
        let updatedSymbols = 
            targetPorts
            |> List.map fst 
            |> List.map getPortPos 
            |> List.map (fun curried -> curried sheet.Wire.Symbol) // target port positions
            |> List.map alignTopPorts
            |> List.mapi (fun i curried -> curried <| snd targetPorts[i])

        (priorSheet.Wire.Symbol.Symbols, updatedSymbols)
        ||> List.fold (fun acc sym -> Map.add sym.Id sym acc)
        |> Optic.set symbols_
        <| priorSheet

    let getConnectingPort portAndSym = 
        let port = fst portAndSym
        sheet.Wire.Wires
        |> (mapValues >> Seq.toList)
        |> List.filter (
            fun wire -> 
                (getSourcePort sheet.Wire wire).Id=port
        )
        |> List.tryItem 0
        |> function
        | None -> None, snd portAndSym
        | Some wire ->Some (getTargetPort sheet.Wire wire).Id, snd portAndSym

    // TODO: smart overlap checker
    // TODO: split this up into case 1 and 2 and case 3 and 4 so we can apply the smart overlap checker

    // TODO: 6. Include, where this is worthwhile (heuristic) aligning arrays of components. Note that
    // aligning components will usually mean that connections between the aligned components
    // cannot be straight and vice versa.

    // Returns each case 1 and 2 symbol with it's corresponding target port
    // e.g if mux output -> not input, returns (not input port id, mux symbol)
    let case1TargetPort = 
        multiplyConnected
        |> List.map (fun sym -> sym.PortMaps.Order[Right], sym) // assumes output ports on the right edge
        |> List.filter (fun (portList, _) ->  portList.Length = 1)
        |> List.map (fun (singleton, sym) -> singleton[0], sym)
        |> List.map getConnectingPort 
        |> List.filter (fun port -> (fst port).IsSome)
        |> List.map (fun (tPort, sym) -> Option.get tPort, sym)
    
    
    let case3and4TargetPort = 
        multiplyConnected
        |> List.map (fun sym -> sym.PortMaps.Order[Right], sym) // assumes output ports on the right edge
        |> List.filter (fun (portList, _) -> portList.Length>1)
        |> List.map (fun (portList, sym) -> portList[0], sym) // get top port
        |> List.map getConnectingPort
        |> List.filter (fun (tPort, sym) -> tPort.IsSome)
        |> List.map (fun (tPort, sym) -> Option.get tPort, sym)
    
    // 1. Align all singly-connected components to eliminate wire bends in parallel wires
    // Singly connected components have either 1 or 2 wires on the output - a simpler case than multiply connected
    // so should (theoretically) use the same framework and be ok        
    let singlyConnectedTargetPort = 
        singlyConnected
        |> List.map (fun sym -> sym.PortMaps.Order[Right], sym) // assumes output ports on the right edge
        |> List.map (fun (portList, sym) -> portList[0], sym)
        |> List.map getConnectingPort
        |> List.filter (fun (tPort, sym) -> tPort.IsSome)
        |> List.map (fun (tPort, sym) -> Option.get tPort, sym)
    
    // 4. Scale custom symbols to reduce wire bends in parallel wires between two custom components
    // get custom symbols and the symbols they are connected to
    let customSymbolPairs = 
        sheet.Wire
        |> getConnSyms
        |> List.filter (
            fun (sym1, sym2) -> 
                match sym1.Component.Type, sym2.Component.Type with
                | Custom _, Custom _ -> true
                | _, _ -> false
        )
    
    // find exactly how two symbols known to be connected are connected to each other
    // returns the edge of sym1 that connects to the edge of sym2
    let findSymbolsEdgeConns symPair =
        let sym1, sym2 = symPair
        let edges = 
            sheet.Wire.Wires
            |> (mapValues >> Seq.toList)
            |> List.map (
                fun wire ->
                    let inputInSym1, outputInSym1, inputInSym2, outputInSym2 =  
                        Map.tryFind (string wire.InputPort) sym1.PortMaps.Orientation, 
                        Map.tryFind (string wire.OutputPort) sym1.PortMaps.Orientation, 
                        Map.tryFind (string wire.InputPort) sym2.PortMaps.Orientation, 
                        Map.tryFind (string wire.OutputPort) sym2.PortMaps.Orientation
                    
                    match inputInSym1, outputInSym1, inputInSym2, outputInSym2 with 
                    | Some edg1, None, None, Some edg2 -> Some edg1, Some edg2
                    | None, Some edg1, Some edg2, None -> Some edg1, Some edg2
                    | _, _, _, _ -> None, None // no other case should be considered 
            )
            |> List.filter (
                fun (edg1, edg2) -> 
                    match edg1, edg2 with 
                    | Some _, Some _ -> true
                    | _, _ -> false
            )
            |> List.item 0 // known that sym1 & sym2 are connected so no issue
        
        (Option.get (fst edges), Option.get (snd edges)), (sym1, sym2)
        
    // count number of ports on both symbol edges and their current dimensions to find scale factor
    // set vscale correspondingly
    // TODO: same with hscale
    let scaledSyms = 
        customSymbolPairs
        |> List.map findSymbolsEdgeConns 
        |> List.filter (
            fun (edges, _) -> 
                match (fst edges), (snd edges) with 
                | Right, Left | Left, Right | Top, Bottom | Bottom, Top -> true
                | _ -> false
            )
        |> List.map (fun ((edg1, edg2), (sym1, sym2)) -> 
            match edg1, edg2 with
            | Right, Left | Left, Right -> ((getPortOrder edg1 sym1).Length, (getCustomCompDims sym1).Y), ((getPortOrder edg2 sym2).Length, (getCustomCompDims sym2).Y)
            | Top, Bottom | Bottom, Top -> ((getPortOrder edg1 sym1).Length, (getCustomCompDims sym1).X), ((getPortOrder edg2 sym2).Length, (getCustomCompDims sym2).X)
            | _ -> (0, 0.), (0, 0.) // shouldn't happen
        )
        // convert to scaling factor applied to second component
        |> List.map (fun (sym1info, sym2info) -> ((snd sym1info)/(float) (fst sym1info))/((snd sym2info)/(float) (fst sym2info)) )
        |> List.mapi (fun i scale -> {snd customSymbolPairs[i] with VScale=Some scale} )

    
    // 2/7. Do not overlap components
    // Go through updated sheet and check which symbols overlap
    // if they overlap, move the symbol to its old position in the original sheet
    
    let alignedSheet = 
        sheet
        |> updateAllComponents case1TargetPort
        |> updateAllComponents case3and4TargetPort
        |> updateAllComponents singlyConnectedTargetPort 

    let scaledSymSheet = 
        (alignedSheet.Wire.Symbol.Symbols, scaledSyms)
        ||> List.fold (fun acc sym -> Map.add sym.Id sym acc)
        |> Optic.set symbols_
        <| alignedSheet

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


    