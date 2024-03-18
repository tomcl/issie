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

/// Check if a symbol is singly connected
/// Singly connected: Has only one parallel wire
/// Parallel wire: Wires which have an odd number of segments and start in the same direction 
/// Returns true or false 
let checkSinglyConnected sheet sym  = 
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


/// Aligns top ports of two symbols 
/// Takes in the target port (the one to be aligned to), the symbol to align and the edge along which
/// aligning must be done. Returns updated Symbol
let alignTopPorts (targetPortPos: XYPos) (currentSymbol: Symbol) edge = 
    let numPorts = (float) (getPortOrder edge currentSymbol).Length
    let portSep = 
        match edge with
        | Right | Left -> currentSymbol.Component.H / (numPorts*(Option.defaultValue 1. currentSymbol.VScale)+1.)
        | Top | Bottom -> currentSymbol.Component.W / (numPorts*(Option.defaultValue 1. currentSymbol.HScale)+1.)
    match edge with
    | Right | Left -> updateSymPos {X=currentSymbol.Component.X; Y=targetPortPos.Y+(0.5*numPorts+0.5)*portSep} currentSymbol
    | Top | Bottom -> updateSymPos {X=targetPortPos.X+(0.5*numPorts+0.5)*portSep; Y=currentSymbol.Component.Y} currentSymbol

/// Update symbols in sheet according to a new list of symbols to update
/// Returns updated sheet
let updateSymbols updatedSymbols priorSheet = 
    (priorSheet.Wire.Symbol.Symbols, updatedSymbols)
    ||> List.fold (fun acc sym -> Map.add sym.Id sym acc)
    |> Optic.set symbols_
    <| priorSheet

/// Updates all components by aligning top ports according to list of target and ports and the edges along which aligning must be done
/// Returns updated sheet
let updateAllComponents targetPorts (sourcePorts: Edge list) priorSheet=   
    let updatedSymbols = 
        targetPorts
        |> List.map fst 
        |> List.map getPortPos 
        |> List.map (fun curried -> curried priorSheet.Wire.Symbol) // target port positions
        |> List.map alignTopPorts
        |> List.mapi (fun i curried -> curried <| snd targetPorts[i])
        |> List.mapi (fun i curried -> curried sourcePorts[i])

    updateSymbols updatedSymbols priorSheet

/// Given a sheet and source port-symbol pair, returns the target port (the port the source port is connected to)
/// And the original symbol as a tuple. If not connected, returns None and original symbol
let getConnectingPort (sheet: SheetT.Model) portAndSym = 
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

/// Get all target ports and their source port symbols across a given set of symbols
/// Returns list of tuples of target port and the source port's symbol
let getCaseTargetPorts sheet comparer symbolList= 
    symbolList
    |> List.map (
        // find output edge's ports
        fun sym -> 
            match sym.STransform.Flipped, sym.STransform.Rotation with // assuming flipped means flipped along y axis of symbol
            | false, Degree0 | true, Degree180->  sym.PortMaps.Order[Right], sym
            | false, Degree90 | true, Degree270 -> sym.PortMaps.Order[Top], sym
            | false, Degree180 | true, Degree0 -> sym.PortMaps.Order[Left], sym
            | false, Degree270 | true, Degree90 -> sym.PortMaps.Order[Bottom], sym
    )
    |> List.filter (fun (portList, _) ->  comparer portList)
    |> List.map (fun (portList, sym) -> portList[0], sym)
    |> List.map (getConnectingPort sheet) 
    |> List.filter (fun port -> (fst port).IsSome)
    |> List.map (fun (tPort, sym) -> Option.get tPort, sym)

/// Get the edge that a source port is on
/// If the source port doesn't have an associated target port, it is not included (match with getCaseTargetPorts)
let getCaseSourcePorts sheet comparer symbolList = 
    symbolList
    |> List.map (
        fun sym -> 
            match sym.STransform.Flipped, sym.STransform.Rotation with // assuming flipped means flipped along y axis of symbol
            | false, Degree0 | true, Degree180->  sym.PortMaps.Order[Right], sym
            | false, Degree90 | true, Degree270 -> sym.PortMaps.Order[Top], sym
            | false, Degree180 | true, Degree0 -> sym.PortMaps.Order[Left], sym
            | false, Degree270 | true, Degree90 -> sym.PortMaps.Order[Bottom], sym
    )
    |> List.filter (fun (portList, _) ->  comparer portList)
    |> List.map (fun (portList, sym) -> portList[0], sym)
    |> List.filter (
        fun (sPort, sym) -> 
            let connPort = getConnectingPort sheet (sPort,sym)
            match connPort with 
            | Some _, _ -> true
            | None, _ -> false
    )
    |> List.map (fun (port, sym) -> Map.find port sym.PortMaps.Orientation)

/// Find exactly how two symbols known to be connected are connected to each other
/// returns the edge of sym1 that connects to edge of sym2 and the original symbol tuple
let findSymbolsEdgeConns sheet symPair =
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

/// Get list of symbols that a given port is connected to 
let getConnectedSymbols sheet port = 
    let connectedPort = fst (getConnectingPort sheet port)
    let key = 
        match connectedPort with 
        | None -> ""
        | Some s -> s
    sheet.Wire.Symbol.Symbols
    |> (mapValues >> Seq.toList)
    |> List.filter (
        fun sym -> 
            Map.tryFind key sym.PortMaps.Orientation
            |> function
            | None -> false
            | Some _ -> true
    )

/// Finds the edgemost coordinate in a list of symbols ie finds the rightmost, leftmost, topmost or bottomost symbol
/// Returns float value specifying coordinate of edgemost symbol. If edge was known, can be used to set X or Y coord
/// of relevant symbol
let findEdgemost edge symList = 
    (0., symList)
    ||> List.fold (
        fun furthest sym -> 
            match edge with 
            | Right -> if (sym.Component.X > furthest) then sym.Component.X else furthest
            | Left -> if (sym.Component.X < furthest) then sym.Component.X else furthest
            | Top ->  if (sym.Component.Y > furthest) then sym.Component.Y else furthest // TODO: check logic
            | Bottom -> if (sym.Component.Y < furthest) then sym.Component.Y else furthest
    )

/// Custom component scaling. Positioning of all components.
let sheetAlignScale (sheet: SheetT.Model) = 
    let singlyConnected, multiplyConnected = 
        sheet.Wire.Symbol.Symbols
        |> (mapValues >> Seq.toList)
        |> List.partition (checkSinglyConnected sheet)
    
    // 3. Where possible align multiply connected components to eliminate wire bends in nearly straight wires
    // This is done first as there are more restrictions on how multiply connected components can move, 
    // so adjusting them first allows us to remove some harder wire bends without affecting the easier ones

    // Four cases:
    // Case 1: 1 output, connected to 1 component (ie only multiply connected on input)
    // Move component to the X or Y position of its output port
    // Case 2: 1 output, connected to several components (A, B, C)
    // Move component to the A's top port X or Y position 
    // Will still have unstraightened parallel wires, but fewer
    // Case 3: Several outputs, all connected to 1 component
    // Move to X or Y position of top port of connected component
    // Case 4: Several outputs, connected to several components (A, B, C)
    // This case is complicated and not easy to align correctly, so it is ignored
    // Case 2 used as heuristic to perform ASB 6 - aligning arrays of components

    // Separates into case 1, 2, 3, 4
    let case1Comparer portList = 
        let portConnectedSymbols = 
            portList
            |> List.map (fun port -> port, 1)
            |> List.map (getConnectedSymbols sheet) 
        match portConnectedSymbols.Length with 
        | 1 -> 
            match portConnectedSymbols[0].Length with 
            | 1-> true
            | _ -> false
        | _ -> false
        
    let case2Comparer portList = 
        let portConnectedSymbols = 
            portList
            |> List.map (fun port -> port, 1)
            |> List.map (getConnectedSymbols sheet) 
        match portConnectedSymbols.Length with 
        | 1 -> 
            match portConnectedSymbols[0].Length with 
            | 1-> false
            | _ -> true
        | _ -> false

    let case3Comparer portList = 
        let portConnectedSymbols = 
            portList
            |> List.map (fun port -> port, 1)
            |> List.map (getConnectedSymbols sheet) 
        match portConnectedSymbols.Length with 
        | 1 -> false
        | _ -> 
            match (List.distinct portList).Length with 
            | 1 -> true
            | _ -> false
    
    let case4Comparer portList = 
        let portConnectedSymbols = 
            portList
            |> List.map (fun port -> port, 1)
            |> List.map (getConnectedSymbols sheet) 
        match portConnectedSymbols.Length with 
        | 1 -> false
        | _ -> 
            match (List.distinct portList).Length with 
            | 1 -> false
            | _ -> true
        


    let case1TargetPort = getCaseTargetPorts sheet case1Comparer multiplyConnected
    let case2TargetPort = getCaseTargetPorts sheet case2Comparer multiplyConnected
    let case3TargetPort = getCaseTargetPorts sheet case3Comparer multiplyConnected 
    
    let case1SourcePort = getCaseSourcePorts sheet case1Comparer multiplyConnected
    let case3SourcePort = getCaseSourcePorts sheet case3Comparer multiplyConnected


    // 6. Include, where this is worthwhile (heuristic) aligning arrays of components. Note that
    // aligning components will usually mean that connections between the aligned components
    // cannot be straight and vice versa.

    // Aligning case 2: components with one output port connected to several symbols
    // Align connected symbols first, then align individual-ported symbol to middle
    
    // Get the symbols the case 2 symbols are connected to
    let case2connectedSyms = 
        sheet.Wire
        |> getConnSyms
        |> List.filter (
            fun (sym1, _) -> List.exists (fun (_, sym) -> sym=sym1) case2TargetPort 
        )
    
    let connSymsMap = 
        (Map.empty<Symbol, Symbol list>, case2connectedSyms)
        ||> List.fold (
            fun currentMap (sym1, sym2) -> 
                Map.tryFind sym1 currentMap
                |> function
                | Some sList -> Map.add sym1 (List.append sList [sym2]) currentMap
                | None -> Map.add sym1 [sym2] currentMap 
        )
        |> Map.toList
    
    let symOutputEdges = 
        connSymsMap
        |> List.map fst
        |> List.map (
            fun sym -> 
                match sym.STransform.Flipped, sym.STransform.Rotation with // assuming flipped means flipped along y axis of symbol
                | false, Degree0 | true, Degree180->  Right
                | false, Degree90 | true, Degree270 -> Top
                | false, Degree180 | true, Degree0 -> Left
                | false, Degree270 | true, Degree90 -> Bottom
        )
    
    let newCoords = 
        connSymsMap
        |> List.mapi (fun i (_, connSyms) -> findEdgemost symOutputEdges[i] connSyms)
        
    let updatedSyms = 
        connSymsMap
        |> List.map (fun (_, symList) -> symList)
        |> List.mapi (
            fun i symList -> 
                List.map (
                    fun sym -> 
                        match symOutputEdges[i] with 
                        | Left | Right -> Optic.set posOfSym_ {X=newCoords[i]; Y=sym.Component.Y} sym
                        | Top | Bottom -> Optic.set posOfSym_ {X=sym.Component.X; Y=newCoords[i]} sym 
                ) symList
        )
        |> List.collect id

    let findAvPos edge symList = 
        (0.,symList)
        ||> List.fold (
            fun sum sym -> 
                match edge with 
                | Left | Right -> sum+sym.Component.Y
                | Top | Bottom -> sum+sym.Component.X
        )
        |> (/)
        <| (float) symList.Length


    let case2UpdatedSymbols = 
        connSymsMap 
        |> List.mapi (
            fun i (sym, symList) ->
                (findAvPos symOutputEdges[i] symList), sym
        )
        |> List.mapi ( 
            fun i (newPos, sym) ->
                match symOutputEdges[i] with 
                | Left | Right -> Optic.set posOfSym_ {X=sym.Component.X; Y=newPos} sym
                | Top | Bottom -> Optic.set posOfSym_ {X=newPos; Y=sym.Component.Y} sym
        )
        |> List.append updatedSyms


    // 1. Align all singly-connected components to eliminate wire bends in parallel wires
    // Singly connected components have either 1 or 2 wires on the output - a simpler case than multiply connected
    // so should (theoretically) use the same framework and be ok
    let alwaysTrue x = true

    let singlyConnectedTargetPort = getCaseTargetPorts sheet alwaysTrue singlyConnected
    let singlyConnectedSourcePort = getCaseSourcePorts sheet alwaysTrue singlyConnected
    
    
    // 4. Scale custom symbols to reduce wire bends in parallel wires between two custom components
    // get custom symbols and the custom symbols they are connected to
    let customSymbolPairs = 
        sheet.Wire
        |> getConnSyms
        |> List.filter (
            fun (sym1, sym2) -> 
                match sym1.Component.Type, sym2.Component.Type with
                | Custom _, Custom _ -> true
                | _, _ -> false
        )
    
    // count number of ports on both symbol edges and their current dimensions to find scale factor
    // set hscale/vscale correspondingly
    let scaledSyms = 
        customSymbolPairs
        |> List.map (findSymbolsEdgeConns sheet) 
        |> List.filter (
            fun (edges, _) -> 
                match (fst edges), (snd edges) with 
                | Right, Left | Left, Right | Top, Bottom | Bottom, Top -> true
                | _ -> false
            )
        |> List.map (fun ((edg1, edg2), (sym1, sym2)) -> 
            match edg1, edg2 with
            | Right, Left | Left, Right -> ((getPortOrder edg1 sym1).Length, (getCustomCompDims sym1).Y), ((getPortOrder edg2 sym2).Length, (getCustomCompDims sym2).Y),true
            | Top, Bottom | Bottom, Top -> ((getPortOrder edg1 sym1).Length, (getCustomCompDims sym1).X), ((getPortOrder edg2 sym2).Length, (getCustomCompDims sym2).X), false
            | _ -> (0, 0.), (0, 0.), false // shouldn't happen
        )
        // convert to scaling factor applied to second component
        |> List.map (fun (sym1info, sym2info, horv) -> ((snd sym1info)/(float) (fst sym1info))/((snd sym2info)/(float) (fst sym2info)), horv )
        |> List.mapi (fun i (scale, horv) -> 
            match horv with 
            | true -> {snd customSymbolPairs[i] with VScale=Some scale} 
            | false -> {snd customSymbolPairs[i] with HScale=Some scale} 
        )
    
    // 2/7. Do not overlap components
    // Go through updated sheet and check which symbols overlap
    // if they overlap, move the symbol to its old position in the original sheet
    
    let scaledSymSheet = 
        sheet
        |> updateAllComponents case1TargetPort case1SourcePort
        |> updateSymbols case2UpdatedSymbols
        |> updateAllComponents case3TargetPort case3SourcePort
        |> updateAllComponents singlyConnectedTargetPort singlyConnectedSourcePort
        |> updateSymbols scaledSyms

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
    