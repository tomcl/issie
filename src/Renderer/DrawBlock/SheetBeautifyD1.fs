module SheetBeautifyD1


// open modules likely to be used
open BlockHelpers
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open Helpers
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics

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
    | 1 -> 
        match wiresConnectedToSym[0].Segments.Length with 
        | n when n%2=0 -> false // even number of segments is not singly connected
        | _ -> true
    | 2 -> (wiresConnectedToSym[0].Segments.Length%2) <> (wiresConnectedToSym[1].Segments.Length % 2)
    | _ -> false

/// Gets the separation between ports for a symbol on a given edge
/// Returns the separation as a float.
let getPortSep edge sym sheet = 
        let portOrder = getPortOrder edge sym
        let length = 
            match edge with
            | Left | Right ->   fst (Symbol.getRotatedHAndW sym)
            | Top | Bottom -> snd (Symbol.getRotatedHAndW sym)
        match portOrder.Length with 
        | 0 | 1 -> length/2.
        | _ -> 
            let portPos1 = getPortPos portOrder[0] sheet.Wire.Symbol 
            let portPos2 = getPortPos portOrder[1] sheet.Wire.Symbol
            match edge with 
            | Left | Right -> (portPos1-portPos2).Y
            | Top | Bottom -> (portPos1-portPos2).X

/// Aligns top ports of two symbols 
/// Takes in the target port (the one to be aligned to), the symbol to align and edge along which
/// aligning must be done, and the current sheet. Returns updated Symbol
let alignTopPorts (targetPortPos: XYPos) (currentSymbol: Symbol) edge sheet = 
    let numPorts = (float) (getPortOrder edge currentSymbol).Length
    let portSep = getPortSep edge currentSymbol sheet
    // distance between ports is different to distance between port and corner
    // this finds distance between port and corner
    let portToCorner = 
        (match edge with 
        | Left | Right -> fst (Symbol.getRotatedHAndW currentSymbol) 
        | Top | Bottom -> snd (Symbol.getRotatedHAndW currentSymbol)
        |> (-)
        <| (numPorts-1.)*portSep)
        |> (/)
        <| 2.

    match edge with
    | Right | Left -> updateSymPos {X=currentSymbol.Component.X; Y=targetPortPos.Y-portToCorner} currentSymbol
    | Top | Bottom -> updateSymPos {X=targetPortPos.X-portToCorner; Y=currentSymbol.Pos.Y} currentSymbol

/// Update symbols in sheet according to a new list of symbols to update
/// Returns updated sheet
let updateSymbols updatedSymbols priorSheet = 
    (priorSheet.Wire.Symbol.Symbols, updatedSymbols)
    ||> List.fold (fun acc sym -> Map.add sym.Id sym acc)
    |> Optic.set symbols_
    <| priorSheet

/// Updates all components by aligning top ports of the source and target symbols
/// Takes in a list of target port and source symbol pairs, a list of edges that the source port is on, and the prior sheet
/// Returns updated sheet
let updateAllComponents targetPorts (sourceEdges: Edge list) priorSheet=   
    let updatedSymbols = 
        match sourceEdges.Length with 
        | 0 -> []
        | _ -> 
            targetPorts
            |> List.map fst 
            |> List.map getPortPos 
            |> List.map (fun curried -> curried priorSheet.Wire.Symbol) // target port positions
            |> List.map alignTopPorts
            |> List.mapi (fun i curried -> curried (snd targetPorts[i]) sourceEdges[i] priorSheet)

    updateSymbols updatedSymbols priorSheet

/// Given a sheet and source port, returns the target port (the port the source port is connected to)
/// If not connected, returns None. Only returns the first target port it sees
let getConnectingPort (sheet: SheetT.Model) port = 
    sheet.Wire.Wires
    |> (mapValues >> Seq.toList)
    |> List.filter (
        fun wire -> 
            (string wire.OutputPort)=port || (string wire.InputPort)=port
    )
    |> List.tryItem 0
    |> function
    | None -> None
    | Some wire ->
        match (string wire.OutputPort)=port with 
        | true -> Some (string wire.InputPort)
        | false -> Some (string wire.OutputPort)
    

/// Gets the output ports of the symbol. If the output port's target port is on the opposite side of the output port
/// (e.g. output (source) port on the right side of symbol and target port to the left of the symbol)
/// It instead gets the input ports. This is done so aligning is done more naturally.
/// Returns a list of ports and the original symbol as a tuple
let getCorrectSide sheet sym = 
    let potentialPort = 
        // This is logic for clockwise rotation, may not be correct but it worked with our test cases??
        match sym.STransform.Flipped, sym.STransform.Rotation with // assuming flipped means flipped along y axis of symbol
            | false, Degree0 | true, Degree180->  List.tryItem 0 sym.PortMaps.Order[Right], sym
            | false, Degree90 | true, Degree270 -> List.tryItem 0 sym.PortMaps.Order[Top], sym
            | false, Degree180 | true, Degree0 -> List.tryItem 0 sym.PortMaps.Order[Left], sym
            | false, Degree270 | true, Degree90 -> List.tryItem 0 sym.PortMaps.Order[Bottom], sym
    
    let connectingPort = 
        match fst potentialPort with 
        | None -> None, sym
        | Some port -> (getConnectingPort sheet port), sym
    
    
    let connectingPortPos = 
        match connectingPort with 
        | Some port, _ -> getPortPos  port sheet.Wire.Symbol
        | None, _ -> sym.Pos
    
    match sym.STransform.Flipped, sym.STransform.Rotation with // assuming flipped means flipped along y axis of symbol
        | false, Degree0 | true, Degree180 when connectingPortPos.X > sym.Pos.X ->  sym.PortMaps.Order[Right], sym
        | false, Degree0 | true, Degree180 when connectingPortPos.X < sym.Pos.X ->  sym.PortMaps.Order[Left], sym
        | false, Degree90 | true, Degree270 when connectingPortPos.Y > sym.Pos.Y -> sym.PortMaps.Order[Bottom], sym
        | false, Degree90 | true, Degree270 when connectingPortPos.Y < sym.Pos.Y -> sym.PortMaps.Order[Top], sym
        | false, Degree180 | true, Degree0 when connectingPortPos.X < sym.Pos.X -> sym.PortMaps.Order[Left], sym
        | false, Degree180 | true, Degree0 when connectingPortPos.X > sym.Pos.X -> sym.PortMaps.Order[Right], sym
        | false, Degree270 | true, Degree90 when connectingPortPos.Y < sym.Pos.Y -> sym.PortMaps.Order[Top], sym
        | false, Degree270 | true, Degree90 when connectingPortPos.Y > sym.Pos.Y -> sym.PortMaps.Order[Bottom], sym   
        | _ -> sym.PortMaps.Order[Right], sym
    

/// Get all target ports and their source port symbols across a given set of symbols
/// Returns list of tuples of target port and the source port's symbol
let getCaseTargetPorts sheet comparer symbolList= 
    symbolList
    |> List.map (fun sym -> getCorrectSide sheet sym)
    |> List.filter (fun (portList, _) -> comparer portList)
    |> List.map (
        fun (portList, sym: Symbol) -> 
            let potential = List.tryLast portList
            match potential with 
            | Some s -> s, sym
            | None -> "", sym
    )
    |> List.map (fun (sPort, sym) -> (getConnectingPort sheet sPort), sym) 
    |> List.filter (fun tPort -> (fst tPort).IsSome)
    |> List.map (fun (tPort, sym) -> Option.get tPort, sym)

/// Given a sheet, a comparison function and a list of symbols, return a list of edges that the correct side source ports of each symbol is on 
/// If the source port doesn't have an associated target port, it is filtered out of the result to match getCaseTargetPorts
/// The comparison function is arbitrary and can filter out symbols we don't want based on their source ports. This is used to create the 
/// multiply connected "cases".
let getCaseSourceEdges sheet comparer symbolList = 
    symbolList
    |> List.map (fun sym -> getCorrectSide sheet sym)
    |> List.filter (fun (portList, _) -> comparer portList)
    |> List.map (fun (portList, sym) -> List.tryItem 0 portList, sym)
    |> List.filter (
        fun (sPort, sym) ->
            let connPort = 
                match sPort with 
                | Some sPort -> (getConnectingPort sheet sPort), sym
                | None -> None, sym 
            match connPort with 
            | Some _, _-> true
            | None, _ -> false
    )
    |> List.map (
        fun (port, sym) -> 
        match port with 
        | Some port -> Map.find port sym.PortMaps.Orientation
        | None -> Right // defaults to right edge, but should be a better way to do it
    )

/// Find exactly how two symbols known to be connected are connected to each other
/// Takes in a sheet and a pair of symbols which are known to be connected (getConnSyms)
/// Returns the edge of sym1 that connects to edge of sym2 and the original symbol tuple
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

/// Gets ALL the ports a port is connected to. The wires connecting the ports must all have the same direction
/// ie the number of segments %2 must be the same.
/// Takes in a sheet and a source port and returns a list of ports the source port is connected to.
let getAllConnectingPorts sheet port =
    let allConnectedWires = 
        sheet.Wire.Wires
        |> (mapValues >> Seq.toList)
        |> List.filter (
            fun wire -> 
                (string wire.OutputPort)=port || (string wire.InputPort)=port
        )
    // assert that all wires have the same # of segments %2
    let wireAssertion = 
        let initialState = 
            match List.tryItem 0 allConnectedWires with 
            | Some wire -> Some (wire.Segments.Length%2) 
            | None -> None
        
        (true, allConnectedWires)
        ||> List.fold (
            fun cs wire -> 
                match initialState with 
                | Some dir -> (wire.Segments.Length%2=dir) && cs
                | None -> false
        )
    if (wireAssertion) then 
        allConnectedWires
        |> List.map (
            fun wire ->
                match (string wire.OutputPort)=port with 
                | true -> (string wire.InputPort)
                | false -> (string wire.OutputPort) 
        )
    else 
        []
    

/// Get list of symbols that a given port is connected to. 
/// Takes in a sheet, a flag that checks if it is case 2 or not, and a source port.
/// The differentiation between case 2 and case 1/3 is that we know beforehand which case it is. 
/// Case 2 requires the wires to have the same number of segments, but case 1 and 3 do not. Therefore, 
/// the connecting port function is different for them. Annoying to do it like this, but can't think
/// of a way to refactor that doesn't break.
let getConnectedSymbols sheet isCase2 port = 
    let connectedPorts = 
        match isCase2 with
        | true -> 
            getAllConnectingPorts sheet port
        | false -> 
            getConnectingPort sheet port
            |> function
            | None -> [""]
            | Some str -> [str]
            
            
    sheet.Wire.Symbol.Symbols
    |> (mapValues >> Seq.toList)
    |> List.filter (
        fun sym -> 
            List.map (
                fun key -> 
                    Map.tryFind key sym.PortMaps.Orientation
            ) connectedPorts
            |> List.fold (
                fun cs ci -> 
                    match ci with 
                    | Some _ -> true
                    | None -> cs || false
            ) false  
    )

/// Finds the edgemost coordinate in a list of symbols ie finds the rightmost, leftmost, topmost or bottomost symbol
/// Takes in the "edge" of edgemost, and a list of symbols for which we are trying to find the edgemost symbol's coordinate
/// Returns float value specifying coordinate of edgemost symbol.
let findEdgemost edge symList = 
    (0., symList)
    ||> List.fold (
        fun furthest sym -> 
            match edge with 
            | Right -> if (sym.Component.X > furthest) then sym.Component.X else furthest
            | Left -> if (sym.Component.X < furthest) then sym.Component.X else furthest
            | Top ->  if (sym.Component.Y < furthest) then sym.Component.Y else furthest 
            | Bottom -> if (sym.Component.Y > furthest) then sym.Component.Y else furthest
    )

// Returns component ids of overlapping components
// takes in a list of component ids and their relevant bounding boxes
let getOverlappingComponents boundingBoxes = 
    boundingBoxes
    |> List.allPairs boundingBoxes
    |> List.filter (fun (bb1, bb2) -> bb1<>bb2)
    |> List.filter (fun (bb1, bb2) -> overlap2DBox (snd bb1) (snd bb2))
    |> List.collect (fun tup -> [fst tup; snd tup])
    |> List.map fst
    |> List.distinct

// reverts any overlapping components to an older sheet
// takes in the new sheet, the old sheet and the cids of overlapping components
let revert newSheet oldSheet overlappingCids = 
    (newSheet.Wire.Symbol.Symbols, overlappingCids)
    ||> List.fold (fun acc cid -> Map.add cid oldSheet.Wire.Symbol.Symbols[cid] acc)
    |> Optic.set symbols_
    <| newSheet
    |> updateBoundingBoxes

/// Custom component scaling. Positioning of all components.
let sheetAlignScale (sheet: SheetT.Model) = 
    let singlyConnected, multiplyConnected = 
        sheet.Wire.Symbol.Symbols
        |> (mapValues >> Seq.toList)
        |> List.partition (checkSinglyConnected sheet)
    
    // Where possible align multiply connected components to eliminate wire bends in nearly straight wires
    // This is done first as there are more restrictions on how multiply connected components can move, 
    // so adjusting them first allows us to remove some harder wire bends without affecting the easier ones

    // Four cases:
    // Case 1: 1 output, connected to 1 component (ie only multiply connected on input)
    // Move component to the X or Y position of its output port
    // Case 2: 1 output, connected to several components (A, B, C)
    // Move component to the A's top port X or Y position if all the wires connecting them have the same number of segments %2
    // Will still have unstraightened parallel wires, but fewer
    // Case 3: Several outputs, all connected to 1 component
    // Move to X or Y position of top port of connected component
    // Case 4: Several outputs, connected to several components (A, B, C)
    // This case is complicated and not easy to align correctly, so it is ignored
    // Case 2 used as heuristic to perform ASB 6 - aligning arrays of components

    // Separates into case 1, 2, 3
    let case1Comparer portList = 
        let portConnectedSymbols = 
            portList
            |> List.map (getConnectedSymbols sheet false) 
        match portConnectedSymbols.Length with 
        | 1 -> 
            match portConnectedSymbols[0].Length with 
            | 1-> true
            | _ -> false
        | _ -> false
        
    let case2Comparer portList = 
        let portConnectedSymbols = 
            portList
            |> List.map (getConnectedSymbols sheet true) 
        match portConnectedSymbols.Length with 
        | 1 -> 
            match portConnectedSymbols[0].Length with 
            | 0 | 1-> false
            | _ -> true
        | _ -> false

    let case3Comparer portList = 
        let portConnectedSymbols = 
            portList
            |> List.map (getConnectedSymbols sheet false) 
        match portConnectedSymbols.Length with 
        | 1 -> false
        | _ -> 
            match (List.distinct (List.collect id portConnectedSymbols)).Length with 
            | 1 -> true
            | _ -> false
                
    let case1TargetPort = getCaseTargetPorts sheet case1Comparer multiplyConnected
    let case2TargetPort = getCaseTargetPorts sheet case2Comparer multiplyConnected
    let case3TargetPort = getCaseTargetPorts sheet case3Comparer multiplyConnected 
    
    let case1SourcePort = getCaseSourceEdges sheet case1Comparer multiplyConnected
    let case3SourcePort = getCaseSourceEdges sheet case3Comparer multiplyConnected


    // Heuristic aligning arrays of components. Heuristic used is case 2 of above.

    // Aligning case 2: components with one output port connected to several symbols
    // Align connected symbols first, then align individual-ported symbol to middle X/Y coordinate
    // of aligned symbols. Has to be done separately from case 1 and 3 as we move symbols other than
    // the case 2 symbol itself.
    
    // Get the symbols the case 2 symbols are connected to
    let case2connectedSyms = 
        sheet.Wire
        |> getConnSyms
        |> List.filter (
            fun (sym1, _) -> List.exists (fun (_, sym) -> sym=sym1) case2TargetPort 
        )
    
    // Get a list of the case2 symbols and all the symbols they are connected to as a tuple
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
    
    // get the output edges of the case 2 symbols.
    let symOutputEdges = 
        connSymsMap
        |> List.map fst
        |> List.map (
            fun sym -> 
                match sym.STransform.Flipped, sym.STransform.Rotation with
                | false, Degree0 | true, Degree180->  Right
                | false, Degree90 | true, Degree270 -> Top
                | false, Degree180 | true, Degree0 -> Left
                | false, Degree270 | true, Degree90 -> Bottom
        )
    
    // get the new coordinate to move each case 2 connected symbol to
    let newCoords = 
        connSymsMap
        |> List.mapi (fun i (_, connSyms) -> findEdgemost symOutputEdges[i] connSyms)
    
    // update the connected symbols to their new positions
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

    // small helper to find the average position of a list of symbols
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

    // update the original case 2 symbol to the average position of its connected symbols
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


    // Align all singly-connected components to eliminate wire bends in parallel wires
    // Singly connected components have either 1 or 2 wires on the output - a simpler case than multiply connected
    // so can use the same framework
    let alwaysTrue x = true

    let singlyConnectedTargetPort = getCaseTargetPorts sheet alwaysTrue singlyConnected
    let singlyConnectedSourcePort = getCaseSourceEdges sheet alwaysTrue singlyConnected

    // Scale custom symbols to reduce wire bends in parallel wires between two custom components
    
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
    
    // Scale the target custom symbols such that the port separations for both the target and the source are identical 
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
                    let sym1sep = getPortSep edg1 sym1 sheet
                    let sym2sep = getPortSep edg2 sym2 sheet
                    let scale = -sym1sep/sym2sep // no clue why this is negative
                    match edg1, edg2 with 
                    | Left, Right | Right, Left -> scale, true
                    | Top, Bottom | Bottom, Top -> scale, false
                    | _ -> 1., true // shouldn't happen
            )
            |> List.mapi (fun i (scale, horv) -> 
                match horv with 
                | true ->
                    match (snd customSymbolPairs[i]).STransform.Rotation with 
                    | Degree0 | Degree180 -> {snd customSymbolPairs[i] with VScale=Some (scale*Option.defaultValue 1. (snd customSymbolPairs[i]).VScale)}
                    | Degree90 | Degree270 -> {snd customSymbolPairs[i] with HScale=Some (scale*Option.defaultValue 1. (snd customSymbolPairs[i]).VScale)}
                | false -> 
                    match (snd customSymbolPairs[i]).STransform.Rotation with 
                    | Degree0 | Degree180 -> {snd customSymbolPairs[i] with HScale=Some (scale*Option.defaultValue 1. (snd customSymbolPairs[i]).HScale);} 
                    | Degree90 | Degree270 -> {snd customSymbolPairs[i] with VScale=Some (scale*Option.defaultValue 1. (snd customSymbolPairs[i]).HScale)} 
            )
    
    // count number of ports on both symbol edges and their current dimensions to find scale factor
    // set hscale/vscale correspondingly
    
    
    // Do not overlap components
    // Go through updated sheet and check which symbols overlap
    // if they overlap, revert to an earlier version that doesn't overlap
    // continue until we return at the original sheet
    let scaledSymsSheet = updateSymbols scaledSyms sheet |> updateBoundingBoxes
    let case1Sheet = updateAllComponents case1TargetPort case1SourcePort scaledSymsSheet |> updateBoundingBoxes
    let case2Sheet = updateSymbols case2UpdatedSymbols case1Sheet |> updateBoundingBoxes
    let case3Sheet = updateAllComponents case3TargetPort case3SourcePort case2Sheet |> updateBoundingBoxes
    let singlyConnectedSheet = updateAllComponents singlyConnectedTargetPort singlyConnectedSourcePort case3Sheet |> updateBoundingBoxes

    // backwards to work better with fold
    let revertList = [case3Sheet; case2Sheet; case1Sheet; scaledSymsSheet]    
    (singlyConnectedSheet, revertList)
    ||> List.fold (
        fun currentSheet priorSheet ->
            let overlappingComps = getOverlappingComponents (Map.toList currentSheet.BoundingBoxes)
            revert currentSheet priorSheet overlappingComps
    )
    |> updateAllComponents singlyConnectedTargetPort singlyConnectedSourcePort // realign singly connected components in case of reshift
    |> updateBoundingBoxes