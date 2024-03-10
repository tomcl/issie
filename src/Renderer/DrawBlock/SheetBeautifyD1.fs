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
    
    let removeCount pair = 
        let s, m = fst pair, snd pair
        List.map fst s, List.map fst m

    // TODO: fix this - singly connected can have two output wires as long as only one of them is parallel
    // find singly and multiply connected symbols (only connected to one symbol, connected to multiple symbols)
    let singlyConnected, multiplyConnected = 
        connectedSymbols
        |> List.collect (fun (sym1, sym2) -> [sym1;sym2])
        |> List.countBy id
        |> List.partition (fun (_, count) -> count=1)
        |> removeCount
    
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

    // TODO: Do the same for vertical
    // Case 1:

    // TODO: Check whether we should use inputportid or outputportid
    let getConnectingWire portAndSym = 
        let port = fst portAndSym
        sheet.Wire.Wires
        |> mapValues
        |> Seq.toList
        |> List.filter (
            fun wire -> 
                (getSourcePort sheet.Wire wire).Id=port
        )
        |> List.tryExactlyOne
        |> function
        | None -> None, snd portAndSym
        | Some wire ->Some (getTargetPort sheet.Wire wire).Id, snd portAndSym

    let case1TargetPort = 
        multiplyConnected
        |> List.map (fun sym -> sym.PortMaps.Order[Right], sym)
        |> List.filter (fun portList -> (fst portList).Length = 1)
        |> List.map (fun (singleton, sym) -> singleton[0], sym)
        |> List.map getConnectingWire 
        |> List.filter (fun port -> (fst port).IsSome)
        |> List.map (fun (tPort, sym) -> Option.get tPort, sym)
    
    let case1PortPosns = 
        case1TargetPort
        |> List.map fst 
        |> List.map getPortPos 
        |> List.map (fun curried -> curried sheet.Wire.Symbol)
    
    let case1UpdatedSymbols = 
        case1PortPosns
        |> List.map updateSymPos
        |> List.mapi (fun i curried -> curried (snd case1TargetPort[i]))
    
    let case1UpdatedSheet = 
        (sheet.Wire.Symbol.Symbols, case1UpdatedSymbols)
        ||> List.fold (fun acc sym -> Map.add sym.Id sym acc)
        |> Optic.set symbols_
        <| sheet

    
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
                    | Some _ -> true
            )
            |> List.tryExactlyOne // TODO: Change to first port so that it always aligns from the top
        match wireConnecting with 
        | None -> None, None
        | Some w -> (Some (getSourcePort sheet.Wire w), Some (getTargetPort sheet.Wire w))



    // Find the singly connected symbol and the symbol it is connected to, and 
    // move the singly connected symbol to the x/y coordinate of the port it is connected to
    // Updates the entire sheet
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


    