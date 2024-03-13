module SheetBeautifyD2

//-----------------------------------------------------------------------------------------//
//----------Module for top-level beautify functions making (mostly) whole-sheet changes----//
//-----------------------------------------------------------------------------------------//

(*
Whole sheet functions are normally applied to whole sheet. In many cases a feature could be to
apply them to currently selected wires or components. That provides users control over what gets beautified.
Ideal beautify code will never make things worse so can be applied to whole sheet always.

Otehr relevant modules:
SheetBeautifyHelpers (helpers)
Generatedata (used to make randomized tests)
TestDrawBlock (used to test the code written here).

*)

// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open RotateScale
open Optics

/// constants used by SheetBeautify
module Constants =
    () // dummy to make skeleton type check - remove when other content exists


module D2Helpers = 
    let rotationPermute ( symbol : SymbolT.Symbol ) =
        let rotations = [Degree0; Degree90; Degree180; Degree270]

        rotations
        |> List.map (fun r -> rotateSymbolByDegree r symbol)

    let flipPermute flip sym =
        let pos = {X = sym.Component.X + sym.Component.W / 2.0 ; Y = sym.Component.Y + sym.Component.H / 2.0 }
        [sym ; flipSymbolInBlock flip pos sym]

    let permutateList lst=
        let removeFromL rmVal = 
            List.filter (fun v' -> v' <> rmVal)

        let rec rPermutate (inL : 'a list) (outL: 'a list) =
            match inL with
            | [] -> [outL]
            // | head::tail when tail = [] -> List.append outL [head]
            | l -> List.collect (fun v -> rPermutate (removeFromL v l) (List.append outL [v])) l

        rPermutate lst []

    let muxInputPortsPermute sym = 
        let inputPortsEdge = 
            match Optic.get symbol_rotation_ sym with
            | Degree0 -> Edge.Left
            | Degree90 -> Edge.Bottom
            | Degree180 -> Edge.Right
            | Degree270 -> Edge.Top

        getPortOrder inputPortsEdge sym
        |> permutateList
        |> List.map (fun newOrder -> 
            putPortOrder inputPortsEdge newOrder sym)


    // stolen from findWireSymbolIntersections in BusWireRoute
    let componentIsMux (comp:Component) =
        match comp.Type with
        | Mux2 | Mux4 | Mux8 -> true
        | _ -> false

    let permutateMux ( symbol : SymbolT.Symbol ) = 
        // obtain all 4 rotations of the symbol (rotatesymbolbyDegree)
        // for each rotation perform port-reordering
        // for each port-reorder flip the select port
        symbol
        |> flipPermute FlipVertical
        |> List.collect rotationPermute
        |> List.collect muxInputPortsPermute

    /// combine a list of symbol permutations into a list of all possible symbol permutations with each other
    let combinePermutations ( allPerms ) = 
        ([], allPerms)
        ||> Map.fold (fun (combinedPerms) cid perms -> 
            match combinedPerms with
            | [] -> perms |> List.map (fun sym -> Map.empty |> Map.add cid sym)
            | c -> 
                List.allPairs c perms
                |> List.map (fun (oldmap, newsym) -> Map.add cid newsym oldmap))

    let updateSymbolsInSheet sheet newSyms = 
        let oldSymbols = Optic.get symbols_ sheet

        let newSymbols = Map.fold (fun old cid sym -> Map.add cid sym old) oldSymbols newSyms

        Optic.set symbols_ newSymbols sheet

    let evaluateFlip ( sheet : SheetT.Model ) ( newSyms : Map<ComponentId,Symbol> ) = 
        updateSymbolsInSheet sheet newSyms
        |> numOfWireRightAngleCrossings

open D2Helpers

let sheetOrderFlip ( sheet : SheetT.Model ) = 
    // Basic Mux Implementation:
    // Obtain all of the muxes in the sheet
    // for each one obtain all permutations of the mux 
    // combine all permutations to obtain all possible permutation of all muxes
    // minimise over the number of wire crossings
    
    sheet.Wire.Symbol.Symbols
    |> Map.filter (fun _cid sym -> componentIsMux <| Optic.get component_ sym)
    |> Map.map (fun _cid sym -> permutateMux sym)
    |> combinePermutations
    |> List.minBy (evaluateFlip sheet)
    |> updateSymbolsInSheet sheet    
