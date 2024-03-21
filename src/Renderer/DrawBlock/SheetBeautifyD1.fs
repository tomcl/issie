module SheetBeautifyD1
// open modules likely to be used
open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SymbolUpdate
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open BlockHelpers
open SegmentHelpers

/// <summary>
/// Filters parallel wires from the model based on the number of visible segments.
/// </summary>
/// <param name="model">The sheet model containing wires and segments.</param>
/// <returns>List of wires with exactly three visible segments.</returns>
let parallelWires (model: SheetT.Model) =
    Map.values model.Wire.Wires
    |> List.ofSeq
    |> List.filter (fun wire -> List.length (visibleSegments wire.WId model) >= 3)

/// <summary>
/// Gets all single-connected symbols associated with a list of wires in the model.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
/// <param name="wires">List of wires to consider for symbol connections.</param>
/// <returns>List of single-connected symbols.</returns>
let getAllSingleConnectedSymbols (model: SheetT.Model) (wires: Wire list) =
    let sourceTargetSymbols =
        List.map (fun wire -> (getSourceSymbol model.Wire wire, getTargetSymbol model.Wire wire, wire)) wires

    let getSingleConnectedSymbols (symbolList: (Symbol * Symbol * Wire) list) =
        symbolList
        |> List.groupBy (fun sym -> sym)
        |> List.filter (fun (_, symList) -> List.length symList = 1)
        |> List.map (fun (sym, _) -> sym)
        // |> (fun symList ->
        //     match symList with
        //     | [] -> []
        //     | (s1,t1,w1)::(s2,t2,w2)::rest -> (s1,t1,w1, t1=s1)

        // )
    let singleConnected = getSingleConnectedSymbols sourceTargetSymbols
    let sourceSymbols = List.map (fun (s, _, _) -> s.Id) singleConnected
    let targetSymbols = List.map (fun (_, t, _) -> t.Id) singleConnected

    let moveSource sl tl s t=
        (not (List.contains s tl) || List.contains t sl)


    singleConnected
    |> List.map (fun (s, t, w) -> (s, t, w, moveSource sourceSymbols targetSymbols s.Id t.Id))




    // getSingleConnectedSymbols sourceTargetSymbols
    // |> moveSourceSymbol

/// <summary>
/// Gets all symbols associated with parallel wires in the model.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
/// <returns>List of symbols connected by parallel wires.</returns>
let getAllSymbols (model: SheetT.Model) =
    getAllSingleConnectedSymbols model (parallelWires model)

/// <summary>
/// Gets the bounding boxes of symbols associated with parallel wires in the model.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
/// <returns>List of bounding boxes of symbols connected by parallel wires.</returns>
let getSymbolsBoundingBox (model: SheetT.Model) =
    let symbols = getAllSymbols model
    symbols
    |> List.map (fun (symS, symT, _, _) -> getSymBoundingBox symS)

/// <summary>
/// Aligns all singly connected symbols by their target port.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols</param>
/// <returns>Updated model with the moved symbols</returns>
let alignSingleConnectedSyms (model: SheetT.Model) (syms) =

    let checkXCoor (s1: Symbol) (s2: Symbol) (b: bool) =
        // printfn "Distance between %s and %s: %f" s1.Component.Label s2.Component.Label (abs(s1.Pos.X - s2.Pos.X))
        let sub = if b then 1 else -1
        if abs(s1.Pos.X - s2.Pos.X) < 60 then
            (-100*sub)
        else 0
    let delta (wire:Wire) (b:bool) (x: int) =
        let sub = if b then -1. else 1.
        match wire.InitialOrientation with
        | Horizontal -> {X=x; Y=wire.EndPos.Y - (wire.StartPos.Y)}
        | Vertical -> {X=wire.EndPos.X - (wire.StartPos.X); Y=0}

    let symbolMap = Optic.get symbols_ model
    let movedSyms = List.map (fun (s,t,w, b) -> moveSymbol (checkXCoor s t b |> delta w b) (if b then s else t)) syms
    let symbols' =  (symbolMap, movedSyms)
                    ||> List.fold (fun s movedSym ->
                        s |> Map.map (fun _ v ->
                            match v.Id with
                            | id when id = movedSym.Id -> movedSym
                            | _ -> v
                        ))

    let NewSymbolModel = Optic.set symbols_ symbols' model //Update the model with the new symbol positions

    //Getting Wire map to update to new wire positions based on updated Symbol postions
    let wireMap = Optic.get wires_ model
    let movedWires = wireMap |> Map.values |> List.ofSeq
                    |> List.map (fun w ->
                    BusWireUpdateHelpers.autoroute NewSymbolModel.Wire w)

    let wires' =  (wireMap, movedWires)
                    ||> List.fold (fun w movedWire ->
                        w |> Map.map (fun _ v ->
                            match v.WId with
                            | id when id = movedWire.WId -> movedWire
                            | _ -> v
                        ))
    //Updating the model with the new wire positions
    // let intersectingPair = numOfIntersectedSymPairs NewSymbolModel
    // printfn "Number of intersecting pairs: %d" intersectingPair
    Optic.set wires_ wires' NewSymbolModel


// let scaleCustomCompAlign model compId =
//     let sym = Optic.get symbols_ model
//               |> Map.tryFind compId
//               |> Option.defaultValue (failwith "Symbol not found")
//     let wires = BusWireUpdateHelpers.getConnectedWires compId model
//                 |> List.filter (fun w -> numOfVisRightAngles )
//
//
// let scaleCustomCompsAlign (model: SheetT.Model) =
//     let customSyms = Optic.get symbols_ model
//                      |> Map.filter (fun _ s  -> match s.Component.Type with
//                                                 | Custom _ -> true
//                                                 | _ -> false)
//                      |> Map.filter (fun _ s -> s.Component.InputPorts > 1)
//                      |> Map.filter (fun k s -> BusWireUpdateHelpers.getConnectedWires k model |> List.length > 1)
//                      |> Map.toList

/// <summary>
/// Aligns and scales symbols based on the positions and bounding boxes of connected symbols.
/// </summary>
/// <param name="model">The sheet model containing wires and symbols.</param>
let sheetAlignScale (model: SheetT.Model) =
    // let symbols = getAllSymbols model
    let syms = getAllSingleConnectedSymbols model (parallelWires model)
    printfn "Running SheetAlign %A" <| List.map (fun(s,t,w,b)->(s.Component.Label, t.Component.Label, w.WId,b)) syms

    let rec runAlignSingleConnectedSyms model symList count =
        match symList with
        | [] -> model
        | _ ->
            if List.length symList > 0 && count < 10 then //
                let model' = alignSingleConnectedSyms model symList
                let updatedSymList = getAllSingleConnectedSymbols model' (parallelWires model')
                runAlignSingleConnectedSyms model' updatedSymList (count + 1)
            else
                model
    //
    runAlignSingleConnectedSyms model syms 0
