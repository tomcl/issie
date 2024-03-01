module SheetBeautifyD2

(*
    Contribution Statement - Samuel Wang - sw2521

    Assigned to D2 sheetOrderFlip implementation.

    Helpers -

    1.  Permutation algorithm completed.
        Helpers to count and find all orientations and permutations of ports were written. Counting functions are useful
        as we can identify the components which will take too much time to try all permutations, and either skip them or
        use the clockface algorithm on them.

    2.  Clockface algorithm completed, integration pending.
        Clockface algorithm to obtain the optimal port order of a component has been implemented. It has not been
        integrated into the beautify function as obtaining the correct rendered port order proved to be difficult. Once
        the rendered port order can be obtained, the clockface algorithm will be very useful in reducing number of
        permutations to try.

    Beautify Function -

    1.  Stater objective completed.
        Beautify function is able to find all muxes, demuxes, and gates to perform reordering. Symbols with more wires
        connected to it are operated on first, as this is likely to reduce dependancy for future operations. Muxes and
        demuxes are first operated on, and then Gates. Clockface algorithm was yet to be implemented.

    Tests -

    1.  Two tests were written to test the basic sheetFlipOrder function.
        These are are "makeOFTestCircuitDemo1" and "makeOFTestCircuitDemo2" in TestDrawBlock.fs. A Gen object of type
        bool was passed into the tests, with the first input being false and the second being true. Inputs specifies
        whether sheetOrderFlip was run. The tests also fails on all cases, which meant we can see the effect of
        sheetOrderFlip going to the next test. The circuit being drawn are as the following:
        a.  Demo Circuit #1 (test5):
            Given circuit in Figure B1.
        b.  Demo Circuit #2 (test6):
            A circuit which includes 2x Muxes, 1x Demux, 2x Gates. Used to demonstrate re-ordering of port across
            multiple components.

    (Content: Ln , Col 120)
*)

open System
open Optics
open CommonTypes
open DrawModelType
open BlockHelpers
open DrawHelpers
open SheetUpdateHelpers
open SheetBeautifyHelpers


module Constants =
    /// <summary>Flip operations to try.</summary>
    let flipOps = [ SymbolT.FlipHorizontal ]

    /// <summary>Rotation operations to try.</summary>
    let rotationOps = [ Degree0; Degree90; Degree180; Degree270 ]

    /// <summary>All edges on a symbol. Listed from 12 o'clock in clockwise order.</summary>
    let edges = [ Top; Right; Bottom; Left ]

    /// <summary>Limit of port permutations to try.
    /// <c>sheetOrderFlip</c> does not check components with permutation count over limit.</summary>
    let portPermLimit = 25


(* ---------------------------------------------------------------------------------------------- *)
(*                                      Permuataion Algorithm                                     *)
(* ---------------------------------------------------------------------------------------------- *)

(* ---------------------------------------- Math Helpers ---------------------------------------- *)

/// <summary>Factorial of input number.</summary>
/// <param name="n">Input number.</param>
/// <returns>n! - but returns 1 if less than 1 is given.</returns>
let factorial (n: int): int =
    let rec factorial' (n: int): int =
        if n <= 1
        then 1
        else n * factorial' n-1

    factorial' n

/// <summary>Permute a list of items of the same type</summary>
/// <param name="list">List of items to operate on.</param>
/// <returns>List of list of permutated items.</returns>
let permute<'T> (list: List<'T>): List<List<'T>> =
    /// <summary>Helper to insert element into all possible index of a list.
    /// Returns a list of original list with element inserted at different locations.</summary>
    let distrubute (elem: 'T) (list: List<'T>): List<List<'T>> =
        [0..(List.length list)-1]
        |> List.map (fun i -> List.splitAt i list)
        |> List.map (fun (front, back) -> front @ [ elem ] @ back)
        |> List.append [ (List.append list [ elem ]) ]

    let rec permute' (list) (result) =
        match list with
        | [] -> result
        | head :: tail -> permute' tail result |> List.map (distrubute head) |> List.collect id

    match list with
    | [] -> [ [] ]
    | _ -> permute' list [[]]

/// <summary>Returns a new list that contains all triplets of the 3 lists.</summary>
/// <param name="list1">The first input list.</param>
/// <param name="list2">The second input list.</param>
/// <param name="list3">The third input list.</param>
/// <returns>List of triplets.</returns>
let allPairs3<'T1, 'T2, 'T3>
        (list1: List<'T1>)
        (list2: List<'T2>)
        (list3: List<'T3>)
            : List<'T1*'T2*'T3> =
    List.allPairs list2 list3
    |> List.allPairs list1
    |> List.map (fun (item1, (item2, item3)) -> (item1, item2, item3))

/// <summary>Return a new list that contains all possible selection from given list of lists. Functionality like <c>allPairs</c>.
/// For example, given <c>[ [ 1; 2 ]; [ 3; 4 ] ]</c>, will return <c>[ [ 1; 3 ]; [ 1; 4 ]; [ 2; 3 ]; [ 2; 4 ] ]</c>.</summary>
/// <param name="list">List of list of items to select.</param>
/// <returns>List of list of selection, wrapped in option.
/// Options are return to avoid the case where an empty list is provided.</returns>
/// <typeparam name="'T">Type of items to select.</typeparam>
let allPairsNSameType<'T> (list: List<List<'T>>): List<List<Option<'T>>> =
    list
    |> List.map (fun sublist -> List.map (fun item -> Some item) sublist) // wrap items in list with options
    |> List.map (fun sublist -> if List.length sublist = 0 then [ None ] else sublist) // add none to empty lists
    |> List.fold
        (fun result sublist ->
            match result with
            | [] ->
                [ sublist ]
            | _ ->
                List.allPairs result sublist
                |> List.map (fun (item, elem) -> item @ [ elem ]))
        [] // use state as a tracker of results of sublists "chosen"


(* --------------------------------------- Implementation --------------------------------------- *)

/// <summary>Get count of possible orientations from list of flip and rotation operations.</summary>
/// <param name="flipOps">List of flip operations to try.</param>
/// <param name="rotationOps">List of rotation operations to try.</param>
/// <returns>Count of symbol's possible orientations.</returns>
let getSymOrientCount (flipOps: List<SymbolT.FlipType>) (rotationOps: List<Rotation>): int =
    (List.length flipOps + 1) * (List.length rotationOps)

/// <summary>Get count of possible port configurations of a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Count of port orders possible.</returns>
/// <remarks>Useful in determining which symbol to try to rotate on sheet.</remarks>
let getSymPortPermCount (sym: SymbolT.Symbol): int =
    Constants.edges
    |> List.map (fun edge -> Map.tryFind edge sym.PortMaps.Order)
    |> List.map (fun opt -> if Option.isNone opt then 1 else List.length (Option.get opt))
    |> List.fold (*) 1

/// <summary>Get count of possible symbol orientations using specified options and port
/// configurations of a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Count of symbol orientations times count of port orders possible.</returns>
/// <remarks>Useful in determining which symbol to try to rotate on sheet.</remarks>
let getSymOrientAndPortPermCount (sym: SymbolT.Symbol): int =
    (getSymOrientCount Constants.flipOps Constants.rotationOps) * (getSymPortPermCount sym)

/// <summary>Get all possible PortMaps.Order on a symbol after reordering ports.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>List of all possible <c>PortMaps.Order</c>.</returns>
/// <remarks>Useful when trying different ordering of the ports of a symbol.</remarks>
let getSymPortPermData
        (sym: SymbolT.Symbol)
            : List<Map<Edge,List<string>>> =
    Constants.edges
    |> List.map (fun edge -> Map.tryFind edge sym.PortMaps.Order)
    |> List.map (fun portListOpt->
        if Option.isNone portListOpt
        then []
        else permute (Option.get portListOpt)) // get list of list of all permutations on all sides
    |> allPairsNSameType // select one possible ordering from all possible orderings of each side to reconstruct order map
    |> List.map (fun selectionOptList -> List.map (Option.defaultValue []) selectionOptList)
    |> List.map (fun portOrder -> List.zip Constants.edges portOrder)
    |> List.map (fun mapList -> Map.ofList mapList)

/// <summary>Get all possible symbol orientation and <c>PortMaps.Order</c> on a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>List of tuples of flip operation, rotate operation, and <c>PortMaps.Order</c>.</returns>
/// <remarks>Useful when trying different orientation of a symbol or when reordering its ports.</remarks>
let getSymOrientAndPortPermData
        (sym: SymbolT.Symbol)
            : List<SymbolT.FlipType*Rotation*Map<Edge,List<string>>> =
    allPairs3 Constants.flipOps Constants.rotationOps (getSymPortPermData sym)


(* ---------------------------------------------------------------------------------------------- *)
(*                                      Clock-Face Algorithm                                      *)
(* ---------------------------------------------------------------------------------------------- *)

(* --------------------------------------- Symbol Helpers --------------------------------------- *)

/// <summary>Get all port IDs of a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>List of port ID strings.</returns>
let getSymPortIds (sym: SymbolT.Symbol): List<string> =
    sym.PortMaps.Orientation
    |> Map.toList
    |> List.map (fun (id, _) -> id)

/// <summary>Get all wires connected to a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>List of connected wires.</returns>
let getSymConnetedWires (sym: SymbolT.Symbol) (sheet: SheetT.Model): List<BusWireT.Wire> =
    let symPortIds = getSymPortIds sym

    getAllWires sheet
    |> List.filter
        (fun wire ->
            List.contains (inputPortStr wire.InputPort) symPortIds ||
            List.contains (outputPortStr wire.OutputPort) symPortIds)

/// <summary>Get the angle of a symbol's bottom right corner from positive x-axis direction.</summary>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of whole sheet.</param>
/// <returns>Angle of bottom-right corner, in range [0, 0.5 * PI).</returns>
let getSymBottomRightAng (sym: SymbolT.Symbol) (sheet: SheetT.Model): float =
    sheet.BoundingBoxes
    |> Map.tryFind sym.Id
    |> Option.bind (fun bbox -> Some (Math.Atan (bbox.H/bbox.W)))
    |> Option.defaultValue 0.


(* ---------------------------------------- Math Helpers ---------------------------------------- *)

/// <summary>Convert vector to angle.</summary>
/// <param name="vec">Target vector.</param>
/// <returns>Angle of range [0, 2 * PI).</returns>
let vecToAng (vec: XYPos): float =
    match vec.X >= 0., Math.Atan (vec.Y/vec.X) with
    | true, rad when rad >= 0. -> rad
    | true, rad when rad < 0. -> 2. * Math.PI + rad
    | false, rad when rad >= 0. -> 1. * Math.PI + rad
    | false, rad when rad < 0. -> 1. * Math.PI + rad
    | _ -> 0. // impossible case, but give typecheck warnings if not included

/// <summary>Shift angle anti-clockwise by a degree and normalize to [0, 2 * PI).</summary>
/// <param name="ang">Base angle, in radians.</param>
/// <param name="shift">Angle to shift by, in radians.</param>
/// <returns>Shifted angle.</returns>
let shiftAng (ang: float) (shift: float) =
    let result = ang + shift
    if result > 2. * Math.PI
    then result - 2. * Math.PI
    else result


(* --------------------------------------- Implementation --------------------------------------- *)

/// <summary>Find optimum order of ports for a symbol with the clockface algorithm.</summary>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of whole sheet.</param>
/// <returns>List of port IDs of target symbol in optimum order starting from the bottom right
/// corner in anti-clockwise direction.</returns>
let findClockfacePortIdOrder (sym: SymbolT.Symbol) (sheet: SheetT.Model): List<string> =
    let symPortIds = getSymPortIds sym
    let symCentre = sheet.BoundingBoxes[sym.Id].Centre()

    let connectedWires = getSymConnetedWires sym sheet

    let connectedPorts = // find the other end of the connected wire
        connectedWires
        |> List.map
            (fun wire ->
                wire,
                List.contains (inputPortStr wire.InputPort) symPortIds,
                List.contains (outputPortStr wire.OutputPort) symPortIds)
        |> List.map
            (function
                | wire, true, false -> Some (inputPortStr wire.InputPort)
                | wire, false, true -> Some (outputPortStr wire.OutputPort)
                | _ -> None) // obtain the port that is not connected to symbol
        |> List.choose id
        |> List.map (fun portId -> Map.tryFind portId sheet.Wire.Symbol.Ports)
        |> List.choose id

    let clockfaceConnectedPortOrder = // sort the other end of the wires
        connectedPorts
        |> List.map (fun port -> (getPortPosOnSheet port sheet, port))
        |> List.map (fun (pos, port) -> (pos-symCentre, port))
        |> List.map (fun (vec, port) -> (vecToAng vec, port))
        |> List.map
            (fun (ang, port) -> (shiftAng ang (getSymBottomRightAng sym sheet), port)) // shift to start from bottom-right
        |> List.sortBy (fun (shiftedRad, _) -> shiftedRad)
        |> List.map snd

    let clockfaceSymPortIdOrder = // find the port id of the symbol end of the wires
        clockfaceConnectedPortOrder
        |> List.map
            (fun port ->
                List.tryFind
                    (fun (wire: BusWireT.Wire) ->
                        inputPortStr wire.InputPort = port.Id || outputPortStr wire.OutputPort = port.Id)
                    connectedWires) // recover the wire which has the port in the list
        |> List.choose id
        |> List.map
            (fun wire ->
                if List.contains (inputPortStr wire.InputPort) symPortIds
                then inputPortStr wire.InputPort
                else outputPortStr wire.OutputPort) // recover the end that is connect to the symbol

    clockfaceSymPortIdOrder


(* ---------------------------------------------------------------------------------------------- *)
(*                                         sheetOrderFlip                                         *)
(* ---------------------------------------------------------------------------------------------- *)

(* ------------------------------ [WIP] Clockface Algorithm Helpers ----------------------------- *)

/// <summary>Get current symbol's orientation and port order information.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Tuple of flipped status, rotation, and <c>PortMaps.Order</c></returns>
let getSymOrientAndPortInfo
        (sym: SymbolT.Symbol)
            : bool*Rotation*Map<Edge,List<string>> =
    sym.STransform.Flipped, sym.STransform.Rotation, sym.PortMaps.Order

/// <summary>[DO NOT USE - WIP] Get list of port IDs, in anti-clockwise order from bottom right corner, after rendering.</summary>
/// <param name="orientAndPortInfo">Tuple of flipped status, rotation, and <c>PortMaps.Order</c>.</param>
/// <returns>List of port IDs.</returns>
/// <remarks>Useful when implementing clockface algorithm.</remarks>
let getPortOrderFromInfo
        (orientAndPortInfo: bool*Rotation*Map<Edge,List<string>>)
            : List<string> =
    [] // placeholder to make program type-check

/// <summary>[DO NOT USE - WIP] Count location differences between port order generated by the clockface algorithm
/// and the proposed transformation of the symbol.</summary>
/// <param name="optimalPortOrder">Optimal port order, as generated by the clockface algorithm.</param>
/// <param name="orientAndPortInfo">Proposed orientation and port order, is tuple of flipped status,
/// rotation, and <c>PortMaps.Order</c>.</param>
/// <remarks>Useful for choosing the correct permutation when implementing clockface algorithm.</remarks>
let countPortOrderMatches
        (optimalPortOrder: List<string>)
        (orientAndPortInfo: bool*Rotation*Map<Edge,List<string>>)
            : int =
    getPortOrderFromInfo orientAndPortInfo
    |> List.zip optimalPortOrder
    |> List.filter (fun (portId1, portId2) -> portId1 = portId2)
    |> List.length


(* ------------------------------------- Symbol Type Filters ------------------------------------ *)

/// <summary>Check if a symbol is of <c>Custom</c> type.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>True if symbol is a custom symbol.</returns>
let isCustomSym (sym: SymbolT.Symbol): bool =
    match sym.Component.Type with
    | Custom _ -> true
    | _ -> false

/// <summary>Check if a symbol is of <c>Mux</c> or <c>Demux</c> type.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>True if symbol is a mux or demux symbol.</returns>
let isMuxSym (sym: SymbolT.Symbol): bool =
    match sym.Component.Type with
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> true
    | _ -> false

/// <summary>Check if a symbol is of <c>Not</c> or <c>GateN</c> type.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>True if symbol is a gate symbol.</returns>
let isGateSym (sym: SymbolT.Symbol): bool =
    match sym.Component.Type with
    | Not | GateN _ -> true
    | _ -> false


(* --------------------------------- Symbol Transform Operations -------------------------------- *)

/// <summary>Add a (updated) symbol to a symbol map of sheet.</summary>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>Updated sheet.</returns>
let updateSymToSheet (sym: SymbolT.Symbol) (sheet: SheetT.Model): SheetT.Model =
    sheet
    |> Optic.get SheetT.symbols_
    |> Map.add sym.Id sym
    |> fun symMap -> Optic.set SheetT.symbols_ symMap sheet

/// <summary>Set a mux's or demux's <c>reversedInputPort</c> state in a sheet.</summary>
/// <param name="muxReverseState">State to set.</param>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>Updated sheet.</returns>
let setMuxSymReverseState
        (muxReverseState: bool)
        (sym: SymbolT.Symbol)
        (sheet: SheetT.Model)
            : SheetT.Model =
    updateSymToSheet (Optic.set reversedInputPorts_ muxReverseState sym) sheet

/// <summary>Set a symbol's <c>PortMaps.Order</c> in a sheet.</summary>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>Updated sheet.</returns>
let setSymPortOrder
        (portOrder: Map<Edge,List<string>>)
        (sym: SymbolT.Symbol)
        (sheet: SheetT.Model)
            : SheetT.Model =
    updateSymToSheet { sym with PortMaps = { sym.PortMaps with Order = portOrder } } sheet


(* --------------------------------------- Implementation --------------------------------------- *)

/// <summary>Auto route all wires on the sheet.</summary>
/// <param name="sheet">Model of sheet to reroute.</param>
/// <returns>Updated sheet.</returns>
let autoRouteAllWires (sheet: SheetT.Model): SheetT.Model =
    let wireModel =
        Optic.get SheetT.wire_ sheet

    let updatedWireModel =
        wireModel.Wires
        |> Map.map (fun _ wire -> BusWireRoute.smartAutoroute wireModel wire)
        |> fun updatedWires -> { wireModel with Wires = updatedWires }

    Optic.set SheetT.wire_ updatedWireModel sheet

/// <summary>Find best sheet by <c>optimisation</c>, without violating <c>limitations</c>,
/// after sequentially applying transfoms to a symbol with the <c>tranformer</c> and <c>conditions</c>.</summary>
/// <param name="transformer">Function to transform the target symbol, takes in a condition to try and the sheet model.</param>
/// <param name="optimisation">Function to check how well the transform performs, takes in the sheet model. Less is better.</param>
/// <param name="limitation">Function to check whether the transform is valid, takes in the sheet model. True is valid.</param>
/// <param name="conditions">List of conditions of the tranform to try on the target symbol.</param>
/// <param name="sym">Target symbol candidate.</param>
/// <param name="sheet">Model of whole sheet.</param>
/// <typeparam name="'T">Condition of operation to do on symbol, via input of <c>transformer</c>.</typeparam>
let getBestOrderFlipResult<'T>
        (transformer: 'T->SymbolT.Symbol->SheetT.Model->SheetT.Model)
        (optimisation: SheetT.Model->int)
        (limitation: SheetT.Model->bool)
        (conditions: List<'T>)
        (sym: SymbolT.Symbol)
        (sheet: SheetT.Model)
            : SheetT.Model =
    conditions
    |> List.map
        (fun sample ->
            let sheet' = transformer sample sym sheet |> autoRouteAllWires
            let optimiser = sheet' |> optimisation
            let limiter = sheet' |> limitation
            (sample, optimiser, limiter)) // find condition sample's results
    |> List.filter (fun (_, _, limiter) -> limiter) // filter out sample if it violates limitation
    |> List.sortBy (fun (_, optimiser, _) -> optimiser) // sort by lowest stats
    |> List.head
    |> fun (sample, _, _) -> sample
    |> fun sample -> transformer sample sym sheet


/// <summary>Find best sheet after sequentially applying transfoms to a list of symbols with the
/// <c>tranformer</c> and the conditions generated by the <c>conditionGenerator</c>.</summary>
/// <param name="transformer">Function to transform the target symbol, takes in a condition to try and the sheet model.</param>
/// <param name="optimisation">Function to check how well the transform performs, takes in the sheet model. Less is better.</param>
/// <param name="limitation">Function to check whether the transform is valid, takes in the sheet model. True is valid.</param>
/// <param name="conditionGenerator">Function to generate conditions to try for a symbol, take in a symbol.</param>
/// <param name="syms">List of symbol candidates.</param>
/// <param name="sheet">Model of whole sheet.</param>
/// <typeparam name="'T">Condition of operation to do on symbol, via input of <c>transformer</c>.</typeparam>
let getBestOrderFlipResultByList<'T>
        (transformer: 'T->SymbolT.Symbol->SheetT.Model->SheetT.Model)
        (optimisation: SheetT.Model->int)
        (limitation: SheetT.Model->bool)
        (conditionGenerator: SymbolT.Symbol->List<'T>)
        (syms: List<SymbolT.Symbol>)
        (sheet: SheetT.Model)
            : SheetT.Model =
        syms
        |> List.map conditionGenerator // list of conditions to try
        |> List.zip syms
        |> List.fold // find best condition for each symbol on sheet sequentially
            (fun sheet' (sym, conditions) ->
                getBestOrderFlipResult transformer optimisation limitation conditions sym sheet')
            sheet

/// <summary>Find list of symbols to transform, of a specified type.</summary>
/// <param name="typefilt">Symbol type filter.</param>
/// <param name="sheet">Model of whole sheet.</param>
/// <returns>List of symbols by the order of most to least wires connected to symbol.</returns>
/// <remarks>Symbols were ordered so because:
/// a) the symbol with most wires are most likely to have crosses, and
/// b) untangling symbols with more wires reduces dependancy for later symbols.</remarks>
let findSymCandidates (typefilt: SymbolT.Symbol->bool) (sheet: SheetT.Model): List<SymbolT.Symbol> =
    sheet.Wire.Symbol.Symbols
    |> Map.toList
    |> List.map snd // get all symbols
    |> List.filter typefilt // filter for symbol type
    |> List.map (fun sym -> (sym, getSymConnetedWires sym sheet |> List.length))
    |> List.sortBy (fun (_, wireCount) -> wireCount) // sort number of wires connected
    |> List.rev // lowest wire count appear first, so we reverse
    |> List.filter // remove symbols with too many permutations to do
        (fun (sym, _) -> (getSymPortPermCount sym) <= Constants.portPermLimit)
    |> List.map fst

/// <summary>Beautify sheet by attempting to orient components differently or to reorder its ports.</summary>
/// <param name="sheet">Model of sheet to beautify.</param>
/// <remarks>OFB Starter implemented: function will reverse input order on muxes and reorder ports on gates.</remarks>
let sheetOrderFlip (sheet: SheetT.Model): SheetT.Model =
    let muxCandidates = findSymCandidates isMuxSym sheet
    let gateCandidates = findSymCandidates isGateSym sheet

    let originalWireBends = findRightAngleCount sheet
    /// <summary>Helper to check on more wire bends is created.</summary>
    let notMoreWireBendsLimiter (sheet: SheetT.Model): bool =
        (sheet |> findRightAngleCount) <= originalWireBends

    sheet
    |> getBestOrderFlipResultByList
        setMuxSymReverseState
        findSegIntersectsSegCount
        notMoreWireBendsLimiter
        (fun _ -> [ false; true ]) // two state of mux input ports
        muxCandidates // change input order of muxes
    |> getBestOrderFlipResultByList
        setSymPortOrder
        findSegIntersectsSegCount
        notMoreWireBendsLimiter
        getSymPortPermData
        gateCandidates // permutate gates
