module SheetBeautifyD2


open System
open Optics
open CommonTypes
open DrawModelType
open BlockHelpers
open DrawHelpers
open SymbolResizeHelpers
open SheetUpdateHelpers
open SheetBeautifyHelpers
open RotateScale


module Constants =
    /// <summary>Flip operations to try. Should only contain 1 entry,
    /// which is either <c>FlipHorizontal</c> or <c>FlipVertical</c>.</summary>
    let flipOps = [ SymbolT.FlipHorizontal ]

    /// <summary>Rotation operations to try.</summary>
    let rotationOps = [ Degree0; Degree90; Degree180; Degree270 ]

    /// <summary>All edges on a symbol. Listed from 12 o'clock in clockwise order.</summary>
    let edges = [ Top; Right; Bottom; Left ]

    /// <summary>
    let orientCount = (List.length flipOps + 1) * (List.length rotationOps)

    /// <summary>Limit of port permutations to try.
    /// <c>sheetOrderFlip</c> does not check components with permutation count over limit.</summary>
    let portPermLimit = 25


(* ---------------------------------------------------------------------------------------------- *)
(*                                Additional Sheet Beautify Helpers                               *)
(* ---------------------------------------------------------------------------------------------- *)

/// <summary>D.U. to describe orientation of a segment.</summary>
/// <remarks>Noted that BusWireT.Orientation is similar, but wanted to represent <c>Other</c> and <c>Zero</c> type,
/// although <c>Other</c> is likely not used.</remarks>
type SegOrientation = | Horizontal | Vertical | Other | Zero

/// <summary>Get a segment's orientation from its start and end position.</summary>
/// <param name="segStart">Segment start position.</param>
/// <param name="segEnd">Segment end position.</param>
/// <returns>Orientation, of <c>SegOrientation</c> type.</returns>
let segOrientation (segStart: XYPos) (segEnd: XYPos): SegOrientation =
    match (segEnd.X - segStart.X), (segEnd.Y - segStart.Y) with
    | dx, dy when dx = 0.0 && dy = 0.0 -> Zero
    | dx, dy when dx <> 0.0 && dy = 0.0 -> Horizontal
    | dx, dy when dx = 0.0 && dy <> 0.0 -> Vertical
    | _ -> Other

/// <summary>Get all wire IDs within a sheet.</summary>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>List of connection IDs of all the wires on the sheet.</returns>
let getAllWireIds (sheet: SheetT.Model): List<ConnectionId> =
    sheet.Wire.Wires |> Map.toList |> List.map fst

/// <summary>Get vertices of visible coalesceed segments of a wire.</summary>
/// <param name="wire">Target wire, of type Wire.</param>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>List of <c>XYPos</c> that represents the visible vertices in a wire.</returns>
/// <remarks>Function refactored from <c>TestDrawBlock.HLPTick3.visibleSegments</c>.</remarks>
let getWireVisibleSegVertices (wire: BusWireT.Wire) (sheet): List<XYPos> =
    SegmentHelpers.visibleSegments wire.WId sheet
    |> List.mapFold (fun currPos segVec -> segVec+currPos, segVec+currPos) wire.StartPos
    |> (fun (vertices, _) -> wire.StartPos :: vertices)

/// <summary>Get all visible segments of all wires within a sheet.</summary>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>List of tuple of segments': wire's <c>OutputPortId</c>, start,
/// and end positions of all segments on the sheet.</returns>
let getAllWireVisibleSegs (sheet: SheetT.Model): List<OutputPortId*XYPos*XYPos> =
    /// <summary>Helper to create data of <c>OutputPortId</c>, start, and end position tuples,
    /// from list of wire vertices.</summary>
    let makeSegData (sourceId: OutputPortId, posList: List<XYPos>) =
        List.pairwise posList
        |> List.map (fun (segStart, segEnd) -> (sourceId, segStart, segEnd))

    getAllWireIds sheet
    |> List.map (fun wireId -> sheet.Wire.Wires[wireId])
    |> List.map (fun wire -> (wire.OutputPort, getWireVisibleSegVertices wire sheet))
    |> List.map makeSegData
    |> List.collect (fun item -> item)

/// <summary>Get count of visible, perpendicular, non-zero-length, non-same-net,
/// and non-consecutive segments pairs, that intersects each other.</summary>
/// <param name="sheet">Target sheet to check.</param>
/// <returns>Total count of intersecting visible wire segment pairs.</returns>
/// <remarks> Assumed that "segments on same net on top of each other" is a
/// superset of "segments same net intersecting at one end".</remarks>
let numOfWireRightAngleCrossingsInDiffNet (sheet: SheetT.Model): int =
    /// <summary>Helper to identify perpendicular segment pairs.</summary>
    let isPerpSegPair (seg1: OutputPortId*XYPos*XYPos, seg2: OutputPortId*XYPos*XYPos): bool =
        let _, segStart1, segEnd1 = seg1
        let _, segStart2, segEnd2 = seg2

        match segOrientation segStart1 segEnd1, segOrientation segStart2 segEnd2 with
        | Horizontal, Vertical | Vertical, Horizontal -> true
        | _ -> false

    getAllWireVisibleSegs sheet
    |> List.filter (fun (_, segStart, segEnd) -> segStart <> segEnd) // remove length=0
    |> (fun list -> List.allPairs list list) // make segment pairs
    |> List.filter (fun ((inputId1, _, _), (inputId2, _, _)) -> inputId1 <> inputId2) // remove same-net pairs
    |> List.filter isPerpSegPair // remove non-perpendicular pairs
    |> List.filter
        (fun ((_, segStart1, segEnd1), (_, segStart2, segEnd2)) ->
            overlap2D (segStart1, segEnd1) (segStart2, segEnd2)) // remove non-overlapping pairs
    |> List.length
    |> fun count -> count/2 // if using allPairs on same list, will create duplicates

/// <summary>Get count of visible right angles on a sheet by counting coalesced segments.</summary>
/// <param name="sheet">Target sheet to check.</param>
/// <returns>Total count of right angles.</returns>
/// <remarks>Right angles are counted so because zero-length segments have already been
/// removed within the <c>getWireVisibleSegVertices</c> function.</remarks>
let findRightAngleCount (sheet: SheetT.Model): int =
    sheet.Wire.Wires 
    |> Map.values 
    |> List.ofSeq
    |> List.map (fun wire -> getWireVisibleSegVertices wire sheet)
    |> List.map List.length
    |> List.map (fun count -> count-2) // excluding start and end, each vertice is a right angle
    |> List.map (fun count -> if count < 0 then 0 else count) // remove cases where there is no segment
    |> List.sumBy id


(* ---------------------------------------------------------------------------------------------- *)
(*                                      Permuataion Algorithm                                     *)
(* ---------------------------------------------------------------------------------------------- *)

(* ----------------------------------------- Info Types ----------------------------------------- *)

/// <summary>Record type to track transformations done to a symbol.</summary>
type OrientAndPortInfo = 
    { STransform: STransform
      PortMaps: SymbolT.PortMaps
      InputsReversedState: Option<bool> }


(* ---------------------------------------- Math Helpers ---------------------------------------- *)

/// <summary>Factorial of input number.</summary>
/// <param name="n">Input number.</param>
/// <returns>n! - but returns 1 if less than 1 is given.</returns>
let factorial (n: int): int =
    let rec factorial' (n: int): int =
        if n <= 1
        then 1
        else n * factorial' (n-1)

    factorial' n

/// <summary>Permute a list of items of the same type</summary>
/// <param name="list">List of items to operate on.</param>
/// <returns>List of list of permutated items.</returns>
let permute<'T> (list: List<'T>): List<List<'T>> =
    /// <summary>Helper to insert element into all possible index of a list.
    /// Returns a list of original list with element inserted at different locations.</summary>
    let distribute (elem: 'T) (list: List<'T>): List<List<'T>> =
        [0..(List.length list)-1]
        |> List.map (fun i -> List.insertAt i elem list)
        |> fun result -> result @ [ list @ [ elem ] ]

    let rec permute' (list: List<'T>) (result: List<List<'T>>): List<List<'T>> =
        match list with
        | [] -> result
        | head :: tail -> permute' tail result |> List.map (distribute head) |> List.collect id

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
let allCombinationsOfLists<'T> (list: List<List<'T>>): List<List<Option<'T>>> =
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

/// <summary>Get count of possible port configurations of a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Count of port orders possible.</returns>
let getPortPermCount (sym: SymbolT.Symbol): int =
    Constants.edges
    |> List.map (fun edge -> Map.tryFind edge sym.PortMaps.Order)
    |> List.choose id
    |> List.map List.length
    |> List.map factorial
    |> List.fold (*) 1

/// <summary>Get count of possible symbol orientations using specified options and port
/// configurations of a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Count of symbol orientations times count of port orders possible.</returns>
let getOrientAndPortPermCount (sym: SymbolT.Symbol): int =
    (Constants.orientCount) * (getPortPermCount sym)

/// <summary>Get all possible PortMaps.Order on a symbol after reordering ports.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>List of all possible <c>PortMaps.Order</c>.</returns>
let getPortPerm
        (sym: SymbolT.Symbol)
            : List<Map<Edge,List<string>>> =
    Constants.edges
    |> List.map (fun edge -> Map.tryFind edge sym.PortMaps.Order)
    |> List.map (fun portListOpt->
        if Option.isNone portListOpt
        then []
        else permute (Option.get portListOpt)) // get list of list of all permutations on all sides
    |> allCombinationsOfLists // select one possible ordering from all possible orderings of each side to reconstruct order map
    |> List.map (fun selectionOptList -> List.map (Option.defaultValue []) selectionOptList)
    |> List.map (fun portOrder -> List.zip Constants.edges portOrder)
    |> List.map (fun mapList -> Map.ofList mapList)

/// <summary>Get all possible orientations and <c>PortMaps.Order</c> on a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>List of <c>OrientAndPortInfo</c> that contains all posibilities to try.</returns>
let getOrientAndPortPerm
        (sym: SymbolT.Symbol)
            : List<OrientAndPortInfo> =
    allPairs3 [ false; true ] Constants.rotationOps (getPortPerm sym)
    |> List.map (fun (flip, rotation, portOrder) ->
        { STransform = { Flipped = flip; Rotation = rotation} 
          PortMaps = { Orientation = sym.PortMaps.Orientation; Order = portOrder }
          InputsReversedState = None })

/// <summary>Get all possible orientations and <c>InputReverseState</c> on a mux symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>List of <c>OrientAndPortInfo</c> that contains all posibilities to try.</returns>
let getMuxOrientAndInputReverseStatePerm
        (sym: SymbolT.Symbol)
            : List<OrientAndPortInfo> =
    allPairs3 [ false; true ] Constants.rotationOps [ Some false; Some true ]
    |> List.map (fun (flip, rotation, inputReverseState) ->
        { STransform = { Flipped = flip; Rotation = rotation} 
          PortMaps = sym.PortMaps
          InputsReversedState = inputReverseState })


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
let getSymConnectedWires (sym: SymbolT.Symbol) (sheet: SheetT.Model): List<BusWireT.Wire> =
    let symPortIds = getSymPortIds sym

    sheet.Wire.Wires
    |> Map.filter
        (fun key wire ->
            List.contains (inputPortStr wire.InputPort) symPortIds ||
            List.contains (outputPortStr wire.OutputPort) symPortIds)
    |> Map.values
    |> List.ofSeq

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
    match vec.X = 0, vec.Y = 0. with
    | true, true -> 
        0.
    | true, false -> 
        if vec.Y > 0. then Math.PI / 4. else Math.PI * 3. / 4.
    | false, false | false, true ->
        match vec.X > 0., Math.Atan (vec.Y/vec.X) with
        | true, rad when rad >= 0. -> rad
        | true, rad when rad < 0. -> 2. * Math.PI + rad
        | false, rad when rad >= 0. -> 1. * Math.PI + rad
        | false, rad when rad < 0. -> 1. * Math.PI + rad
        | _ -> 0. // impossible case, but give typecheck warnings if not included

/// <summary>Normalise angle.</summary>
/// <param name="ang">Target angle.</param>
/// <returns>Normalised angle of [0, 2 * PI).</returns>
let normalise (ang: float)=
    if ang > 2. * Math.PI
    then ang - 2. * Math.PI
    else ang


(* --------------------------------------- Implementation --------------------------------------- *)

/// <summary>Find optimum order of ports for a symbol with the clockface algorithm.</summary>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of whole sheet.</param>
/// <returns>List of port IDs of target symbol in optimum order starting from the bottom right
/// corner in anti-clockwise direction.</returns>
let findClockfacePortIdOrder (sym: SymbolT.Symbol) (sheet: SheetT.Model): List<string> =
    let symPortIds = getSymPortIds sym
    let connectedWires = getSymConnectedWires sym sheet

    /// <summary>Get the port ID of one end of the wire, end is specified by <c>isSymSide</c>.</summary>
    let tryFindOtherEndPortId (isSymSide: bool) (wire: BusWireT.Wire): Option<string> =
        let isInputSideSym = List.contains (inputPortStr wire.InputPort) symPortIds
        let isOutputSideSym = List.contains (outputPortStr wire.OutputPort) symPortIds
        let inputSide = Some (inputPortStr wire.InputPort)
        let outputSide = Some (outputPortStr wire.OutputPort)
        match isInputSideSym, isOutputSideSym with
        | true, false -> if isSymSide then inputSide else outputSide
        | false, true -> if isSymSide then outputSide else inputSide
        | _ -> None // also ignoring self-wires

    /// <summary>Get the wire from the <c>connectedWires</c> list that has one end of port.</summary>
    let tryFindWireOfPort (port: Port): Option<BusWireT.Wire> =
        List.tryFind
            (fun wire -> inputPortStr wire.InputPort = port.Id || outputPortStr wire.OutputPort = port.Id)
            connectedWires

    let otherPorts = // find the other end of the connected wire
        connectedWires
        |> List.map (tryFindOtherEndPortId false)
        |> List.choose id
        |> List.map (fun portId -> Map.tryFind portId sheet.Wire.Symbol.Ports)
        |> List.choose id

    let otherPortsInClockfaceOrder = // sort the other end of the wires
        let makePortPos (port: Port) = port, getPortPos port.Id sheet.Wire.Symbol
        let symCentre = sheet.BoundingBoxes[sym.Id].Centre()
        let makePortPosVecFromSymCentre (port: Port) (pos: XYPos) = pos-symCentre
        let makePortAngFromVec (port: Port) (vec: XYPos) = vecToAng vec
        let bottomRightAng = getSymBottomRightAng sym sheet
        let makePortAngFromBottomRight (port: Port) (ang: float) = normalise ang+bottomRightAng

        otherPorts
        |> List.map makePortPos
        |> Map.ofList
        |> Map.map makePortPosVecFromSymCentre
        |> Map.map makePortAngFromVec
        |> Map.map makePortAngFromBottomRight
        |> Map.toList
        |> List.sortBy snd // sort by angle
        |> List.map fst // retrieve port id

    otherPortsInClockfaceOrder
    |> List.map tryFindWireOfPort // recover the wire which has the port in the list
    |> List.choose id
    |> List.map (tryFindOtherEndPortId true) // recover the end that is connect to the symbol
    |> List.choose id


(* ---------------------------------------------------------------------------------------------- *)
(*                                         sheetOrderFlip                                         *)
(* ---------------------------------------------------------------------------------------------- *)

(* --------------------------------- Clockface Algorithm Helpers -------------------------------- *)

/// <summary>Get current symbol's orientation and port order information.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Tuple of flipped status, rotation, and <c>PortMaps.Order</c></returns>
let getSymOrientAndPortInfo (sym: SymbolT.Symbol): OrientAndPortInfo =
    { STransform = sym.STransform; 
      PortMaps = sym.PortMaps; 
      InputsReversedState = sym.ReversedInputPorts }

/// <summary>[DO NOT USE - WIP] Get list of port IDs, in anti-clockwise order from bottom right corner, after rendering.</summary>
/// <param name="orientAndPortInfo">Tuple of flipped status, rotation, and <c>PortMaps.Order</c>.</param>
/// <returns>List of port IDs.</returns>
let getPortOrderFromInfo (orientAndPortInfo: OrientAndPortInfo): List<string> =
    /// <summary>Helper to update <c>PortMaps</c> for a horizontal flip.
    /// Refactored trom <c>RotateScale.flipBlock</c> function.</summary>
    let flipPortInfo (flipped: bool) (portMaps: SymbolT.PortMaps): SymbolT.PortMaps =
        if flipped
        then 
            let newPortOrientation =
                portMaps.Orientation
                |> Map.map (fun _ side -> flipSideHorizontal side)

            let newPortOrder =
                portMaps.Order
                |> Map.keys
                |> List.ofSeq
                |> List.fold
                    (fun currPortOrder side -> Map.add (flipSideHorizontal side) portMaps.Order[side] currPortOrder)
                    Map.empty
                |> Map.map (fun _ order -> List.rev order)
            
            { Order = newPortOrder; Orientation = newPortOrientation }
        else
            portMaps

    let updatedPortOrder =
        orientAndPortInfo.PortMaps
        |> rotatePortInfo orientAndPortInfo.STransform.Rotation
        |> flipPortInfo orientAndPortInfo.STransform.Flipped
        |> (fun portMaps -> portMaps.Order)
    
    // port order is rendered in anti-clockwise order
    updatedPortOrder[Right] @ updatedPortOrder[Top] @ updatedPortOrder[Left] @ updatedPortOrder[Bottom]


/// <summary>[DO NOT USE - WIP] Count location differences between port order generated by the clockface algorithm
/// and the proposed transformation of the symbol.</summary>
/// <param name="optimalPortOrder">Optimal port order, as generated by the clockface algorithm.</param>
/// <param name="orientAndPortInfo">Proposed orientation and port order, is tuple of flipped status,
/// rotation, and <c>PortMaps.Order</c>.</param>
let countPortOrderMatches
        (optimalPortOrder: List<string>)
        (orientAndPortInfo: OrientAndPortInfo)
            : int =
    getPortOrderFromInfo orientAndPortInfo
    |> List.zip optimalPortOrder
    |> List.filter (fun (portId1, portId2) -> portId1 = portId2)
    |> List.length


(* ------------------------------------- Symbol Type Filters ------------------------------------ *)

/// <summary>Active pattern to categorise types of symbols.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Symbol category.</returns>
let (| CustomSym | MuxSym | GateSym | OtherSym |) (sym: SymbolT.Symbol) =
    match sym.Component.Type with
    | Custom _ -> CustomSym
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 -> MuxSym
    | Not | GateN _ -> GateSym
    | _ -> OtherSym


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
        (muxReverseState: Option<bool>)
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

/// <summary>Set a symbol's orientation in a sheet.</summary>
/// <param name="orientAndPortInfo">Orientataion and port information.</param>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>Updated sheet.</returns>
let setSymOrient
        (orientAndPortInfo: OrientAndPortInfo)
        (sym: SymbolT.Symbol)
        (sheet: SheetT.Model)
            : SheetT.Model =
    sheet
    |> Optic.get SheetT.symbol_
    |> rotateBlock' orientAndPortInfo.STransform.Rotation [ sym.Id ]
    |> (fun model -> 
        if orientAndPortInfo.STransform.Flipped 
        then flipBlock' Constants.flipOps[0] [ sym.Id ] model
        else model)
    |> (fun model -> Optic.set SheetT.symbol_ model sheet)

/// <summary>Set a mux symbols's orientation and input reverse state in a sheet.</summary>
/// <param name="orientAndPortInfo">Orientataion and port information.</param>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>Updated sheet.</returns>
let setMuxSymOrientAndInputReverseState
        (orientAndPortInfo: OrientAndPortInfo)
        (sym: SymbolT.Symbol)
        (sheet: SheetT.Model)
            : SheetT.Model =
    sheet
    |> setMuxSymReverseState orientAndPortInfo.InputsReversedState sym
    |> setSymOrient orientAndPortInfo sym

/// <summary>Set a symbols's orientation and <c>PortMaps.Order</c> in a sheet.</summary>
/// <param name="orientAndPortInfo">Orientataion and port information.</param>
/// <param name="sym">Target symbol.</param>
/// <param name="sheet">Model of the whole sheet.</param>
/// <returns>Updated sheet.</returns>
let setSymOrientAndPortOrder
        (orientAndPortInfo: OrientAndPortInfo)
        (sym: SymbolT.Symbol)
        (sheet: SheetT.Model)
            : SheetT.Model =
    sheet
    |> setSymPortOrder orientAndPortInfo.PortMaps.Order sym
    |> setSymOrient orientAndPortInfo sym


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
    |> List.tryHead
    |> Option.bind (fun (sample, _, _) -> Some sample)
    |> Option.bind (fun sample -> Some (transformer sample sym sheet))
    |> Option.defaultValue sheet


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
    /// <summary>Helper to count the number of connections the symbols has.</summary>
    let countWireConnections sym = sym, getSymConnectedWires sym sheet |> List.length

    /// <summary>Helper to filter symbols with permutations greater than set limit.</summary>
    let tooManyPerms (sym, _) = (getPortPermCount sym) <= Constants.portPermLimit

    sheet.Wire.Symbol.Symbols
    |> Map.toList
    |> List.map snd // get all symbols
    |> List.filter typefilt // filter for symbol type
    |> List.map countWireConnections
    |> List.sortBy (fun (_, wireCount) -> wireCount) // sort by number of wires connected
    |> List.rev // lowest wire count appear first, so we reverse
    |> List.filter tooManyPerms // remove symbols with too many permutations to do
    |> List.map fst

/// <summary>Beautify sheet by attempting to orient components differently or to reorder its ports.</summary>
/// <param name="sheet">Model of sheet to beautify.</param>
/// <remarks>OFB Starter implemented: function will reverse input order on muxes and reorder ports on gates.</remarks>
let sheetOrderFlip (sheet: SheetT.Model): SheetT.Model =
    let muxCandidates = findSymCandidates (fun sym -> match sym with | MuxSym -> true | _ -> false) sheet
    let gateCandidates = findSymCandidates (fun sym -> match sym with | GateSym -> true | _ -> false) sheet
    let customCandidates = findSymCandidates (fun sym -> match sym with | CustomSym -> true | _ -> false) sheet

    let originalWireBends = findRightAngleCount sheet
    /// <summary>Helper to check on more wire bends is created.</summary>
    let notMoreWireBendsLimiter (sheet: SheetT.Model): bool =
        (sheet |> findRightAngleCount) <= originalWireBends

    sheet
    |> getBestOrderFlipResultByList
        setMuxSymOrientAndInputReverseState
        numOfWireRightAngleCrossingsInDiffNet
        notMoreWireBendsLimiter
        getMuxOrientAndInputReverseStatePerm
        muxCandidates // permute orientation and input port order of muxes
    |> getBestOrderFlipResultByList
        setSymOrientAndPortOrder
        numOfWireRightAngleCrossingsInDiffNet
        notMoreWireBendsLimiter
        getOrientAndPortPerm
        gateCandidates // permute orientation and port order of muxes
    |> getBestOrderFlipResultByList
        setSymPortOrder
        numOfWireRightAngleCrossingsInDiffNet
        notMoreWireBendsLimiter
        getPortPerm
        customCandidates // permute custom component port order        
    |> autoRouteAllWires
