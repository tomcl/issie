module SheetBeautifyD2

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
    let FlipOps = [ SymbolT.FlipHorizontal ]

    /// <summary>Rotation operations to try.</summary>
    let RotationOps = [ Degree0; Degree90; Degree180; Degree270 ]
    
    /// <summary>Edges on a symbol.</summary>
    let Edges = [ Top; Right; Bottom; Left ]


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

/// <summary>Return a new list that contains all possible selection from given list of lists. Like allPairs.
/// For example, given [ [ 1; 2 ]; [ 3; 4 ] ], will return [ [ 1; 3 ]; [ 1; 4 ]; [ 2; 3 ]; [ 2; 4 ] ].</summary>
/// <param name="list">List of list of items to select.</param>
/// <returns>List of list of selection, wrapped in option. 
/// Options are return to avoid the case where an empty list is provided.</returns>
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

/// <summary>Get count of possible port configurations of a custom symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Count of port orders possible.</returns>
/// <remarks>Useful in determining which symbol to try to rotate on sheet.</remarks>
let getSymPortPermCount (sym: SymbolT.Symbol): int =
    Constants.Edges
    |> List.map (fun edge -> Map.tryFind edge sym.PortMaps.Order)
    |> List.map (fun opt -> if Option.isNone opt then 1 else List.length (Option.get opt))
    |> List.fold (*) 1

/// <summary>Get count of possible symbol orientations using specified options and port 
/// configurations of a custom symbol (not checked).</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>Count of symbol orientations times count of port orders possible.</returns>
/// <remarks>Useful in determining which symbol to try to rotate on sheet.</remarks>
let getSymOrientationAndPortPermCount (sym: SymbolT.Symbol): int =
    (List.length Constants.FlipOps + 1) * (List.length Constants.RotationOps) * (getSymPortPermCount sym)

/// <summary>Get all possible PortMaps.Order on a symbol after reordering ports.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>List of all possible PortMaps.Order.</returns>
/// <remarks>Useful when trying different ordering of the ports of a symbol.</remarks>
let getSymPortPermData
        (sym: SymbolT.Symbol)
            : List<Map<Edge,List<string>>> =
    Constants.Edges
    |> List.map (fun edge -> Map.tryFind edge sym.PortMaps.Order)
    |> List.map (fun portListOpt-> 
        if Option.isNone portListOpt 
        then [] 
        else permute (Option.get portListOpt)) // get list of list of all permutations on all sides
    |> allPairsNSameType // select one possible ordering from all possible orderings of each side to reconstruct order map
    |> List.map (fun selectionOptList -> List.map (Option.defaultValue []) selectionOptList)
    |> List.map (fun portOrder -> List.zip Constants.Edges portOrder)
    |> List.map (fun mapList -> Map.ofList mapList)

/// <summary>Get all possible symbol orientation and PortMaps.Order on a symbol.</summary>
/// <param name="sym">Target symbol.</param>
/// <returns>List of tuples of flip operation, rotate operation, and PortMaps.Order.</returns>
/// <remarks>Useful when trying different orientation of a symbol or when reordering its ports.</remarks>
let getSymOrientationAndPortPermData
        (sym: SymbolT.Symbol)
            : List<SymbolT.FlipType*Rotation*Map<Edge,List<string>>> =
    allPairs3 Constants.FlipOps Constants.RotationOps (getSymPortPermData sym)

    
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

    let connectedWires = // find wires connect to this symbol
        getAllWires sheet
        |> List.filter 
            (fun wire -> 
                List.contains (inputPortStr wire.InputPort) symPortIds ||
                List.contains (outputPortStr wire.OutputPort) symPortIds)

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
