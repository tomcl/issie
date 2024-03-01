module SheetBeautifyD1
open SheetBeautifyHelpers
open Optics

open CommonTypes
open DrawModelType.SymbolT
open DrawModelType
open DrawModelType.SheetT
open DrawModelType.BusWireT

//________________________________________________________________________________________________________________________
//TEAM  DELIVERABLES
//D1. sheetAlignScale ASB
//________________________________________________________________________________________________________________________

// Align all singly-connected components to eliminate wire bends in parallel wires

//----> helper functions for the algorithm

//I am using parallel as "wire that is straight, or a candidate for straightening".
//Wires, for example, with 4 visual segments, are not parallel, and  are more complex. You could ignore them.

//A component is single-connected (in a given direction) when it has a single straightenable 3-visual-segment 
//wire connecting it to another component in that direction. In that case you can be sure that moving the component 
//can straighten that connection without unstraightening any other connections.
//Straightening: turn 3 visual segments into one
//Unstraightening: turning one visual segment into 3

//A *single-constrained wire* is one which can be straightened without unstraightening any other wires: in other 
//words one of its ends is a single-connected component. - 



//List of all the symbols on the sheet
let symbolList (sheet: SheetT.Model) = 
    Map.toList sheet.Wire.Symbol.Symbols
    |> List.map snd



///Returns True if a wire is streightened.
let streightenedWire (wire: Wire) (sheet: SheetT.Model) = 
    let wId = wire.WId
    (visibleSegments wId sheet |> List.length) = 1
///Returns True if a wire has potential to be streightened.
let straighteningPotentialWire (wire: Wire) (sheet: SheetT.Model) =
    let wId = wire.WId
    (visibleSegments wId sheet |> List.length) = 3


// Need to calculate the sign of the offset and return it as the second element of the tuple with this function
// Can use PortMaps.Orientation - This is in the Symbol Record
let checkIfSinglePortComponent (sym: Symbol) =
    let numInputPorts= sym.Component.InputPorts |> List.length 
    let numOutputPorts= sym.Component.OutputPorts |> List.length
    (numInputPorts + numOutputPorts) = 1


let endOfWire (wire: BusWireT.Wire) (sheet: SheetT.Model) = 
    visibleSegments wire.WId sheet
    |> List.fold (fun start segEnd -> start + segEnd) wire.StartPos

let changeOffsetSign (sym: Symbol) (portId: string) =
    match Map.tryFind portId sym.PortMaps.Orientation with
    | Some Left -> -1.0
    | _ -> 1.0



//BlockHelpers.getSourcePort
    //let inline getSourcePort (model:Model) (wire:Wire)
//BlockHelpers.moveSymbol
    //moveSymbol (offset:XYPos) (sym:Symbol)
//BusWireRoute.updateWires
    //let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) = 
    // list opf moved componnts

//symbol.
    //getPortLocation (defPos: XYPos option) (model: Model) (portId : string) : XYPos=



//Should be used just for wires with 3 visible segments
// Calculates the offSet based on the middle segmant of a potential wire 
let calculateOffset (wire: Wire) (sheet: SheetT.Model) =
    wire.WId
    |> (fun wId -> visibleSegments wId sheet)
    |> (fun visSegs -> if List.length visSegs = 3 then visSegs[1] else XYPos.zero)

//let calculateOffset_old (wire: Wire) (sheet: SheetT.Model) =
//    let wId = wire.WId
//    let nodeslist = visibleSegments wId sheet
//    let firstNode = List.item 0 nodeslist
//    let secondNode = List.item 1 nodeslist
//    {
//        X = firstNode.X - secondNode.X
//        Y = firstNode.Y - secondNode.Y
//    }


///Returns a list of the potential wire if there are no straight wires already.
let listPotentialWires (wires: list<Wire> ) (sheet: SheetT.Model) =
    let streightWires = wires |> List.exists (fun wire -> streightenedWire wire sheet)
    if streightWires = false
    then wires |> List.filter (fun wire -> straighteningPotentialWire wire sheet)
    else []


//Write a  function that changes the position of the symbol according rhe first potential wire
let getPotentialWireOffset (sheet: SheetT.Model) (wires: list<Wire> ) =
    if wires = []
    then XYPos.zero
    else
        let firstWire = List.item 0 wires
        calculateOffset firstWire sheet
    






    
        


let getWiresFromPort (sheet: SheetT.Model) (port: Port) (wireInputPort: bool) =
            sheet.Wire.Wires
            |> Map.toList
            |> List.map snd
            |> List.filter (fun wire -> if wireInputPort = true
                                        then match wire.InputPort with
                                                | InputPortId id when id = port.Id -> true
                                                | _ -> false
                                        else match wire.OutputPort with
                                                | OutputPortId id when id = port.Id -> true
                                                | _ -> false)


///Get all the wires from a Symbol that has strictly just one Port
let allWires1PortSym (sym: Symbol) (sheet: SheetT.Model)=
        //let i = List.item 0 sym.Component.InputPorts 
        // let o = List.item 0  sym.Component.OutputPorts
        if sym.Component.OutputPorts = []
        then
            let i = List.item 0 sym.Component.InputPorts
            let wires = getWiresFromPort sheet i true
            wires
        else
            let o = List.item 0  sym.Component.OutputPorts
            let wires = getWiresFromPort sheet o false
            wires
        
/// VERY IMPORTANT THAT YOU CHECK THAT THE SYMBOL HAS ONLY ONE PORT
let align1PortSymbol (onePortSym: Symbol) (sheet: SheetT.Model)= // (offsetSign: +/-)
    let portId = Map.toList onePortSym.PortMaps.Orientation |> List.item 0 |> fst
    let offsetSign = changeOffsetSign onePortSym portId
    let potentialList = 
        sheet
        |> allWires1PortSym onePortSym
        |> listPotentialWires 
    sheet
    |> potentialList 
    |> getPotentialWireOffset sheet
    |> (fun offset -> BlockHelpers.moveSymbol {X = offset.X * offsetSign; Y = offset.Y * offsetSign} onePortSym)
    // |> (fun newSym -> Optic.set (SheetT.symbolOf_ onePortSym.Id) newSym sheet)
    // |> SheetUpdateHelpers.updateBoundingBoxes


/// <summary>
/// After all the symbols have been moved, update the wiring on the entire sheet.
/// </summary>
/// <param name="newcIdList">List of cIds of all symbols that have moved.</param>
/// <param name="sheet">The sheet to be changed.</param>
/// <param name="symbolMovedBy">Take as 0 for now, not sure what this does, needed in updateWires.</param>
/// <returns>Model with rerouted </returns>

let rerouteWires (newcIdList: List<ComponentId>) (symbolMovedBy: XYPos) (sheet: SheetT.Model) = 
    BusWireRoute.updateWires sheet.Wire newcIdList symbolMovedBy
    |> (fun newWireModel -> Optic.set SheetT.wire_ newWireModel sheet)

let scaleSymbol (newVertical: float option) (newHorizontal: float option) (symbol: Symbol) (sheet: SheetT.Model) =
    let symbols = sheet.Wire.Symbol.Symbols

    let newSymbol = {symbol with VScale = newVertical; HScale = newHorizontal}
    // let newComp = {symbol.Component with H = symbol.Component.H * newVertical; W = symbol.Component.W * newHorizontal}
    // let newSymbol = {symbol with Component = newComp}

    let newSymbols = Map.add symbol.Id newSymbol symbols

    Optic.set SheetT.symbols_ newSymbols sheet
    |> SheetUpdateHelpers.updateBoundingBoxes
    //fix the function Update boundingBoxes for scaling!!!

//Not used but might be needed later
//let offestPortPos (firstPort:Port) (secondPort:Port) (sheet: SheetT.Model)=
//    let firstPortPos = readPortPosOnSheet sheet firstPort
//    let secPortPos = readPortPosOnSheet sheet secondPort
//    let Xdiff  = abs(firstPortPos.Y - secPortPos.Y)
//    let Ydiff = abs(firstPortPos.X - secPortPos.X)
//    let diff = max Xdiff Ydiff
//    diff

///Calculates port offset between two consecutive ports of same type (input or output).
let calcPortOffset (sym: SymbolT.Symbol) (portType: PortType) =
    let portList =
        match portType with
        | PortType.Input -> sym.Component.InputPorts
        | PortType.Output -> sym.Component.OutputPorts
    if List.length portList < 2
    then None
    else 
        (Symbol.getPortPos sym portList[1]) - (Symbol.getPortPos sym portList[0])
        |> (fun pos -> Some (max pos.X pos.Y))

///Calculates the ratio that is needed for scaleSymbol (newVertical and newHorizontal)
let calcPortRatio (outSym: SymbolT.Symbol) (inSym: SymbolT.Symbol) =
    match calcPortOffset outSym PortType.Output, calcPortOffset inSym PortType.Input with
    | Some (outOff), Some (inOff) -> outOff/inOff
    | _ ->
        printfn "Something's wrong I can feel it..." 
        1.0


///Finds two symbols connected by a wire.
let symbolsConnected (wire: Wire) (sheet: SheetT.Model) = 
    //Checks if port is on the symbol based on port id
    let portOnSymbol (pId: string) (symbol: Symbol) =
        Map.toList symbol.PortMaps.Orientation
        |> List.map fst
        |> List.contains pId

    match wire.InputPort, wire.OutputPort with 
    | InputPortId (iId), OutputPortId(oId)  -> 
        List.find (fun sym -> portOnSymbol oId sym) (symbolList sheet),
        List.find (fun sym -> portOnSymbol iId sym) (symbolList sheet)


///Finds the wires between any two symbols on the sheet for all symbols on the sheet.
let connectedSymbolsMap (sheet: SheetT.Model) = 
    Map.toList sheet.Wire.Wires
        |> List.map snd
        |> List.fold (fun map wire -> 
                            let syms = symbolsConnected wire sheet
                            match Map.tryFind syms map with
                            | Some wireList -> Map.add syms (wire :: wireList) map
                            | None -> Map.add syms [wire] map) Map.empty

 

let firstPhaseStraightening (sheet: SheetT.Model) =
    let initialOverlap = countIntersectedSymbols sheet
    let singlePortComponents = 
        sheet.Wire.Symbol.Symbols
        |> Map.toList 
        |> List.filter (fun (_, sym) -> checkIfSinglePortComponent sym)


    let changedSymbolList = 
        singlePortComponents
        |> List.map (fun (_, sym) -> align1PortSymbol sym sheet)
    changedSymbolList
    |> List.fold (fun sheet newSym -> 
                        let newSheet = Optic.set (SheetT.symbolOf_ newSym.Id) newSym sheet
                        if countIntersectedSymbols newSheet > initialOverlap
                        then sheet
                        else newSheet) sheet
    |> SheetUpdateHelpers.updateBoundingBoxes
    |> rerouteWires (List.map (fun sym -> sym.Id) changedSymbolList) XYPos.zero
    


let secondPhaseStraightening (sheet: SheetT.Model)=
    connectedSymbolsMap sheet
    |> Map.filter (fun _ value -> List.length value > 1 )
    |> Map.toList 
    |> List.map fst
    |> List.fold (fun sheet syms -> 
                    let scaleValue = calcPortRatio (fst syms) (snd syms)
                    let scaledModel = 
                        scaleSymbol (Some scaleValue) None (snd syms) sheet
                        |> rerouteWires [(snd syms).Id] XYPos.zero
                    let newSym = align1PortSymbol(fst syms) scaledModel
                    Optic.set (SheetT.symbolOf_ newSym.Id) newSym scaledModel
                    |> rerouteWires [newSym.Id] XYPos.zero) sheet

    
// let newSym = align1PortSymbol(snd syms) scaledModel
//                     Optic.set (SheetT.symbolOf_ newSym.Id) newSym scaledModel
//                     |> rerouteWires [newSym.Id] XYPos.zero


    //get the actual position of the input port and allign the output port  with the input
    //getPortPosOnSheet  

//BlockHelpers.getSourcePort
    //let inline getSourcePort (model:Model) (wire:Wire)
//BlockHelpers.moveSymbol
    //moveSymbol (offset:XYPos) (sym:Symbol)
//BusWireRoute.updateWires
    //let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) = 
    // list opf moved componnts

//for custom components I want to make the port distance equal on both sides
//if I have two symbols and the input symbol is not singly connected then just scale the input symbol
