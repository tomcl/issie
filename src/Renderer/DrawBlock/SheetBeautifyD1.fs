module SheetBeautifyD1

open CommonTypes
open DrawModelType
open BlockHelpers
open BusWire
open SymbolT

// this is the module for team phase work D1 

let mapValuesToList map = 
    map
    |> Helpers.mapValues
    |> Array.toList

/// Return Wire list that has desired port either in input or output
let filterWiresByPort (wires: BusWireT.Wire list) (portId: string) : BusWireT.Wire list= 
    List.filter (fun wire -> wire.InputPort = InputPortId portId || wire.OutputPort = OutputPortId portId) wires

/// Given three visual (non-zero) segments, determine whether combine into a parallel wire
let isParallelWire (wire: BusWireT.Wire) : bool= 
    let segsLst = getNonZeroAbsSegments wire
    
    match segsLst.Length with
        | 3 ->  getSegmentOrientation segsLst[0].Start segsLst[0].End = getSegmentOrientation segsLst[2].Start segsLst[2].End &&
                getSegmentOrientation segsLst[0].Start segsLst[0].End <> getSegmentOrientation segsLst[1].Start segsLst[1].End &&
                segsLst[0].Segment.Length * segsLst[2].Segment.Length > 0.0 //lengths of seg1 and seg3 need to have the same sign
        | _ -> false

/// Determine whether a symbol is a Singly-connected component
let isSinglyConnectedComponent (sheetModel:SheetT.Model) (sym: Symbol) : bool =
    let wiresLst = 
        sheetModel.Wire.Wires
        |> Helpers.mapValues
        |> Array.toList

    let hasOnlyOnePort : string option= 
        let portsLst=
            sym.PortMaps.Order
            |> Map.toList
            |> List.collect (fun tuple -> snd tuple)
        match portsLst.Length with
        | 1 -> Some portsLst.Head
        | _ -> None
        
    let portHasOnlyOneWire (portId : string) : bool = 
        filterWiresByPort wiresLst portId
        |> List.length = 1

    match hasOnlyOnePort with
    | Some portId -> portHasOnlyOneWire portId
    | _ -> false

/// Determine whether given waire is a Singly-constrained parallel wire
let isSinglyConstrainedParallelWire (sheetModel:SheetT.Model) (wire: BusWireT.Wire) : bool =
    let sourceSym = getSourceSymbol sheetModel.Wire wire
    let targetSym = getTargetSymbol sheetModel.Wire wire
    isSinglyConnectedComponent sheetModel sourceSym || isSinglyConnectedComponent sheetModel targetSym


/// Extract segments of parallel wires that can be straightened from wires list
let parallelWires (sheetModel:SheetT.Model) :  BusWireT.Wire list= 
    sheetModel.Wire.Wires
    |> mapValuesToList
    |> List.filter (fun wire -> isParallelWire wire)

/// Extract wires that can be straightened 
let toStraightenWires (sheetModel:SheetT.Model) = 
    let sourceSym = getSourceSymbol sheetModel.Wire
    let targetSym = getTargetSymbol sheetModel.Wire

    parallelWires sheetModel
    |> List.filter (fun wire -> 
                        isSinglyConnectedComponent sheetModel (sourceSym wire) ||
                        isSinglyConnectedComponent sheetModel (targetSym wire))