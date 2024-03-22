module SheetBeautifyD3

open Optics // for Lens
open BlockHelpers 
open EEExtensions
open DrawModelType.SheetT
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open CommonTypes
open DrawModelType
open SheetUpdateHelpers
open Sheet
open SheetBeautifyHelpers
open Optics.Operators
open SymbolUpdate



// deleting a wire 
let delWire (model : SheetT.Model) (wId : ConnectionId) : SheetT.Model= 
    let updatedWireMap = model.Wire.Wires |> Map.filter (fun connId w -> connId <> wId)
    { model with Wire = { model.Wire with Wires = updatedWireMap } }

// tryFind to avoid exception
let symIsIOLabel (model : SheetT.Model) (symId : ComponentId) : bool = 
    let symOpt = model.Wire.Symbol.Symbols |> Map.tryFind symId
    match symOpt with 
    | Some sym -> sym.Component.Type = IOLabel
    | None -> false

// get the suymbol-port pair for a wire
let wireSymbolPortPair (wId : ConnectionId) (model : SheetT.Model) = 
    let wireMap = model.Wire.Wires
    let w = wireMap.[wId]
    
    // symbols
    let sym1 = getSourceSymbol model.Wire w
    let sym2 = getTargetSymbol model.Wire w

    // get PIds
    let sym1PId = OutputId w.OutputPort
    let sym2PId = InputId w.InputPort

    (sym1, sym1PId), (sym2, sym2PId)


// let putLabelsOnSheet (label : string) (model : SheetT.Model) (pos : XYPos) = 
//     let mod, compId = SymbolUpdate.

    


// helper functions
let calculatePosition (pPos : XYPos) (model: SymbolT.Model) (portId: SymbolT.PortId) =
    let edge = getPortOrientation model portId
    match edge with 
    | Right -> { X = pPos.X + 10.0; Y = pPos.Y }
    | Left -> { X = pPos.X - 10.0; Y = pPos.Y }
    | Top -> { X = pPos.X; Y = pPos.Y - 10.0 }
    | Bottom -> { X = pPos.X; Y = pPos.Y + 10.0 }

// let putWireLabel (model : SheetT.Model) (pos : XYPos) (labelName : string) = 
//     pos |> calculatePosition |> putLabelsOnSheet


// let addWLabelToPort (wireLabelName : string) (pId : SymbolT.PortId) (compId : ComponentId) (sourceWireLabel : bool) (model : SheetT.Model) = 
//     let pIdStr = getPortIdStr pId
//     let pPos = getPortPos pIdStr model.Wire.Symbol

//     let symOpt = model.Wire.Symbol.Symbols |> Map.tryFind compId

//     let portIdx = match symOpt with
//                     | Some sym -> 
//                         let ports = 
//                             if sourceWireLabel 
//                             then sym.Component.OutputPorts
//                             else sym.Component.InputPorts
//                         ports
//                         |> List.mapi (fun idx p -> (idx, p.Id))
//                         |> List.tryFind (fun (a, id) -> id = pIdStr)
//                         |> Option.map fst
//                         |> Option.get
//                     | None -> failwithf "Error"

//     let name = if sourceWireLabel
//                 then wireLabelName + string portIdx
//                 else wireLabelName
