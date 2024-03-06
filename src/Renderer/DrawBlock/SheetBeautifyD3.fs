module SheetBeautifyD3

open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open CommonTypes
open ModelType
open DrawModelType
open SymbolUpdate
open BlockHelpers
open Symbol
open SheetBeautifyHelpers

/// Return a list of wires that are long which can potentially be replaced with wire labels
let getLongWires (wireLengthlimit: float) (wireList: List<BusWireT.Wire> )  = 
    wireList
    |> List.filter (fun wire -> (getWireLength wire > wireLengthlimit) )

/// Return a map of wires grouped by net (multiple wires with same source port).
/// and a list of single wire that is too long.
/// Both need wire labels generated.
let getWiresNeedLabels (sheet: SheetT.Model) (wireLengthlimit: float)=
    let wireInNet, singleWires = 
        getWireListFromSheet sheet
        |> List.groupBy (fun wire -> wire.InputPort)
        |> Map.ofList
        |> Map.partition (fun sourcePort wires -> wires.Length > 1)
    let longWires = 
        singleWires 
        |> Map.toList
        |> List.map snd
        |> List.collect id
        |> List.distinct
        |> getLongWires wireLengthlimit
    wireInNet, longWires

let checkRoomForWireLabel (sheet: SheetT.Model) (symbol : SymbolT.Symbol) = 
    failwithf "Not implemented"

/// generate wire label for a wire connected between source symbol and target symbol
/// or keep the wires if not enough room for label
let generateWireLabel (sheet: SheetT.Model) (wire: BusWireT.Wire) = 
    let startSym = getSourceSymbol sheet.Wire wire
    let endSym = getTargetSymbol sheet.Wire wire
    let inputPortID, outputPortID = wire.InputPort, wire.OutputPort
    // wire label should be at the same level as the port they connect to
    let wireLabelInputPos = getPortPos (string inputPortID )startSym
    let wireLabelOutputPos = getPortPos (string outputPortID ) endSym

    // delete old wires
    // work out what to do next
    
    match checkRoomForWireLabel sheet startSym, checkRoomForWireLabel sheet endSym with
    | Some room1, Some room2 -> failwithf "generate wire symbol not implemented"
    | _, _ -> failwithf "can't generate label, no room"

//Can use this to generate wire label

    // | AddNotConnected (ldcs, port, pos, rotation) ->
    //     let (newSymModel, ncID) = SymbolUpdate.addSymbol ldcs model.Wire.Symbol pos NotConnected ""
    //     let ncPortId = newSymModel.Symbols[ncID].Component.InputPorts[0].Id
    //     // add a newly created wire to the model
    //     // then send BusWidths message which will re-infer bus widths
    //     // the new wires (extarcted as connections) are not added back into Issie model. 
    //     // This happens on save or when starting a simulation (I think)
    //     let newWireModel, msgOpt = BusWireUpdate.newWire (InputPortId ncPortId) (OutputPortId port.Id) {model.Wire with Symbol = newSymModel}
    //     let wCmd = if msgOpt.IsSome then wireCmd (Option.get msgOpt) else Cmd.none
            
    //     {model with Wire = newWireModel}, Cmd.batch [wCmd;
    //                                 symbolCmd (RotateAntiClockAng ([ncID], rotation));
    //                                 wireCmd (UpdateConnectedWires [ncID]);
    //                                 sheetCmd (UpdateBoundingBoxes)]

let autoGenerateWireLabels (sheet: SheetT.Model) = 
    let limit = 10 // need clarify how long a wire is considered too long
    let wireInNet, longWires = getWiresNeedLabels sheet limit
    let updateWireInNet = 
        wireInNet
        |> Map.map (fun port wireList -> 
            wireList
            |> List.map (generateWireLabel sheet))
    let updatedLongWires = 
        longWires
        |> List.map (generateWireLabel sheet)
    failwithf "Not implemented"