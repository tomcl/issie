module SheetBeautifyD3
// open modules likely to be used

open CommonTypes
open DrawHelpers
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open Optic
open BlockHelpers
open System
open EEExtensions
open Symbol
open SymbolUpdate
open DrawModelType
open Helpers
open SheetT
let busWireModel_:Lens<SheetT.Model,BusWireT.Model> = wire_
let maxSheetCoord = Sheet.Constants.defaultCanvasSize
let symbolModel_ = SheetT.symbol_
type SymbolPort = { Label: string; PortNumber: int }
//some helper functions, also defined in tick3 testdrawblock
let getOkOrFail (res: Result<'a,string>) =
    match res with
    | Ok x -> x
    | Error mess ->
        failwithf "%s" mess

let caseInvariantEqual str1 str2 =
    String.toUpper str1 = String.toUpper str2

let extractIntOption (data: int option) =
    match data with
    | Some x -> 
        x
    | None ->
        0 // return default int value
/// constants used by SheetBeautify
module Constants =
     //arbitary number 
    () 

//work from ll3621 to make a start on team deliverable D3

//some helper functions

/// this function counts over a list of wires and outputs a Wire list from that list that are considered long
let findLongWires (wires: Wire List)=
    let longWireLen=50.
    wires
    |> List.map (fun wire -> (getWireLength wire),wire) //calculates the length for each wire
    |> List.filter( fun (wireLen,wire) -> wireLen> longWireLen) // filters out the short wires
    |> List.map ( fun tuple -> snd tuple)// returns a list of long wires


    


    
/// Place a new symbol with label symLabel onto the Sheet with given position.
        /// Return error if symLabel is not unique on sheet, or if position is outside allowed sheet coordinates (0 - maxSheetCoord).
        /// To be safe place components close to (maxSheetCoord/2.0, maxSheetCoord/2.0).
        /// symLabel - the component label, will be uppercased to make a standard label name
        /// compType - the type of the component
        /// position - the top-left corner of the symbol outline.
        /// model - the Sheet model into which the new symbol is added.
        /// function from tick3 testdrawblock and modified, added extra symbol output because we need it in our function
        /// added rotation
        /// might need to add position detection and same net detection
let placeSymbol (symLabel: string) (compType: ComponentType) (position: XYPos) (model: SheetT.Model)(rotation)  =
    let symLabel = String.toUpper symLabel // make label into its standard casing
    let symModel, symId = SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
    let sym:Symbol = symModel.Symbols[symId]
    let rotatedSym= writeSymbolRotationState rotation sym 
    let symModel = replaceSymbol symModel sym symId
    let res=
        match position + sym.getScaledDiagonal with
        | {X=x;Y=y} when x > maxSheetCoord || y > maxSheetCoord ->
            Error $"symbol '{symLabel}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
        | _ ->
            model
            |> Optic.set symbolModel_ symModel
            |> updateBoundingBoxes // could optimise this by only updating symId bounding boxes
            |> Ok
    sym,res
/// copied from TestDrawBlock with minor changes to make it work in this module
/// this function places the a new wire with the specified ports, or error if there is an exsisting wire
let placeWire (source: SymbolPort) (target: SymbolPort) (model: SheetT.Model) : Result<SheetT.Model,string> =
    let symbols = model.Wire.Symbol.Symbols
    let getPortId (portType:PortType) symPort =
        mapValues symbols
        |> Array.tryFind (fun sym -> caseInvariantEqual sym.Component.Label symPort.Label)
        |> function | Some x -> Ok x | None -> Error "Can't find symbol with label '{symPort.Label}'"
        |> Result.bind (fun sym ->
            match portType with
            | PortType.Input -> List.tryItem symPort.PortNumber sym.Component.InputPorts
            | PortType.Output -> List.tryItem symPort.PortNumber sym.Component.OutputPorts
            |> function | Some port -> Ok port.Id
                        | None -> Error $"Can't find {portType} port {symPort.PortNumber} on component {symPort.Label}")
    
    match getPortId PortType.Input target, getPortId PortType.Output source with
            | Error e, _ | _, Error e -> Error e
            | Ok inPort, Ok outPort ->
                let newWire = BusWireUpdate.makeNewWire (InputPortId inPort) (OutputPortId outPort) model.Wire
                if model.Wire.Wires |> Map.exists (fun wid wire -> wire.InputPort=newWire.InputPort && wire.OutputPort = newWire.OutputPort) then
                        // wire already exists
                        Error "Can't create wire from {source} to {target} because a wire already exists between those ports"
                else
                     {model with Wire={model.Wire with Wires= Map.add newWire.WId newWire model.Wire.Wires}}// optic did not work, so changed to this equivalent
                     |> Ok

///this functions does the following
///places 2 new IOlabel components on either end of the wire, connecting their original connections
///according to the edge that the port is in, rotate the symbol accordingly
/// removes the original long wire
/// to do: detect wether there is room for the symbol to be created
/// detect if there already exists a IOlabel for that input port (so that no multiple wire labels are the source end for same net wires)
let replaceWireWithWireLabels(wire:Wire, sheet:SheetT.Model)=


    // if isInput=true, then the label is acting as an input, else output
    //calculates the position of the newly added io labels
    let calcLabelPositionAndRotation (port:Port) sheet (portPos:XYPos) isInput =
        let a,b,height,width=getComponentProperties IOLabel "1"
        let orientation:Edge=getPortOrientationFrmPortIdStr  sheet.Wire.Symbol port.Id 
        // let portPos= port.
        let labelPos=
            match orientation with
            | Top -> {X= portPos.X ; Y=portPos.Y+1.5*height} 
            | Bottom-> {X= portPos.X ; Y=portPos.Y-1.5*height}
            | Left-> {X= portPos.X-1.5*width ; Y=portPos.Y}
            | Right ->{X= portPos.X+1.5*width ; Y=portPos.Y}
        let labelRotation=
            match orientation,isInput with
            | Top, true-> Degree90
            | Top, false-> Degree270
            | Bottom, true-> Degree270
            | Bottom, false-> Degree90
            | Left,_-> Degree0 //assuming all inputs are at the left and outputs are at right
            | Right,_ -> Degree0
        labelPos, labelRotation

    
    let (targetPort:Port)= getTargetPort sheet.Wire wire
    let sourcePort= getSourcePort sheet.Wire wire
    let targetPortPos= readPortPos  sheet targetPort // used B5R from helpers
    let sourcePortPos= readPortPos  sheet sourcePort
    let label= generateIOLabel sheet.Wire.Symbol IOLabel "I"
    let targetIOLabelPos, targetIORotation= calcLabelPositionAndRotation targetPort sheet targetPortPos true
    let sourceIOLabelPos, sourceIORotation=calcLabelPositionAndRotation sourcePort sheet sourcePortPos false

    //place IO label symbols and connect the ports of the newly added IO labels to the ports that the wire is connected tos
    let targetIOLabel= placeSymbol label IOLabel targetIOLabelPos sheet targetIORotation
    // let intermediateModel=(getOkOrFail(snd targetIOLabel))
    // let ModelwithRotatedTarget= intermediateModel with
    let targetIOLabelOutPort= (fst targetIOLabel).Component.OutputPorts.Head // only 1 outputport of IOLabel
    let placeTargetWire= 
        placeWire {Label=targetIOLabelOutPort.Id; PortNumber=(extractIntOption targetIOLabelOutPort.PortNumber)} {Label=targetPort.Id; PortNumber=(extractIntOption targetPort.PortNumber)}  sheet
   
    let sourceIOLabel = placeSymbol label IOLabel sourceIOLabelPos (getOkOrFail placeTargetWire) sourceIORotation
    let sourceIOLabelInPort=(fst sourceIOLabel).Component.InputPorts.Head
    let placeSourceWire= 
        placeWire {Label=sourcePort.Id; PortNumber=(extractIntOption sourcePort.PortNumber)} {Label=sourceIOLabelInPort.Id; PortNumber=(extractIntOption sourceIOLabelInPort.PortNumber)}  sheet
    
    let sheetWithWireLabels=getOkOrFail (placeSourceWire)
    {sheetWithWireLabels with Wire={sheetWithWireLabels.Wire with Wires = sheetWithWireLabels.Wire.Wires.Remove(wire.WId)}} // remove the wire



