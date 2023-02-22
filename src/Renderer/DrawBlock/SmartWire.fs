module SmartWire
open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers
open SmartHelpers
open SheetCreator
open SymbolUpdate
open DrawModelType

open Optics
open Operators


(* HLP23

    This module will normally be used exclusively by team member doing the "smart autoroute on single
    wire creation" part of the individual coding. During group phase work how it is used is up to the
    group. Functions from other members MUST be documented by "HLP23: AUTHOR" XML 
    comment as in SmartHelpers.

    It does not need specific "get started" test code since is called whenever a new wire is created 
    or (not usual) a symbol is moved so far that the topology of a wire chnages and it is autorouted 
    again from scratch.

*)


// define a D.U. for the different return types of the smart autoroute function
type SmartAutorouteResult =
    // | SmartAutorouteModel of Model
    // | SmartAutorouteWire of Wire
    | ModelT of Model
    | WireT of Wire


// helper function for finding matching symbol in model for given port ids on wire - current issue is finding the symbol with the matching input port id
let findInputSymbol (model: Model) (wire: Wire) : Symbol option = 
    let port = string wire.InputPort
    let symbolValues =
        model.Symbol.Symbols
        |> Map.toList
        |> List.map snd
    let symbolsWithPortId =
        symbolValues
        |> List.filter (fun symbol ->
            symbol.PortMaps.Orientation.ContainsKey(port))
        |> List.tryHead
    symbolsWithPortId

// helper function for finding matching symbol in model for given port ids on wire - current issue is finding the symbol with the matching input port id
let findOutputSymbol (model: Model) (wire: Wire) : Symbol option = 
    let port = string wire.OutputPort
    let symbolValues =
        model.Symbol.Symbols
        |> Map.toList
        |> List.map snd
    let symbolsWithPortId =
        symbolValues
        |> List.filter (fun symbol ->
            symbol.PortMaps.Orientation.ContainsKey(port))
        |> List.tryHead
    symbolsWithPortId


// helper function for finding if wire is connected from and to the same symbol
let isSelfConnected (model: Model) (wire: Wire) : bool = 
    let inputPort = string wire.InputPort
    let outputPort = string wire.OutputPort
    let symbolValues =
        model.Symbol.Symbols
        |> Map.toList
        |> List.map snd
    let symbolsWithPortId =
        symbolValues
        |> List.filter (fun symbol ->
            symbol.PortMaps.Orientation.ContainsKey(inputPort) && symbol.PortMaps.Orientation.ContainsKey(outputPort))
        |> List.tryHead
    match symbolsWithPortId with
        | Some symbol -> true
        | None -> false


/// returns the height needed to hug the symbol, if needed
/// MAKE IT WORK FOR VERTICAL HUGGING CASES
let huggingDistance (wire: Wire) (symbol: Symbol) : float = 
    let inputPort = string wire.InputPort
    let portPos = symbol.PortMaps.Orientation |> Map.find inputPort
    let boundaryBox = symbolBox symbol
    let hugDistance = (snd boundaryBox[3]) - wire.StartPos.Y
    match portPos with
        | Left -> hugDistance
        | Right -> hugDistance
        | _ -> 0.0


/// helper function that routes a wire from input port to output port around the symbol, rather than through it
let routeAroundSymbol (model: Model) (wire: Wire) (symbol: Symbol Option) : Wire = 
    let autoWire = autoroute model wire
    let selfConnected = isSelfConnected model wire
    let routing = 
        match selfConnected with
            | true -> 
                let symbolFound = symbol |> Option.get
                let hugLength = (huggingDistance wire symbolFound) + 15.0
                let newWires = 
                    let newOutputSegment: Segment = {wire.Segments[2] with Length =  wire.Segments[2].Length - 7.0}
                    let newInputSegment: Segment = {wire.Segments[6] with Length =  wire.Segments[6].Length + 7.0}
                    let newFirstSegment: Segment = {wire.Segments[3] with Length =  wire.Segments[3].Length + hugLength}
                    let newMiddleSegment: Segment = {wire.Segments[4] with Length =  wire.Segments[4].Length + 5.0}
                    let newThirdSegment: Segment = {wire.Segments[5] with Length =  wire.Segments[5].Length - hugLength}
                    let newRoute: Wire = {wire with Segments = [wire.Segments[0]; wire.Segments[1]; newOutputSegment; newFirstSegment;  newMiddleSegment; newThirdSegment; newInputSegment; wire.Segments[7]]}
                    newRoute 
                match hugLength with
                    | 15.0 -> wire
                    | _ -> newWires
            | false -> autoWire
    
    // // new model with routing wire added to wire map
    // let newModel: BusWireT.Model = 
    //     let newWireMap = 
    //         model.Wires
    //         |> Map.add wire.WId routing
    //     {model with Wires = newWireMap}
    // newModel
    routing


let generateWireLabels (model: Model) (wire: Wire) (symbol: Symbol) : SmartAutorouteResult = 
    let inputPort = wire.InputPort
    let outputPort = wire.OutputPort
    let inputPortSymbol = (findInputSymbol model wire) |> Option.get
    let outputPortSymbol = (findOutputSymbol model wire) |> Option.get
    let inputPortEdge = inputPortSymbol.PortMaps.Orientation |> Map.find (string inputPort)
    let outputPortEdge = outputPortSymbol.PortMaps.Orientation |> Map.find (string outputPort)
    // get port positions
    let outputPortPos, inputPortPos =
        Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)
    // create 2 wire label symbols to add to model
    let uuid: string = JSHelpers.uuid()
    let inputLabel: Symbol = 
        let inputLabelPos = 
            match inputPortEdge with
                | Left -> {X = inputPortPos.X - 10.0; Y = inputPortPos.Y}
                | Right -> {X = inputPortPos.X + 10.0; Y = inputPortPos.Y}
                | _ -> {X = inputPortPos.X - 10.0; Y = inputPortPos.Y}
        let inputLabelComp = createComponent IOLabel uuid
        {symbol with
            Id = ComponentId (JSHelpers.uuid())
            Component = inputLabelComp
            Pos = inputLabelPos
            Appearance = 
                {symbol.Appearance with
                    ShowPorts = ShowNone
                    // ShowOutputPorts = false 
            }
        }

    let outputLabel: Symbol = 
        let outputLabelPos = 
            match outputPortEdge with
                | Left -> {X = outputPortPos.X - 10.0; Y = outputPortPos.Y}
                | Right -> {X = outputPortPos.X + 10.0; Y = outputPortPos.Y}
                | _ -> {X = outputPortPos.X - 10.0; Y = outputPortPos.Y}
        let outputLabelComp = createComponent IOLabel uuid

        {symbol with
            Id = ComponentId uuid
            Component = outputLabelComp
            Pos = outputLabelPos
            Appearance = 
                {symbol.Appearance with
                    ShowPorts = ShowNone
                    // ShowOutputPorts = false
            }
        }
        // |> Symbol.autoScaleHAndW
    // [inputLabel; outputLabel]
    let newModel = {model with Symbol = {model.Symbol with Symbols = Map.add inputLabel.Id inputLabel model.Symbol.Symbols}}
    let newModel2 = {newModel with Symbol = {newModel.Symbol with Symbols = Map.add outputLabel.Id outputLabel newModel.Symbol.Symbols}}
    ModelT newModel2


    /// Creates and adds a symbol into model, returns the updated model and the component id
    // let addSymbol sym=
    //     // let newPorts = Symbol.addToPortModel model sym
    //     let addPortsToModel currModel _ sym =
    //         { currModel with Ports = Symbol.addToPortModel model sym }
    //     let newModel = addPortsToModel model

    //     let newSymModel = Map.add sym.Id sym newModel.Symbol.Symbols
    //     newSymModel
    

    // let symbols = createSymb
    // let updatedModel =
    //     symbols
    //     |> List.fold (fun model symbol -> addSymbol model symbol) model
    // updatedModel






 
    //create wires
    // let inputLabelWire = createWire inputLabel outputLabel inputLabelPos outputLabelPos
    // let outputLabelWire = createWire outputLabel inputLabel outputLabelPos inputLabelPos
    // call function to create wire labels at input port and output port
    

let findWire (model: Model) (connId: ConnectionId) : Option<Wire> =
    match model.Wires |> Map.toList |> List.tryFind (fun (_, wire) -> wire.Segments.[0].WireId = connId) with
    | Some (_, wire) -> Some wire
    | _ -> None



/// helper function that deletes wire from model by filtering out the wire
/// FIXME: Wire remains connected to input port when moving function away from threshold
let deleteWire (model: Model) (wire: Wire) : SmartAutorouteResult = 
    
    printfn "wire id: %A" wire.WId 
    
    let newWires =
        model.Wires
        |> Map.filter (fun id w -> not (wire.WId = id))
    let model =
        {model with Wires = newWires}

    printfn "updated model: %A" newWires
    ModelT model


// /// helper function that replaces long wire with wire labels at input port and output port
// let replaceLongWire (model: Model) (wire: Wire) (symbol: Symbol) : Model = 
//     let wireLength =  wire.Segments[2].Length
//     match wireLength with
//         | length when length > 200.0 -> 
//             // generateWireLabels model wire symbol
//             // let newWire = {wire with Segments = []}
//             // newWire
//             deleteWire model wire

//             // replace wire with wire labels at input port and output port
//             // let newModel: Model = generateWireLabels model wire symbol
//             // let inputLabel = newModel.Symbol.Symbols |> List.filter (fun x -> x.Component = IOLabel) |> List.head
//             // let outputLabel = newModel.Symbol.Symbols |> List.filter (fun x -> x.Component = IOLabel) |> List.last
//             // let inputLabelPos = inputLabel.Pos
//             // let outputLabelPos = outputLabel.Pos
//             // let inputLabelWire = createWire inputLabel outputLabel inputLabelPos outputLabelPos
//             // let outputLabelWire = createWire outputLabel inputLabel outputLabelPos inputLabelPos
//             // let newWires = [inputLabelWire; outputLabelWire]
//             // let newModel = updateModelWires newModel newWires
//             // let newWire = newModel.Wires |> List.filter (fun x -> x.InputPort = wire.InputPort && x.OutputPort = wire.OutputPort) |> List.head
//             // newWire

//         | _ -> model

            
    
/// top-level function which replaces autoupdate and implements a smarter version of same
/// it is called every time a new wire is created, so is easily tested.
let smartAutoroute (model: Model) (wire: Wire): SmartAutorouteResult =     
    let symbol = findInputSymbol model wire

    // let newModel = replaceLongWire model wire (symbol |> Option.get)
    let autoWire = autoroute model wire

    printfn "%s" $"Wire: Initial Orientation={wire.InitialOrientation}\nSegments={autoWire.Segments}"
    

    let wireLength = autoWire.Segments[4].Length
    match wireLength with
        | l when l > 200.0 -> 
            // delete wire
            let newWireMap = deleteWire model wire
            // add wire labels at posiitons near ports
            match newWireMap with
                | ModelT newModel -> 
                    // generate wire labels
                    let foundsymbol = symbol |> Option.get
                    generateWireLabels newModel wire foundsymbol // function is already type ModelT
                    // match newWireMap with
                    //     | ModelT newModel -> ModelT newModel
                    //     | _ -> ModelT newModel
                | _ -> 
                    printfn "error" 
                    newWireMap
            

        | _ -> WireT (routeAroundSymbol model autoWire symbol)



    // printfn "Symbol found: %A" symbol
    // printfn "%s" $"hugging distance={huggingDistance autoWire (symbol |> Option.get)}"
    // printfn "%s" $"WIRE START POS={wire.StartPos.Y}"

    // let selfConnected = isSelfConnected model wire
    // match selfConnected with
    //     | true -> routeAroundSymbol model autoWire symbol 
    //     | false -> autoWire
    
    // let newModel = replaceLongWire model wire (symbol |> Option.get)



// WIRE LABELS:
    // 1: DELETE WIRE 
    // 2: Place wire labels at appropriate position to input port and output port
    // 3: call function to create new wire (see if this works)
    // Change the output of autowire to return BusWire Model instead of Wire
    // Change the test code to use the new return of autowire 

