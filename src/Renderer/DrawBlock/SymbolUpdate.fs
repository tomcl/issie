module SymbolUpdate

open Elmish
open DrawHelpers
open CommonTypes
open Fable.React
open System.Text.RegularExpressions
open DrawModelType
open DrawModelType.SymbolT
open Symbol
open Optics
open Operators

//--------------------- GENERATING LABEL FUNCTIONS-------------------------------

/// Returns the number of the component label (i.e. the number 1 from IN1 or ADDER16.1)
let getLabelNumber (str : string) = 
    let index = Regex.Match(str, @"\d+$")
    match index with
    | null -> 0
    | _ -> int index.Value

/// Generates the label number for compType (i.e. the number 1 in IN1 or ADDER16.1) in a string format
let generateLabelNumber listSymbols compType =
    let samePrefix (target: ComponentType) (symbol: Symbol) : bool =
        let compType = symbol.Component.Type
        (getPrefix target) = (getPrefix compType)

    let samePrefixLst = 
        listSymbols
        |> List.filter (samePrefix compType)

    match compType with
    | MergeWires | SplitWire _ -> ""
    | _ ->
        if List.isEmpty samePrefixLst then 1 
        else samePrefixLst
            |> List.map (fun sym -> getLabelNumber sym.Component.Label)
            |> List.max
            |> (+) 1
        |> string

/// Generates the label for a component type
let generateLabel (model: Model) (compType: ComponentType) : string =
    let listSymbols = List.map snd (Map.toList model.Symbols) 
    let prefix = getPrefix compType
    match compType with
    | IOLabel | BusSelection _ -> prefix
    | _ -> prefix + (generateLabelNumber listSymbols compType)

/// Initialises and returns the PortMaps of a pasted symbol
let initCopiedPorts (oldSymbol:Symbol) (newComp: Component): PortMaps =
    let inPortIds = List.map (fun (p:Port) -> p.Id)  newComp.InputPorts
    let outPortIds = List.map (fun (p:Port) -> p.Id) newComp.OutputPorts
    let oldInPortIds =  
        List.map (fun (p:Port) -> p.Id) oldSymbol.Component.InputPorts
    let oldOutPortIds =
        List.map (fun (p:Port) -> p.Id) oldSymbol.Component.OutputPorts
    let equivPortIds = 
        List.zip oldInPortIds inPortIds @ List.zip oldOutPortIds outPortIds
        |> Map.ofList
    let portOrientation = 
        (Map.empty,oldSymbol.PortMaps.Orientation)
        ||> Map.fold 
            (fun currMap oldPortId edge -> Map.add equivPortIds[oldPortId] edge currMap)

    let emptyPortOrder = 
        (Map.empty, [Edge.Top; Edge.Bottom; Edge.Left; Edge.Right])
        ||> List.fold (fun currMap edge -> Map.add edge [] currMap)
    let portOrder =
        (emptyPortOrder, oldSymbol.PortMaps.Order)
        ||> Map.fold 
            (fun currMap side oldList -> 
                let newList =
                    ([], oldList)
                    ||> List.fold 
                        (fun currList oldPortId ->
                            currList @ [equivPortIds[oldPortId]])
                Map.add side newList currMap)
    {Order=portOrder; Orientation=portOrientation}


/// Interface function to paste symbols. Is a function instead of a message because we want an output.
/// Currently drag-and-drop.
/// Pastes a list of symbols into the model and returns the new model and the id of the pasted modules.
let pasteSymbols (model: Model) (newBasePos: XYPos) : (Model * ComponentId list) =
    let addNewSymbol (basePos: XYPos) ((currSymbolModel, pastedIdsList) : Model * ComponentId List) (oldSymbol: Symbol): Model * ComponentId List =
        let newId = JSHelpers.uuid()
        let newPos = oldSymbol.Pos - basePos + newBasePos
        let compType = oldSymbol.Component.Type
        let newLabel = 
            compType
            |> generateLabel { model with Symbols = currSymbolModel.Symbols}

        let newComp = makeComponent newPos compType newId newLabel
       
        let newSymbol =
            { oldSymbol with
                Id = ComponentId newId
                Component = newComp
                Pos = newPos
                Appearance = 
                    {oldSymbol.Appearance with
                        ShowPorts = ShowNone
                        // ShowOutputPorts = false
                }
                PortMaps = initCopiedPorts oldSymbol newComp
                LabelHasDefaultPos = true
            }
            |> Symbol.autoScaleHAndW

        
             
        let newSymbolMap = currSymbolModel.Symbols.Add (ComponentId newId, newSymbol)
        let newPorts = addToPortModel currSymbolModel newSymbol
        let newModel = { currSymbolModel with Symbols = newSymbolMap; Ports = newPorts }
        let newPastedIdsList = pastedIdsList @ [ newSymbol.Id ]
        newModel, newPastedIdsList
        
    let oldSymbolsList =
        model.CopiedSymbols
        |> Map.toList
        |> List.map snd

    match oldSymbolsList with
    | [] -> model, []
    | _ -> 
        let baseSymbol = List.minBy (fun sym -> sym.Pos.X) oldSymbolsList
        let basePos = baseSymbol.Pos + { X = (float baseSymbol.Component.W) / 2.0; Y = (float baseSymbol.Component.H) / 2.0 }
        ((model, []), oldSymbolsList) ||> List.fold (addNewSymbol basePos)
 
/// Returns the hostId of the port in model
let getPortHostId (model: Model) portId =
   model.Ports[portId].HostId

/// Tries to find the target in copiedIds, and tries to return the item at the same index in pastedIds.
/// Returns Some if there is exactly one element in copiedIds matching the target AND if there is an element in pastedIds at that same index, None otherwise.
let tryGetPastedEl copiedIds pastedIds target =
    // try to look for a symbol in copiedIds, get the index and return pastedIds[index]
    let indexedTarget = 
        copiedIds
        |> List.indexed
        |> List.filter (fun (_, id) -> id = target)
        |> List.tryExactlyOne
    match indexedTarget with
    | Some (index, _) -> List.tryItem index pastedIds
    | _ -> None

/// Returns a tuple of the list of input ports of a given input symbol, and list of output ports of a given output symbol
let getPortIds (input: Symbol) (output: Symbol) : (string list * string list)=
    let inPortIds = 
        input.Component.InputPorts
        |> List.map (fun port -> port.Id)
    let outPortIds =
        output.Component.OutputPorts
        |> List.map (fun port -> port.Id)
    inPortIds, outPortIds

/// Given a tuple of options, returns an Some (v1, v2) if both tuple elements are some, else None
let mergeOptions =
    function
    | Some v1, Some v2 -> Some (v1, v2)
    | _ -> None

/// Returns the symbol containing the given portId in the model's CopiedSymbols map
let getCopiedSymbol model portId =
    let symbolId = getPortHostId model portId
    model.CopiedSymbols[ComponentId symbolId]

/// Given two componentId list of same length and input / output ports that are in list 1, return the equivalent ports in list 2.
/// ComponentIds at same index in both list 1 and list 2 need to be of the same ComponentType.
/// CompIds1 need to be in model.CopiedSymbols.
/// Assumes ports are in the same order in equivalent symbols
let getEquivalentCopiedPorts (model: Model) (copiedIds) (pastedIds) (InputPortId copiedInputPort, OutputPortId copiedOutputPort) =
    let findEquivalentPorts compId1 compId2 =
        let copiedComponent = model.CopiedSymbols[compId1].Component
        let pastedComponent = model.Symbols[compId2].Component // TODO: These can be different for an output gate for some reason.
        
        let tryFindEquivalentPort (copiedPorts: Port list) (pastedPorts: Port list) targetPort =
            if copiedPorts.Length = 0 || pastedPorts.Length = 0
            then None
            else
                match List.tryFindIndex ( fun (port: Port) -> port.Id = targetPort ) copiedPorts with
                | Some portIndex -> 

                    Some pastedPorts[portIndex].Id // Get the equivalent port in pastedPorts. Assumes ports at the same index are the same (should be the case unless copy pasting went wrong).
                | _ -> None
        
        let pastedInputPortId = tryFindEquivalentPort copiedComponent.InputPorts pastedComponent.InputPorts copiedInputPort
        let pastedOutputPortId = tryFindEquivalentPort copiedComponent.OutputPorts pastedComponent.OutputPorts copiedOutputPort
    
        pastedInputPortId, pastedOutputPortId
        
    let foundPastedPorts =
        List.zip copiedIds pastedIds
        |> List.map (fun (compId1, compId2) -> findEquivalentPorts compId1 compId2)
    
    let foundPastedInputPort = List.collect (function | Some a, _ -> [a] | _ -> []) foundPastedPorts
    let foundPastedOutputPort = List.collect (function | _, Some b -> [b] | _ -> []) foundPastedPorts
    
    match foundPastedInputPort, foundPastedOutputPort with 
    | [pastedInputPort], [pastedOutputPort] -> Some (pastedInputPort, pastedOutputPort) 
    | _ -> None // If either of source or target component of the wire was not copied then we discard the wire

/// Creates and adds a symbol into model, returns the updated model and the component id
let addSymbol (ldcs: LoadedComponent list) (model: Model) pos compType lbl =
    let newSym = createNewSymbol ldcs pos compType lbl
    let newPorts = addToPortModel model newSym
    let newSymModel = Map.add newSym.Id newSym model.Symbols
    { model with Symbols = newSymModel; Ports = newPorts }, newSym.Id

/// Helper function to change the number of bits expected in a port of each component type.
let changeNumberOfBitsf (symModel:Model) (compId:ComponentId) (newBits : int) =
    let symbol = Map.find compId symModel.Symbols

    let newcompotype = 
        match symbol.Component.Type with
        | Input (_, defaultVal) -> Input (newBits, defaultVal)
        | Output _ -> Output newBits
        | Viewer _ -> Viewer newBits
        | NbitsAdder _ -> NbitsAdder newBits
        | NbitsXor _ -> NbitsXor newBits
        | Register _ -> Register newBits
        | RegisterE _ -> RegisterE newBits
        | SplitWire _ -> SplitWire newBits
        | BusSelection (_,b) -> BusSelection (newBits,b)
        | BusCompare (_,b) -> BusCompare (newBits,b)
        | Constant1 (_,b,txt) -> Constant1 (newBits,b,txt)
        | c -> c

    let newcompo = {symbol.Component with Type = newcompotype}
    {symbol with Component = newcompo}

/// Helper function to change the number of bits expected in the LSB port of BusSelection and BusCompare
let changeLsbf (symModel:Model) (compId:ComponentId) (newLsb:int64) =
    let symbol = Map.find compId symModel.Symbols

    let newcompotype = 
        match symbol.Component.Type with
        | BusSelection (w, _) -> BusSelection (w, int32(newLsb))
        | BusCompare (w, _) -> BusCompare (w, uint32(newLsb)) 
        | Constant1(w, _,txt) -> Constant1 (w, newLsb,txt)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"

    let newcompo = {symbol.Component with Type = newcompotype}
    {symbol with Component = newcompo}

let changeInputValue (symModel: Model) (compId: ComponentId) (newVal: int) =
    let symbol = Map.find compId symModel.Symbols
    let width =
        match symbol.Component.Type with
        | Input (width, _) -> width
        | _ -> failwithf "changeInputValue should only be called for Input components"

    let newComp = {symbol.Component with Type = Input (width, Some newVal)}
    {symbol with Component = newComp}

/// Updates the value of a constant1 component and returns the updated symbol
let changeConstantf (symModel:Model) (compId:ComponentId) (constantVal:int64) (constantText: string) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Component.Type with
        | Constant1 (w, _, _) -> Constant1 (w, constantVal,constantText)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    let newcompo = {symbol.Component with Type = newcompotype}
    printfn "Changing symbol to: %A" newcompotype
    {symbol with Component = newcompo}

//---------------------Helper functions for the upadte function------------------------------//


/// Given a model and a list of component ids deletes the specified components from the model and returns the updated model
let inline deleteSymbols (model: Model) compIds =
    let newSymbols = 
        (model.Symbols, compIds)
        ||> List.fold (fun prevModel sId -> Map.remove sId prevModel) 
    { model with Symbols = newSymbols }

/// Given a model and a list of component ids copies the specified components and returns the updated model
let copySymbols (model: Model) compIds =
    let copiedSymbols = 
        model.Symbols
        |> Map.filter (fun compId _ -> List.contains compId compIds) 

    { model with CopiedSymbols = copiedSymbols }




/// Given a model it shows all input ports and hides all output ports, then returns the updated model
let inline showAllInputPorts (model: Model) =
    let showSymbolInPorts _ sym = 
        Optic.map appearance_ (fun app -> {app with ShowPorts = ShowInput}) sym 

    let newSymbols = 
        model.Symbols
        |> Map.map showSymbolInPorts

    { model with Symbols = newSymbols }

/// Given a model it shows all output ports and hides all input ports, then returns the updated model
let inline showAllOutputPorts (model: Model) =
    let showSymbolOutPorts _ sym = 
        Optic.map appearance_ (fun app -> {app with ShowPorts = ShowOutput}) sym

    let newSymbols = 
        model.Symbols
        |> Map.map showSymbolOutPorts

    { model with Symbols = newSymbols }

/// Given a model it shows all ports of custom components and hides all other ports, then returns the updated model
let inline showAllCustomPorts (model: Model) =
    let showSymbolOutPorts _ sym = 
        Optic.map appearance_ (fun app -> {app with ShowPorts = ShowBothForPortMovement}) sym

    let newSymbols = 
        model.Symbols
        |> Map.map showSymbolOutPorts

    { model with Symbols = newSymbols }

/// Given a model it hides all ports and returns the updated model
let inline deleteAllPorts (model: Model) =
    let hideSymbolPorts _ sym = 
        Optic.map appearance_ (fun app -> {app with ShowPorts = ShowNone}) sym

    let updatedSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    { model with Symbols = updatedSymbols}

/// Given a model it shows all the specified components' ports and hides all the other ones
let inline showPorts (model: Model) compList =
    let hideSymbolPorts _ sym =
        Optic.map appearance_ (fun app -> {app with ShowPorts = ShowNone}) sym

    let showSymbolPorts sym =
        Optic.map appearance_ (fun app -> {app with ShowPorts = ShowBoth}) sym

    let resetSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    let addUpdatedSymbol prevSymbols sId =
        match Map.containsKey sId resetSymbols with
        | false -> prevSymbols
        | true ->
            prevSymbols |>
            Map.add sId (showSymbolPorts resetSymbols[sId])

    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold addUpdatedSymbol

    { model with Symbols = newSymbols }

/// Given a model it shows only the custom components of all the specified components' ports and hides all the other ones
/// Different from the above (only custom components).
let inline showCustomPorts (model: Model) compList =
    let hideSymbolPorts _ sym =
        Optic.map appearance_ (fun app -> {app with ShowPorts = ShowNone}) sym

    let showSymbolPorts sym =
        Optic.map appearance_ (fun app -> {app with ShowPorts = ShowBothForPortMovement}) sym

    let resetSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    let addUpdatedSymbol prevSymbols sId =
        match resetSymbols[sId].Component.Type with
        | Custom _ -> prevSymbols |> Map.add sId (showSymbolPorts resetSymbols[sId])
        | _ -> prevSymbols
    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold addUpdatedSymbol

    { model with Symbols = newSymbols }
    
/// Move a symbol by the amount specified by move
let private moveSymbol (move: XYPos) (sym: Symbol) : Symbol =
    {sym with
        Moving = true
        Pos = sym.Pos + move
        Component = {sym.Component with
                        X = sym.Component.X + move.X
                        Y = sym.Component.Y + move.Y
                    }
        LabelBoundingBox = {sym.LabelBoundingBox with
                                TopLeft =  sym.LabelBoundingBox.TopLeft + move}
    }


/// Given a model, a component id list and an offset, moves the components by offset and returns the updated model
let moveSymbols (model:Model) (compList: ComponentId list) (offset: XYPos)=
    let resetSymbols = 
        model.Symbols
        |> Map.map (fun _ sym -> { sym with Moving = false}) 

    let moveSymbolInMap prevSymbols sId =
        prevSymbols
        |> Map.add sId (moveSymbol offset model.Symbols[sId] )


    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold moveSymbolInMap

    { model with Symbols = newSymbols }

/// Given a model and a component id list, sets the color of the sepcified symbols to red and every other symbol's color to gray
let inline symbolsHaveError model compList =
    let resetSymbols = 
        model.Symbols
        |> Map.map (fun _ sym -> Optic.set (appearance_ >-> colour_) "lightgray" sym)

    let setSymColorToRed prevSymbols sId =
        Map.add sId (Optic.set (appearance_ >-> colour_)  "Red" resetSymbols[sId]) prevSymbols

    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold setSymColorToRed 
    { model with Symbols = newSymbols }

/// Given a model and a component id list, it updates the specified symbols' colour to green with max opacity, and every other symbols' colour to gray
let inline selectSymbols model compList =
    let resetSymbols = 
        model.Symbols
        |> Map.map (fun _ sym -> 
            Optic.map appearance_ (Optic.set colour_ "lightgray" >> Optic.set opacity_ 1.0 ) sym)

    let updateSymbolColour prevSymbols sId =
        Map.add sId (Optic.set (appearance_ >-> colour_)  "lightgreen" resetSymbols[sId]) prevSymbols
    
    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold updateSymbolColour 

    { model with Symbols = newSymbols}

/// Given a model, an error component list, a selected component id list, it updates the selected symbols' color to green if they are not selected, and changes the symbols with errors to red. It returns the updated model.
let inline errorSymbols model (errorCompList,selectCompList,isDragAndDrop) =
    let resetSymbols = 
        model.Symbols
        |> Map.map 
            (fun _ sym ->  Optic.map appearance_ (Optic.set colour_ "lightgray" >> Optic.set opacity_ 1.0) sym)
            
    let updateSymbolStyle prevSymbols sId =
        if not isDragAndDrop then 
            Map.add sId (Optic.set (appearance_ >-> colour_) "lightgreen" resetSymbols[sId]) prevSymbols
        else 
            Map.add sId (Optic.set (appearance_ >-> opacity_) 0.2 resetSymbols[sId]) prevSymbols

    let selectSymbols =
        (resetSymbols, selectCompList)
        ||> List.fold updateSymbolStyle 

    let setSymColourToRed prevSymbols sId =
        Map.add sId (Optic.set (appearance_ >-> colour_) "Red" resetSymbols[sId]) prevSymbols

    let newSymbols = 
        (selectSymbols, errorCompList)
        ||> List.fold setSymColourToRed
        
    { model with Symbols = newSymbols }

/// Given a model, a symbol id and a new label changes the label of the symbol to the new label and returns the updated model.
let inline changeLabel (model: Model) sId newLabel=
    let oldSym = model.Symbols[sId]
    let newComp = {oldSym.Component with Label = newLabel}
    let newSym = 
        { oldSym with Component = newComp; LabelHasDefaultPos = true}
        |> calcLabelBoundingBox
    Optic.set (symbolOf_ sId) newSym model


/// Given a model, a component id list and a color, updates the color of the specified symbols and returns the updated model.
let inline colorSymbols (model: Model) compList colour =
    let changeSymColour (prevSymbols: Map<ComponentId, Symbol>) (sId: ComponentId) =
        let newSymbol = Optic.set (appearance_ >-> colour_) (string colour) prevSymbols[sId] 
        prevSymbols |> Map.add sId newSymbol

    let newSymbols =
        (model.Symbols, compList)
        ||> List.fold changeSymColour

    { model with Symbols = newSymbols }

/// Given a map of current symbols and a component, initialises a symbol containing the component and returns the updated symbol map containing the new symbol
let createSymbol ldcs prevSymbols comp =
        let clocked = isClocked [] ldcs comp
        let portMaps = 
            match comp.SymbolInfo with
                | None -> 
                    initPortOrientation comp
                | Some info -> 
                    {Order=info.PortOrder; Orientation=info.PortOrientation}
        let xyPos = {X = comp.X; Y = comp.Y}
        let (h,w) =
            if comp.H = -1 && comp.W = -1 then
                printfn $"Weird component {comp.Label}"
                let comp' = makeComponent xyPos comp.Type comp.Id comp.Label
                comp'.H,comp'.W
            else
                // recompute height and width on load in case component have changed.
                let (_, _, height, width) = getComponentProperties comp.Type comp.Label
                height,width
        let hasDefault, labelBoundingBox =
            match comp.SymbolInfo with
            | Some {LabelBoundingBox=Some info} ->  false, info
            | _ -> true, {TopLeft=xyPos; W=0.;H=0.}
        let newSymbol =
            { 
                Pos = xyPos
                LabelHasDefaultPos = hasDefault
                LabelBoundingBox = labelBoundingBox
                LabelRotation = comp.SymbolInfo |> Option.bind (fun info -> info.LabelRotation)
                Appearance = {
                    HighlightLabel = false
                    ShowPorts = ShowNone //do not show input ports initially
                    // ShowOutputPorts = false //do not show output ports initially
                    Colour = "lightgray"     // initial color 
                    Opacity = 1.0
                }
                Id = ComponentId comp.Id
                Component = {comp with H=h ; W = w}
                
                Moving = false
                InWidth0 = None
                InWidth1 = None
                STransform = getSTransformWithDefault comp.SymbolInfo
                PortMaps = portMaps
                
                MovingPort = None
                IsClocked = clocked
                MovingPortTarget = None
            }
            |> autoScaleHAndW
            |> calcLabelBoundingBox
        prevSymbols
        |> Map.add (ComponentId comp.Id) newSymbol
 

/// Given a model and a list of components, it creates and adds the symbols containing the specified components and returns the updated model.
let loadComponents loadedComponents model comps=
    let symbolMap =
        (model.Symbols, comps) ||> List.fold (createSymbol loadedComponents)
    let addPortsToModel currModel _ sym =
        { currModel with Ports = addToPortModel currModel sym }
        
    let newModel = ( model, symbolMap ) ||> Map.fold addPortsToModel

    { newModel with Symbols = symbolMap }

/// Given a model, a component id, an address and a value it updates the data in the component and returns the new model.
let inline writeMemoryLine model (compId, addr, value) =
    let symbol = model.Symbols[compId]
    let comp = symbol.Component

    let newCompType =
        match comp.Type with
        | RAM1 mem -> RAM1 { mem with Data = Map.add addr value mem.Data }
        | AsyncRAM1 mem -> AsyncRAM1 { mem with Data = Map.add addr value mem.Data }
        | ROM1 mem -> ROM1 { mem with Data = Map.add addr value mem.Data }
        | AsyncROM1 mem -> AsyncROM1 { mem with Data = Map.add addr value mem.Data }
        | _ -> comp.Type

    let newComp = { comp with Type = newCompType }
    
    let newSymbols = Map.add compId { symbol with Component = newComp } model.Symbols
    
    { model with Symbols = newSymbols }

/// Given a model, a component Id and a memory component type, updates the type of the component to the specified memory type and returns the updated model.
let inline writeMemoryType model compId memory =
    let symbol = model.Symbols[compId]
    let comp = symbol.Component 
    
    let newCompType =
        match comp.Type with
        | RAM1 _ | AsyncRAM1 _ | ROM1 _ | AsyncROM1 _ -> memory
        | _ -> 
            printfn $"Warning: improper use of WriteMemoryType on {comp} ignored"
            comp.Type
    
    let newComp = { comp with Type = newCompType }
    
    Optic.set (symbolOf_ compId >-> component_) newComp model

let rotateSide (rotation: RotationType) (side:Edge) :Edge =
    match rotation, side with
    | RotateAntiClockwise, Top -> Left
    | RotateAntiClockwise, Left -> Bottom
    | RotateAntiClockwise, Bottom -> Right
    | RotateAntiClockwise, Right -> Top
    | RotateClockwise, Top -> Right
    | RotateClockwise, Left -> Top
    | RotateClockwise, Bottom -> Left
    | RotateClockwise, Right -> Bottom


/// return a new orientation based on old one and a rotation
let rotateAngle (rot: RotationType) (rotation: Rotation) : Rotation =
    match rot, rotation with
    | RotateAntiClockwise, Degree0 -> Degree90
    | RotateAntiClockwise, Degree90 -> Degree180
    | RotateAntiClockwise, Degree180 -> Degree270
    | RotateAntiClockwise, Degree270 -> Degree0
    | RotateClockwise, Degree0 -> Degree270
    | RotateClockwise, Degree90 -> Degree0
    | RotateClockwise, Degree180 -> Degree90
    | RotateClockwise, Degree270 -> Degree180

/// rotates the portMap information left or right as per rotation
let rotatePortInfo (rotation:RotationType) (portMaps:PortMaps) : PortMaps=
    //need to update portOrientation and portOrder
    let newPortOrientation = 
        portMaps.Orientation |> Map.map (fun id side -> rotateSide rotation side)

    let rotatePortList currPortOrder side =
        currPortOrder |> Map.add (rotateSide rotation side) portMaps.Order[side]

    let newPortOrder = 
        (Map.empty, [Top; Left; Bottom; Right]) ||> List.fold rotatePortList
    {Orientation= newPortOrientation; Order = newPortOrder}

let adjustPosForRotation 
        (rotation:RotationType) 
        (h: float)
        (w:float)
        (pos: XYPos)
         : XYPos =
    let posOffset =
        match rotation with
        | RotateClockwise -> { X = (float)w/2.0 - (float) h/2.0 ;Y = (float) h/2.0 - (float)w/2.0 }
        | RotateAntiClockwise -> { X = (float)w/2.0 - (float) h/2.0 ;Y = (float) h/2.0 - (float)w/2.0 }
    pos + posOffset


/// Takes a symbol in and returns the same symbol rotated left or right
let rotateSymbol (rotation: RotationType) (sym: Symbol) : Symbol =
    // update comp w h
    match sym.Component.Type with
    | Custom _->
        let portMaps = rotatePortInfo rotation sym.PortMaps
        let getHW (sym:Symbol) = {X=sym.Component.W;Y=sym.Component.H}
        let sym' =
            {sym with PortMaps = portMaps}
            |> autoScaleHAndW
        {sym' with Pos = sym.Pos + (getHW sym - getHW sym') * 0.5}
        
    | _ ->
        let h,w = getRotatedHAndW sym

        let newPos = adjustPosForRotation rotation h w sym.Pos
        let newComponent = { sym.Component with X = newPos.X; Y = newPos.Y}

        let newSTransform = 
            match sym.STransform.flipped with
            | true -> 
                {sym.STransform with Rotation = rotateAngle (invertRotation rotation) sym.STransform.Rotation} // hack for rotating when flipped 
            | false -> 
                {sym.STransform with Rotation = rotateAngle rotation sym.STransform.Rotation}
        { sym with 
            Pos = newPos;
            PortMaps = rotatePortInfo rotation sym.PortMaps
            STransform =newSTransform 
            LabelHasDefaultPos = true
            Component = newComponent
        } |> calcLabelBoundingBox



/// Flips a side horizontally
let flipSideHorizontal (edge: Edge) : Edge =
    match edge with
    | Left | Right ->
        edge
        |> rotateSide RotateClockwise
        |> rotateSide RotateClockwise
    | _ -> edge

/// Takes in a symbol and returns the same symbol flipped
let flipSymbol (orientation: FlipType) (sym:Symbol) : Symbol =
    let portOrientation = 
        sym.PortMaps.Orientation |> Map.map (fun id side -> flipSideHorizontal side)

    let flipPortList currPortOrder side =
        currPortOrder |> Map.add (flipSideHorizontal side ) sym.PortMaps.Order[side]

    let portOrder = 
        (Map.empty, [Top; Left; Bottom; Right]) ||> List.fold flipPortList
        |> Map.map (fun edge order -> List.rev order)       

    let newSTransform = 
        {flipped= not sym.STransform.flipped;
        Rotation= sym.STransform.Rotation} 

    { sym with
        PortMaps = {Order=portOrder;Orientation=portOrientation}
        STransform = newSTransform
        LabelHasDefaultPos = true
    }
    |> calcLabelBoundingBox
    |> (fun sym -> 
        match orientation with
        | FlipHorizontal -> sym
        | FlipVertical -> 
            sym
            |> rotateSymbol RotateAntiClockwise
            |> rotateSymbol RotateAntiClockwise)

type Rectangle = {TopLeft: XYPos; BottomRight: XYPos}

let inline getX (pos: XYPos) =
    pos.X
let inline getY (pos: XYPos) =
    pos.Y

/// Checks if 2 rectangles intersect
let rectanglesIntersect (rect1: Rectangle) (rect2: Rectangle) =
    /// Checks if there is an intersection in the X or Y dimension
    let intersect1D (xOrY: XYPos -> float): bool =
        let qHi = min (xOrY rect1.BottomRight) (xOrY rect2.BottomRight)
        let qLo = max (xOrY rect1.TopLeft) (xOrY rect2.TopLeft)
        qLo <= qHi

    (intersect1D getX) && (intersect1D getY)

let moveCustomPortsPopup() : ReactElement =
    div [] [
        str "Normal components can be rotated and flipped to change port orientation, \
              however port positions cannot be changed."
        br []
        str "Custom components (sheets inserted as components) can have ports moved \
              to any side of the symbol and reordered."
        br []
        str "To move custom component ports press CTRL and use a mouse to drag \
              a port to another position on the outline of the symbol. \
              You can reorder ports and place them on any symbol edge including top and bottom."
        br []
        str "Note that the symbol will resize itself if you change the edge of a port"

    ]

   
/// Returns an Option Edge. Returns Some edge if position is on edge of Symbol, and None if it was not on an edge
/// Separates the symbol as shown below where the two triangles have height = 0.3*symbolHeight
// |-----------|
// |\   TOP   /|
// |  \     /  |       
// |    \ /    |
// |LEFT |     |
// |     |RIGHT|
// |    / \    |
// |  /     \  |
// |/ BOTTOM  \|
// |-----------|
let getCloseByEdge (sym:Symbol) (mousePos:XYPos) : Option<Edge> =
    let h,w = getRotatedHAndW sym
    let triangleCorner = {X=w/2.;Y=h*0.3}
    let tanTheta = triangleCorner.Y/triangleCorner.X 
    let mouseOffset = mousePos - sym.Pos
    let minX = min (abs mouseOffset.X) (abs w-mouseOffset.X)
    let outMargin = 60.  //how many pixels outside the symbol the port can be when moving it
    
    // Top Edge
    if ((-outMargin <= mouseOffset.Y) && (mouseOffset.Y <= (minX*tanTheta)) && ((-outMargin <= mouseOffset.X)) && ((w+outMargin) >= mouseOffset.X)) then Some Top
    // Bottom Edge
    elif (((h+outMargin) >= mouseOffset.Y) && (mouseOffset.Y >= (h - minX*tanTheta)) && ((-outMargin <= mouseOffset.X)) && ((w+outMargin) >= mouseOffset.X)) then Some Bottom
    // Away from symbol -> None
    elif ((-outMargin >= mouseOffset.Y) ||  ((h+outMargin) <= mouseOffset.Y) || (-outMargin >= mouseOffset.X) ||  ((w+outMargin) <= mouseOffset.X)) then None
    // Left Edge
    elif (-outMargin <= mouseOffset.X) && (mouseOffset.X <= (w/2.0)) then Some Left
    // Right Edge
    elif ((w+outMargin) >= mouseOffset.X) && (mouseOffset.X > (w/2.0)) then Some Right
    else None

///Given a symbol and a port, it returns the offset of the port from the top left corner of the symbol
let getPosIndex (sym: Symbol) (pos: XYPos) (edge: Edge): int =
    let ports = sym.PortMaps.Order[edge] //list of ports on the same side as port
    //let index = float( List.findIndex (fun (p:string)  -> p = port.Id) ports ) need to find index
    let gap = getPortPosEdgeGap sym.Component.Type 
    let baseOffset = getPortBaseOffset sym edge  //offset of the side component is on
    let pos' = pos - sym.Pos + baseOffset 
    let h,w = getRotatedHAndW sym
    match ports.Length, edge with
    | 0, _ -> 0 
    | _, Left ->
        int (pos'.Y * ( float( ports.Length + 1) + 2.0*gap - 1.0) / float(h)  - gap + 0.5)
    | _, Right -> 
        -1 * int (pos'.Y * ( float( ports.Length + 1 ) + 2.0*gap - 1.0) / float(h) + 1.0 - gap - float( ports.Length + 1) - 0.5)
    | _, Bottom -> 
        int (pos'.X * (float (ports.Length + 1) + 2.0*gap - 1.0) / (float(w)) - gap + 0.5)
    | _, Top ->
        -1 * int (pos'.X * (float (ports.Length + 1) + 2.0*gap - 1.0) / float(w) - float( ports.Length + 1) + 1.0 - gap - 0.5)

let updatePortPos (sym:Symbol) (pos:XYPos) (portId: string) : Symbol =
    match sym.Component.Type with
    | Custom x ->
        let oldMaps = sym.PortMaps
        match getCloseByEdge sym pos with
        | None -> 
            printfn "not on edge"
            {sym with MovingPort = None}
        | Some edge -> 
            printfn $"{edge}"
            let newPortOrientation = oldMaps.Orientation |> Map.add portId edge
            let oldEdge = oldMaps.Orientation[portId]
            let newPortIdx = getPosIndex sym pos edge
            let oldIdx = oldMaps.Order[oldEdge] |> List.findIndex (fun el -> el = portId)
            
            let oldPortOrder' =
                oldMaps.Order 
                |> Map.add oldEdge (oldMaps.Order[oldEdge] |> List.filter (fun el -> el <> portId))
            let newPortIdx' =
                if newPortIdx > oldPortOrder'[edge].Length then oldPortOrder'[edge].Length
                else if edge = oldEdge && oldIdx < newPortIdx then newPortIdx - 1
                else newPortIdx
            printfn $"{(newPortIdx, newPortIdx')}"
            
            let newPortOrder = 
                oldPortOrder'
                |> Map.add edge (oldPortOrder'[edge] |> List.insertAt newPortIdx' portId) // to do then get index and insert at index
            let newSym =
                {sym with 
                    MovingPort = None;
                    PortMaps = {Orientation = newPortOrientation; Order = newPortOrder}
                }
            autoScaleHAndW newSym
    | _ -> {sym with MovingPort = None;}

let inline replaceSymbol (model: Model) (newSymbol: Symbol) (compId: ComponentId) : Model =
    let symbolswithoutone = model.Symbols.Remove compId
    let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newSymbol)
    { model with Symbols = newSymbolsWithChangedSymbol }

let inline transformSymbols transform model compList =
    let transformedSymbols = 
        compList |> List.map (fun id-> transform model.Symbols[id])
    let newSymbolMap = 
        (model.Symbols, transformedSymbols) 
        ||> List.fold (fun currSymMap sym -> currSymMap |> Map.add sym.Id sym)
    { model with Symbols = newSymbolMap }

//------------------------------------------------------------------------//
//-------------------------------- Label processing code------------------//
//------------------------------------------------------------------------//


//------------------------------------------------------------------------//
//------------------------------------Update function---------------------//
//------------------------------------------------------------------------//

/// Move a symbol's label by the amount specified by move
let private moveLabel (move: XYPos) (sym: Symbol) : Symbol =
    {sym with
        LabelBoundingBox = 
            {sym.LabelBoundingBox with TopLeft = sym.LabelBoundingBox.TopLeft + move}
        LabelHasDefaultPos = false
    }
/// Modifies the symbols in syms that are associated with the ids in compIds using the modifier
let private modifySymbolsByCompIds
    (modifier: Symbol -> Symbol)
    (syms: Map<ComponentId, Symbol>)
    (compIds: ComponentId list)
    : Map<ComponentId, Symbol> =

    (syms, compIds)
    ||> List.fold (fun syms id ->
            let newSym = modifier syms[id]
            Map.add id newSym syms
        )

/// Obtain all layout info on symbol that needs to be saved.
/// It is saved in extra SymbolInfo type and corresp field in Component.
let getLayoutInfoFromSymbol symbol =
    { STransform = symbol.STransform
      PortOrientation = symbol.PortMaps.Orientation
      PortOrder = symbol.PortMaps.Order 
      LabelRotation = symbol.LabelRotation
      LabelBoundingBox = 
        if symbol.LabelHasDefaultPos then 
            None 
        else 
            Some symbol.LabelBoundingBox}
/// Return a symbol with its embedded component correctly updated with symbol layoiut info.
/// Should be called juts before saving a component.
let storeLayoutInfoInComponent _ symbol =
    { symbol with
        Component =
            { symbol.Component with
                SymbolInfo = Some (getLayoutInfoFromSymbol symbol)
                X = symbol.Pos.X
                Y = symbol.Pos.Y} }

let checkSymbolIntegrity (sym: Symbol) =
    failwithf ""

/// Contains the code for the MovePort update msg
let movePortUpdate (model:Model) (portId:string) (pos:XYPos) : Model*Cmd<'a> =
    
    /// Get a port's position given the symbol, the side the port is on, the number of ports on that side and the index of the port on that side  
    let getPortPosWithIndex (sym: Symbol) portsNumber side portIndex: XYPos =
        let index = float(portIndex)
        let gap = getPortPosEdgeGap sym.Component.Type 
        let topBottomGap = gap + 0.3 // extra space for clk symbol
        let baseOffset = getPortBaseOffset sym side  //offset of the side component is on
        let baseOffset' = baseOffset + getMuxSelOffset sym side
        let portDimension = float portsNumber - 1.0
        let h,w = getRotatedHAndW sym
        match side with
        | Left ->
            let yOffset = float h * ( index + gap )/(portDimension + 2.0*gap)
            baseOffset' + {X = 0.0; Y = yOffset }
        | Right -> 
            let yOffset = float h * (portDimension - index + gap )/(portDimension + 2.0*gap)
            baseOffset' + {X = 0.0; Y = yOffset }
        | Bottom -> 
            let xOffset = float  w * (index + topBottomGap)/(portDimension + 2.0*topBottomGap)
            baseOffset' + {X = xOffset; Y = 0.0 }
        | Top ->
            let xOffset = float w * (portDimension - index + topBottomGap)/(portDimension + 2.0*topBottomGap)
            baseOffset' + {X = xOffset; Y = 0.0 }
    
    /// Helper function to get the X or Y offset of the Moving Port target, when the target is on the same side the port was before 
    let findOffsetSameEdge (symbol:Symbol) edge =
        let portsOnEdge = List.length symbol.PortMaps.Order[edge]
        if portsOnEdge = 1 then 0.0
        elif portsOnEdge = 2 then
            match edge with
            |Bottom -> ((getPortPosWithIndex symbol 2 edge 0).X)/2.0
            |Top -> (-(getPortPosWithIndex symbol 2 edge 1).X)/2.0
            |Left -> ((getPortPosWithIndex symbol 2 edge 0).Y)/2.0
            |Right -> (-(getPortPosWithIndex symbol 2 edge 1).Y)/2.0
        elif portsOnEdge > 2 then
            match edge with
            |Bottom |Top -> ((getPortPosWithIndex symbol portsOnEdge edge 1).X - (getPortPosWithIndex symbol portsOnEdge edge 0).X)/2.0
            | _ -> ((getPortPosWithIndex symbol portsOnEdge edge 1).Y - (getPortPosWithIndex symbol portsOnEdge edge 0).Y)/2.0 
        else 0.0

    /// Helper function to get the X or Y offset of the Moving Port target, when the target is NOT on the same side the port was before        
    let findOffsetDifferentEdge (symbol:Symbol) edge order=
        let portsOnEdge = List.length symbol.PortMaps.Order[edge]
        if portsOnEdge = 0 then 
            match edge with
            | Top | Bottom -> ((snd (getRotatedHAndW symbol))/2.0 )
            | _ -> ((fst (getRotatedHAndW symbol))/2.0 )
        
        elif (portsOnEdge>=1 && ((order=0) || (order=portsOnEdge)) ) then
            let (h,w) = (getRotatedHAndW symbol)
            let firstPortPos = getPortPosWithIndex symbol portsOnEdge edge 0
            let lastPortPos = getPortPosWithIndex symbol portsOnEdge edge (portsOnEdge-1)
            let firstPortXorY = match edge with |Top |Bottom -> firstPortPos.X | _ -> firstPortPos.Y 
            let lastPortXorY = match edge with | Top | Bottom ->  lastPortPos.X | _ -> lastPortPos.Y
            let hORw = match edge with | Top | Bottom -> w |_ -> h
            match (order,edge) with
            |(0,Bottom) |(0,Left) -> firstPortXorY/2.0 - 2.5
            |(0,_) -> (hORw + firstPortXorY)/2.0 + 2.5
            |(_,Bottom) | (_,Left) -> (hORw+lastPortXorY)/2.0 + 2.5
            |(_,_) -> lastPortXorY/2.0 - 2.5
        
        else 
            match edge with
            | Top | Bottom -> ((getPortPosWithIndex symbol portsOnEdge edge (order-1)).X + (getPortPosWithIndex symbol portsOnEdge edge order).X)/2.0
            | _ -> ((getPortPosWithIndex symbol portsOnEdge edge (order-1)).Y + (getPortPosWithIndex symbol portsOnEdge edge order).Y)/2.0

    /// Find the position of the target Port given the old/new edge and old/new order
    let findTargetPos (port:Port) (symbol:Symbol) = 
        let tempSymbol = updatePortPos symbol pos port.Id
        let oldEdge = symbol.PortMaps.Orientation[port.Id]
        let oldOrder = List.findIndex (fun elem -> elem = port.Id) symbol.PortMaps.Order[oldEdge]
        let newEdge = tempSymbol.PortMaps.Orientation[port.Id]
        let newOrder = List.findIndex (fun elem -> elem = port.Id) tempSymbol.PortMaps.Order[newEdge]
        let newPortPos = Symbol.getPortPos tempSymbol port
        
        let x = 
            match newEdge with
            | Right -> snd (getRotatedHAndW symbol)
            | _ -> newPortPos.X
        let y = 
            match newEdge with
            | Bottom -> fst (getRotatedHAndW symbol)
            | _ -> newPortPos.Y
        
        if newEdge = oldEdge then
            let diff = findOffsetSameEdge symbol newEdge
            if (newEdge = Top) || (newEdge = Bottom) then
                if oldOrder < newOrder then ({X=x+diff;Y=y},pos)
                elif oldOrder = newOrder then ({X=x;Y=y},pos)
                else ({X=x-diff;Y=y},pos)
            else
                if oldOrder < newOrder then ({X=x;Y=y+diff},pos)
                elif oldOrder = newOrder then ({X=x;Y=y},pos)
                else ({X=x;Y=y-diff},pos)      
        else 
            let offset = findOffsetDifferentEdge symbol newEdge newOrder
            if (newEdge = Top) || (newEdge = Bottom) then ({X=offset;Y=y},pos)
            else ({X=x;Y=offset},pos)
    
    /// return the correctly parameterised symbol given the edge the moving port is (or isn't) on
    let isTouchingEdge port symId oldSymbol = 
        match getCloseByEdge oldSymbol pos with
        | None -> 
            let newSymbol = {oldSymbol with MovingPort = Some {|PortId = portId; CurrPos = pos|}; MovingPortTarget = None; Appearance = {oldSymbol.Appearance with ShowPorts = ShowOneNotTouching port}}
            Optic.set (symbolOf_ symId) newSymbol model, Cmd.none            
        | Some _ -> 
            let target = Some (findTargetPos port oldSymbol)  
            let newSymbol = {oldSymbol with MovingPort = Some {|PortId = portId; CurrPos = pos|}; MovingPortTarget = target; Appearance = {oldSymbol.Appearance with ShowPorts = ShowOneTouching port}}
            Optic.set (symbolOf_ symId) newSymbol model, Cmd.none
    
    let port = model.Ports[portId]
    let symId = ComponentId port.HostId
    let oldSymbol = model.Symbols[symId]
    match oldSymbol.Component.Type with
    | Custom _ -> isTouchingEdge port symId oldSymbol
    | _ -> model, Cmd.none 





/// Update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =     
    match msg with
    | UpdateBoundingBoxes ->
        // message used to update symbol bounding boxes in sheet.
        failwithf "What? This message is intercepted by Sheet update function and never found here"
    | DeleteSymbols compIds ->
        (deleteSymbols model compIds), Cmd.none

    | AddSymbol (ldcs, pos,compType, lbl) ->
        let (newModel, _) = addSymbol ldcs model pos compType lbl
        newModel, Cmd.none

    | CopySymbols compIds ->
        (copySymbols model compIds), Cmd.none

    | ShowAllInputPorts ->
        (showAllInputPorts model), Cmd.none

    | ShowAllOutputPorts ->
        (showAllOutputPorts model), Cmd.none

    | DeleteAllPorts ->
        (deleteAllPorts model), Cmd.none 

    | ShowPorts compList ->
        (showPorts model compList), Cmd.none

    | ShowCustomOnlyPorts compList ->
        (showCustomPorts model compList), Cmd.none
    | MoveSymbols (compList, move) -> 
        (moveSymbols model compList move), Cmd.none

    | MoveLabel (compId, move) ->
         Optic.map (symbolOf_ compId) (moveLabel move) model, Cmd.none

    | SymbolsHaveError compList ->
        (symbolsHaveError model compList), Cmd.none

    | SelectSymbols compList ->
        (selectSymbols model compList), Cmd.none  

    | ErrorSymbols (errorCompList,selectCompList,isDragAndDrop) -> 
        (errorSymbols model (errorCompList,selectCompList,isDragAndDrop)), Cmd.none 
        
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messages

    | ChangeLabel (sId, newLabel) ->
        (changeLabel model sId newLabel), Cmd.none

    | PasteSymbols compList ->
        let newSymbols =
            (model.Symbols, compList)
            ||> List.fold (fun prevSymbols sId -> 
                Map.add sId (Optic.set (appearance_ >-> opacity_) 0.4 model.Symbols[sId]) prevSymbols) 
        { model with Symbols = newSymbols }, Cmd.none  
    
    | ColorSymbols (compList, colour) -> 
        (colorSymbols model compList colour), Cmd.none 
    
    | ChangeNumberOfBits (compId, newBits) ->
        let newsymbol = changeNumberOfBitsf model compId newBits
        (replaceSymbol model newsymbol compId), Cmd.none
    
    | ChangeLsb (compId, newLsb) -> 
        let newsymbol = changeLsbf model compId newLsb
        (replaceSymbol model newsymbol compId), Cmd.none

    | ChangeInputValue (compId, newVal) ->
        let newSymbol = changeInputValue model compId newVal
        (replaceSymbol model newSymbol compId), Cmd.none

    | ChangeConstant (compId, newVal, newText) -> 
        let newsymbol = changeConstantf model compId newVal newText
        (replaceSymbol model newsymbol compId), Cmd.none
    
    | ResetModel -> 
        { model with Symbols = Map.empty; Ports = Map.empty; }, Cmd.none
    
    | LoadComponents (ldcs,comps) ->
        (loadComponents ldcs model comps), Cmd.none
 
    | WriteMemoryLine (compId, addr, value) ->
        writeMemoryLine model (compId, addr, value), Cmd.none

    | WriteMemoryType (compId, memory) ->
        (writeMemoryType model compId memory), Cmd.none

    | RotateLeft(compList, rotation) ->
        (transformSymbols (rotateSymbol rotation) model compList), Cmd.none

    | Flip(compList, orientation) ->
        (transformSymbols (flipSymbol orientation) model compList), Cmd.none

    | MovePort (portId, pos) ->      
        movePortUpdate model portId pos

    | MovePortDone (portId, pos)->
        let port = model.Ports[portId]
        let symId = ComponentId port.HostId
        let oldSymbol = model.Symbols[symId]
        let newSymbol = {(updatePortPos oldSymbol pos portId) with MovingPortTarget = None}
        Optic.set (symbolOf_ symId) newSymbol model, Cmd.ofMsg (unbox UpdateBoundingBoxes)
    | SaveSymbols -> // want to add this message later, currently not used
        let newSymbols = Map.map storeLayoutInfoInComponent model.Symbols
        { model with Symbols = newSymbols }, Cmd.none




// ----------------------interface to Issie----------------------------- //
let extractComponent (symModel: Model) (sId:ComponentId) : Component = 
    let symbol = symModel.Symbols[sId]
    let symWithInfo = storeLayoutInfoInComponent () symbol
    symWithInfo.Component

let extractComponents (symModel: Model) : Component list =
    symModel.Symbols
    |> Map.toList
    |> List.map (fun (key, _) -> extractComponent symModel key)
