module SymbolUpdate

open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers
open CommonTypes
open System.Text.RegularExpressions
open Symbol

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
    | IOLabel -> prefix
    | _ -> prefix + (generateLabelNumber listSymbols compType)

/// Initialises and returns the new portOrientation and portOrder of a pasted symbol as a tuple
let initCopiedPorts (oldSymbol:Symbol) (newComp: Component) =
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
        (Map.empty,oldSymbol.PortOrientation)
        ||> Map.fold 
            (fun currMap oldPortId edge -> Map.add equivPortIds[oldPortId] edge currMap)

    let emptyPortOrder = 
        (Map.empty, [Top; Bottom; Left; Right])
        ||> List.fold (fun currMap side -> Map.add side [] currMap)
    let portOrder =
        (emptyPortOrder, oldSymbol.PortOrder)
        ||> Map.fold 
            (fun currMap side oldList -> 
                let newList =
                    ([], oldList)
                    ||> List.fold 
                        (fun currList oldPortId ->
                            currList @ [equivPortIds[oldPortId]])
                Map.add side newList currMap)
    portOrientation, portOrder


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
        let portOrientation, portOrder = initCopiedPorts oldSymbol newComp
        let newSymbol =
            { oldSymbol with
                Id = ComponentId newId
                Component = newComp
                Pos = newPos
                ShowInputPorts = false
                ShowOutputPorts = false
                PortOrientation = portOrientation
                PortOrder = portOrder
            }
             
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
        | Input _ -> Input newBits
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
let inline copySymbols (model: Model) compIds =
    let copiedSymbols = 
        model.Symbols
        |> Map.filter (fun compId _ -> List.contains compId compIds) 

    { model with CopiedSymbols = copiedSymbols }

/// Given a model it shows all input ports and hides all output ports, then returns the updated model
let inline showAllInputPorts (model: Model) =
    let showSymbolInPorts _ sym = 
        {sym with ShowInputPorts = true; ShowOutputPorts = false}

    let newSymbols = 
        model.Symbols
        |> Map.map showSymbolInPorts

    { model with Symbols = newSymbols }

/// Given a model it shows all output ports and hides all input ports, then returns the updated model
let inline showAllOutputPorts (model: Model) =
    let showSymbolOutPorts _ sym = 
        {sym with ShowInputPorts = false; ShowOutputPorts = true}

    let newSymbols = 
        model.Symbols
        |> Map.map showSymbolOutPorts

    { model with Symbols = newSymbols }

/// Given a model it hides all ports and returns the updated model
let inline deleteAllPorts (model: Model) =
    let hideSymbolPorts _ sym = 
        {sym with ShowInputPorts = false; ShowOutputPorts = false}

    let updatedSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    { model with Symbols = updatedSymbols}

/// Given a model it shows all the specified components' ports and hides all the other ones
let inline showPorts (model: Model) compList =
    let hideSymbolPorts _ sym =
        {sym with ShowInputPorts = false; ShowOutputPorts = false}

    let showSymbolPorts sym =
        {sym with ShowInputPorts = true; ShowOutputPorts = true}

    let resetSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    let addUpdatedSymbol prevSymbols sId =
        prevSymbols |>
        Map.add sId (showSymbolPorts resetSymbols[sId])

    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold addUpdatedSymbol

    { model with Symbols = newSymbols }

/// Given a model, a component id list and an offset, moves the components by offset and returns the updated model
let inline moveSymbols (model:Model) (compList: ComponentId list) (offset: XYPos)=
    let resetSymbols = 
        model.Symbols
        |> Map.map (fun _ sym -> { sym with Moving = false}) 

    let moveSymbol prevSymbols sId =
        let newX = model.Symbols[sId].Pos.X + offset.X;
        let newY = model.Symbols[sId].Pos.Y + offset.Y;
        let newComp = 
            { model.Symbols[sId].Component with 
                X = newX;
                Y = newY }

        prevSymbols
        |> Map.add sId 
            { model.Symbols[sId] with 
                Moving = true; 
                Pos = { X = newX; Y = newY };
                Component = newComp } 

    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold moveSymbol

    { model with Symbols = newSymbols }

/// Given a model and a component id list, sets the color of the sepcified symbols to red and every other symbol's color to gray
let inline symbolsHaveError model compList =
    let resetSymbols = 
        model.Symbols
        |> Map.map (fun _ sym -> {sym with Colour = "Lightgray"}) 

    let setSymColorToRed prevSymbols sId =
        Map.add sId {resetSymbols[sId] with Colour = "Red"} prevSymbols

    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold setSymColorToRed 
    { model with Symbols = newSymbols }

/// Given a model and a component id list, it updates the specified symbols' colour to green with max opacity, and every other symbols' colour to gray
let inline selectSymbols model compList =
    let resetSymbols = 
        model.Symbols
        |> Map.map (fun _ sym -> 
            { sym with Colour = "Lightgray"; Opacity = 1.0 }) 

    let updateSymbolColour prevSymbols sId =
        Map.add sId {resetSymbols[sId] with Colour = "lightgreen"} prevSymbols
    
    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold updateSymbolColour 

    { model with Symbols = newSymbols }

/// Given a model, an error component list, a selected component id list, it updates the selected symbols' color to green if they are not selected, and changes the symbols with errors to red. It returns the updated model.
let inline errorSymbols model (errorCompList,selectCompList,isDragAndDrop) =
    let resetSymbols = 
        model.Symbols
        |> Map.map 
            (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 })
            
    let updateSymbolStyle prevSymbols sId =
        if not isDragAndDrop then 
            Map.add sId {resetSymbols[sId] with Colour = "lightgreen"} prevSymbols
        else 
            Map.add sId { resetSymbols[sId] with Opacity = 0.2 } prevSymbols

    let selectSymbols =
        (resetSymbols, selectCompList)
        ||> List.fold updateSymbolStyle 

    let setSymColourToRed prevSymbols sId =
        Map.add sId {resetSymbols[sId] with Colour = "Red"} prevSymbols

    let newSymbols = 
        (selectSymbols, errorCompList)
        ||> List.fold setSymColourToRed
        
    { model with Symbols = newSymbols }

/// Given a model, a symbol id and a new label changes the label of the symbol to the new label and returns the updated model.
let inline changeLabel (model: Model) sId newLabel=
    let oldSym = model.Symbols[sId]
    let newComp = {oldSym.Component with Label = newLabel}
    let newSym = {oldSym with Component = newComp}
    { model with Symbols = Map.add sId newSym model.Symbols }

/// Given a model, a component id list and a color, updates the color of the specified symbols and returns the updated model.
let inline colorSymbols (model: Model) compList colour =
    let changeSymColour (prevSymbols: Map<ComponentId, Symbol>) (sId: ComponentId) =
        let newSymbol = {prevSymbols[sId] with Colour = string colour}
        prevSymbols |> Map.add sId newSymbol

    let newSymbols =
        (model.Symbols, compList)
        ||> List.fold changeSymColour

    { model with Symbols = newSymbols }

/// Given a map of current symbols and a component, initialises a symbol containing the component and returns the updated symbol map containing the new symbol
let createSymbol ldcs prevSymbols comp =
        let clocked = isClocked [] ldcs comp
        let (portOrder, portOrientation) = initPortOrientation comp
        let xyPos = {X = comp.X; Y = comp.Y}
        let (h,w) =
            if comp.H = -1 && comp.W = -1 then
                printfn $"Weird component {comp.Label}"
                let comp' = makeComponent xyPos comp.Type comp.Id comp.Label
                comp'.H,comp'.W
            else
                comp.H, comp.W
        let newSymbol =
            { 
                Pos = xyPos
                ShowInputPorts = false //do not show input ports initially
                ShowOutputPorts = false //do not show output ports initially
                Colour = "lightgrey"     // initial color 
                Id = ComponentId comp.Id
                Component = {comp with H=h ; W = w}
                Opacity = 1.0
                Moving = false
                InWidth0 = None
                InWidth1 = None
                STransform = getSTransformWithDefault comp.SymbolInfo
                PortOrientation = portOrientation
                PortOrder = portOrder
                MovingPort = None
                IsClocked = clocked
            }
            |> autoScaleHAndW
        prevSymbols
        |> Map.add (ComponentId comp.Id) newSymbol
 

/// Given a model and a list of components, it creates and adds the symbols containing the specified components and returns the updated model.
let loadComponents loadedComponents model comps=
    printfn "loading components"
    let symbolMap =
        (model.Symbols, comps) ||> List.fold (createSymbol loadedComponents)
    printfn "Adding ports..."
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
    
    let newSymbols = Map.add compId { symbol with Component = newComp } model.Symbols
    
    { model with Symbols = newSymbols }

let rotateSideLeft (side:Edge) :Edge =
    match side with
    | Top -> Left
    | Left -> Bottom
    | Bottom -> Right
    | Right -> Top

let rotateSideRight (side:Edge) :Edge =
    match side with
    | Top -> Right
    | Left -> Top
    | Bottom -> Left
    | Right -> Bottom

let rotateAngleLeft (rotation: Rotation) : Rotation =
    match rotation with
    | Degree0 -> Degree90
    | Degree90 -> Degree180
    | Degree180 -> Degree270
    | Degree270 -> Degree0

let rotateAngleRight (rotation: Rotation) : Rotation =
    match rotation with
    | Degree0 -> Degree270
    | Degree90 -> Degree0
    | Degree180 -> Degree90
    | Degree270 -> Degree180

/// Takes a symbol in and returns the same symbol rotated left
let rotateSymbolLeft (sym: Symbol) : Symbol =
    // update comp w h
    match sym.Component.Type with
    | Custom _-> sym
    | _ ->
        let h,w = getHAndW sym
        let newXY = sym.Pos + { X = (float)w/2.0 - (float) h/2.0 ;Y = (float) h/2.0 - (float)w/2.0 }

        //need to update portOrientation and portOrder
        let newPortOrientation = 
            sym.PortOrientation |> Map.map (fun id side -> rotateSideLeft side)

        let rotatePortListLeft currPortOrder side =
            currPortOrder |> Map.add (rotateSideLeft side ) sym.PortOrder[side]

        let newPortOrder = 
            (Map.empty, [Top; Left; Bottom; Right]) ||> List.fold rotatePortListLeft

        let newSTransform = 
            match sym.STransform.flipped with
            | true -> {sym.STransform with Rotation = rotateAngleRight sym.STransform.Rotation} // hack for rotating when flipped 
            | false -> {sym.STransform with Rotation = rotateAngleLeft sym.STransform.Rotation}

        { sym with 
            Pos = newXY;
            PortOrientation = newPortOrientation;
            PortOrder = newPortOrder;
            STransform =newSTransform;  
        }

/// Takes in a symbol and returns the same symbol rotated right
let rotateSymbolRight (sym: Symbol) : Symbol =
    match sym.Component.Type with
    | Custom _-> sym
    | _ ->
        let h,w = getHAndW sym
        let newXY = sym.Pos + { X = (float)w/2.0 - (float) h/2.0 ;Y = (float) h/2.0 - (float)w/2.0 }

        //need to update portOrientation and portOrder
        let newPortOrientation = 
            sym.PortOrientation |> Map.map (fun id side -> rotateSideRight side)

        let rotatePortListRight currPortOrder side =
            currPortOrder |> Map.add (rotateSideRight side ) sym.PortOrder[side]

        let newPortOrder = 
            (Map.empty, [Top; Left; Bottom; Right]) ||> List.fold rotatePortListRight

        let newSTransform = 
            match sym.STransform.flipped with
            | true -> {sym.STransform with Rotation = rotateAngleLeft sym.STransform.Rotation}
            | false -> {sym.STransform with Rotation = rotateAngleRight sym.STransform.Rotation}


        { sym with 
            Pos = newXY;
            PortOrientation = newPortOrientation;
            PortOrder = newPortOrder;
            STransform =newSTransform;  
        }
// /// Flips an angle horizontally
// let flipAngleHorizontal (rotation: Rotation): Rotation =
//     match rotation with
//     // | Degree90 | Degree270 | _ -> 
//     //     rotation
//     //     |> rotateAngleRight
//     //     |> rotateAngleRight
//     | _ -> rotation
// not needed

/// Flips a side horizontally
let flipSideHorizontal (edge: Edge) : Edge =
    match edge with
    | Left | Right ->
        edge
        |> rotateSideRight
        |> rotateSideRight
    | _ -> edge

/// Takes in a symbol and returns the same symbol flipped
let flipSymbol (orientation: Orientation) (sym:Symbol) : Symbol =
    match sym.Component.Type with
    | Custom _ -> sym
    | _ ->
        let newPortOrientation = 
            sym.PortOrientation |> Map.map (fun id side -> flipSideHorizontal side)

        let flipPortList currPortOrder side =
            currPortOrder |> Map.add (flipSideHorizontal side ) sym.PortOrder[side]

        let newPortOrder = 
            (Map.empty, [Top; Left; Bottom; Right]) ||> List.fold flipPortList
            |> Map.map (fun edge order -> List.rev order)

        

        let newSTransform = 
            {flipped= not sym.STransform.flipped;
            Rotation= sym.STransform.Rotation}

        { sym with
            PortOrientation = newPortOrientation
            PortOrder = newPortOrder
            STransform = newSTransform
        }
        |> (fun sym -> 
            match orientation with
            | Horizontal -> sym
            | Vertical -> 
                sym
                |> rotateSymbolLeft
                |> rotateSymbolLeft)

type Rectangle = {TopLeft: XYPos; BottomRight: XYPos}

let getX (pos: XYPos) =
    pos.X
let getY (pos: XYPos) =
    pos.Y

/// Checks if 2 rectangles intersect
let rectanglesIntersect (rect1: Rectangle) (rect2: Rectangle) =
    /// Checks if there is an intersection in the X or Y dimension
    let intersect1D (xOrY: XYPos -> float): bool =
        let qHi = min (xOrY rect1.BottomRight) (xOrY rect2.BottomRight)
        let qLo = max (xOrY rect1.TopLeft) (xOrY rect2.TopLeft)
        qLo <= qHi

    (intersect1D getX) && (intersect1D getY)

/// Returns an Option Edge. Returns Some edge if position is on edge of Symbol, and None if it was not on an edge
let getCloseByEdge (sym:Symbol) (pos:XYPos) : Option<Edge> =
    let h',w' = getHAndW sym
    let h, w = float h', float w'
    let symbolOffset = pos-sym.Pos
    let (cursorRect: Rectangle) = {TopLeft = symbolOffset; BottomRight = symbolOffset}
    let bbW = 5.
    let edgePosLst = 
        [
            Top, {TopLeft = {X= 0.+bbW; Y= 0.-bbW}; BottomRight = {X= w-bbW; Y= 0.+bbW}};
            Right, {TopLeft = {X= w-bbW; Y= 0.+bbW}; BottomRight = {X= w+bbW; Y= h-bbW}};
            Bottom, {TopLeft = {X= 0.+bbW; Y= h-bbW}; BottomRight = {X= w-bbW; Y= h+bbW}};
            Left, {TopLeft = {X= 0.-bbW; Y= 0.+bbW}; BottomRight = {X= 0.+bbW; Y= h-bbW}};
        ]
    let closeByEdges = List.filter (fun (edge, rect) -> (rectanglesIntersect cursorRect rect))  edgePosLst
    match closeByEdges with
    | [] -> None
    | lst -> Some (fst lst[0])

// need a function that takes in the position on the edge and returns the index on that edge

///Given a symbol and a port, it returns the offset of the port from the top left corner of the symbol
let getPosIndex (sym: Symbol) (pos: XYPos) (edge: Edge): int =
    let ports = sym.PortOrder[edge] //list of ports on the same side as port
    //let index = float( List.findIndex (fun (p:string)  -> p = port.Id) ports ) need to find index
    let gap = getPortPosEdgeGap sym.Component.Type 
    let baseOffset = getPortBaseOffset sym edge  //offset of the side component is on
    let pos' = pos - sym.Pos + baseOffset 
    let h,w = getHAndW sym
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
        let oldPortOrder, oldPortOrientation = sym.PortOrder, sym.PortOrientation
        match getCloseByEdge sym pos with
        | None -> 
            printfn "not on edge"
            {sym with MovingPort = None}
        | Some edge -> 
            printfn $"{edge}"
            let newPortOrientation = oldPortOrientation |> Map.add portId edge
            let oldEdge = oldPortOrientation[portId]
            let newPortIdx = getPosIndex sym pos edge
            let oldIdx = oldPortOrder[oldEdge] |> List.findIndex (fun el -> el = portId)
            
            let oldPortOrder' =
                oldPortOrder 
                |> Map.add oldEdge (oldPortOrder[oldEdge] |> List.filter (fun el -> el <> portId))
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
                    PortOrientation = newPortOrientation;
                    PortOrder = newPortOrder}
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

/// Update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
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

    | MoveSymbols (compList, move) -> 
        (moveSymbols model compList move), Cmd.none

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
            ||> List.fold (fun prevSymbols sId -> Map.add sId { model.Symbols[sId] with Opacity = 0.4 } prevSymbols) 
        { model with Symbols = newSymbols }, Cmd.none  
    
    | ColorSymbols (compList, colour) -> 
        (colorSymbols model compList colour), Cmd.none 
    
    | ChangeNumberOfBits (compId, newBits) ->
        let newsymbol = changeNumberOfBitsf model compId newBits
        (replaceSymbol model newsymbol compId), Cmd.none
    
    | ChangeLsb (compId, newLsb) -> 
        let newsymbol = changeLsbf model compId newLsb
        (replaceSymbol model newsymbol compId), Cmd.none

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

    | RotateLeft compList ->
        (transformSymbols rotateSymbolLeft model compList), Cmd.none
    | RotateRight compList ->
        (transformSymbols rotateSymbolRight model compList), Cmd.none

    | Flip(compList, orientation) ->
        (transformSymbols (flipSymbol orientation) model compList), Cmd.none

    | MovePort (portId, pos) ->
        let port = model.Ports[portId]
        let oldSymbol = model.Symbols[ComponentId port.HostId]
        match oldSymbol.Component.Type with
        | Custom _ -> 
            let newSymbol = {oldSymbol with MovingPort = Some {|PortId = portId; CurrPos = pos|}}
            {model with Symbols = Map.add newSymbol.Id newSymbol model.Symbols}, Cmd.none
        | _ -> model, Cmd.none
    | MovePortDone (portId, pos)->
        let port = model.Ports[portId]
        let oldSymbol = model.Symbols[ComponentId port.HostId]
        let newSymbol = updatePortPos oldSymbol pos portId
        {model with Symbols = Map.add newSymbol.Id newSymbol model.Symbols}, Cmd.none
    | SaveSymbols -> // want to add this message later, currently not used
        let getSymbolInfo symbol =
            { STransform = symbol.STransform
              PortOrientation = symbol.PortOrientation
              PortOrder = symbol.PortOrder }
        //need to store STransform in the component for reloading and stuffs

        let storeSymbolInfo _ symbol =
            { symbol with
                Component =
                    { symbol.Component with
                        SymbolInfo = Some (getSymbolInfo symbol)
                        X = symbol.Pos.X
                        Y = symbol.Pos.Y} }

        let newSymbols = Map.map storeSymbolInfo model.Symbols
        { model with Symbols = newSymbols }, Cmd.none


// ----------------------interface to Issie----------------------------- //
let extractComponent (symModel: Model) (sId:ComponentId) : Component = 
    let symbol = symModel.Symbols[sId]
    symbol.Component

let extractComponents (symModel: Model) : Component list =
    symModel.Symbols
    |> Map.toList
    |> List.map (fun (key, _) -> extractComponent symModel key)
