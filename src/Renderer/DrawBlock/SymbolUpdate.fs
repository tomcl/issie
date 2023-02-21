module SymbolUpdate

open Elmish
open Fable.React.Props
open DrawHelpers
open CommonTypes
open Fable.React
open System.Text.RegularExpressions
open DrawModelType
open DrawModelType.SymbolT
open Symbol
open SymbolUpdatePortHelpers
open SymbolReplaceHelpers
open Optics
open Optic
open Operators
open System

//--------------------- GENERATING LABEL FUNCTIONS-------------------------------
let rec extractIOPrefix (str : string) (charLst: char list) =
    let len = String.length str
    match len with
    |0 -> "",-1
    |_ -> 
        match str[len-1] with
        |c when Char.IsNumber(Convert.ToChar(c)) -> 
            let newstr = str.Remove(len-1)
            extractIOPrefix newstr ([str[len-1]]@charLst)
        | _ -> 
            let strNo = 
                match List.length charLst with
                |0 -> ""
                |_ -> ("", charLst) ||> List.fold (fun s v -> s+(string v))
            let no = match strNo with |"" -> -1 |_ -> int <| strNo
            str,no

let generateIOLabel (model: Model) (compType: ComponentType) (name:string) : string =
    let listSymbols = List.map snd (Map.toList model.Symbols)
    let newCompBaseName, newCompNo = extractIOPrefix name []
    //printfn "s %s , n%i" newCompBaseName newCompNo
    let existingNumbers =
        listSymbols
        |> List.collect (fun sym ->
            match sym.Component.Type with
            |IOLabel -> []
            |_ -> 
                let baseName,no = extractIOPrefix sym.Component.Label []
                if baseName = newCompBaseName then
                    [no]
                else []
        )
    match existingNumbers with
    |[] -> name
    |[-1] ->
        if newCompNo = -1 then
            name+"1"
        else name
    |lst -> 
        let max = List.max existingNumbers
        if List.exists (fun x -> x=newCompNo) lst then
            newCompBaseName + (string (max+1))
        else 
            name


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

let generateCopiedLabel (model: Model) (oldSymbol:Symbol) (compType: ComponentType) : string =
    let listSymbols = List.map snd (Map.toList model.Symbols) 
    let prefix = getPrefix compType
    match compType with
    | IOLabel -> oldSymbol.Component.Label
    | BusSelection _ -> prefix
    |Input _ | Input1 (_,_) |Output _ |Viewer _ -> generateIOLabel model compType oldSymbol.Component.Label
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
let pasteSymbols (model: Model) (wireMap:Map<ConnectionId,DrawModelType.BusWireT.Wire>) (newBasePos: XYPos) : (Model * ComponentId list) =
    
    let oldSymbolsList =
            model.CopiedSymbols
            |> Map.toList
            |> List.map snd
    
    let addNewSymbol (basePos: XYPos) ((currSymbolModel, pastedIdsList) : Model * ComponentId List) (oldSymbol: Symbol): Model * ComponentId List =
        
        let newId = JSHelpers.uuid()
        let newPos = oldSymbol.Pos - basePos + newBasePos
        let compType = oldSymbol.Component.Type
        let newLabel = 
            match compType with
            | IOLabel  -> //Wire label is special case: if the driver of the wire label is not included -> keep same name
                          //else generate new label (cannot have wire labels with same name driven by 2 different components)
                let inPortId = oldSymbol.Component.InputPorts[0].Id
                let wires = wireMap |> Map.toList |> List.map snd
                let targetWire = 
                    wires
                    |> List.tryFind (fun w -> w.InputPort = (InputPortId inPortId)) 
                match targetWire with
                |Some w -> 
                    let origSymPortId = match w.OutputPort with |OutputPortId id -> id
                    let origSym = 
                        oldSymbolsList 
                        |> List.tryFind (fun s -> (List.exists (fun (p:Port) -> p.Id = origSymPortId) s.Component.OutputPorts))
                    
                    match origSym with 
                    |Some s -> generateIOLabel { model with Symbols = currSymbolModel.Symbols} compType oldSymbol.Component.Label
                    |None -> generateCopiedLabel { model with Symbols = currSymbolModel.Symbols} oldSymbol compType
                |None -> generateCopiedLabel { model with Symbols = currSymbolModel.Symbols} oldSymbol compType
            | _ -> 
                compType
                |> generateCopiedLabel { model with Symbols = currSymbolModel.Symbols} oldSymbol

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
        |> Map.map (fun _ sym -> set (appearance_ >-> colour_) (getSymbolColour sym.Component.Type sym.IsClocked model.Theme) sym)

    let setSymColorToRed prevSymbols sId =
        Map.add sId (set (appearance_ >-> colour_)  "Red" resetSymbols[sId]) prevSymbols

    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold setSymColorToRed 
    { model with Symbols = newSymbols }

/// Given a model and a component id list, it updates the specified symbols' colour to green with max opacity, and every other symbols' colour to gray
let inline selectSymbols model compList =
    let resetSymbols = 
        model.Symbols
        |> Map.map (fun _ sym -> 
            sym
            |> map appearance_ (
                set colour_ (getSymbolColour sym.Component.Type sym.IsClocked model.Theme) >> 
                set opacity_ 1.0
            )
        )

    let updateSymbolColour prevSymbols sId =
        Map.add sId (set (appearance_ >-> colour_)  "lightgreen" resetSymbols[sId]) prevSymbols
    
    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold updateSymbolColour 

    { model with Symbols = newSymbols}

/// Given a model, an error component list, a selected component id list, it updates the selected symbols' color to green if they are not selected, and changes the symbols with errors to red. It returns the updated model.
let inline errorSymbols model (errorCompList,selectCompList,isDragAndDrop) =
    let resetSymbols = 
        model.Symbols
        |> Map.map 
            (fun _ sym ->  Optic.map appearance_ (set colour_ (getSymbolColour sym.Component.Type sym.IsClocked model.Theme) >> set opacity_ 1.0) sym)
            
    let updateSymbolStyle prevSymbols sId =
        if not isDragAndDrop then 
            Map.add sId (set (appearance_ >-> colour_) "lightgreen" resetSymbols[sId]) prevSymbols
        else 
            Map.add sId (set (appearance_ >-> opacity_) 0.2 resetSymbols[sId]) prevSymbols

    let selectSymbols =
        (resetSymbols, selectCompList)
        ||> List.fold updateSymbolStyle 

    let setSymColourToRed prevSymbols sId =
        Map.add sId (set (appearance_ >-> colour_) "Red" resetSymbols[sId]) prevSymbols

    let newSymbols = 
        (selectSymbols, errorCompList)
        ||> List.fold setSymColourToRed
        
    { model with Symbols = newSymbols }

/// Given a model, a symbol id and a new label changes the label of the symbol to the new label and returns the updated model.
let inline changeLabel (model: Model) sId newLabel=
    match Map.tryFind sId model.Symbols with
    | None ->
        model // do nothing if symbol has been deleted
    | Some oldSym ->
        let newComp = {oldSym.Component with Label = newLabel}
        let newSym = 
            { oldSym with Component = newComp; LabelHasDefaultPos = true}
            |> calcLabelBoundingBox
        set (symbolOf_ sId) newSym model


/// Given a model, a component id list and a color, updates the color of the specified symbols and returns the updated model.
let inline colorSymbols (model: Model) compList colour =
    let changeSymColour (prevSymbols: Map<ComponentId, Symbol>) (sId: ComponentId) =
        let newSymbol = set (appearance_ >-> colour_) (string colour) prevSymbols[sId] 
        prevSymbols |> Map.add sId newSymbol

    let newSymbols =
        (model.Symbols, compList)
        ||> List.fold changeSymColour

    { model with Symbols = newSymbols }

/// Given a map of current symbols and a component, initialises a symbol containing the component and returns the updated symbol map containing the new symbol
let createSymbol ldcs theme prevSymbols comp =
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
                    Colour = getSymbolColour comp.Type clocked theme
                    Opacity = 1.0
                }
                Id = ComponentId comp.Id
                Component = {comp with H=h ; W = w}
                
                Moving = false
                InWidth0 = None
                InWidth1 = None
                STransform = getSTransformWithDefault comp.SymbolInfo
                ReversedInputPorts = match comp.SymbolInfo with |Some si -> si.ReversedInputPorts |_ -> None
                PortMaps = portMaps
                
                MovingPort = None
                IsClocked = clocked
                MovingPortTarget = None
                HScale = match comp.SymbolInfo with |Some si -> si.HScale |_ -> None
                VScale = match comp.SymbolInfo with |Some si -> si.VScale |_ -> None
            }
            |> autoScaleHAndW
            |> calcLabelBoundingBox
        prevSymbols
        |> Map.add (ComponentId comp.Id) newSymbol
 

/// Given a model and a list of components, it creates and adds the symbols containing the specified components and returns the updated model.
let loadComponents loadedComponents model comps=
    let symbolMap =
        (model.Symbols, comps) ||> List.fold (createSymbol loadedComponents model.Theme)
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


    let newSym = (set (component_ >-> type_) newCompType symbol)
    set (symbolOf_ compId) newSym model

    
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
    
    set (symbolOf_ compId >-> component_) newComp model

/// Given a model, a component Id and a memory component type, updates the type of the component to the specified memory and returns the updated model.
let inline updateMemory model compId updateFn =
    let symbol = model.Symbols[compId]
    let comp = symbol.Component 
    
    let newCompType =
        match comp.Type with
        | RAM1 m -> RAM1 (updateFn m)
        | ROM1 m -> ROM1 (updateFn m)
        | AsyncROM1 m -> AsyncROM1 (updateFn m)
        | AsyncRAM1 m -> AsyncRAM1 (updateFn m)
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

let inline replaceSymbol (model: Model) (newSymbol: Symbol) (compId: ComponentId) : Model =
    set (symbolOf_ compId) newSymbol model


let inline updateSymbol (updateFn: Symbol->Symbol) (compId: ComponentId) (model: Model): Model =
    { model with Symbols = model.Symbols.Add (compId, updateFn model.Symbols[compId]) }

let inline transformSymbols transform model compList =
    let transformedSymbols = 
        compList |> List.map (fun id-> transform model.Symbols[id])
    let newSymbolMap = 
        (model.Symbols, transformedSymbols) 
        ||> List.fold (fun currSymMap sym -> currSymMap |> Map.add sym.Id sym)
    

    set symbols_ newSymbolMap model

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
      ReversedInputPorts = symbol.ReversedInputPorts
      PortOrientation = symbol.PortMaps.Orientation
      PortOrder = symbol.PortMaps.Order 
      LabelRotation = symbol.LabelRotation
      LabelBoundingBox = 
        if symbol.LabelHasDefaultPos then 
            None 
        else 
            Some symbol.LabelBoundingBox
      HScale = symbol.HScale
      VScale = symbol.VScale
    }
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

type BusWireHelpers = {
    WireIntersect: BusWireT.Wire -> BusWireT.Wire -> bool
    GetConnectedWireIds: DrawModelType.BusWireT.Model -> ComponentId list -> ConnectionId list
}

(*let foo (sym: Symbol) (model: Model)=
    printf "FLIPPED!"
    let wireList = BusWireHelpers.GetConnectedWireIds model [sym]
    match (BusWireHelpers.WireIntersect wireList[0] wireList[1]) with
                                        | true -> flipSymbol FlipVertical sym
                                        | false -> sym*)
    
let addSymbol (ldcs: LoadedComponent list) (model: Model) pos compType lbl =
    let newSym = createNewSymbol ldcs pos compType lbl model.Theme
    (*let newSym2 = match newSym.Component with
                        | {Type = Mux2|Mux4|Mux8} -> foo newSym model
                        | _ -> newSym*)
    let newPorts = addToPortModel model newSym
    let newSymModel = Map.add newSym.Id newSym model.Symbols
    { model with Symbols = newSymModel; Ports = newPorts }, newSym.Id


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
                Map.add sId (set (appearance_ >-> opacity_) 0.4 model.Symbols[sId]) prevSymbols) 
        { model with Symbols = newSymbols }, Cmd.none  
    
    | ColorSymbols (compList, colour) -> 
        (colorSymbols model compList colour), Cmd.none 
    
    | ChangeNumberOfBits (compId, newBits) ->
        let newsymbol = changeNumberOfBitsf model compId newBits
        (replaceSymbol model newsymbol compId), Cmd.none
    
    | ChangeScale (compId,newScale,whichScale) ->
        let symbol = Map.find compId model.Symbols
        let newSymbol = 
            match whichScale with
            |Horizontal -> {symbol with HScale=Some newScale}
            |Vertical -> {symbol with VScale=Some newScale}
        (replaceSymbol model newSymbol compId), Cmd.none

    | ChangeLsb (compId, newLsb) -> 
        let newsymbol = changeLsbf model compId newLsb
        (replaceSymbol model newsymbol compId), Cmd.none

    | ChangeInputValue (compId, newVal) ->
        let newSymbol = changeInputValue model compId newVal
        (replaceSymbol model newSymbol compId), Cmd.none

    | ChangeReversedInputs (compId) ->
        let newSymbol = changeReversedInputs model compId
        (replaceSymbol model newSymbol compId), Cmd.none
    | ChangeAdderComponent (compId, oldComp, newComp) ->
        let newSymbol = changeAdderComponent model compId oldComp newComp
        let newPorts = addToPortModel model newSymbol
        let newModel = {model with Ports = newPorts}  
        (replaceSymbol newModel newSymbol compId), Cmd.none
    | ChangeCounterComponent (compId, oldComp, newComp) ->
        let newSymbol = changeCounterComponent model compId oldComp newComp
        let newPorts = addToPortModel model newSymbol
        let newModel = {model with Ports = newPorts}  
        (replaceSymbol newModel newSymbol compId), Cmd.none
    | ChangeConstant (compId, newVal, newText) -> 
        let newsymbol = changeConstantf model compId newVal newText
        (replaceSymbol model newsymbol compId), Cmd.none
    
    | ChangeBusCompare (compId, newVal, newText) -> 
        let newsymbol = changeBusComparef model compId newVal newText
        (replaceSymbol model newsymbol compId), Cmd.none

    | ResetModel -> 
        { model with Symbols = Map.empty; Ports = Map.empty; }, Cmd.none
    
    | LoadComponents (ldcs,comps) ->
        (loadComponents ldcs model comps), Cmd.none
 
    | WriteMemoryLine (compId, addr, value) ->
        writeMemoryLine model (compId, addr, value), Cmd.none

    | WriteMemoryType (compId, memory) ->
        (writeMemoryType model compId memory), Cmd.none

    | UpdateMemory (compId, updateFn) ->  
        (updateMemory model compId updateFn), Cmd.none

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
        let newSymbol = 
            {(updatePortPos oldSymbol pos portId) with MovingPortTarget = None}
            |> autoScaleHAndW
        set (symbolOf_ symId) newSymbol model, Cmd.ofMsg (unbox UpdateBoundingBoxes)
    | SaveSymbols -> // want to add this message later, currently not used
        let newSymbols = Map.map storeLayoutInfoInComponent model.Symbols
        { model with Symbols = newSymbols }, Cmd.none
    | SetTheme (theme) ->
        let resetSymbols = 
            model.Symbols
            |> Map.map 
                (fun _ sym ->  Optic.map appearance_ (set colour_ (getSymbolColour sym.Component.Type sym.IsClocked theme)) sym)
        {model with Theme=theme; Symbols = resetSymbols}, Cmd.none



// ----------------------interface to Issie----------------------------- //
let extractComponent (symModel: Model) (sId:ComponentId) : Component = 
    let symbol = symModel.Symbols[sId]
    let symWithInfo = storeLayoutInfoInComponent () symbol
    symWithInfo.Component

let extractComponents (symModel: Model) : Component list =
    symModel.Symbols
    |> Map.toList
    |> List.map (fun (key, _) -> extractComponent symModel key)
