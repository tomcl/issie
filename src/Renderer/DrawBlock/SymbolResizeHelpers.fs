module SymbolResizeHelpers

open Elmish
open CommonTypes
open DrawModelType.SymbolT
open Symbol
open Optics
open Optic
open Operators

open SymbolHelpers
open BlockHelpers


module Constants =
    [<Literal>]
    let smallPosOffset = 0.0001

let rotateSide (rotation: Rotation) (side:Edge) :Edge =
    match rotation, side with
    | Degree0, _ -> side
    | Degree90, Top -> Left
    | Degree90, Left -> Bottom
    | Degree90, Bottom -> Right
    | Degree90, Right -> Top
    | Degree180, Top -> Bottom
    | Degree180, Bottom -> Top
    | Degree180, Left -> Right
    | Degree180, Right -> Left
    | Degree270, Top -> Right
    | Degree270, Left -> Top
    | Degree270, Bottom -> Left
    | Degree270, Right -> Bottom



/// rotates the portMap information left or right as per rotation
let rotatePortInfo (rotation:Rotation) (portMaps:PortMaps) : PortMaps=
    //need to update portOrientation and portOrder
    let newPortOrientation = 
        portMaps.Orientation |> Map.map (fun id side -> rotateSide rotation side)

    let rotatePortList currPortOrder side =
        currPortOrder |> Map.add (rotateSide rotation side) portMaps.Order[side]

    let newPortOrder = 
        (Map.empty, [Top; Left; Bottom; Right]) ||> List.fold rotatePortList
    {Orientation= newPortOrientation; Order = newPortOrder}

let adjustPosForRotation 
        (rotation:Rotation) 
        (h: float)
        (w:float)
        (pos: XYPos)
         : XYPos =
    let posOffset =
        match rotation with
        | Degree90 | Degree270 -> { X = (float)w/2.0 - (float) h/2.0 ;Y = (float) h/2.0 - (float)w/2.0 }
        | _ ->  failwithf "Can't encounter Degree0 or Degree180 here in SymbolResizeHelpers/adjustPosForRotation function"
    pos - posOffset


/// Takes a symbol in and returns the same symbol rotated left or right
let rotateSymbol (rotation: Rotation) (sym: Symbol) : Symbol =
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
                {sym.STransform with Rotation = combineRotation (invertRotation rotation) sym.STransform.Rotation} // hack for rotating when flipped 
            | false -> 
                {sym.STransform with Rotation = combineRotation rotation sym.STransform.Rotation}
        { sym with 
            Pos = newPos;
            PortMaps = rotatePortInfo rotation sym.PortMaps
            STransform = newSTransform 
            LabelHasDefaultPos = true
            Component = newComponent
        } |> calcLabelBoundingBox

let rec rotateAntiClockByAng (rotAngle: Rotation) (sym: Symbol) : Symbol =
    match rotAngle with
    | Degree0 -> sym
    | deg ->
        let newSym = rotateSymbol Degree270 sym
        match deg with
        | Degree90 -> newSym
        | Degree180 ->
            rotateAntiClockByAng (Degree90) newSym
        | Degree270 ->
            rotateAntiClockByAng (Degree180) newSym
        | Degree0 -> failwithf "Can't encounter Degree0 here"

/// Flips a side horizontally
let flipSideHorizontal (edge: Edge) : Edge =
    match edge with
    | Left | Right ->
        edge
        |> rotateSide Degree180
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
            |> rotateSymbol Degree180)

let changeSymbolCorners showCorners sym = 
    set (appearance_ >-> showCorners_) showCorners sym

let hideCompCorners (model: Model) = 
    let resetSymbols =
        model.Symbols
        |> Map.map (fun _ sym -> changeSymbolCorners DontShow sym)

    { model with Symbols = resetSymbols }

/// Given a model it will change the appearance of all the specified components' corners
let showCompCorners (model: Model) (cornerShow) (compIds: ComponentId list) =
    let resetSymbols =
        let model' = hideCompCorners model
        model'.Symbols
    
    let updateSymbolInMap prevMap cId = 
        prevMap
        |> if (Map.containsKey cId model.Symbols) then  Map.add cId (changeSymbolCorners cornerShow model.Symbols[cId]) else id

    let newSymbols = 
        (resetSymbols, compIds) 
        ||> List.fold updateSymbolInMap

    { model with Symbols = newSymbols }



/// Resize a custom component so corner follows current mouse location.
/// Note that we expect current mouse to be close to one of the corners.
let manualSymbolResize
        (model: Model)
        (compId : ComponentId)
        (fixedCornerLoc: XYPos) // XYPos of corner opposite that which is clicked - this will not change
        (mPos: XYPos) // XYPos of mouse. Symbol will be resized to make its clicked corner match this
        = 
    let symbol = model.Symbols[compId]
    let symPos = get posOfSym_ symbol
    let comp = symbol.Component 
    let scale = get scaleF_ symbol

    /// Componentwise multiply
    let outerProduct ({X=x;Y=y}:XYPos) ({X=x1;Y=y1}:XYPos) = {X=x*x1;Y=y*y1}

    /// True if symbol is rotated 90 or 270 degrees swapping X,Y box dimensions
    let symXyAreSwapped =
        match symbol.STransform.Rotation with
        | Degree0 | Degree180 -> false
        | Degree90 | Degree270 -> true

    /// Does not include rotation or scaling
    let compBox = {X=comp.W; Y=comp.H}

    /// Function will return X,Y swapped iff symbols is rotated 90 or 270 degrees
    let swapXYByRot ({X=x;Y=y}:XYPos) =
        if symXyAreSwapped then {X=y; Y=x} else {X=x; Y=y}

    /// Correct dimensions for scaled and unscaled component box
    let scaledSymBox, symBox =
        outerProduct compBox scale |> swapXYByRot,
        compBox |> swapXYByRot

    /// Vector outer product
    let outerProduct (a: XYPos) (b: XYPos) = {X=a.X*b.X; Y=a.Y*b.Y}

    /// apply function f to both the components of xy
    let xyApplyF (f: float -> float) (xy: XYPos) =
        {X = f xy.X; Y = f xy.Y}


    /// Diagonal between moving corner and opposite fixed corner
    let diag = mPos - fixedCornerLoc

    /// Indicates for X & Y is the diagonal dimension negative
    let invert = xyApplyF (sign >> float) diag

    /// diagonal with abs value of each component
    let posDiag = outerProduct diag invert
    /// difference between current and required moving corner position.
    /// signs pos or neg.
    let deltaDiag = posDiag - scaledSymBox
    let scale' = {X=posDiag.X/symBox.X; Y = posDiag.Y/symBox.Y}

    /// Adjustment to top left corner symbol position
    /// to keep fixed point (which may not be top left) fixed
    let posDelta: XYPos =
        match int invert.X, int invert.Y with
        | 1, -1 ->     0.,          -deltaDiag.Y
        | -1, 1 ->    -deltaDiag.X,  0.
        | -1, -1 ->   -deltaDiag.X, -deltaDiag.Y
        | 1, 1 | _ ->  0.,           0.
        |> fun (x,y) -> {X=x; Y=y}

    let newSymbol =
        let scale'' = swapXYByRot scale'
        match scale' with
        | {X=x;Y=y} when x <= 0.001 || y <= 0.001 -> symbol // hack to avoid divide by zero errors
        | _ ->
            symbol 
            |> set  scaleF_ scale'' // set symbol scaling
            |> set posOfSym_ (posDelta + symPos) // set symbol position
           
        |> set (appearance_ >-> showCorners_) ShowAll
        |> map (labelBoundingBox_ >-> topLeft_) (fun lPos -> lPos + posDelta) // set label position as symbol

    set (symbolOf_ compId) newSymbol model, Cmd.none  // update map      
