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
        |> Map.add cId (changeSymbolCorners cornerShow model.Symbols[cId])

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
