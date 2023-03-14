module SymbolHelpers

open Fable.React
open Fable.React.Props
open Elmish
open Optics

open CommonTypes
open DrawHelpers
open DrawModelType.SymbolT


/// HLP23 AUTHOR: BRYAN TAN

type scaleFactor = { x: float; y: float }

/// Returns the XYPos of points defining custom component 
let getCustomSymCorners (sym: Symbol) =
    match sym.Component.Type with
    | Custom _ -> 
        let HScale = Option.defaultValue 1.0 sym.HScale
        let VScale = Option.defaultValue 1.0 sym.VScale
        [|{X=0.0;Y=0.0}; {X=0.0;Y=sym.Component.H*VScale}; {X=sym.Component.W*HScale;Y=sym.Component.H*VScale}; {X=sym.Component.W*HScale;Y=0.0}|]
    | _ -> Array.empty // should never match

let translatePoints vector (points: XYPos[]) = 
    match points with
    | [||] -> [||] 
    | _ -> Array.map ((+) vector) points

/// Scale a point as a vector 
let scalePoint (scale: scaleFactor) (point: XYPos) =
    { X=point.X * scale.x; Y=point.Y * scale.y }

/// Apply scaling centered at fixedPos (p0) to vector movePos (p1)
/// In matrix operations: (p0-p1) * M + p1
let scaleWrtFixed (scale: scaleFactor) (fixedPos: XYPos) (movePos: XYPos) = 
    movePos 
    |> (-) fixedPos
    |> scalePoint scale
    |> (+) fixedPos

// get the new XYPos of the symbol after scaling to ensure fixed point is fixed
let getNewPos fixedPos scaleF sym = 
    let transform = { x = scaleF.x / (Option.defaultValue 1.0 sym.HScale); y = scaleF.y / (Option.defaultValue 1.0 sym.VScale) }
    scaleWrtFixed transform fixedPos sym.Pos
