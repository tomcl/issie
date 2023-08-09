module SymbolHelpers

open Fable.React
open Fable.React.Props
open Elmish
open Optics

open CommonTypes
open DrawHelpers
open DrawModelType.SymbolT




/// Returns the XYPos of custom component symbol corners relative to Pos (= LH corner)
let getCustomSymCorners (sym: Symbol) =
    let comp = sym.Component
    match comp.Type with
    | Custom _ ->
        let getScale = Option.defaultValue 1.0
        let xDim, yDim =  getScale sym.HScale*comp.W, getScale sym.VScale*comp.H
        let xDim', yDim' =
            match sym.STransform.Rotation with
            | Degree0 | Degree180 -> xDim, yDim
            | Degree90 | Degree270 -> yDim, xDim
        [|{X=0.0;Y=0.0}; {X=0.0;Y=yDim'}; {X=xDim';Y=yDim'}; {X=xDim';Y=0.0}|]
    | _ -> Array.empty // should never match

let translatePoints vector (points: XYPos[]) = 
    match points with
    | [||] -> [||] 
    | _ -> Array.map ((+) vector) points

/// Scale a point as a vector 
let scalePoint (scale: XYPos) (point: XYPos) =
    { X=point.X * scale.X; Y=point.Y * scale.Y }

/// Apply scaling centered at fixedPos (p0) to vector movePos (p1)
/// In matrix operations: (p0-p1) * M + p1
let scaleWrtFixed (scale: XYPos) (fixedPos: XYPos) (movePos: XYPos) = 
    movePos 
    |> (-) fixedPos
    |> scalePoint scale
    |> (+) fixedPos

// get the new XYPos of the symbol after scaling to ensure fixed point is fixed
let getNewPos fixedPos (scaleF: XYPos) sym = 
    let transform = { X = scaleF.X / (Option.defaultValue 1.0 sym.HScale); Y = scaleF.Y / (Option.defaultValue 1.0 sym.VScale) }
    scaleWrtFixed transform fixedPos sym.Pos
