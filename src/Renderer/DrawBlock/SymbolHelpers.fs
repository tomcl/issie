module SymbolHelpers

open Fable.React
open Fable.React.Props
open Elmish
open Optics

open CommonTypes
open DrawHelpers
open DrawModelType.SymbolT


/// HLP23 AUTHOR: BRYAN TAN

let getCustomSymCorners (sym: Symbol) =
    let getScale = function | Some x -> x | _ -> 1.0 
    match sym.Component.Type with
    | CommonTypes.Custom _ -> 
        let HScale = getScale sym.HScale
        let VScale = getScale sym.VScale
        [|{X=0.0;Y=0.0}; {X=0.0;Y=sym.Component.H*VScale}; {X=sym.Component.W*HScale;Y=sym.Component.H*VScale}; {X=sym.Component.W*HScale;Y=0.0}|]
    | _ -> Array.empty // should never match

let translatePoints vector (points: XYPos[]) = 
    match points with
    | [||] -> [||] 
    | _ -> Array.map ((+) vector) points