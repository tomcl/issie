module SymbolUpdateResizeHelpers

open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType.SymbolT
open Symbol
open Optics
open Optic
open Operators

open SymbolHelpers

/// HLP23 AUTHOR: BRYAN TAN

type ExternalHelpers =
    { FlipSymbol: FlipType -> Symbol -> Symbol }

type CornerIndex = { CornerIndex: int }

type reflectAxis = | YHorizontal of float | XVertical of float

/// /// reflect a symbol along a vertical or horizontal line
/// TODO: this is probably a useful function that can be applied in other places, find a better place to put it
let reflectSymbol (axis: reflectAxis) (symbol: Symbol) (helpers: ExternalHelpers)=
    match axis with
    | YHorizontal y -> 
        symbol
        |> helpers.FlipSymbol FlipVertical 
        |> moveSymbol ({X = 0.0; Y = y - 2.0 * symbol.Pos.Y})
    | XVertical x ->
        symbol
        |> helpers.FlipSymbol FlipHorizontal 
        |> moveSymbol ({X = x - 2.0 * symbol.Pos.X; Y = 0.0})

/// Resize a custom component based on current mouse location
let manualSymbolResize (model: Model) (compId : ComponentId) (fixedCornerLoc: XYPos) (pos: XYPos) (helpers: ExternalHelpers) = 
    let symbol = model.Symbols[compId]
    let hScale = (pos.X - fixedCornerLoc.X) / symbol.Component.W
    let vScale = (pos.Y - fixedCornerLoc.Y) / symbol.Component.H

    let offset = pos - symbol.Pos
    // let axis =
    //     if offset.X > 0.0 && offset.Y  0.0 then
    //         3
    //     else if offset.X < 0.0 && offset.Y < 0.0 then
    //         [|XVertical symbol.Pos.X|]
    //     else if offset.X < 0.0 && offset.Y > 0.0 then
    //         [|XVertical symbol.Pos.X|]
    //     else
    //         [||]

    let newPos = 
        let transform = { x = hScale / (Option.defaultValue 1.0 symbol.HScale); y = vScale / (Option.defaultValue 1.0 symbol.VScale) }
        scaleWrtFixed transform fixedCornerLoc symbol.Pos

    let newSymbol =
        {symbol with HScale = Some (abs hScale); VScale = Some (abs vScale) }
        |> moveSymbol (newPos - symbol.Pos)
        |> set (appearance_ >-> showCorners_) ShowAll

    set (symbolOf_ compId) newSymbol model, Cmd.none    