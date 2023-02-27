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

type ExternalHelpers =
    { FlipSymbol: FlipType -> Symbol -> Symbol }

/// HLP23 AUTHOR: BRYAN TAN

let manualSymbolResize (model: Model) (compId : ComponentId) (fixedCornerLoc: XYPos) (pos: XYPos) (helpers: ExternalHelpers) = 
    printfn $"Mouse: {pos}"
    printfn $"Fixed: {fixedCornerLoc}"

    let symbol = model.Symbols[compId]
    let hScale = (pos.X - fixedCornerLoc.X) / symbol.Component.W 
    let vScale = (pos.Y - fixedCornerLoc.Y) / symbol.Component.H 

    let flipIfNegative direction scale sym =
        if scale < 0.0 then
            helpers.FlipSymbol direction sym
        else
            sym

    let newSymbol =
        symbol
        |> flipIfNegative FlipHorizontal hScale
        |> flipIfNegative FlipVertical vScale
        |> (fun sym -> {sym with HScale=Some (abs hScale); VScale=Some (abs vScale)})
        |> set (appearance_ >-> showCorners_) ShowAll
    
    printfn $"h:{symbol.Component.H}, w:{symbol.Component.W}"
    printfn $"hscale: {hScale}, vscale: {vScale}"

    set (symbolOf_ compId) newSymbol model, Cmd.none    