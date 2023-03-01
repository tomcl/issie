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

let changeSymbolCorners cornerShow sym = 
    set (appearance_ >-> showCorners_) cornerShow sym

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

type ExternalHelpers =
    { FlipSymbol: FlipType -> Symbol -> Symbol }

type reflectType = | YVertical of float | XHorizontal of float

/// reflect a symbol along a vertical or horizontal line
/// TODO: this is probably a useful function that can be applied in other places, find a way to get FlipSymbol in a nicer way and find a better place to put it
let reflectSymbol (helpers: ExternalHelpers) (axis: reflectType) (symbol: Symbol) =
    match axis with
    | YVertical y -> 
        symbol
        |> helpers.FlipSymbol FlipVertical 
        |> moveSymbol ({X = 0.0; Y = 2.0 * (y - symbol.Pos.Y) - symbol.Component.H * Option.defaultValue 1.0 symbol.VScale})
    | XHorizontal x ->
        symbol
        |> helpers.FlipSymbol FlipHorizontal 
        |> moveSymbol ({X = 2.0 * (x - symbol.Pos.X) - symbol.Component.W * Option.defaultValue 1.0 symbol.HScale; Y = 0.0})

/// Resize a custom component based on current mouse location
let manualSymbolResize (model: Model) (compId : ComponentId) (fixedCornerLoc: XYPos) (pos: XYPos) (helpers: ExternalHelpers) = 
    let symbol = model.Symbols[compId]
    
    let W, H = getRotatedHAndW symbol

    printfn "--------------------"
    printfn $"Pos: {pos}"
    // printfn $"Fix: {fixedCornerLoc}"
    // printfn $"Old: {symbol.Pos}"
    // printfn $"New: {newPos}"

    let reflections =
        // hack to get the sign of the vector from fixed point to opposite diagonal
        // without smallPosOffset diag may have a 0 element which is troublesome
        let smallPosOffset = 0.0001
        let diag = symbol.Pos + {X = smallPosOffset; Y = smallPosOffset} - fixedCornerLoc
        let fixedToMouse = pos - fixedCornerLoc
        let vReflect = if sign diag.X <> sign fixedToMouse.X then Some (XHorizontal fixedCornerLoc.X) else None
        let hReflect = if sign diag.Y <> sign fixedToMouse.Y then Some (YVertical fixedCornerLoc.Y) else None
        (vReflect, hReflect)

    let reflectSym = reflectSymbol helpers

    let applyReflections reflects symbol =
        let tryApply reflect sym = match reflect with | Some r -> reflectSym r sym | None -> sym
        let vR, hR = reflects
        symbol |> tryApply vR |> tryApply hR

    
    let getNewPos sym = 
        let hScale = (pos.X - fixedCornerLoc.X) / sym.Component.W
        let vScale = (pos.Y - fixedCornerLoc.Y) / sym.Component.H
        let transform = { x = hScale / (Option.defaultValue 1.0 sym.HScale); y = vScale / (Option.defaultValue 1.0 sym.VScale) }
        scaleWrtFixed transform fixedCornerLoc sym.Pos

    // resize and reposition symbol so that the fixed point is still in the right place
    let reSizePosSym sym =
        // {symbol with HScale = Some (abs hScale); VScale = Some (abs vScale) }
        // |> moveSymbol (newPos - symbol.Pos)
        let hScale = (pos.X - fixedCornerLoc.X) / sym.Component.W
        let vScale = (pos.Y - fixedCornerLoc.Y) / sym.Component.H
        let newPos = getNewPos sym
        let sym' = {sym with HScale = Some (abs hScale); VScale = Some (abs vScale) }
        moveSymbol (newPos - sym'.Pos) sym'

    let newSymbol = 
        symbol 
        |> applyReflections reflections
        |> reSizePosSym
        |> set (appearance_ >-> showCorners_) ShowAll
        
    set (symbolOf_ compId) newSymbol model, Cmd.none        