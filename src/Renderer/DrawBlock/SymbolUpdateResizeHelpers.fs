module SymbolUpdateResizeHelpers

open Elmish
open CommonTypes
open DrawModelType.SymbolT
open Symbol
open Optics
open Optic
open Operators

open SymbolHelpers
open BlockHelpers

/// HLP23 AUTHOR: BRYAN TAN 
module Constants =
    [<Literal>]
    let smallPosOffset = 0.0001

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

// resize and reposition symbol so that the fixed point is still in the right place
let reSizePosSym fixedPos scaleF sym =
    let newPos = getNewPos fixedPos scaleF sym
    let sym' = {sym with HScale = Some (abs scaleF.x); VScale = Some (abs scaleF.y) }
    moveSymbol (newPos - sym'.Pos) sym'

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
    
    let reflections =
        // hack to get the sign of the vector components from fixed point to opposite diagonal
        // without smallPosOffset diag may have a 0 element which is troublesome
        let diag = symbol.Pos + {X = Constants.smallPosOffset; Y = Constants.smallPosOffset} - fixedCornerLoc
        let fixedToMouse = pos - fixedCornerLoc
        let vReflect = if sign diag.X <> sign fixedToMouse.X then Some (XHorizontal fixedCornerLoc.X) else None
        let hReflect = if sign diag.Y <> sign fixedToMouse.Y then Some (YVertical fixedCornerLoc.Y) else None
        (vReflect, hReflect)

    let reflectSym = reflectSymbol helpers

    let applyReflections reflects symbol =
        let vR, hR = reflects
        symbol 
        |> Option.foldBack reflectSym vR 
        |> Option.foldBack reflectSym hR

    let scaleFactor = {x = (pos.X - fixedCornerLoc.X) / symbol.Component.W; y = (pos.Y - fixedCornerLoc.Y) / symbol.Component.H }

    let newSymbol = 
        match scaleFactor with
        | {x = 0.0} | {y = 0.0} -> symbol // hack to avoid divide by zero errors
        | _ ->
            symbol 
            |> applyReflections reflections
            |> reSizePosSym fixedCornerLoc scaleFactor
            |> set (appearance_ >-> showCorners_) ShowAll
        
    set (symbolOf_ compId) newSymbol model, Cmd.none        
