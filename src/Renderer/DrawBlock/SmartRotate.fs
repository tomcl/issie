module SmartRotate
open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Operators

open SmartHelpers



(*
    HLP23: this is a placeholder module for some work that can be done in the individual or team phase
    but has not been given a "starter" by me. however it is pretty easy to get started on it.

    This code would be HIGHLY USEFUL (especially the "scaling" option).

    Currently if multiple symbols are selected and rotated, each symbol will rotate, but the positions
    of teh symbols will stay fixed. The desired function for the entire block of symbols to rotate,
    applying 2-D rotation 90 degree rotation and flipping to the symbol positions about the centre of 
    the block of selected symbols.

    This operation has "simple" and "better" implementations, like all the initial tasks:
    1. Rotate all symbols and wires exact - do not allow custom components
    2. Rotate all symbols, allow custom components, CC wires will change due to CC shape changes,
      and must be partial autorouted.
    3. Allow scaling as well as rotation (autoroute wires since components will not scale and therefore
      exact wire shape will change). This operation can include Custom components since all wires are
      autorouted anyway.
    Driver test code can easily be adapted from existing Smart module Test menu items. Menu commands 
    which operate on selected symbols - the tests will be more or less how this operation is actually used).

    One key UI challenge for SmartRotate is that when a block of symbols is rotated it may overlap other
    symbols. To allow valid placement it should be possible to move the block on the sheet until a place
    to drop it is found, using an interface identical to the "copy" and "paste" interface - which works 
    fine with multiple symbols (try it). It should be possible to use that exact same interface by placing
    the rotated blokc into the copy buffer (not sure - maybe the copy buffer will need to be modified a bit).

    This could be ignored initially writing code, but muts be addressed somehow for teh operation to be usable.

    Those interested can ask me for details.
*)

let rotateBlock (compList:ComponentId list) (model:SymbolT.Model) rotation = 

    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))

    let origPos = List.map (fun x -> x.Pos) SelectedSymbols
    printfn "origPos: %A" origPos

    //Find the maximum and minimum x and y values of the selected components
    let corners = SmartHelpers.getBlockCorners SelectedSymbols
    //Find the center of the selected components
    let centerX = (corners.topRight.X + corners.topLeft.X) / 2.
    let centerY = (corners.topLeft.Y  + corners.bottomLeft.Y) / 2.
    let center = {X = centerX; Y = centerY}
    printfn "center: %A" center

    //Find rotated Pos of each selected component, and rotated symbol about the center
    let newSymbols = 
        List.map (fun x -> (rotatePointAboutBlockCentre x.Pos center x.STransform rotation)) SelectedSymbols
        |> List.map2 (fun x y -> rotateSymbolInBlock rotation x y) SelectedSymbols 

    //return model with block of rotated selected symbols, and unselected symbols
    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}

let scaleBlock (compList:ComponentId list) (model:SymbolT.Model) (scaleType:ScaleType) = 
    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))

    let corners = SmartHelpers.getBlockCorners SelectedSymbols

    let blockWidth = (corners.topRight.X - corners.topLeft.X)
    let blockHeight = (corners.bottomRight.Y - corners.topRight.Y)

    printfn "blockWidth: %A" blockWidth
    printfn "blockHeight: %A" blockHeight

    // For each symbol in selectedsymbols, find the proportion of the symbol that is in the x and y direction
    // Then scale the symbol by the proportion of the symbol that is in the x and y 
    
    let scaleSymbol (sym:Symbol) = 

      let symCenter = getRotatedSymbolCentre sym
      printfn "symCenter: %A" symCenter
      let xProp, yProp = (symCenter.X - corners.topLeft.X) / blockWidth, (symCenter.Y - corners.topLeft.Y) / blockHeight
      printfn "xProp: %A" xProp
      printfn "yProp: %A" yProp
      let newCenter =
        match scaleType with
          | ScaleUp ->
            ((corners.topLeft.X-5.) + ((blockWidth+10.) * xProp), (corners.topLeft.Y-5.) + ((blockHeight+10.) * yProp))
          | ScaleDown ->
            ((corners.topLeft.X+5.) + ((blockWidth-10.) * xProp), (corners.topLeft.Y+5.) + ((blockHeight-10.) * yProp))

      let h,w = getRotatedHAndW sym
      let newPos = {X = (fst newCenter) - w/2.; Y= (snd newCenter) - h/2.}
      let newComponent = { sym.Component with X = newPos.X; Y = newPos.Y}
    
      {sym with Pos = newPos; Component=newComponent; LabelHasDefaultPos=true}
      
    let newSymbols = List.map scaleSymbol SelectedSymbols
    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}


    


    
  

    

    


   
