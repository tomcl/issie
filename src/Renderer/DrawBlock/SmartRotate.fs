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
open SymbolView

open SymbolUpdate

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

let rotateSymbol2 (rotation: RotationType) (sym: Symbol) (newPos:XYPos) : Symbol =
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
          printfn "rot: %A" rotation
          let newPos2 = adjustPosForRotation2 rotation h w newPos  

          let newComponent = { sym.Component with X = newPos2.X; Y = newPos2.Y}

          let newSTransform = 
              match sym.STransform.flipped with
              | true -> 
                  {sym.STransform with Rotation = rotateAngle (invertRotation rotation) sym.STransform.Rotation} // hack for rotating when flipped 
              | false -> 
                  {sym.STransform with Rotation = rotateAngle rotation sym.STransform.Rotation}
          printfn "newPos2 : {%A}" newPos2
          { sym with 
              Pos = newPos2;
              PortMaps = rotatePortInfo rotation sym.PortMaps
              STransform =newSTransform 
              LabelHasDefaultPos = true
              Component = newComponent
          } |> calcLabelBoundingBox 

let rotateSelectedSymbols (compList:ComponentId list) (model:SymbolT.Model) rotation = 

    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))
    let origPos = List.map (fun x -> x.Pos) SelectedSymbols
    printfn "origPos: %A" origPos
    //Find the maximum and minimum x and y values of the selected components
    let maxX = List.maxBy (fun (x:Symbol) -> x.Pos.X+(snd (getRotatedHAndW x))) SelectedSymbols
    let minX = List.minBy (fun (x:Symbol) -> x.Pos.X) SelectedSymbols
    let maxY = List.maxBy (fun (x:Symbol) -> x.Pos.Y+(fst (getRotatedHAndW x))) SelectedSymbols
    let minY = List.minBy (fun (x:Symbol) -> x.Pos.Y) SelectedSymbols
    printfn "maxX: %A" (maxX.Pos.X + (snd (getRotatedHAndW maxX)))
    printfn "minX: %A" minX.Pos.X
    printfn "maxY: %A" (maxY.Pos.Y + (fst (getRotatedHAndW minY)))
    printfn "minY: %A" (minY.Pos.Y)
    //Find the center of the selected components
    let centerX = (maxX.Pos.X+(snd (getRotatedHAndW maxX)) + minX.Pos.X) / 2.
    let centerY = (maxY.Pos.Y+ (fst (getRotatedHAndW minY))  + minY.Pos.Y) / 2.
    let center = {X = centerX; Y = centerY}
    printfn "center: %A" center

    //Find rotated Pos of each selected component
    let rotateTopLeft = List.map (fun x -> (rotatePoints2 x.Pos center x.STransform rotation)) SelectedSymbols
    printfn "newPos: %A" rotateTopLeft

    //Get the new symbols
    let newSymbols = List.map2 (fun x y -> rotateSymbol2 rotation x y) SelectedSymbols rotateTopLeft

    //return model with block of rotated selected symbols, and unselected symbols
    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}
    
    
    

    


   
