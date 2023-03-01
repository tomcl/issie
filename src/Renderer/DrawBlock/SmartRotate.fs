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

(*HLP23: AUTHOR Ismagilov
  SheetUpdate.fs: 'Rotate' and 'Flip' msg in update function is replaced with this Smart implentation of rotate and flip.
  DrawModelType.fs: Added type ScaleType in SymbolT Module, which Distinguishes the type of scaling the user does.

  Added 2 keyboard messages in Renderer (CrtlU & CrtrlI) to scale the block of symbols up and down respectively.
  Invalid placement handled by giving model action drag and drop, therefore requiring user to place down/continue changing until valid

  SmartHelpers.fs contains all helper functions, e.g Rotating/Flipping symbols or points in general about any center point,
  as opposed to original rotate/flip functions.
*)

let rotateBlock (compList:ComponentId list) (model:SymbolT.Model) (rotation:RotationType) = 

    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))

    //Get block properties of selected symbols
    let block = SmartHelpers.getBlock SelectedSymbols

    //Rotated symbols about the center
    let newSymbols = 
        List.map (fun x -> SmartHelpers.rotateSymbolInBlock rotation (block.Centre()) x) SelectedSymbols 

    //return model with block of rotated selected symbols, and unselected symbols
    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}

let scaleBlock (compList:ComponentId list) (model:SymbolT.Model) (scaleType:ScaleType) =
    //Similar structure to rotateBlock, easy to understand

    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))

    let block = SmartHelpers.getBlock SelectedSymbols
      
    let newSymbols = List.map (SmartHelpers.scaleSymbolInBlock scaleType block) SelectedSymbols

    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}

let flipBlock (compList:ComponentId list) (model:SymbolT.Model) (flip:FlipType) = 
    //Similar structure to rotateBlock, easy to understand
    let SelectedSymbols = List.map (fun x -> model.Symbols |> Map.find x) compList
    let UnselectedSymbols = model.Symbols |> Map.filter (fun x _ -> not (List.contains x compList))
    
    let block = SmartHelpers.getBlock SelectedSymbols
  
    let newSymbols = 
        List.map (fun x -> SmartHelpers.flipSymbolInBlock flip (block.Centre()) x ) SelectedSymbols

    {model with Symbols = 
                ((Map.ofList (List.map2 (fun x y -> (x,y)) compList newSymbols)
                |> Map.fold (fun acc k v -> Map.add k v acc) UnselectedSymbols)
    )}
