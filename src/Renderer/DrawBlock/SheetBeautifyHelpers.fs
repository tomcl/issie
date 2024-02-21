module SheetBeautifyHelpers

open Fable.Core
open CommonTypes
open DrawHelpers
open Fable.React
open Optics
open Operators
open Node.ChildProcess
open DrawModelType
open Symbol
open Elmish
open EEExtensions
open Optics
open Optics.Operators
open DrawHelpers
open Helpers
open CommonTypes
open ModelType
open DrawModelType
open Sheet.SheetInterface

//-----------------Module for beautify Helper functions--------------------------//
// Typical candidates: all individual code library functions.
// Other helpers identified by Team


// Key Type Difficulty Value read or written 
// B1R, B1W RW 
//  Value read or written: The dimensions of a custom component symbol


/// Calculates the final dimensions of a symbol, considering potential scaling.
let getSymbolDimensions (symbol: SymbolT.Symbol) : float * float = 
    let baseWidth = symbol.Component.W
    let baseHeight = symbol.Component.H
    let scaledWidth = 
        match symbol.HScale with
        | Some(scale) -> baseWidth * scale
        | None -> baseWidth
    let scaledHeight = 
        match symbol.VScale with
        | Some(scale) -> baseHeight * scale
        | None -> baseHeight
    (scaledWidth, scaledHeight)


let caseInvariantEqual str1 str2 =
        String.toUpper str1 = String.toUpper str2
let getlabel (model:SheetT.Model) (label:string): SymbolT.Symbol option = 
        model.Wire.Symbol.Symbols
        |> Map.values
        |> Seq.tryFind (fun sym -> caseInvariantEqual label sym.Component.Label)
let updateCustomComponentDimensions (symLabel: string) (desiredWidth: float) (desiredHeight: float) (model: SheetT.Model): Result<SheetT.Model, string> =
    match getlabel model symLabel with
    | Some symbol ->
        let originalWidth = symbol.Component.W
        let originalHeight = symbol.Component.H
        let hScale = desiredWidth / originalWidth
        let vScale = desiredHeight / originalHeight
        let updatedSymbol = { symbol with HScale = Some hScale; VScale = Some vScale }
        
        // Assuming `replaceSymbol` updates and returns a modified Symbol.Model, which is part of SheetT.Model
        let symbolModelUpdated = SymbolUpdate.replaceSymbol model.Wire.Symbol updatedSymbol symbol.Id
        
        // Here, ensure that SheetT.symbol_ correctly targets where the Symbol.Model lives within SheetT.Model
        // and replaces it with the updated one
        let updatedSheetModel = model |> Optic.set SheetT.symbol_ symbolModelUpdated
        
        Ok updatedSheetModel
    | None -> Error (sprintf "Symbol with label '%s' not found." symLabel)

//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
// B7R, B7W RW Low The rotation state of a symbol 
let getSymbolById (model: SheetT.Model) (id: ComponentId) : Option<SymbolT.Symbol> =
    let symbols = model.Wire.Symbol.Symbols
    symbols |> Map.tryFind id

let getSymbolRotation (symbol: SymbolT.Symbol) : Rotation =
    symbol.STransform.Rotation

let getSymbolFlipped (symbol: SymbolT.Symbol) : bool =
    symbol.STransform.Flipped


//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//
//-------------------------------------------------------------------------------------------------//

// T1R R Low
// The number of pairs of symbols that intersect each other. See Tick3 for a related function.
// Count over all pairs of symbols.
let countIntersectingPairs (sheet: SheetT.Model) =
    let wireModel = sheet.Wire
    let boxes =
        mapValues sheet.BoundingBoxes
        |> Array.toList
        |> List.mapi (fun n box -> n,box)
    List.allPairs boxes boxes 
    |> List.filter (fun ((n1,box1),(n2,box2)) -> (n1 <> n2) && BlockHelpers.overlap2DBox box1 box2)
    |> List.length

// T2R R Low 
// The number of distinct wire visible segments that intersect with one or more symbols. See Tic
let countSymbolIntersectingWire (sheet: SheetT.Model) =
    let wireModel = sheet.Wire
    wireModel.Wires
    |> Map.fold (fun acc _ wire -> 
        if BusWireRoute.findWireSymbolIntersections wireModel wire <> [] then acc + 1 else acc
    ) 0


