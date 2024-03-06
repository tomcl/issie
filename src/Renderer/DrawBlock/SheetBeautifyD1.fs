module SheetBeautifyD1
//-----------------Module for D1 beautify Helper functions--------------------------//
// I try to section the helpers out handling symbol/custom components
// next are wire helpers


open Optics
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Helpers
open Symbol
open BlockHelpers
open SheetBeautifyHelpers

module Helpers =
    // SYMBOL/CUSTOM COMPONENT HELPER FUNCTIONS ----------------------------------------------------------------------

    // Given D1 we are scaling custom components, we must have r/w to the dimensions

    type CustomComponentLens = {
        Get: Symbol -> (float * float)
        Set: float -> float -> Symbol -> Symbol
    }
    /// Getter and Setter function to retrive/update Custom Component Height and Width
    let dimensionsLens = {
        Get = fun sym -> (sym.Component.H, sym.Component.W)
        Set = fun h w sym ->
            let updatedComponent = { sym.Component with H = h; W = w }
            { sym with Component = updatedComponent }
    }

    /// get list of all Port Positions - This can help in testing with 
    let getAllPortPos (model: SymbolT.Model) =
        model.Ports
        |> Map.toList
        |> List.map snd
        |> List.map (fun port -> getPortPos port model)

    //This helper may be useful when aligning same-type components
    /// returns all the Symbols in a sheet grouped by Component Type
    let getSameTypeSymbol (sheet: SheetT.Model) =
        let allSymbols = sheet.Wire.Symbol.Symbols |> Map.toList |> List.map snd
        let compGroups = 
            allSymbols
            |> List.groupBy (fun symbol -> symbol.Component.Type)
        compGroups


    // WIRE HELPER FUNCTIONS ------------------------------------------------------------------------------------------------

    // from Derek Lai's code. These helpers will be used to detect segment crossings
    /// Returns true if two 1D line segments intersect
    let overlap1D ((a1, a2): float * float) ((b1, b2): float * float) : bool =
        let a_min, a_max = min a1 a2, max a1 a2
        let b_min, b_max = min b1 b2, max b1 b2
        a_max >= b_min && b_max >= a_min
    /// Returns true if two Boxes intersect, where each box is passed in as top right and bottom left XYPos tuples
    let overlap2D ((a1, a2): XYPos * XYPos) ((b1, b2): XYPos * XYPos) : bool =
        (overlap1D (a1.X, a2.X) (b1.X, b2.X)) && (overlap1D (a1.Y, a2.Y) (b1.Y, b2.Y))

    // May be useful when handingling wire/segment intersections
    /// Returns a list of all the wires in the given model 
    let getWireList (model: Model) =
        model.Wires
        |> Map.toList
        |> List.map snd

    // The XYPos of segments will be a useful helper for tracking segment locations on the sheet 
    /// Convert a wire and its segment displacement into actual segment start and end positions
    let getSegmentPositions (sheet:SheetT.Model) wire =
        let startPos = wire.StartPos
        SegmentHelpers.visibleSegments wire.WId sheet
        |> List.fold (fun (acc, lastPos) seg ->
            let newPos = lastPos + seg
            ((lastPos, newPos) :: acc, newPos) // Prepend to list for efficiency
        ) ([], startPos)
        |> fst
        |> List.rev // Reverse the list to maintain original order
    
    /// Update BusWire model with given wires. Can also be used to add new wires.
    let updateModelWires (model: BusWireT.Model) (wiresToAdd: Wire list) : BusWireT.Model =
        model
        |> Optic.map wires_ (fun wireMap ->
            (wireMap, wiresToAdd)
            ||> List.fold (fun wireMap wireToAdd -> Map.add wireToAdd.WId wireToAdd wireMap))

    

module Beautify =
    // this will be exremely useful in aligning same-type components. This function may need some editing
    /// Attempts to align two symbols together  
    let alignSymbols
        (wModel: BusWireT.Model)
        (symbolToSize: Symbol)
        (otherSymbol: Symbol)
        : BusWireT.Model =
    
        match RotateScale.getOppEdgePortInfo (wModel:BusWireT.Model) symbolToSize otherSymbol with
        | None -> wModel
        | Some(movePortInfo, otherPortInfo) ->
            let offset = RotateScale.alignPortsOffset movePortInfo otherPortInfo
            let symbol' = moveSymbol offset symbolToSize
            let model' = Optic.set (symbolOf_ symbolToSize.Id) symbol' wModel
            BusWireSeparate.routeAndSeparateSymbolWires model' symbolToSize.Id
