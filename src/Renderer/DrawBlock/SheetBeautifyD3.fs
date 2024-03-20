module SheetBeautifyD3

open DrawModelType
open CommonTypes
open DrawModelType.SymbolT
open DrawModelType.SheetT
open DrawModelType.BusWireT
open Optics
open Optics.Operators
open Helpers
open SymbolHelpers
open BlockHelpers
open Symbol
open BusWireRoute
open BusWire
open SheetBeautifyHelpers
open EEExtensions
open SheetBeautifyD2

//--------------------------------------------------------------------------------------//
//                                  Constants for D3                                    //
//--------------------------------------------------------------------------------------//

/// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_ = SheetT.symbol_

/// allowed max X or y coord of svg canvas
let maxSheetCoord = Sheet.Constants.defaultCanvasSize

//--------------------------------------------------------------------------------------//
//                               Helper Functions for D3                                //
//--------------------------------------------------------------------------------------//

// Returns the wire along with its length in a tuple
let getWireLength (wire: Wire) : Wire * float =
    wire, List.fold (fun sum seg -> sum + seg.Length) 0.0 wire.Segments

// Get all the source ports of the wires in the list
let getOutputPortId (wireList: Wire list) : OutputPortId list =
    List.map (fun wire -> wire.OutputPort) wireList

// Get all the input ports of the wires that have the same output port
let getInputPortId (wireList: Wire list) (outputPort: OutputPortId) : InputPortId list =
    wireList
    |> List.filter (fun wire -> wire.OutputPort = outputPort)
    |> List.map (fun wire -> wire.InputPort)

// From Tick3
let placeSymbol
    (symLabel: string)
    (compType: ComponentType)
    (position: XYPos)
    (model: SheetT.Model)
    : Result<SheetT.Model, string>
    =
    let symLabel = String.toUpper symLabel // make label into its standard casing
    let symModel, symId =
        SymbolUpdate.addSymbol [] (model.Wire.Symbol) position compType symLabel
    let sym = symModel.Symbols[symId]
    match position + sym.getScaledDiagonal with
    | { X = x; Y = y } when x > maxSheetCoord || y > maxSheetCoord ->
        Error $"symbol '{symLabel}' position {position + sym.getScaledDiagonal} lies outside allowed coordinates"
    | _ ->
        model
        |> Optic.set symbolModel_ symModel
        |> SheetUpdateHelpers.updateBoundingBoxes // could optimise this by only updating symId bounding boxes
        |> Ok

let wireLabelPositions (outputID: OutputPortId) (wires: Wire list) (model: BusWireT.Model) =
    let outputPos = getPortPos (outputID.ToString()) model

    let inputPortList = getInputPortId wires outputID
    let inputPosList =
        [ for inputPort in inputPortList do
              getPortPos (inputPort.ToString()) model ]

    outputPos :: inputPosList

//--------------------------------------------------------------------------------------//
//                                D3                                                    //
//--------------------------------------------------------------------------------------//

// Very initial start to D3, currently filters wires on a sheet into long and short wires

// TODOS:
// - Find a way of replacing longWires with labels (e.g using AddNotConnected).
// - Figure out where to position the label.
// - What names to give each label.
let sheetWireLabelSymbol (model: SheetT.Model) : SheetT.Model =
    let wires = getAllWires model
    let wireLengths = List.map getWireLength wires
    let longWires =
        wireLengths
        |> List.filter (fun (_, length) -> length > 500.0) // Some threshold e.g 500
        |> List.map fst // Remove the length from snd as Wire list is now filtered
    let OutputPortIds = longWires |> getOutputPortId |> List.distinct
    let gap: XYPos = { X = 10; Y = 0 }
    for outputPort in OutputPortIds do
        let posList = wireLabelPositions outputPort wires model.Wire
        let outputSymModel, outputSymId =
            SymbolUpdate.addSymbol [] (model.Wire.Symbol) posList.Head IOLabel "I1"
        for pos in posList.Tail do
            let inputSymModel, inputSymId =
                SymbolUpdate.addSymbol [] (model.Wire.Symbol) pos IOLabel "I1"

            model |> Optic.set symbolModel_ inputSymModel
    model

