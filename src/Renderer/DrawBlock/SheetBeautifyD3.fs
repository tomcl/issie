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
open BusWireUpdateHelpers
open SheetBeautifyHelpers
open EEExtensions
open SheetBeautifyD2

//--------------------------------------------------------------------------------------//
//                                  Constants for D3                                    //
//--------------------------------------------------------------------------------------//

/// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_ = SheetT.symbol_

let wireModel_ = SheetT.wire_

/// allowed max X or y coord of svg canvas
let maxSheetCoord = Sheet.Constants.defaultCanvasSize

//--------------------------------------------------------------------------------------//
//                               Helper Functions for D3                                //
//--------------------------------------------------------------------------------------//

// Returns the wire along with its length in a tuple
let getWireLength (wire: Wire) : Wire * float =
    wire, List.fold (fun sum seg -> sum + abs(seg.Length)) 0.0 wire.Segments

// Get all the source ports of the wires in the list
let getWireOutputPortId (wireList: Wire list) : OutputPortId list =
    List.map (fun wire -> wire.OutputPort) wireList

// Get all the input ports of the wires that have the same output port
let getWireInputPortId (wireList: Wire list) (outputPort: OutputPortId) : InputPortId list =
    wireList
    |> List.filter (fun wire -> wire.OutputPort = outputPort)
    |> List.map (fun wire -> wire.InputPort)


let wireLabelPositions (outputID: OutputPortId) (wires: Wire list) (model: BusWireT.Model) =
    let outputPos = getPortPos (outputID.ToString()) model

    let inputPortList = getWireInputPortId wires outputID
    let inputPosList =
        [ for inputPort in inputPortList do
              getPortPos (inputPort.ToString()) model ]

    outputPos :: inputPosList

let newWireModel (model: SheetT.Model) (input: InputPortId) (output: OutputPortId) =
    let newModel, msgOpt =
        BusWireUpdate.newWire input output model.Wire
    newModel

//let findComponentPortID =
    
    
    
    //--------------------------------------------------------------------------------------//
    //                                D3                                                    //
    //--------------------------------------------------------------------------------------//
    
    // Very initial start to D3, currently filters wires on a sheet into long and short wires
    
    // TODOS:
    // - Find a way of replacing longWires with labels (e.g using AddNotConnected).
    // - Figure out where to position the label.
    // - What names to give each label.
let sheetWireLabelSymbol (model: SheetT.Model) : SheetT.Model =
    let symbols = getAllSymbols model
    let wires = getAllWires model
    let wireLengths = List.map getWireLength wires
    let longWires =
        wireLengths
        |> List.filter (fun (_, length) -> length > 500.0) // Some threshold e.g 500
        |> List.map fst // Remove the length from snd as Wire list is now filtered
    let OutputPortIds = longWires |> getWireOutputPortId |> List.distinct
    let gap: XYPos = { X = 40; Y = 0 }
    let mutable count = 0

    let updatedModel: SheetT.Model = 
        OutputPortIds
        // Accumulator is currentModel. We repeat this List.fold for each outputPort
        |> List.fold (fun currentModel outputPort ->
            let InputPortIds = getWireInputPortId longWires outputPort
            let posList = wireLabelPositions outputPort wires currentModel.Wire
            let label = "I" + count.ToString()
            count <- count + 1
            // add the IOLabel for the output symbol to the SymbolT.Model
            let (outputSymModel: SymbolT.Model), outputSymId =
                SymbolUpdate.addSymbol [] (currentModel.Wire.Symbol) (posList.Head + gap) IOLabel label
            // Find the portID for the outputportID
            let IOPortID = outputSymModel.Symbols[outputSymId].Component.InputPorts[0].Id

            // create a new BusWireT.Model with the wires deleted
            let (delWireModel: BusWireT.Model) =
                deleteWiresWithPort
                    [ Some currentModel.Wire.Symbol.Ports[outputPort.ToString()] ] // since we only need to delete one port (as part of the fold loop, we create a list with one element, referring to our outputport)
                    currentModel.Wire

            // create a new SheetT.model with long wires deleted
            let (modelDelW: SheetT.Model) =
                currentModel
                |> Optic.set SheetT.wire_ delWireModel

            // create a new SheetT.model with output IOLabel added
            let (modelWithOutputSym: SheetT.Model) = 
                modelDelW
                |> Optic.set SheetT.symbol_ outputSymModel
            
            // create a new BusWireT.Model with wires added
            let addOutputWireModel, msgOpt =
                BusWireUpdate.newWire 
                    (InputPortId IOPortID) 
                    outputPort 
                    modelWithOutputSym.Wire
            
            let (modelWithNewOutputWires: SheetT.Model) = 
                modelWithOutputSym 
                |> Optic.set SheetT.wire_ addOutputWireModel
                
            // take the postList.Tail list of XYPos and add the IOLabel for each symbol to the SymbolT.Model
            posList.Tail
            // Accumulator is currentModelWithOutputSym. Smaller inside loop, we repeat this List.fold for each inputPort pos
            |> List.fold (fun currentModelAddingInputSyms pos ->

                let inputSymModel, inputSymId =
                    SymbolUpdate.addSymbol 
                        [] 
                        (currentModelAddingInputSyms.Wire.Symbol) 
                        (pos - gap) 
                        IOLabel 
                        label

                let IOPortID = inputSymModel.Symbols[inputSymId].Component.OutputPorts[0].Id

                let modelwithInputSym = 
                    currentModelAddingInputSyms 
                    |> Optic.set SheetT.symbol_ inputSymModel
                        
                
                //This is not working
                InputPortIds
                |> List.fold (fun inputModel inputPort ->
                    let addInputWireModel, msgOpt =
                        BusWireUpdate.newWire 
                            inputPort 
                            (OutputPortId IOPortID)
                            inputModel.Wire
                    
                    let (modelWithNewInputWires: SheetT.Model) = 
                        inputModel
                        |> Optic.set SheetT.wire_ addInputWireModel
                    
                    modelWithNewInputWires) modelwithInputSym
                   
                    
            ) modelWithNewOutputWires // feed modelWithOutputSym as our initial starting point (aka default value)

        ) model // feed model as our initial starting point (aka default value) for the outer loop
        // return updatedModel
        
    updatedModel
    
    
                
                