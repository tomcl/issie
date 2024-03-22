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
open SymbolUpdate

//--------------------------------------------------------------------------------------//
//                                  Constants for D3                                    //
//--------------------------------------------------------------------------------------//

/// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_ = SheetT.symbol_

let wireModel_ = SheetT.wire_

/// allowed max X or y coord of svg canvas
let maxSheetCoord = Sheet.Constants.defaultCanvasSize

//--------------------------------------------------------------------------------------//
//                              Threshold Record for D3                                 //
//--------------------------------------------------------------------------------------//

type ThresholdRecord = { Threshold : float;
                         Gap : XYPos }


//--------------------------------------------------------------------------------------//
//                               Helper Functions for D3                                //
//--------------------------------------------------------------------------------------//

/// Returns the wire along with its length in a tuple
let getWireLength (wire: Wire) : Wire * float =
    wire, List.fold (fun sum seg -> sum + abs(seg.Length)) 0.0 wire.Segments

/// Get all the source ports of the wires in the list
let getWireOutputPortId (wireList: Wire list) : OutputPortId list =
    List.map (fun wire -> wire.OutputPort) wireList

/// Get all the input ports of the wires that have the same output port
let getWireInputPortId (wireList: Wire list) (outputPort: OutputPortId) : InputPortId list =
    wireList
    |> List.filter (fun wire -> wire.OutputPort = outputPort)
    |> List.map (fun wire -> wire.InputPort)

/// Get all the port positions in a subnet of the same output port
let wirePortPositions (outputID: OutputPortId) (wires: Wire list) (model: BusWireT.Model) =
    let outputPos = getPortPos (outputID.ToString()) model

    let inputPortList = getWireInputPortId wires outputID
    let inputPosList =
        [ for inputPort in inputPortList do
              getPortPos (inputPort.ToString()) model ]

    outputPos :: inputPosList

    
//--------------------------------------------------------------------------------------//
//                                D3                                                    //
//--------------------------------------------------------------------------------------//

let sheetWireLabelSymbol (model: SheetT.Model) : SheetT.Model =
    // Easily customisable threshold conditions
    let conditions =  
        { Threshold = 500; 
          Gap = {X = 40; Y=0}}

    // Generate the number of Wirelabels currently on the sheet
    let symbols = getAllSymbols model
    let wirelabel = generateLabelNumber symbols IOLabel

    // Will be used to assign unique labels to the wire labels
    let mutable count = int wirelabel
    
    // Calculate wire lengths of all wires and filter the long wires
    let wires = getAllWires model
    let wireLengths = List.map getWireLength wires
    let longWires =
        wireLengths
        |> List.filter (fun (_, length) -> length > conditions.Threshold) // Some threshold e.g 500
        |> List.map fst // Remove the length from snd as Wire list is now filtered

    // Get distinct output port IDs from long wires
    let OutputPortIds = longWires |> getWireOutputPortId |> List.distinct

    // Update the model with new IOlabels and wires for every subnet of wires
    let updatedModel: SheetT.Model = 
        OutputPortIds
        // Accumulator is currentModel. We repeat this List.fold for each outputPort
        |> List.fold (fun currentModel outputPort ->
            // Get input port IDs associated with the current output port
            let InputPortIds = getWireInputPortId longWires outputPort

            // Get positions of wire ports
            let posList = wirePortPositions outputPort wires currentModel.Wire

            // Generate label for the IOLabel
            let label = "I" + count.ToString()
            count <- count + 1

            // ----- DELETE WIRES -----
            // create a new BusWireT.Model with the wires deleted
            let (delWireModel: BusWireT.Model) =
                deleteWiresWithPort
                    [ Some currentModel.Wire.Symbol.Ports[outputPort.ToString()] ] // since we only need to delete one port (as part of the fold loop, we create a list with one element, referring to our outputport)
                    currentModel.Wire

            // create a new SheetT.model with long wires deleted
            let (modelDelW: SheetT.Model) =
                currentModel
                |> Optic.set SheetT.wire_ delWireModel

            // ----- ADD Output port IOLabel -----
            // add the IOLabel for the output symbol to the SymbolT.Model
            let (outputSymModel: SymbolT.Model), outputSymId =
                SymbolUpdate.addSymbol 
                    [] 
                    (currentModel.Wire.Symbol) 
                    (posList.Head + conditions.Gap) 
                    IOLabel 
                    label

            // Find the InputPortID for the placed IOLabel
            let IOPortID = outputSymModel.Symbols[outputSymId].Component.InputPorts[0].Id

            // create a new SheetT.model with output IOLabel added
            let (modelWithOutputSym: SheetT.Model) = 
                modelDelW
                |> Optic.set SheetT.symbol_ outputSymModel
            
            // ----- Connect wires between component and IOLabel -----
            // create a new BusWireT.Model with wires added
            let addOutputWireModel, msgOpt =
                BusWireUpdate.newWire 
                    (InputPortId IOPortID) 
                    outputPort 
                    modelWithOutputSym.Wire
            
            let (modelWithNewOutputWires: SheetT.Model) = 
                modelWithOutputSym 
                |> Optic.set SheetT.wire_ addOutputWireModel
            
            //  ----- INPUTS -----

            // take the postList.Tail list of XYPos and add the IOLabel for each symbol to the SymbolT.Model
            posList.Tail
            // Accumulator is currentModelWithOutputSym. Smaller inside loop, we repeat this List.fold for each inputPort pos
            |> List.fold (fun currentModelAddingInputSyms pos ->
                
                // Repeat for every Input Port in Subnet
                let inputSymModel, inputSymId =
                    SymbolUpdate.addSymbol 
                        [] 
                        (currentModelAddingInputSyms.Wire.Symbol) 
                        (pos - conditions.Gap) 
                        IOLabel 
                        label

                let IOPortID = inputSymModel.Symbols[inputSymId].Component.OutputPorts[0].Id

                let modelwithInputSym = 
                    currentModelAddingInputSyms 
                    |> Optic.set SheetT.symbol_ inputSymModel
                        
                
                // Connect wires from IOLabel output ports to Component input ports
                InputPortIds
                |> List.fold (fun inputModel inputPort ->
                    let inputPortPos = getPortPos (inputPort.ToString()) inputModel.Wire
                    match inputPortPos with
                    | x when x = pos -> 
                    // only place wire if it is the matching Input port to the generated IOLabel
                        let addInputWireModel, msgOpt =
                            BusWireUpdate.newWire 
                                inputPort 
                                (OutputPortId IOPortID)
                                inputModel.Wire
                        
                        let (modelWithNewInputWires: SheetT.Model) = 
                            inputModel
                            |> Optic.set SheetT.wire_ addInputWireModel
                        modelWithNewInputWires
                    | _ -> inputModel
                        
                ) modelwithInputSym // feed modelwithInputSym as our initial starting point (aka default value)
                   
                    
            ) modelWithNewOutputWires // feed modelWithNewOutputWires as our initial starting point (aka default value)     

        ) model // feed model as our initial starting point (aka default value) for the outer loop
        // return updatedModel
        
    updatedModel
    
    
                
                