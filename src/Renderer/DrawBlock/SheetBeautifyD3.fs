module SheetBeautifyD3

//-----------------------------------------------------------------------------------------//
//----------------------------- Module for SheetBeuaitfyD3 --------------------------------//
//-----------------------------------------------------------------------------------------//


open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open SheetUpdateHelpers
open SheetBeautifyHelpers
open Optics
open BlockHelpers
open EEExtensions
open Optics.Operators
open Symbol
open SheetBeautifyHelpers.SegmentHelpers
open BusWireUpdateHelpers



module Constants = 



    type Thresholds = {
    Length : float
    RightAngle : int
    RightAngleCrossings : int
    OverloadedPort : int
    }



    type PortPositions = {
        LengthStart : float
        LengthEnd : float
        LengthStep : float
        Zero  : float
        WidthStart : float
        WidthEnd : float
        WidthStep : float

    }


    let thresholds = {
        Length = 300.0
        RightAngle = 3 
        RightAngleCrossings = 1
        OverloadedPort = 2
    }


    let portPositions = {
        LengthStart = 25.0
        LengthEnd = 100.0
        LengthStep = 1.0
        Zero = 0.0  
        WidthStart = -20.0 
        WidthEnd = 20.0  
        WidthStep = 1.0
    }

open Constants

//----------------------------------------------------------------------------------------------------//
//------------------------------Helpers functions for SheetBeautifyD3---------------------------------//
//----------------------------------------------------------------------------------------------------//



/// allowed max X or y coord of svg canvas
let maxSheetCoord = Sheet.Constants.defaultCanvasSize



/// Optic to access SymbolT.Model from SheetT.Model
let symbolModel_: Lens<SheetT.Model,SymbolT.Model> = SheetT.symbol_



/// Optic to access BusWireT.Model from SheetT.Model
let busWireModel_ = SheetT.wire_



/// Add a newly routed wire
/// Source and target specify a string of InputPortId and OutputPortId
/// Return an error if the wire duplicates an existing one
let placeWire (source: string) (target: string) (model: SheetT.Model) : Result<SheetT.Model,string> =
    let newWire = BusWireUpdate.makeNewWire (InputPortId source) (OutputPortId target) model.Wire
    if model.Wire.Wires |> Map.exists (fun _ wire -> wire.InputPort=newWire.InputPort && wire.OutputPort = newWire.OutputPort) then
        Error "Can't create wire from {source} to {target} because a wire already exists between those ports"
    else
        model
        |> Optic.set (busWireModel_ >-> BusWireT.wireOf_ newWire.WId) newWire
        |> Ok



/// Return list of all Wire Ids in model
let getConnectionIdList (model: SheetT.Model) =
    Optic.get BusWireT.wires_ model.Wire
    |> Map.keys
    |> Array.toList



/// length of a wire
let getWireLength (wId: ConnectionId) (model: SheetT.Model): float =
    visibleSegments wId model
    |> List.sumBy (fun vector -> abs vector.X + abs vector.Y)



// Mapping of Wire Ids and their respective length
let getWireLengths (model: SheetT.Model): Map<ConnectionId, float> =
    getConnectionIdList model
    |> List.map (fun (wId) ->
        let length = getWireLength wId model
        wId, length
    )
    |> Map.ofList



/// Start and end portIds of a wire in a tuple
let getWirePortIds (model: SheetT.Model) (wId: ConnectionId) = 
    let wire = model.Wire.Wires[wId]
    wire.InputPort, wire.OutputPort



/// Return True if wire has a segment which overlaps a symbol
let wireIntersectSymbol (wId: ConnectionId) (model: SheetT.Model) = 
    let wire = model.Wire.Wires[wId]
    BusWireRoute.findWireSymbolIntersections model.Wire wire <> []



/// Checks if a symbol intersects with either a wire or another symbol
let symbolIntersects (sId: ComponentId) (model: SheetT.Model) : bool =

    let symbolBoundingBox = getBoundingBox model.Wire.Symbol sId


    let symbolIntersectsSymbol =
        getBoundingBoxes model.Wire.Symbol
        |> Map.filter (fun symId _ -> symId <> sId)
        |> Map.toList
        |> List.exists (fun (_ , otherBox) -> overlap2DBox symbolBoundingBox otherBox)


    let symbolIntersectsWire =
        getWireList model.Wire
        |> List.exists (fun (wire) ->
            wire
            |> getAbsSegments
            |> List.exists (fun aseg ->
                match segmentIntersectsBoundingBox symbolBoundingBox aseg.Start aseg.End with
                | Some _ -> true
                | None -> false
            )
        )

    symbolIntersectsSymbol || symbolIntersectsWire



/// Return a model with wires removed
let removeWires (model: SheetT.Model) (wIds: ConnectionId list) : SheetT.Model =
    let wires =
        Optic.get BusWireT.wires_ model.Wire

    let updateWires wires =
        wIds
        |> List.fold (fun acc wId -> Map.remove wId acc) wires


    let composedLens =
        busWireModel_ >-> BusWireT.wires_
    
    model
    |> Optic.set composedLens (updateWires wires)



//----------------------------------------------------------------------------------------------------//
//--------------------------------- Threshold Helpers Submodule --------------------------------------//
//----------------------------------------------------------------------------------------------------//



module ThresholdHelpers= 
    /// The number of pairs of distinct visible wire segments that cross each other at right angles in a wire.
    /// Returns the number right angle intersections between wire segments.
    /// Does not include crossings that are "T junction"
    /// counts segments that overlap only once
    /// ASSUMPTION: overlapping segments are in same Net and have same starting point.
    let numOfWireRightAngleCrossings (wId: ConnectionId) (model: SheetT.Model)  =
        let wire = model.Wire.Wires[wId]
        let wireSegs = SegmentHelpers.visibleSegsWithVertices wire model
        let otherSegs = 
            getWireList model.Wire
            |> List.map (fun (wire) ->
            SegmentHelpers.visibleSegsWithVertices wire model
        )
            
        let countProperCrossings (seg1: (XYPos * XYPos)) (seg2: (XYPos * XYPos) list) =
            seg2
            |> List.filter (fun otherSeg -> seg1 > otherSeg && SegmentHelpers.isProperCrossing seg1 otherSeg)
            |> List.length

        List.allPairs wireSegs otherSegs
        |> List.sumBy (fun (seg1, seg2) -> countProperCrossings seg1 seg2)



    /// Returns the start of wires (OutputPortId) grouped 
    /// with a list of wire end points (list of InputPortId)
    let groupWirePortIdsByOutputPortId (model: SheetT.Model) (wIds: ConnectionId list) =
        wIds
        |> List.map (fun wId -> getWirePortIds model wId)
        |> List.groupBy snd
        |> List.map (fun (outputPortId, portIds) -> (outputPortId, List.map fst portIds))



    /// Returns a list of Source Ports where there are above
    /// a threshold of wires coming out
    let overloadedSourcePorts (model: SheetT.Model) =
        getConnectionIdList model
        |> groupWirePortIdsByOutputPortId model
        |> List.filter (fun (_, wires) -> List.length wires > thresholds.OverloadedPort)



    let countRightAnglesWire (wId: ConnectionId) (model: SheetT.Model): int =
        let visibleSegmentVectors = visibleSegments wId model
        let numVectors = List.length visibleSegmentVectors

        if numVectors >= 2 then
            numVectors - 1
        else
            0



//------------------------------end of Threshold Calculation Submodule -------------------------------//



module ComplexFilter = 
    open ThresholdHelpers

    /// Prints how the code has been evaluated 
    /// Returning the values, thresholds and True/False
    /// if below or above set threshold 
    let printComplexWireFilterResults (model: SheetT.Model) =

        getWireLengths model
        |> Map.filter (fun wId length ->
            let rightAngleCount = countRightAnglesWire wId model
            let intersectsSymbol = wireIntersectSymbol wId model
            let rightAngleCrossings = numOfWireRightAngleCrossings wId model
            printfn "Wire ID: %s" (string wId)
            printfn "Length: %f, Length condition: %b" length (length > thresholds.Length)
            printfn "Right angle count: %d, Right angle condition: %b" rightAngleCount (rightAngleCount > thresholds.RightAngle)
            printfn "Right angle crossing count: %d, Right angle crossing condition: %b" rightAngleCrossings (rightAngleCrossings > thresholds.RightAngleCrossings)
            printfn "Intersects symbol: %b" intersectsSymbol

            length > thresholds.Length ||
            rightAngleCount > thresholds.RightAngle ||
            intersectsSymbol
        )
        |> Map.iter (fun wId length ->
            printfn "Wire ID: %s, Length: %f" (string wId) length
        )


    /// Returns list of wires that are considered complex,
    /// where the start of wire (OutputPortId) is grouped 
    /// with a list of wire end points (list of InputPortId)
    /// A wire is complex if:
    /// Wire Length above a threshold
    /// Wire has too many right angles
    /// Wire's segments cross at right angles too much
    /// Wire overlaps a symbol
    /// If a wire's Source Port has too many wires coming out
    let getComplexWirePorts (model: SheetT.Model)  =

        printComplexWireFilterResults model

        /// Find out Port Source Ids that are considered complex
        let filteredWireSources = 
            getWireLengths model
                |> Map.filter (fun wId length ->
                    length > thresholds.Length ||
                    countRightAnglesWire wId model > thresholds.RightAngle ||
                    wireIntersectSymbol wId model ||
                    numOfWireRightAngleCrossings wId model> thresholds.RightAngleCrossings
                )
            |> Map.keys
            |> List.ofSeq
            /// Group the wires by start and end points
            |> groupWirePortIdsByOutputPortId model
            /// Add wires that come from ports that have too many wires
            |> List.append (overloadedSourcePorts model)
            |> List.map fst
            /// Take out duplicates
            |> List.distinct
        
        
        let wIdsList = 
            getWireLengths model
            |> Map.keys
            |> List.ofSeq


        /// Filters the list so that if any ports have wires
        /// that are considered complex, all wires from that 
        /// port are considered complex.
        
        groupWirePortIdsByOutputPortId model wIdsList
        |> List.filter (fun (outputPortId, _) ->
            List.exists (fun outputPortId' -> outputPortId = outputPortId') filteredWireSources) 



//------------------------------end of Complex Filter Submodule --------------------------------------//



open ComplexFilter



/// Place a Wire Label with label symLabel onto the sheet with given position
/// Also return the ComponentId of the placed symbol
/// Return None if unsuccessful
let placeIOLabelAtPosition (symLabel: string) (position: XYPos) (model: SheetT.Model) : option<(SheetT.Model * ComponentId)> =
    let symModel, symId = SymbolUpdate.addSymbol [] (model.Wire.Symbol) position IOLabel symLabel
    let sym = symModel.Symbols[symId]
    match position + sym.getScaledDiagonal with
    | {X=x;Y=y} when x > maxSheetCoord || y > maxSheetCoord ->
        None // Position outside allowed coordinates
    | _ ->
        let updatedModel =
            model
            |> Optic.set symbolModel_ symModel
            |> updateBoundingBoxes
        match symbolIntersects symId updatedModel with
        | false -> Some (updatedModel, symId)
        | true -> None



/// OutputPortId: start point of wires (Only 1)
/// InputPortId list: end point of any wires that come out of OutputPortId 
/// Generate a list of positions where a label could be placed 
/// for each InputPortId and the OutputPortId
let getLabelPositions (portIds: OutputPortId * InputPortId list) (model: SymbolT.Model) : XYPos list * XYPos list list =
    let inputPortIds = 
        snd portIds
        |> List.map getInputPortIdStr

    let outputPortId = 
        fst portIds
        |> getOutputPortIdStr 
    
    let singlePortPosition portId =
        let portPos = getPortLocation None model portId
        let edge = getPortOrientationFrmPortIdStr model portId
        let adjustPosition length width =
            match edge with
            | Top -> { X = portPos.X + width; Y = portPos.Y + length }
            | Bottom -> { X = portPos.X + width; Y = portPos.Y - width }
            | Left -> { X = portPos.X - length; Y = portPos.Y + width}
            | Right -> { X = portPos.X + length; Y = portPos.Y + width}
        
        /// List of lengths how far you want to consider placing the label
        let lengthRange = [portPositions.LengthStart .. portPositions.LengthStep .. portPositions.LengthEnd]

        /// List of "widths" so how wide of an area you want to consider placing the label
        /// Zero is concatanated to try place on a straight line
        let widthRange = [portPositions.Zero] @ [portPositions.WidthStart .. portPositions.WidthStep .. portPositions.WidthEnd]
        
        /// Combinations of Length and Width
        let combinations =
            List.collect (fun length ->
                widthRange 
                |> List.map (fun offset -> (length, offset))) lengthRange
        
        combinations
        |> List.map (fun (length, offset) -> adjustPosition length offset)

    
    let sourcePositions = singlePortPosition outputPortId
    let targetPositions = List.map singlePortPosition inputPortIds

    sourcePositions, targetPositions



/// Places the Wire Label at the first successful position in given positions
/// Returns an Error if Wire Label overlaps with Wire or Symbol at any position
let tryPlaceIOLabel (symLabel: string) (positions: XYPos list) (model: SheetT.Model) : Result<(SheetT.Model * ComponentId), string> =
    match List.tryPick (fun pos -> placeIOLabelAtPosition symLabel pos model) positions with
    | Some (updatedModel, symId) -> Ok (updatedModel, symId)
    | _ -> Error "Failed To Place Label" 



/// Returns a sheet with wires of Start:OutputPortId and End InputPortId
/// removed the model, replaced with a Wire Label at a suitable position
/// and all with the same label
let wireToLabel (portIds: OutputPortId * InputPortId list) (model: SheetT.Model) (label: string) =
    let outputPortId, inputPortIds = portIds
    
    /// Get a list of Wire Ids 
    let findWIds (inputPortIdToFind: InputPortId) =
        getWireList model.Wire
        |> List.filter (fun wire -> wire.InputPort = inputPortIdToFind)
        |> List.map (fun wire -> wire.WId)

    /// Find potential positions where Wire Labels could be placed
    let sourcePositions, targetPositions = getLabelPositions portIds model.Wire.Symbol

    /// Updated model without given Wire Ids
    let removedWireModel =
        let wIds =
            inputPortIds
            |> List.collect findWIds
        if List.isEmpty wIds then
            model 
        else
            removeWires model wIds
    
    /// Place a Label at a given position with connected wire 
    /// and return updated sheet if succesful
    /// positions will correspond to inputPortId with an offset
    /// (also the end point of where a wire was)
    let placeTargetLabel (inputPortId: InputPortId) (positions: XYPos list) (model: SheetT.Model) =
        let target = getInputPortIdStr inputPortId
        match tryPlaceIOLabel label positions model with
        | Ok result ->
            let labeledModel, sId = result
            let comp = labeledModel.Wire.Symbol.Symbols[sId]
            // Get IOLabel Output Port
            let source =
                match List.tryHead comp.Component.OutputPorts with
                | Some head -> string head.Id
                | None -> "DefaultId"
            placeWire source target labeledModel
        | _ -> Error "Failed to place label"

    /// Place a Label at a given position with connected wire 
    /// and return updated sheet if succesful
    /// positions will correspond to outputPortId with an offset
    /// (also the start point of where a wire was)
    let placeSourceLabel (positions: XYPos list) (currentModel: SheetT.Model) = 
        let source = getOutputPortIdStr outputPortId
        match tryPlaceIOLabel label positions currentModel with
        | Ok result ->
            let labeledModel, sId = result
            let comp = labeledModel.Wire.Symbol.Symbols[sId]
            // Get IOLabel Input Port
            let target =
                match List.tryHead comp.Component.InputPorts with
                | Some head -> string head.Id
                | None -> "DefaultId"
            placeWire source target labeledModel
        | _ -> Ok model 


    portIds
    |> snd 
    |> List.zip targetPositions 
    |> List.fold (fun acc (positions, inputPortId) ->
        match placeTargetLabel inputPortId positions acc with
        | Ok acc ->  acc 
        | Error _ ->  model) removedWireModel 
    |> placeSourceLabel sourcePositions
    


/// Returns a model with any wires that are considered to be
/// too ugly replcaed with a label
/// If labels cannot be placed in a suitable position,
/// only paired labels are placed, or original model is returned 
/// if none.
let removeComplexWires (model: SheetT.Model): SheetT.Model =

    // let mutable labelCounter = 0
    // let getNextLabel () =
    //     let currentLabel = sprintf "I%d" labelCounter
    //     labelCounter <- labelCounter + 1
    //     currentLabel

    
    
    // getComplexWirePorts model
    // |> List.fold (fun acc portIds ->
    //     match wireToLabel portIds acc (getNextLabel ()) with
    //     | Ok labeledModel -> labeledModel
    //     | Error _ -> acc) model


    /// Implementation below avoids use of mutable 
    let labelWire (labelCounter, model) portIds =
        match wireToLabel portIds model (sprintf "I%d" labelCounter) with
        | Ok labeledModel -> (labelCounter + 1, labeledModel)
        | Error _ -> (labelCounter, model)
    
    
    let portIdsLists = getComplexWirePorts model
    
    let _, finalModel = 
        List.fold labelWire (0, model) portIdsLists
    finalModel
