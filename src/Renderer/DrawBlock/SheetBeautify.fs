module SheetBeautify

//-----------------------------------------------------------------------------------------//
//----------Module for top-level beautify functions making (mostly) whole-sheet changes----//
//-----------------------------------------------------------------------------------------//

(*
Whole sheet functions are normally applied to whole sheet. In many cases a feature could be to
apply them to currently selected wires or components. That provides users control over what gets beautified.
Ideal beautify code will never make things worse so can be applied to whole sheet always.

Otehr relevant modules:
SheetBeautifyHelpers (helpers)
Generatedata (used to make randomized tests)
TestDrawBlock (used to test the code written here).

*)

// open modules likely to be used
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open DrawModelType.SheetT
open Optics
open SheetBeautifyD1
open SheetBeautifyD2
open SheetBeautifyD3
open EEExtensions
open Optics.Operators
open Symbol
open BlockHelpers
open RotateScale
open BusWire
open CommonTypes
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open SheetBeautifyHelpers
open Helpers




/// constants used by SheetBeautify
module Constants =
    () // dummy to make skeleton type check - remove when other content exists

module D1Build =
    
    module D1Helpers =
            // D1 HELPER FUNCTIONS ----------------------------------------------------------------------
    
            /// Attempts to create degrees of freedom for any symbols overlapping
            /// Will translate the symbol by half the width/height of the the host symbol based on port Orientation
            let handleSymOverlap (wModel:BusWireT.Model) (sym: Symbol) (side: Edge): Symbol =
                let h,w = getRotatedHAndW sym
                let symId = sym.Component.Id
                let symBox = getSymbolBoundingBox sym
                let otherSyms = wModel.Symbol.Symbols |> Map.filter (fun id _ -> not(string id = symId))
                let otherSymBoxes = otherSyms |> Map.values |> Seq.map (fun sym -> getSymbolBoundingBox sym) |> Seq.toList
                let otherSym =
                    otherSymBoxes
                    |>
                    List.filter (fun otherSymBox -> overlap2DBox symBox otherSymBox )
                if not(List.isEmpty otherSym)  then
                    let sym' =
                        match side with
                        | Top -> moveSymbol {X=0.0;Y = - h/2.0} sym
                        | Bottom -> moveSymbol {X=0.0;Y =  h/2.0} sym
                        | Left -> moveSymbol {X= w/2.0 ;Y = 0.0} sym
                        | Right -> moveSymbol {X= - w/2.0 ;Y = 0.0} sym
                    sym'
                else
                    sym

            /// assuming movePortInfo and otherPortInfo are on opposite edges
            /// returns true if the wire connecting the ports is a loop shape
            let handleWireLoop (movePortInfo: PortInfo) (otherPortInfo: PortInfo): bool =
                let portPos,otherPortPos = calculatePortRealPos movePortInfo, calculatePortRealPos otherPortInfo
                let isLoop =
                    match otherPortInfo.side with
                    | Left ->
                        if otherPortPos.X < portPos.X then
                            true
                        else
                            false
                    | Right ->
                        if otherPortPos.X > portPos.X then
                            true
                        else
                            false
                    | Top  ->
                        if otherPortPos.Y > portPos.Y then
                            true
                        else
                            false
                    | Bottom ->
                        if otherPortPos.Y < portPos.Y then
                            true
                        else
                            false
                isLoop

            /// Align all Components with multiple ports
            let alignMultiplyComponents 
                (wModel: BusWireT.Model)
                (symA: Symbol)
                (portA: Port)
                (symB: Symbol)
                (portB: Port)
                : Symbol =
            
                // Only attempt to align symbols if they are connected by ports on parallel edges.
                let (movePortInfo, otherPortInfo) = (makePortInfo symA portA,makePortInfo symB portB) 
            
                let offset = alignPortsOffset movePortInfo otherPortInfo
                let offset' = if ((offset.X > (3.0 * symA.Component.W/4.0)) || (offset.Y > (3.0 * symA.Component.H/4.0)) || not(movePortInfo.side = otherPortInfo.side.Opposite) || (handleWireLoop movePortInfo otherPortInfo)) then
                                {X=0.0;Y=0.0}
                              else
                                offset            
                let symbol' = moveSymbol offset' symA
                let symbol'' = handleSymOverlap wModel symbol' movePortInfo.side
                symbol''
            
            /// Align all components that have eiter one input or output port
            let alignSinglyComponents 
                (wModel: BusWireT.Model)
                (symA: Symbol)
                (portA: Port)
                (symB: Symbol)
                (portB: Port)
                : BusWireT.Model =
            
                // Only attempt to align symbols if they are connected by ports on parallel edges.
                let (movePortInfo, otherPortInfo) = (makePortInfo symA portA,makePortInfo symB portB) 

                let offset = alignPortsOffset movePortInfo otherPortInfo
                let offset' = if  not(movePortInfo.side = otherPortInfo.side.Opposite) then
                                {X=0.0;Y=0.0}
                              else
                                offset 
                let symbol' = moveSymbol offset' symA
                let symbol'' = handleSymOverlap wModel symbol' movePortInfo.side
                let model' = Optic.set (symbolOf_ symA.Id) symbol'' wModel
                BusWireSeparate.routeAndSeparateSymbolWires model' symA.Id



            /// returns all singly compoenents with one output xor input port
            let getSinglyComp (wModel: BusWireT.Model): Symbol list option =
                Optic.get symbols_ wModel.Symbol
                |> Map.fold (fun acc (sId:ComponentId) (sym:SymbolT.Symbol) ->
                    match getComponentProperties sym.Component.Type sym.Component.Label with
                    | (0, 1, _, _) -> sym :: acc
                    | (1, 0, _, _) -> sym :: acc
                    | _ -> acc) []
                |> List.rev
                |> fun result -> 
                    if List.isEmpty result then None
                    else Some result

    
            /// returns all multiply connected components on a sheet
            let getMultiplyComp (wModel: BusWireT.Model): Symbol list option =
                Optic.get symbols_ wModel.Symbol
                |> Map.fold (fun acc (sId:ComponentId) (sym:SymbolT.Symbol) ->
                    let (numInputs, numOutputs, _, _) = getComponentProperties sym.Component.Type sym.Component.Label
                    match (numInputs, numOutputs) with
                    | (i, o) when i + o > 1 -> sym :: acc  // Components with more than one input or output
                    | _ -> acc) []  // Ignore components with one or no inputs/outputs
                |> List.rev
                |> fun result -> 
                    if List.isEmpty result then None
                    else Some result

            /// For a list of connecting Symbol Pairs, return the list of pairs where both symbols are custom Components
            let filterCustomComp (portPair: (Symbol * Symbol) list): ((Symbol * Symbol) list) =
                portPair
                |> List.filter (fun (syma, symb) -> 
                    match syma.Component.Type, symb.Component.Type with
                    | Custom _, Custom _ -> true
                    | _ -> false)

    
        
            /// returns the ports for a given symbol
            let getPortSym (model: SymbolT.Model) (sym: Symbol): Map<string,Port>  =
                let portId = sym ^. (portMaps_ >-> orientation_) |> Map.keys
                portId
                |> Seq.choose (fun id ->
                    match Map.tryFind id model.Ports with
                    | Some port -> Some (id, port)
                    | None -> None)
                |> Map.ofSeq

                (*|> Seq.fold (fun acc id -> 
                    match Map.tryFind id model.Ports with
                    | Some port -> Map.add id port acc
                    | None -> acc) Map.empty*)
    
            /// Finds the corresponding WId(s) and other port(s) for a given Port based on connections.
            /// `port` is the given port to find matches for.
            /// `connections` is the list of all connections to search through.
            let findOtherPorts (port: Port) (connections: Connection list): (string * Port) list =
            
                connections |> List.collect (fun connection ->
                    if connection.Source.Id = port.Id then
                        [(connection.Id,connection.Target)]
                    elif connection.Target.Id = port.Id then
                        [(connection.Id,connection.Source)]
                    else
                        [] // No match, proceed to the next connection
                )
        
            /// return the corresponding Symbol for a given Port
            let getSym (wModel:BusWireT.Model) (port:Port): Symbol =
                let sym =
                    port.HostId
                    |> ComponentId
                    |> fun id -> wModel.Symbol.Symbols[id]
                sym

            /// For a given symbol and edge, will return the number of ports on that edge
            let getPortCount (sym: Symbol) (edge: Edge): int =
                sym 
                |> Optic.get portMaps_
                |> (fun pm -> pm.Order)
                |> (fun orderMap -> Map.find edge orderMap)
                |> List.length

            /// Returns the offset of two ports relative to portA
            let getPortOffset (wModel:BusWireT.Model) (portA: Port) (portB:Port): XYPos =
                let symA = getSym wModel portA
                let symB = getSym wModel portB
                let offset =
                    {
                        X = (calculatePortRealPos (makePortInfo symB portB)).X - (calculatePortRealPos (makePortInfo symA portA)).X;
                        Y = (calculatePortRealPos (makePortInfo symB portB)).Y - (calculatePortRealPos (makePortInfo symA portA)).Y
                    }
                offset

            /// For Components with multiple output ports, will return the most suitable Port to align
            let choosePortAlign (wModel:BusWireT.Model) (ports: Port list): (Port * Port) option =
                let connections = extractConnections wModel
                let portPair =
                    ports
                    |> List.collect (fun port -> 
                        let portOrientation = getSymbolPortOrientation (getSym wModel port) port
                        findOtherPorts port connections
                        |> List.choose (fun (_, otherPort) ->
                            let otherPortOrientation = getSymbolPortOrientation (getSym wModel otherPort) otherPort
                            if portOrientation = otherPortOrientation.Opposite then
                                let (portInfo,otherPortInfo) = (makePortInfo (getSym wModel port) port, makePortInfo (getSym wModel otherPort) otherPort)
                                let offset = alignPortsOffset portInfo otherPortInfo
                                Some (offset, (port, otherPort))
                            else
                                None))
                match portPair with
                    | [] -> None
                    | _ ->
                        portPair
                        |> List.minBy fst
                        |> snd
                        |> Some
            
        

            /// Chooses other Symbol to align a port based on the condition that they are opposite parallel edges
            /// and will choose the symbol with the smallest offset distance to help avoid symbol overlaps
            let chooseSymAlign (port: Port) (otherPorts: (string * Port) list) (wModel:BusWireT.Model): (Symbol * Port) option=
                if List.length otherPorts >1 then
                    let portOrientation = (makePortInfo (getSym wModel port) port).side
                
                    let filteredAndMappedPorts =
                        otherPorts
                        |> List.choose (fun (_, otherPort) ->
                            let otherPortOrientation = (makePortInfo (getSym wModel otherPort) otherPort).side
                            if portOrientation = otherPortOrientation.Opposite then //check if the other component port are on opposite edges
                                let (portInfo,otherPortInfo) = (makePortInfo (getSym wModel port) port, makePortInfo (getSym wModel otherPort) otherPort)
                                let offset = alignPortsOffset portInfo otherPortInfo
                                Some (offset, otherPort)
                            else
                                None) 
                    match filteredAndMappedPorts with
                    | [] -> None // No matching ports
                    | _ ->
                        filteredAndMappedPorts
                        |> List.minBy fst //Choose the port with the smallest offset (X Y Is chosen depending on orientation)
                        |> snd
                        |> (fun port -> Some ((getSym wModel port),port))
                else
                    let otherSym =
                        otherPorts
                        |> List.tryHead
                        |> Option.map (fun (_,port) -> ((getSym wModel port),port))
                    otherSym
    
        
            /// If SymA and SymB have multiple Parallel lines connected, Will resize either SymA or SymB to align ports
            /// This will only work for custom components
            let scaleMultiplyComp (wModel: BusWireT.Model) (syma: Symbol) (edgea:Edge) (symb: Symbol) (edgeb:Edge) : BusWireT.Model =
                // Determine scaleSym and otherSym based on port counts
                let scaleSym, otherSym =
                    if (getPortCount syma edgea < getPortCount symb edgeb) then symb, syma else syma, symb
                let scaleSym' = reSizeSymbol wModel scaleSym otherSym
                let wModel' = Optic.set (symbolOf_ scaleSym.Id) scaleSym' wModel
                BusWireSeparate.routeAndSeparateSymbolWires wModel' scaleSym.Id

            /// Align all multiply components where possible
            let alignMultiplyComp (wModel: BusWireT.Model) (syms: Symbol list): BusWireT.Model =
                let adjustments =
                    syms
                    |> List.map (fun sym ->
                        let outputPorts = getPortSym wModel.Symbol sym
                                         |> Map.filter (fun _ port -> port.PortType = PortType.Output)
                                         |> Map.values
                                         |> Seq.toList
                        match outputPorts with
                        | [] -> (sym,sym) // If there are no output ports, return the sheet as is
                        | _ -> 
                            match choosePortAlign wModel outputPorts with
                                | Some (portA, portB) -> 
                                    let otherSym = getSym wModel portB
                                    let sym' = alignMultiplyComponents wModel sym portA otherSym portB
                                    (sym,sym')
                                | None -> (sym,sym))
                 
                let wModel' =
                    adjustments
                    |> List.fold (fun model (sym, sym') ->
                        // Update the model for each sym to sym' adjustment.
                        // This assumes 'Optic.set' can update the model based on sym.Id, and 'symbolOf_' gets the appropriate lens/path.
                        let model' = Optic.set (symbolOf_ sym.Id) sym' model
                        BusWireSeparate.routeAndSeparateSymbolWires model' sym.Id
                    ) wModel
                wModel'
               
               
    module Beautify =
        open D1Helpers

        /// Align all Singly Components in a sheet
        let alignSinglyComp (wModel: BusWireT.Model): BusWireT.Model =
            let singlyComp = getSinglyComp wModel
            
            let connections = extractConnections wModel
            match singlyComp with
            | Some syms ->
                List.fold (fun wModel' sym ->
                    let portsMap = getPortSym wModel'.Symbol sym
                    // Since it's a singly component, assume there is only one port
                    let portOption = portsMap |> Map.values |> Seq.tryHead
                    match portOption with
                    | Some port ->
                        let otherPorts = findOtherPorts port connections
                        
                        let otherSymOpt = chooseSymAlign port otherPorts wModel'
                        match otherSymOpt with
                        | Some (otherSym,otherPort) ->
                            let wModel'' = alignSinglyComponents  wModel' sym port otherSym otherPort
                            wModel''
                        | None ->
                            
                            wModel' // If no suitable alignment, keep the model unchanged
                    | None ->
                        
                        wModel' // If no port is found, proceed to the next symbol                            
                ) wModel syms
            | None ->
                
                wModel // If getSinglyComp returns None, return the model unchanged


    
        /// Align all components with multiple ports and scale custom components to reduce wire crossings
        let alignScaleComp (wModel: BusWireT.Model): BusWireT.Model =
            let multiplySyms = getMultiplyComp wModel
            let connPairs = getConnSyms wModel
            let custPairs = filterCustomComp connPairs
            let customSymbols = custPairs |> List.collect (fun (a, b) -> [a; b])

            let alignSyms = 
                match multiplySyms with
                | Some multiplySyms ->
                    List.filter (fun sym -> not (List.contains sym customSymbols)) multiplySyms
                | None -> [] // If there are no multiplySyms, use an empty list for alignSyms
            // Iterate over each pair in custPairs for processing
            let wModel' =
                if List.isEmpty custPairs then
                    wModel
                else
                    List.fold (fun wModel' (symA, symB) ->
                        printfn $"CUSTY"
                        match getOppEdgePortInfo wModel' symA symB with
                        | Some (portInfoA, portInfoB) ->
                            let custEdges = (portInfoA.side, portInfoB.side)
                            scaleMultiplyComp wModel' symA (fst custEdges) symB (snd custEdges)                   
                        | None -> wModel'
                    ) wModel custPairs
                
            let wModel'' = 
                match alignSyms with
                | [] -> wModel' // If no symbols to align, skip this step
                | _ -> alignMultiplyComp wModel' alignSyms // Proceed with alignment if there are symbols to align
            // Return the final model after adjustments
            wModel''
        /// Top-Level function to align all Singly Components in a sheet
        let sheetSingly (sheet: SheetT.Model): SheetT.Model =
            let singlyModel = alignSinglyComp sheet.Wire
            let sheet' = Optic.set SheetT.wire_ singlyModel sheet
            sheet'
        /// Top-Level function to align all components with multiple ports and scale custom components to reduce wire crossings
        let sheetMultiply (sheet: SheetT.Model): SheetT.Model =
            let multiplyModel = alignScaleComp sheet.Wire
            let multiplySheet = Optic.set SheetT.wire_ multiplyModel sheet
            multiplySheet

        /// Top-Level function to scale Custom Components and aligns components in a sheet to reduce wire segments without increasing wire crossings.
        let sheetAlignScale (sheet: SheetT.Model): SheetT.Model =
           let multiplyModel = alignScaleComp sheet.Wire
           let multiplySheet = Optic.set SheetT.wire_ multiplyModel sheet
           let singlyModel = alignSinglyComp multiplySheet.Wire
           let sheet' = Optic.set SheetT.wire_ singlyModel multiplySheet
           sheet'

module D2Build =

    let symbolModel_ = SheetT.symbol_

    /// Checks if a component type is a MUX or DEMUX type.
    let isMuxType (componentType: ComponentType) : bool = 
        [Mux2; Mux4; Mux8; Demux2; Demux4; Demux8] 
        |> List.contains componentType

    /// Checks if a component type is a Gate type.
    let isGateType (componentType: ComponentType) : bool = 
        match componentType with
        | GateN _ -> true
        | _ -> false

    /// Checks if a component type is a Custom Component type.
    let isCustomType (componentType: ComponentType) : bool = 
        match componentType with
        | Custom _ -> true
        | _ -> false


    /// Returns symbols that are of given component type.
    /// where isSymbolType is a function that returns a bool for a given component type.
    let getCompTypeSymbols (isSymbolType) (model: SheetT.Model) : Symbol list =
        mapValues model.Wire.Symbol.Symbols
        |> Array.filter (fun sym -> isSymbolType sym.Component.Type)
        |> Array.toList

    /// Checks if two symbols are directly connected through any wire.
    let areSymbolsConnected (wModel: BusWireT.Model) symA symB =
        wModel.Wires
        |> Map.exists (fun _ wire -> isConnBtwnSyms wire symA symB)


    /// Generates all permutations for given number of MUX and Gate components.
    /// where MUX components have 4 states, Gate components have 2 states
    let generateMuxGatePermutations  (muxCount: int, gateCount: int) =
        let muxStates = [(false, false); (false, true); (true, false); (true, true)]
        let gateStates = [false; true]

        let rec generatePermutations depth states acc =
            match depth with
            | 0 -> acc
            | _ -> 
                let nextAcc = 
                    acc |> List.collect (fun accPerm -> 
                        states |> List.map (fun state -> 
                            state :: accPerm))
                generatePermutations (depth - 1) states nextAcc

        let muxPermutations = generatePermutations muxCount muxStates [[]]
        let gatePermutations = generatePermutations gateCount gateStates [[]]

        List.allPairs muxPermutations gatePermutations

    //let printMixedPermutations (muxCount: int, gateCount: int) =
    //    let mixedPermutations = makeMixedPermutations (muxCount, gateCount)
    //    let stateToString (state: bool) = if state then "1" else "0"
    //    let muxStateToString (state: (bool * bool)) =
    //        let (rev, flip) = state
    //        sprintf "[%s,%s]" (stateToString rev) (stateToString flip)
    //    mixedPermutations |> List.iter (fun (muxPerm, gatePerm) ->
    //        let muxStatesStr = muxPerm |> List.map muxStateToString 
    //        let gateStatesStr = gatePerm |> List.map stateToString 

    //        printf $"MUX: {muxStatesStr} | Gates: {gateStatesStr}"
    //    )


    /// Applies permutation of MUX and Gate states to a sheet
    let genSheetFromMuxGatePermutation (model: SheetT.Model)
                                       (muxSymbols: Symbol list, gateSymbols: Symbol list)
                                       (muxPermutation: (bool * bool) list, gatePermutation: bool list) =
        let allComponents =
            (muxSymbols @ gateSymbols)
            |> List.map (fun sym -> sym.Id)

        let updateGates accModel (flipped, sym) =
            if flipped then
                SheetBeautifyHelpers.flipSymbol sym.Component.Label FlipVertical accModel
            else accModel

        let updateMuxes accModel ((reversed, flipped), sym) =
            let symReversed = Optic.set reversedInputPorts_ (Some reversed) sym
            if flipped then
                SheetBeautifyHelpers.flipSymbol symReversed.Component.Label FlipVertical accModel
            else accModel

        let updatedGates =
            List.fold updateGates model (List.zip gatePermutation gateSymbols)

        let updatedModel =
            List.fold updateMuxes updatedGates (List.zip muxPermutation muxSymbols)

        let reroutedModel = BusWireSeparate.reRouteWiresFrom allComponents updatedModel.Wire
        { updatedModel with Wire = reroutedModel }


    /// Objective function used to evaluate the quality of a sheet
    /// Lower is better
    let calculateSheetObjective (model: SheetT.Model) : int =
        // TODO: objective function : #wire crossings, #right angles ?
        numOfWireRightAngleCrossings model


    /// Exhaustive search through all permutations of MUX and Gate flips to find the sheet with the lowest objective.
    let permuteMuxGateState (model: SheetT.Model) (muxSymbols: Symbol list, gateSymbols: Symbol list) : SheetT.Model =
        let muxCount = List.length muxSymbols
        let gateCount = List.length gateSymbols

        let permutations = generateMuxGatePermutations (muxCount, gateCount)
        let originalObjective = calculateSheetObjective model
        printf $"Permutation length: {permutations.Length}\n"

        // Generate all permuted sheets
        // filtering out the ones that have worse objective than original
        let permutedSheets = 
            permutations
            |> List.map (fun (muxPerm, gatePerm) ->
                    genSheetFromMuxGatePermutation model (muxSymbols, gateSymbols) (muxPerm, gatePerm))
            |> List.filter (fun updatedSheet ->
                    calculateSheetObjective updatedSheet < originalObjective)

        // Return the sheet with the lowest objective, if any
        match permutedSheets with
        | [] -> model
        | _ -> permutedSheets |> List.minBy calculateSheetObjective


    /// Groups directly connected gates and muxes for localised permuting.
    /// Doesn't produce isolated, distinct groups
    let groupConnectedSymbols (model: SheetT.Model) (symbols: Symbol list) : Symbol list list =
        let wModel = model.Wire

        let getConnectedSymbols symA =
            symbols
            |> List.filter (fun symB -> symA <> symB && areSymbolsConnected wModel symA symB)
            |> List.append [symA]
            |> List.sortBy (fun sym -> sym.Id)

        let getSymbolById id =
            symbols |> List.find (fun sym -> sym.Id = id)

        symbols
        |> List.map getConnectedSymbols
        |> List.map (List.map (fun sym -> sym.Id))
        |> List.distinct
        |> List.map (List.map getSymbolById)


    // Group by splitting into Symbol Lists of connected gates + muxes (only directly connected)
    // Generate all the permutations for group
    // Apply permutations to sheet + autoroute + filter to find best group sheet
    // Use that local best going forwards for next group - until all groups done
    let optimiseGroupedMuxGate (model: SheetT.Model) : SheetT.Model =
        let muxSymbols = getCompTypeSymbols isMuxType model
        let gateSymbols = getCompTypeSymbols isGateType model
        let allSymbols = muxSymbols @ gateSymbols

        let groupedSymbols = groupConnectedSymbols model allSymbols
        //groupedSymbols |> List.iteri (fun i group ->
        //    printfn "Group %d size: %d" (i + 1) (List.length group))

        // Iterate over each group, updating sheet iteratively with the best permutation found for each group
        let finalSheet = 
            groupedSymbols
            |> List.fold (fun currentSheet group ->
                let groupMuxSymbols = 
                    List.filter (fun sym -> isMuxType sym.Component.Type) group
                let groupGateSymbols = 
                    List.filter (fun sym -> isGateType sym.Component.Type) group
                permuteMuxGateState currentSheet (groupMuxSymbols, groupGateSymbols)
            ) model

        finalSheet


    /// Calculates the angle for an edge based on the centre and destination port positions.
    let calculateAngleForEdge (centre: XYPos) (destPort: XYPos) (edge: Edge) =
        let deltaX = destPort.X - centre.X
        // Invert deltaY for top and bottom edges
        let deltaY = 
            match edge with
            | Top | Bottom -> centre.Y - destPort.Y  // Invert deltaY for top and bottom edges
            | Left | Right -> destPort.Y - centre.Y 
    
        let angleRadians = atan2 deltaY deltaX
        // printf $"Edge: {edge}, destPort: {destPort}, angle: {angleRadians}"
        angleRadians


    /// Gets the centre position of a custom component symbol.
    let getCustomCompCentre (sym: Symbol) : XYPos =
        let dims = getCustomCompDims sym
        // calculate centre of custom component symbol
        { X = sym.Pos.X + dims.X / 2.0; Y = sym.Pos.Y + dims.Y / 2.0 }


    /// Gets the connected port ID for a given port ID.
    let getConnectedPort (model: SheetT.Model) (portID: string) =
        let wires = 
            model.Wire.Wires
            |> Map.filter (fun _ wire -> 
                wire.InputPort = InputPortId portID || wire.OutputPort = OutputPortId portID)
            |> Map.toList

        wires
        |> List.tryPick (fun (_, wire) ->
            match wire.InputPort, wire.OutputPort with
            | InputPortId pid, _ when pid <> portID -> Some pid
            | _, OutputPortId pid when pid <> portID -> Some pid
            | _ -> None
        )
    

    /// Reorders the ports of a custom component symbol by angle.
    let reorderPortsByAngle (sym: Symbol) (model: SheetT.Model) : Symbol =
        let centre = getCustomCompCentre sym

        // Function to process and reorder ports for a given edge
        let processEdge sym edge =
            let portIds = getPortOrder edge sym

            // Separate connected and unconnected ports
            let connectedPorts, unconnectedPorts =
                portIds
                |> List.partition (fun portId -> getConnectedPort model portId |> Option.isSome)

            let connectedPortAngles =
                connectedPorts
                |> List.map (fun portId ->
                    let connectedPortId = getConnectedPort model portId |> Option.get
                    let portPos = getPortPos connectedPortId model.Wire.Symbol
                    let angle = calculateAngleForEdge centre portPos edge
                    (portId, angle))
                |> List.sortBy snd
                |> List.map fst
        
            let sortedPortIds = connectedPortAngles @ unconnectedPorts

            putPortOrder edge sortedPortIds sym

        [Edge.Top; Edge.Bottom; Edge.Left; Edge.Right]
        |> List.fold processEdge sym


    /// Applies custom component port reordering to a sheet.
    let applyCustomCompReorder (model: SheetT.Model) (customCompSym: Symbol) : SheetT.Model =
        let reorderedCustomComp = reorderPortsByAngle customCompSym model
        let updatedModel = 
            model
            |> Optic.set (symbolModel_ >-> SymbolT.symbolOf_ reorderedCustomComp.Id) reorderedCustomComp
        let reroutedModel = BusWireSeparate.reRouteWiresFrom [customCompSym.Id] updatedModel.Wire

        { updatedModel with Wire = reroutedModel }


    /// Iteratively optimise custom component ports by reordering them per edge
    let optimiseCustomCompPorts (sheet: SheetT.Model) : SheetT.Model =
        let customCompSymbols = getCompTypeSymbols isCustomType sheet
        let originalObjective = calculateSheetObjective sheet

        let optimisedSheet =
            customCompSymbols
            |> List.fold (fun currentSheet customCompSym ->
                let reorderedSheet = applyCustomCompReorder currentSheet customCompSym
                let reorderedObjective = calculateSheetObjective reorderedSheet
                if reorderedObjective < calculateSheetObjective currentSheet then
                    reorderedSheet
                else
                    currentSheet
            ) sheet

        if calculateSheetObjective optimisedSheet < originalObjective then
            optimisedSheet
        else
            sheet


    /// For testing: Reorder all custom component ports on sheet
    /// TODO: Consider custom connected to custom 
    let reorderAllCustomCompPorts (sheet: SheetT.Model) : SheetT.Model =
        let customCompSymbols = getCompTypeSymbols isCustomType sheet
        printfn $"Custom Comp Count {List.length customCompSymbols}"
        customCompSymbols
        |> List.fold (fun currentSheet customCompSym ->
            applyCustomCompReorder currentSheet customCompSym
        ) sheet









let sheetAlignScale (sheet:SheetT.Model) = Beautify.sheetAlignScale sheet
    
(*let sheetOrderFlip (sheet:SheetT.Model) = 
    permuteMuxState sheet*)

let sheetWireLabelSymbol (sheet:SheetT.Model) = 
    removeComplexWires sheet

let sheetOrderFlip (sheet:SheetT.Model) = 
    sheet

let beautifySheet (sheet:SheetT.Model) = 
    sheet
    |> sheetAlignScale
    |> sheetOrderFlip
    |> sheetWireLabelSymbol



    
