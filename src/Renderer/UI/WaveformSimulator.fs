module WaveformSimulator

open Fulma
open Fable.React
open Fable.React.Props

open ModelType
open DiagramStyle
open CommonTypes
open FileMenuView
open SimulatorTypes
open Sheet.SheetInterface
open DrawModelType

/// Determines whether a clock cycle is generated with a vertical bar at the beginning,
/// denoting that a waveform changes value at the start of that clock cycle. NB this
/// does not determine whether a waveform changes value at the end of that clock cycle.
type BinaryTransition =
    | ZeroToZero
    | ZeroToOne
    | OneToZero
    | OneToOne
    // Is this last transition really necessary?
    | Start

/// Determines whether a non-binary waveform changes value at the beginning and end of
/// that clock cycle.
type NonBinaryTransition =
    | ChangeToChange
    | ConstToConst
    | ChangeToConst
    | ConstToChange

/// Waveforms can be either binary or non-binary; these have different properties.
type Transition =
    | BinaryTransition of BinaryTransition
    | NonBinaryTransition of NonBinaryTransition

/// TODO: Make a Constants module
/// TODO: Tweak these values
/// Height of a waveform
let waveHeight = 0.3
let clkCycleWidth = 1.0

// TODO: Originally located in WaveSimHelpers.fs. Not really sure where this should go.

//------------------------------------//
//    Interaction with Model.Diagram  //
//------------------------------------//

/// is the given connection in the given NLTarget list
let private isConnInNLTrgtLst (connId: ConnectionId) (nlTrgtLst: NLTarget list) =
    List.exists (fun nlTrgt -> nlTrgt.TargetConnId = connId) nlTrgtLst

/// is the given component connected to the NLTarget list
let private isCompInNLTrgtLst (nlComponent: NetListComponent) (nlTrgtLst: NLTarget list) =
    Map.exists (fun _ compNLTrgtLst -> compNLTrgtLst = nlTrgtLst) nlComponent.Outputs
    || List.exists (fun nlTrgt -> nlTrgt.TargetCompId = nlComponent.Id) nlTrgtLst

/// is the given NLTarget list selected by the given selection
let private isNLTrgtLstSelected (netList: NetList) ((comps, conns): CanvasState) (nlTrgtLst: NLTarget list) =
    List.exists (fun (comp: Component) -> 
        match Map.tryFind (ComponentId comp.Id) netList with
        | Some comp -> isCompInNLTrgtLst comp nlTrgtLst
        | None -> false ) comps
    || List.exists (fun (conn: Connection) -> 
           isConnInNLTrgtLst (ConnectionId conn.Id) nlTrgtLst) conns

/// is the given waveform selected by the given selection
let private isNetGroupSelected (netList: NetList) ((comps, conns): CanvasState) (trgtLstGroup: NetGroup) =
    Array.append [|trgtLstGroup.driverNet|] trgtLstGroup.connectedNets
    |> Array.exists (isNLTrgtLstSelected netList (comps, conns)) 

/// get Ids of connections in a trgtLstGroup
let private wave2ConnIds (netGrp: NetGroup) =
    Array.append [|netGrp.driverNet|] netGrp.connectedNets
    |> Array.collect (fun net -> 
        List.toArray net 
        |> Array.map (fun net -> net.TargetConnId))

///Takes a connection and model, and returns the netgroup as a list of connectionIds associated with that connection
let getNetSelection (canvas : CanvasState) (model : Model) =
    let netList = 
        model.LastSimulatedCanvasState
        |> Option.map Helpers.getNetList 
        |> Option.defaultValue (Map.empty)

    let netGroups = makeAllNetGroups netList

    let selectedConnectionIds (ng:NetGroup) =
        if isNetGroupSelected netList canvas ng then 
            wave2ConnIds ng
        else [||]
            
    Array.collect selectedConnectionIds netGroups
    |> Array.toList

let binaryWavePoints (clkCycle: int) (transition: BinaryTransition) : XYPos list * int =
    let xLeft = float clkCycle * clkCycleWidth
    let xRight = float (clkCycle + 1) * clkCycleWidth
    let yTop = 0
    let yBot = 10//waveHeight
    let topL = {X = xLeft; Y = yTop}
    let topR = {X = xRight; Y = yTop}
    let botL = {X = xLeft; Y = yBot}
    let botR = {X = xRight; Y = yBot}
    // Each match condition generates a specific transition type
    match transition with
    | ZeroToZero ->
        [botL; botR], clkCycle + 1
    | ZeroToOne ->
        [botL; topL; topR], clkCycle + 1
    | OneToZero ->
        [topL; botL; botR], clkCycle + 1
    | OneToOne ->
        [topL; topR], clkCycle + 1

let nonBinaryWavePoints (clkCycle: int) (transition: NonBinaryTransition) : (XYPos list * XYPos list) * int =
    // Use start coord to know where to start the polyline
    let xLeft = float clkCycle * clkCycleWidth
    let xRight = float (clkCycle + 1) * clkCycleWidth
    let yTop = 0
    let yBot = waveHeight
    let topL = {X = xLeft; Y = yTop}
    let topR = {X = xRight; Y = yTop}
    let botL = {X = xLeft; Y = yBot}
    let botR = {X = xRight; Y = yBot}
    let crossHatchTop = {X = xLeft + nonBinaryTransLen; Y = yTop}
    let crossHatchBot = {X = xLeft + nonBinaryTransLen; Y = yBot}
    // Each match condition generates a specific transition type
    match transition with
    // This needs to account for different zoom levels:
    // Can probably just look at screen size and zoom level
    // And then scale the horizontal part accordingly
    // When zoomed out sufficiently and values changing fast enough,
    // The horizontal part will have length zero.
    // May need to account for odd/even clock cycle
    | Change ->
        ([topL; crossHatchBot; botR], [botL; crossHatchTop; topR]), clkCycle + 1
    | Const ->
        ([topL; topR], [botL; botR]), clkCycle + 1
    // | ChangeToChange
    // | ConstToConst
    // | ChangeToConst
    // | ConstToChange ->


// /// Generates the points for one clock cycle of a binary or non-binary waveform
// /// When generating points for non-binary transitions, the start of that clock
// /// cycle is offset to the left a bit so that the full cross-hatch of a transition
// /// can be generated
// let generateClkCycle (startCoord: XYPos) (clkCycle: int) (transition: Transition) : XYPos list =
//     // Use start coord to know where to start the polyline
//     let xLeft = float clkCycle * clkCycleWidth
//     let xRight = float (clkCycle + 1) * clkCycleWidth
//     let yTop = 0
//     let yBot = waveHeight
//     let topL = {X = xLeft; Y = yTop}
//     let topR = {X = xRight; Y = yTop}
//     let botL = {X = xLeft; Y = yBot}
//     let botR = {X = xRight; Y = yBot}
//     let crossHatchTop = {X = xLeft + nonBinaryTransLen; Y = yTop}
//     let crossHatchBot = {X = xLeft + nonBinaryTransLen; Y = yBot}
//     // Each match condition generates a specific transition type
//     match transition with
//     | BinaryTransition x ->
//         match x with
//         | ZeroToZero ->
//             [botL; botR]
//         | ZeroToOne ->
//             [botL; topL; topR]
//         | OneToZero ->
//             [topL; botL; botR]
//         | OneToOne ->
//             [topL; topR]
//     | NonBinaryTransition x ->
//         // This needs to account for different zoom levels:
//         // Can probably just look at screen size and zoom level
//         // And then scale the horizontal part accordingly
//         // When zoomed out sufficiently and values changing fast enough,
//         // The horizontal part will have length zero.
//         match x with
//         // May need to account for odd/even clock cycle
//         | Change ->
//             []
//         | Const ->
//         // | ChangeToChange
//         // | ConstToConst
//         // | ChangeToConst
//         // | ConstToChange ->
//             failwithf "NonBinaryTransition not implemented"

/// Generates SVG to display waveform values when there is enough space
let displayValuesOnWave (startCycle: int) (endCycle: int) (waveValues: WireData list) : ReactElement =
    // enough space means enough transitions such that the full value can be displayed before a transition occurs
    // values can be displayed repeatedly if there is enough space
    // try to centre the displayed values?
    failwithf "displayValuesOnWave not implemented"

let determineBinaryTransitions waveValues =
    let firstValue = List.head waveValues
    (firstValue, waveValues)
    ||> List.mapFold
        (fun prev value ->
            match prev, value with
            | [Zero], [Zero] -> ZeroToZero, value
            | [Zero], [One] -> ZeroToOne, value
            | [One], [Zero] -> OneToZero, value
            | [One], [One] -> OneToOne, value
            | _ ->
                failwithf "Unrecognised transition"
        )
    |> fst

let determineNonBinaryTransitions waveValues =
    // Use [] so that first clock cycle always starts with Change
    ([], waveValues)
    ||> List.mapFold
        (fun prev value ->
            if prev = value then
                Const, value
            else
                Change, value
        )
    |> fst

let makeLinePoints style (x1, y1) (x2, y2) =
    line
        (List.append style 
             [ X1 x1
               Y1 y1
               X2 x2
               Y2 y2 ]) []

let makeSvg style elements = svg style elements
let makeLine style = line style []
let makeText style t = text style [ str t ]

let makePolyline style elements = polyline style elements

// let makeLinePoints style (x1, y1) (x2, y2) =
//     line
//         (List.append style 
//              [ X1 x1
//                Y1 y1
//                X2 x2
//                Y2 y2 ]) []

// let makeSigLine =
//     makeLinePoints
//         [ Class "sigLineStyle"
//           Style [ Stroke("blue") ] ]

let pointsToString (points: XYPos list) : string =
    List.fold (fun str (point: XYPos) ->
        str + string point.X + "," + string point.Y + " "
    ) "" points

/// Generates the SVG for a specific waveform
let generateWave (startCycle: int) (endCycle: int) (waveName: string) (wave: Wave): ReactElement =
    // need to know type of waveValues
    // fold or iter over each value in waveValues (i.e. once for each clock cycle)
    // fold function generates an svg for each clock cycle? 

    // TODO: How to calculate this?
    let startCoord = {X = 0; Y = 0}



    // let transitions =
    match wave.Width with
        | 0 -> failwithf "Cannot have wave of width 0"
        | 1 ->
            let transitions = determineBinaryTransitions wave.WaveValues
            let wavePoints =
                List.mapFold binaryWavePoints startCycle transitions
                |> fst
                |> List.concat

            printf "%A: %A" wave.DisplayName wavePoints

            let line = 
                polyline
                    [ 
                        SVGAttr.Stroke "blue"
                        SVGAttr.Fill "none"
                        SVGAttr.StrokeWidth 5// lineThickness
                        
                        // Points "0,0 0.25,0.25 5,5"

                        Points (pointsToString wavePoints)
                    ]
                    []
                    // wavePoints
            // Generate polyline
            line
        | _ -> 
            let transitions = determineNonBinaryTransitions wave.WaveValues
            let wavePoints = List.mapFold nonBinaryWavePoints startCycle transitions
            let line = 
                polyline
                    [ Style 
                        [
                            Stroke "blue"
                            Fill "none"
                            StrokeWidth lineThickness
                            
                        ]
                      Points "0,0 0.25,0.25"
                    ]
                    []
                    // wavePoints
            // Generate polyline
            line


    // match wave.Width with

    // let wavePoints = Array.map (generateClkCycle startCoord) transitions

    // // Use wavePoints to generate SVG polyline

    // // This is only for non-binary waveforms though.
    // let waveValuesSVG = displayValuesOnWave startCycle endCycle waveValues

    // TODO: Combine waveValuesSVG and wavesSVG

    // failwithf "generateWave not implemented"

/// Generates the SVG for all waves
let generateAllWaves (waves: Map<string, Wave>) (startCycle: int) (endCycle: int) : ReactElement array = 
    // Iterate over each wave to generate that wave's SVG
    // printf "%A" waves["C2"].WaveValues
    // printf "%A" waves["C2"]

    waves
    |> Map.map (generateWave startCycle endCycle)
    |> Map.values
    |> Seq.toArray
    // failwithf "Not implemented"


let generateAllLabels waves =
    failwithf "generateAllLabels not implemented"

let generateValuesPerClkCycle waves clkCycle =
    failwithf "generateValuesPerClkCycle not implemented"

/// TODO: Test if this function actually works.
let displayErrorMessage error =
    [ div
        [ Style [ Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ]
        [
            SimulationView.viewSimulationError error
        ]
    ]

/// Upon clicking the View buttonm, the wave selection pane will change to the wave viewer pane.
let viewWaveformsButton model dispatch =
    let wsModel = model.WaveSim
    let viewButtonAction =
        let selectedWaves = Map.filter (fun _ key -> key.Selected) wsModel.AllWaves

        match Map.count selectedWaves with
        | 0 -> [ Button.IsLight; Button.Color IsSuccess ]
        | _ ->
            [
                Button.Color IsSuccess
                // Button.IsLoading (showSimulationLoading wSModel dispatch)
                Button.OnClick(fun _ ->
                    // let par' = {wSModel.SimParams with DispNames = viewableWaves }
                    let waveSVGs = generateAllWaves selectedWaves wsModel.StartCycle wsModel.EndCycle
                    let wsMod' = {wsModel with State = Running; SVG = waveSVGs}

                    let msgs = [
                        (StartUICmd ViewWaveSim)
                        ClosePropertiesNotification
                        InitiateWaveSimulation wsMod'
                        // (InitiateWaveSimulation( WSViewerOpen, par'))
                    ]
                    dispatch (Sheet (SheetT.SetSpinner true))
                    dispatch <| SendSeqMsgAsynch msgs)
            ]
        |> (fun lst -> 
                Button.Props [ Style [ MarginLeft "10px" ] ] :: lst)

    div [ Style [ Display DisplayOptions.Block ] ]
        [ Button.button viewButtonAction [str "View"] ]

// let selectConns (model: Model)  (conns: ConnectionId list) (dispatch: Msg -> unit) =
//     let allConns =
//         snd <| model.Sheet.GetCanvasState()
//         |> List.map (fun conn -> ConnectionId conn.Id)
//         |> Set.ofList
//     let otherConns = allConns - Set.ofList conns
//     let sheetDispatch sMsg = dispatch (Sheet sMsg)
//     model.Sheet.SelectConnections sheetDispatch true conns
//     model.Sheet.SelectConnections sheetDispatch false (Set.toList otherConns)

/// Sets all waves as selected or not selected depending on value of newState
let toggleSelectAll newState model dispatch : unit =
    let waveSimModel = model.WaveSim
    let waves = Seq.toList (Map.values waveSimModel.AllWaves)
    // let conns =
    //     if newState then
    //         waves
    //         |> List.collect (fun wave -> wave.Conns)
    //     else
    //         []
    let allWaves' = Map.map (fun _ wave -> {wave with Selected = newState}) waveSimModel.AllWaves
    dispatch <| SetWSMod {waveSimModel with AllWaves = allWaves'}
    // selectConns model conns dispatch

let isWaveSelected (waveSimModel: WaveSimModel) (name: string) : bool =
    waveSimModel.AllWaves[name].Selected

let selectAll (model: Model) dispatch =
    let waveSimModel = model.WaveSim
    // TODO: Implement function that returns true if all waves selected.
    let allWavesSelected = Map.forall (fun name _ -> isWaveSelected waveSimModel name) waveSimModel.AllWaves
    // let allOn = Map.forall (fun k wave -> isWaveSelected model wave) wSModel.AllWaves
    tr
        [ Class "rowHeight"
          Style [ VerticalAlign "middle" ]
        ]
        [ td
            [ Class "wACheckboxCol"
              Class "rowHeight"
              Style [ VerticalAlign "middle" ]
            ] [ input [
                    Type "checkbox"
                    Class "check"
                    Checked allWavesSelected
                    Style [ Float FloatOptions.Left ]
                    OnChange(fun _ -> toggleSelectAll (not allWavesSelected) model dispatch ) 
                ]
            ]
          td [ Style [ FontWeight "bold" ] ] [ str "Select All" ]
        ]

let toggleConnsSelect (name: string) (waveSimModel: WaveSimModel) (dispatch: Msg -> unit) =
    // let newState = not (isWaveSelected waveSimModel name)
    printf "toggleConnsSelect"
    let wave = waveSimModel.AllWaves[name]
    let wave' = {wave with Selected = not wave.Selected}
    printf "%A" wave'

    let waveSimModel' = {waveSimModel with AllWaves = Map.add name wave' waveSimModel.AllWaves}
    printf "%A" waveSimModel'.AllWaves[name]
    dispatch <| SetWSMod waveSimModel'

    // changeWaveSelection name model waveSimModel dispatch


let checkboxAndName (name: string) (model: Model) (dispatch: Msg -> unit) =
    let waveSimModel = model.WaveSim
    let allWaves = waveSimModel.AllWaves
    // TODO: Fix this to bold only Selected waves
    let getColorProp name  =
        if Map.containsKey name waveSimModel.AllWaves then
            [FontWeight "Bold"]
        else
            []
    // printf "checkboxAndName %A"  (str <| allWaves[name].DisplayName )

    tr
        [ Class "rowHeight"
          Style [ VerticalAlign "middle" ]
        ] 
        [ td
            [ 
                Class "wAcheckboxCol"
                Class "rowHeight"
                Style [ VerticalAlign "middle" ] ]
                [ input
                    [
                        Type "checkbox"
                        Class "check"
                        Checked <| isWaveSelected waveSimModel name
                        Style [ Float FloatOptions.Left ]
                        OnChange(fun _ -> toggleConnsSelect name waveSimModel dispatch)
                    ] 
                ]
          td [] [ label [Style (getColorProp name)] [ str <| allWaves[name].DisplayName ] ]
        ]

let checkboxListOfWaves (model: Model) (dispatch: Msg -> unit) =
    // printf "checkboxListOfWaves"
    let waveSimModel = model.WaveSim
    Seq.toArray (Map.keys waveSimModel.AllWaves)
    |> Array.map (fun name -> checkboxAndName name model dispatch)
    // failwithf "checkboxListOfWaves not implemented"

let selectWaves (model: Model) dispatch = 
    div [ Style [ Position PositionOptions.Relative
                  CSSProp.Top "20px"
                ]
        ]
        [ table []
            [ tbody []
                (Array.append 
                    [| selectAll model dispatch |]
                    (checkboxListOfWaves model dispatch)
                )
            ]
        ]

let closeWaveSimButton (wsModel: WaveSimModel) (dispatch: Msg -> unit) : ReactElement =
    let wsModel' = {wsModel with State = NotRunning}
    button 
        [Button.Color IsSuccess]
        (fun _ -> dispatch <| CloseWaveSim wsModel')
        "Edit waves / close simulator"


/// Generates the points for one clock cycle of a binary or non-binary waveform
let generateClkCycle (startCoord: XYPos) (transition: Transition) : XYPos list =
    // Use start coord to know where to start the polyline
    // Each match condition generates a specific transition type
    match transition with
    | BinaryTransition x ->
        match x with
        | ZeroToZero
        | ZeroToOne
        | OneToZero
        | OneToOne
        | Start ->
            failwithf "BinaryTransition not implemented"
    | NonBinaryTransition x ->
        // This needs to account for different zoom levels:
        // Can probably just look at screen size and zoom level
        // And then scale the horizontal part accordingly
        // When zoomed out sufficiently and values changing fast enough,
        // The horizontal part will have length zero.
        match x with
        | ChangeToChange
        | ConstToConst
        | ChangeToConst
        | ConstToChange ->
            failwithf "NonBinaryTransition not implemented"

/// Generates SVG to display waveform values when there is enough space
let displayValuesOnWave (startCycle: int) (endCycle: int) (waveValues: int array) : ReactElement =
    // enough space means enough transitions such that the full value can be displayed before a transition occurs
    // values can be displayed repeatedly if there is enough space
    // try to centre the displayed values?
    failwithf "displayValusOnWave not implemented"

let determineTransitions (waveValues: int array) : Transition array = 
    // waveValues
    // |> Array.map (fun val ->

    // )
    failwithf "determineTransitions not implemented"

/// Generates the SVG for a specific waveform
let generateWave (startCycle: int) (endCycle: int) (waveValues: int array) : ReactElement array =
    // need to know type of waveValues
    // fold or iter over each value in waveValues (i.e. once for each clock cycle)
    // fold function generates an svg for each clock cycle? 

    // TODO: How to calculate this?
    let startCoord = {X = 0; Y = 0}

    let transitions = determineTransitions waveValues
    let wavePoints = Array.map (generateClkCycle startCoord) transitions

    // Use wavePoints to generate SVG polyline

    // This is only for non-binary waveforms though.
    let waveValuesSVG = displayValuesOnWave startCycle endCycle waveValues

    // TODO: Combine waveValuesSVG and wavesSVG

    failwithf "generateWave not implemented"

let generateValues (wave: string) : int array =
    failwithf "generateValues not implemented"

/// Generates the SVG for all waves
let generateAllWaves (waves: string list) (startCycle: int) (endCycle: int) : ReactElement array array = 
    // Iterate over each wave to generate that wave's SVG
    waves
    |> List.map generateValues
    |> List.map (generateWave startCycle endCycle)
    |> List.toArray
    // failwithf "Not implemented"


let generateAllLabels waves =
    failwithf "generateAllLabels not implemented"

let generateValuesPerClkCycle waves clkCycle =
    failwithf "generateValuesPerClkCycle not implemented"

let showWaveforms simData rState (model: Model) (dispatch: Msg -> unit) : ReactElement list =
    [
        div [] []
    ]

    // Calls generateAllLabels, generateAllWaves, generateValuesPerClkCycle
    // inputs tbd

/// get common NLSource of list of NLTarget with the same source
let private nlTrgtLst2CommonNLSource (netList: NetList) (nlTrgtLst: NLTarget list) : NLSource option =
    List.tryPick (fun (nlTrgt: NLTarget) -> 
        match Map.tryFind nlTrgt.TargetCompId netList with
        | Some comp -> 
            match Map.tryFind nlTrgt.InputPort comp.Inputs with
            | Some (Some src) -> Some src
            | _ -> None
        | None -> None ) nlTrgtLst

/// get label without (x:x) part at the end
let private labelNoParenthesis (netList: NetList) compId = 
    let lbl = netList[compId].Label
    match Seq.tryFindIndexBack ((=) '(') lbl with
    | Some i -> lbl[0..i - 1]
    | None -> lbl

/// get integer from OutputPortInt
let private outPortInt2int outPortInt =
    match outPortInt with
    | OutputPortNumber pn -> pn

/// get NLSource option from ComponentId and InputPortNumber
let private drivingOutput (netList: NetList) compId inPortN =
    netList[compId].Inputs[inPortN]

/// get labels of Output and IOLabel components in nlTargetList
let net2outputsAndIOLabels (netList: NetList) (netLst: NLTarget list) =
    let nlTrgt2Lbls st nlTrgt = 
        match Map.tryFind nlTrgt.TargetCompId netList with
        | Some nlComp -> match nlComp.Type with
                         | IOLabel | Output _ -> List.append st [netList[nlTrgt.TargetCompId].Label]
                         | _ -> st
        | None -> st
    List.fold nlTrgt2Lbls [] netLst
    |> List.distinct

/// get labels of Output and IOLabel components in TargetListGroup
let netGroup2outputsAndIOLabels netList (netGrp: NetGroup) =
    Array.append [|netGrp.driverNet|] netGrp.connectedNets
    |> Array.toList
    |> List.collect (net2outputsAndIOLabels netList)
    |> List.distinct

/// get WaveLabel corresponding to a NLTarget list
let rec private findName (compIds: ComponentId Set) (sd: SimulationData) (net: NetList) netGrp nlTrgtList =
    let graph = sd.Graph
    let fs = sd.FastSim
    match nlTrgtLst2CommonNLSource net nlTrgtList with
    //nlTrgtLst is not connected to any driving components
    | None -> { OutputsAndIOLabels = []; ComposingLabels = [] }
    | Some nlSource ->
        //TODO check if its ok to comment this?
        if not (Set.contains nlSource.SourceCompId compIds) then
            // printfn "DEBUG: In findname, if not \n nlSource = %A \n compIds = %A" nlSource compIds
            // printfn "What? graph, net, netGrp, nltrgtList should all be consistent, compIds is deprecated"
            // component is no longer in circuit due to changes
            { OutputsAndIOLabels = []; ComposingLabels = [] }
        else   
            let compLbl = labelNoParenthesis net nlSource.SourceCompId
            let outPortInt = outPortInt2int nlSource.OutputPort
            let drivingOutputName inPortN =
                match drivingOutput net nlSource.SourceCompId inPortN with
                | Some nlSource' ->
                    net[nlSource'.SourceCompId].Outputs[nlSource'.OutputPort]
                    |> findName compIds sd net netGrp
                | None ->  { OutputsAndIOLabels = []; ComposingLabels = [] } 
            let srcComp = net[nlSource.SourceCompId]
            match net[nlSource.SourceCompId].Type with
            | ROM _ | RAM _ | AsyncROM _ -> 
                    failwithf "What? Legacy RAM component types should never occur"
            | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4 | Mux2 | Mux4 | Mux8 | BusCompare _ -> 
                [ { LabName = compLbl; BitLimits = 0, 0 } ] 
            | Input w | Output w | Constant1(w, _,_) | Constant(w,_) | Viewer w -> 
                [ { LabName = compLbl; BitLimits = w - 1, 0 } ] 
            | Demux2 | Demux4 | Demux8 -> 
                [ { LabName = compLbl + "." + string outPortInt; BitLimits = 0, 0 } ]
            | NbitsXor w -> 
                [ { LabName = compLbl; BitLimits = w - 1, 0 } ]
            | NbitsAdder w ->
                match outPortInt with
                | 0 -> [ { LabName = compLbl + ".Sum"; BitLimits = w - 1, 0 } ]
                | _ -> [ { LabName = compLbl + ".Cout"; BitLimits = w - 1, 0 } ]
            | DFF | DFFE -> 
                [ { LabName = compLbl + ".Q"; BitLimits = 0, 0 } ]
            | Register w | RegisterE w -> 
                [ { LabName = compLbl + ".Dout"; BitLimits = w-1, 0 } ]
            | RAM1 mem | AsyncRAM1 mem | AsyncROM1 mem | ROM1 mem -> 
                [ { LabName = compLbl + ".Dout"; BitLimits = mem.WordWidth - 1, 0 } ]
            | Custom c -> 
                [ { LabName = compLbl + "." + (fst c.OutputLabels[outPortInt])
                    BitLimits = snd c.OutputLabels[outPortInt] - 1, 0 } ]
            | MergeWires -> 
                List.append (drivingOutputName (InputPortNumber 1)).ComposingLabels 
                            (drivingOutputName (InputPortNumber 0)).ComposingLabels
            | SplitWire w ->
                let mostsigBranch (_, b) =
                    match outPortInt with
                    | 0 -> b >= 16 - w
                    | 1 -> b < 16 - w
                    | _ -> failwith "SplitWire output port number greater than 1"

                let split { LabName = name; BitLimits = msb, lsb } st =
                    List.zip [ lsb .. msb ] [ st + msb - lsb .. -1 .. st ]
                    |> List.filter mostsigBranch
                    |> List.unzip
                    |> function
                    | [], _ -> None 
                    | lst, _ -> Some { LabName = name
                                       BitLimits = List.max lst, List.min lst } 

                let updateState { LabName = _; BitLimits = msb, lsb } st =
                    st + msb - lsb + 1

                (0, (drivingOutputName (InputPortNumber 0)).ComposingLabels)
                ||> List.mapFold (fun st lstEl -> split lstEl st, updateState lstEl st)
                |> fst
                |> List.choose id
            | BusSelection(w, oLSB) ->
                let filtSelec { LabName = name; BitLimits = msb, lsb } st =
                    List.zip [ lsb .. msb ] [ st .. st + msb - lsb ]
                    |> List.filter (fun (_, b) -> oLSB <= b && b <= oLSB + w - 1)
                    |> List.unzip
                    |> function
                    | [], _ -> None
                    | lst, _ -> Some { LabName = name
                                       BitLimits = List.max lst, List.min lst } 

                let updateState { LabName = _; BitLimits = msb, lsb } st =
                    st + msb - lsb + 1 

                ((drivingOutputName (InputPortNumber 0)).ComposingLabels, 0)
                ||> List.mapFoldBack (fun lstEl st -> filtSelec lstEl st, updateState lstEl st)
                |> fst
                |> List.choose id
                |> List.rev
            | IOLabel -> 
                let drivingComp = fs.FIOActive[ComponentLabel srcComp.Label,[]]
                let ioLblWidth = FastRun.extractFastSimulationWidth fs (drivingComp.Id,[]) (OutputPortNumber 0)
                match ioLblWidth with
                | None ->
                    failwithf $"What? Can't find width for IOLabel {net[srcComp.Id].Label}$ "
                | Some width ->
                            
                            [ { LabName = compLbl
                                BitLimits = width - 1, 0 } ]

            |> (fun composingLbls -> { OutputsAndIOLabels = netGroup2outputsAndIOLabels net netGrp
                                       ComposingLabels = composingLbls })

/// get string in the [x:x] format given the bit limits
let private bitLimsString (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "[%d]" msb
    | (msb, lsb) -> sprintf "[%d:%d]" msb lsb

let rec private removeSubSeq startC endC chars =
    ((false,[]), chars)
    ||> Seq.fold (fun (removing,res) ch ->
                    match removing,ch with
                    | true, ch when ch = endC -> false,res
                    | true, _ -> true, res
                    | false, ch when ch = startC -> true,res
                    |false, ch -> false, ch :: res)
    |> snd
    |> List.rev
    |> List.map string
    |> String.concat ""

/// truncate names to remove redundant width specs
let private simplifyName name =
    name
    |> removeSubSeq '(' ')'

/// get the label of a waveform
let netGroup2Label compIds (sd:SimulationData) netList (netGrp: NetGroup) =
    // let start = getTimeMs()
    let waveLabel = findName compIds sd netList netGrp netGrp.driverNet
    //printfn "Finding label for %A\n%A\n\n" netGrp.driverComp.Label waveLabel.OutputsAndIOLabels
    let tl =
        match waveLabel.ComposingLabels with
        | [ el ] -> el.LabName + bitLimsString el.BitLimits
        | lst when List.length lst > 0 ->
            let appendName st lblSeg = st + lblSeg.LabName + bitLimsString lblSeg.BitLimits + ", "
            List.fold appendName "{" lst 
            |> (fun lbl -> lbl[0..String.length lbl - 3] + "}")
        |  _ -> ""
    let appendName st name = st + name + ", "
    match waveLabel.OutputsAndIOLabels with
    | [] -> tl
    | hdLbls -> 
        List.fold appendName "" hdLbls
        |> (fun hd -> hd[0..String.length hd - 3] + " : " + tl)
    |> simplifyName
    // |> instrumentInterval "netGroup2Label" start

let getWaveFromNetGroup 
        (fs: FastSimulation)
        (connMap: Map<ConnectionId,ConnectionId array>)
        (nameOf: NetGroup -> string) 
        (netGroup: NetGroup) : Wave =
    let netGroupName = nameOf netGroup
    // let fId, opn = getFastDriver fs netGroup.driverComp netGroup.driverPort
    let driverConn = netGroup.driverNet[0].TargetConnId
    let conns =
        Map.tryFind driverConn connMap
        |> Option.defaultValue [||]
    if conns = [||] then
        printfn $"Warning: {netGroupName} has no connections"
    {
        WaveId = netGroupName // not unique yet - may need to be changed
        Selected = true
        Conns = List.ofArray conns
        SheetId = [] // all NetGroups are from top sheet at the moment
        // Driver = fId,opn
        DisplayName = netGroupName
        // Width = getFastOutputWidth fs.FComps[fId] opn
    }

let getWaveFromFC (fc: FastComponent) =
    let viewerName = extractLabel fc.SimComponent.Label
    {
        WaveId = viewerName // not unique yet - may need to be changed
        Selected = true
        // WType = ViewerWaveform false
        Conns = [] // don't use connection nets for Viewer (yet)
        SheetId = snd fc.fId
        // Driver = fc.fId, OutputPortNumber 0
        DisplayName = viewerName
        // Width = getFastOutputWidth fc (OutputPortNumber 0)
    }

let getWaveforms
        (netGroupToLabel: Set<ComponentId> -> SimulationData -> NetList -> NetGroup -> string) 
        (simData: SimulationData) 
        (reducedState: CanvasState) =
    let comps, conns = reducedState
    // let compIds = comps |> List.map (fun comp -> comp.Id)
    // let comps, conns = model.Sheet.GetCanvasState ()
    let compIds = comps |> List.map (fun c -> ComponentId c.Id) |> Set

    let fastSim = simData.FastSim
    let fastComps = mapValues fastSim.FComps
    let viewers = 
        fastComps
        |> Array.filter (fun fc -> match fc.FType with Viewer _ -> true | _ -> false)

    /// NetList is a simplified version of circuit with connections and layout
    /// info removed. Component ports are connected directly. ConnectionIds are
    /// preserved so we can reference connections on diagram
    let netList = Helpers.getNetList reducedState
    /// Netgroups are connected Nets: note the iolabel components can connect
    /// together multiple nets on the schematic into a single NetGroup.
    /// Wave simulation allows every distinct NetGroup to be named and displayed
    let netGroups = makeAllNetGroups netList
    /// connMap maps each connection to the set of connected connections within the same sheet
    let connMap = makeConnectionMap netGroups
    /// work out a good human readable name for a Netgroup. Normally this is the
    /// label of the driver of the NetGroup. Merge, Split, and BusSelection components
    /// (as drivers) are removed, replaced by corresponding selectors on busses.
    /// Names are tagged with labels or IO connectors. It is easy to change these
    /// names to make them more human readable.
    let nameOf ng  = netGroupToLabel compIds simData netList ng

    // findName (via netGroup2Label) will possibly not generate unique names for
    // each netgroup. Names are defined via waveSimModel.AllPorts which adds to
    // each name an optional unique numeric suffic (.2 etc). These suffixes are
    // stripped from names when they are displayed
    // TODO: make sure suffixes are uniquely defined based on ComponentIds (which will not change)
    // display them in wave windows where needed to disambiguate waveforms.
    // Allports is the single reference throughout simulation of a circuit that associates names with netgroups

    Array.append
        (Array.map (getWaveFromNetGroup  fastSim connMap nameOf) netGroups)
        (Array.map getWaveFromFC viewers)
    |> Array.groupBy (fun wave -> wave.WaveId)
    |> Array.map (fun (root, specs) -> 
        match specs with 
        | [|_|] as oneSpec -> oneSpec
        | specL -> specL |> Array.mapi (fun i wSpec -> {wSpec with WaveId = $"{wSpec.WaveId}!{i}"}))
    |> Array.concat
    |> Array.map (fun wave -> wave.WaveId, wave)
    |> Map.ofArray

/// This function needs to show a list of what waveforms can be displayed, as well as a
/// check box list showing which ones are selectable. Should have a 'select all' box
/// available as well.
let waveSelectionPane simData reducedState (model: Model) dispatch : ReactElement list = 
    /// Generate popup over waveeditor screen if there are undriven input connections
    let inputWarningPopup (simData:SimulatorTypes.SimulationData) dispatch =
        if simData.Inputs <> [] then
            let inputs = 
                simData.Inputs
                |> List.map (fun (_,ComponentLabel lab,_) -> lab)
                |> String.concat ","
            let popup = Notifications.warningPropsNotification (sprintf "Inputs (%s) will be set to 0." inputs)
            dispatch <| SetPropertiesNotification popup

    [ div
        [ Style
            [
                Width "90%"
                MarginLeft "5%"
                MarginTop "15px"
            ]
        ] [ 
            Heading.h4 [] [ str "Waveform Simulation" ] 
            str "Ctrl-click on diagram connections or use tick boxes below to add or remove waveforms."
            str "Test combinational logic by closing this simulator and using Simulate tab."
            hr []
            div []
                [
                    viewWaveformsButton model dispatch
                    selectWaves model dispatch
                ]
        ]
    ]

/// Entry point to the waveform simulator.
let viewWaveSim (model: Model) dispatch : ReactElement list =
    printf "%A" model.WaveSim.State
    printf "%A" model.WaveSim.OutOfDate
    printf "%A" model.WaveSim.AllWaves

    let simData = SimulationView.makeSimData model
    match simData with
        | None -> failwithf "simRes has value None" // IColor.IsWhite, ""
        | Some (Ok simData', reducedState) -> // IsSuccess, "Start Simulation"
            match model.WaveSim.State with
            // Open waveform adder
            | NotRunning ->
                // let allWaves = getWaveforms netGroup2Label simData' reducedState
                // let wsModel' = {model.WaveSim with AllWaves = allWaves}
                // dispatch <| SetWSMod wsModel'
                waveSelectionPane simData' reducedState model dispatch
            // Open waveform viewer
            | Running ->
                showWaveforms simData' reducedState model dispatch
        | Some (Error e, _) -> 
            displayErrorMessage e //IsWarning, "See Problems"
