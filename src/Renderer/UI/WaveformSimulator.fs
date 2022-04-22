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

/// TODO: Test if this function actually works.
let displayErrorMessage error =
    [ div
        [ Style [ Width "90%"; MarginLeft "5%"; MarginTop "15px" ] ]
        [
            SimulationView.viewSimulationError error
        ]
    ]



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
