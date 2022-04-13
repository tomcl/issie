module WaveformSimulator

open Fulma
open Fable.React
open Fable.React.Props

open ModelType
open DiagramStyle
open CommonTypes
open WaveSimHelpers
open FileMenuView
open SimulatorTypes
open Sheet.SheetInterface
open DrawModelType

/// Determines whether a clock cycle is generated with a vertical bar
/// at the beginning, denoting that a waveform changes value at the
/// start of that clock cycle. NB this does not determine whether a
/// waveform changes value at the end of that clock cycle.
type BinaryTransition =
    | ZeroToZero
    | ZeroToOne
    | OneToZero
    | OneToOne
    // Is this last transition really necessary?
    | Start

/// Determines whether a non-binary waveform changes value at the beginning
/// and end of that clock cycle.
type NonBinaryTransition =
    | ChangeToChange
    | ConstToConst
    | ChangeToConst
    | ConstToChange

/// Waveforms can be either binary or non-binary; these have different properties.
type Transition =
    | BinaryTransition of BinaryTransition
    | NonBinaryTransition of NonBinaryTransition

/// TODO: Tweak these values
/// Height of a waveform
let waveHeight = 0.3
let clkCycleWidth = 1.0

/// Generates the SVG for one clock cycle of a binary or non-binary waveform
let generateClkCycle (startCoord: XYPos) (transition: Transition) : ReactElement =
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
            failwithf "Not implemented"
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
            failwithf "Not implemented"

/// Generates SVG to display waveform values when there is enough space
let displayValuesOnWave (startCycle: int) (endCycle: int) (waveValues: int array) : ReactElement =
    // enough space means enough transitions such that the full value can be displayed before a transition occurs
    // values can be displayed repeatedly if there is enough space
    // try to centre the displayed values?
    failwithf "Not implemented"

let determineTransitions (waveValues: int array) : Transition array = 
    // waveValues
    // |> Array.map (fun val ->

    // )
    failwithf "Not implemented"

/// Generates the SVG for a specific waveform
let generateWave (startCycle: int) (endCycle: int) (waveValues: int array) : ReactElement array =
    // need to know type of waveValues
    // fold or iter over each value in waveValues (i.e. once for each clock cycle)
    // fold function generates an svg for each clock cycle? 

    // TODO: How to calculate this?
    let startCoord = {X = 0; Y = 0}

    let transitions = determineTransitions waveValues
    let wavesSVG = Array.map (generateClkCycle startCoord) transitions

    // This is only for non-binary waveforms though.
    let waveValuesSVG = displayValuesOnWave startCycle endCycle waveValues

    // TODO: Combine waveValuesSVG and wavesSVG

    failwithf "Not implemented"

let generateValues (wave: string) : int array =
    failwithf "Not implemented"

/// Generates the SVG for all waves
let generateAllWaves (waves: string list) (startCycle: int) (endCycle: int) : ReactElement array array = 
    // Iterate over each wave to generate that wave's SVG
    waves
    |> List.map generateValues
    |> List.map (generateWave startCycle endCycle)
    |> List.toArray
    // failwithf "Not implemented"


let generateAllLabels waves =
    failwithf "Not implemented"

let generateValuesPerClkCycle waves clkCycle =
    failwithf "Not implemented"

let showWaveforms : ReactElement =
    // Calls generateAllLabels, generateAllWaves, generateValuesPerClkCycle
    // inputs tbd
    failwithf "Not implemented"