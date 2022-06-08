module WaveSimHelpers

open Fulma
open Fable.React
open Fable.React.Props

open CommonTypes
open ModelType
open DiagramStyle
open FileMenuView
open SimulatorTypes
open NumberHelpers
open DrawModelType
open Sheet.SheetInterface

/// Determines whether a clock cycle is generated with a vertical bar at the beginning,
/// denoting that a waveform changes value at the start of that clock cycle. NB this
/// does not determine whether a waveform changes value at the end of that clock cycle.
type BinaryTransition =
    | ZeroToZero
    | ZeroToOne
    | OneToZero
    | OneToOne

/// Determines whether a non-binary waveform changes value at the beginning of that clock cycle.
type NonBinaryTransition =
    | Change
    | Const

/// Waveforms can be either binary or non-binary; these have different properties.
type Transition =
    | BinaryTransition of BinaryTransition
    | NonBinaryTransition of NonBinaryTransition

module Constants = 
    let nonBinaryTransLen : float = 0.2

    let viewBoxHeight : float = 1.0

    /// Height of a waveform
    let waveHeight : float = 0.8
    let spacing : float = (viewBoxHeight - waveHeight) / 2.

    let yTop = spacing
    let yBot = waveHeight + spacing

    /// TODO: Remove this limit. This stops the waveform simulator moving past 500 clock cycles.
    let maxLastClk = 500

    /// TODO: Use geometric sequence parametrised by startZoom, endZoom, numZoomLevels
    let zoomLevels = [|
    //   0     1     2    3     4     5    6    7    8    9    10   11    12   13   14   15
        0.25; 0.33; 0.5; 0.67; 0.75; 0.8; 0.9; 1.0; 1.1; 1.5; 1.75; 2.0; 2.50; 3.0; 4.0; 5.0;
    |]
    /// TODO: Explain why: 30*width, width is 1.5, so that's 45. This is 8 cycles (0 to 7)
    /// This should be divisible by 45
    let initialWaveformColWidth = int( 1.5 * float (30 * 7)) //rightSectionWidthViewerDefault - namesColMinWidth - valuesColMinWidth


let initWSModel : WaveSimModel = {
    State = NotRunning
    AllWaves = Map.empty
    // ShownWaves = Map.empty
    StartCycle = 0
    ShownCycles = 7
    // EndCycle = 7
    OutOfDate = true
    ReducedState = [], []
    // SVG = [||]
    CurrClkCycle = 0
    ClkCycleBoxIsEmpty = false
    Radix = CommonTypes.Hex
    ZoomLevel = 1.5
    ZoomLevelIndex = 9
    WaveformColumnWidth = Constants.initialWaveformColWidth
}

let getWSModel model : WaveSimModel =
    Map.tryFind model.WaveSimSheet model.WaveSim
    |> function
        | Some wsModel ->
            printf "Sheet %A found in model" model.WaveSimSheet
            wsModel
        | None ->
            printf "Sheet %A not found in model" model.WaveSimSheet
            initWSModel

let viewBoxMinX m = string (float m.StartCycle * m.ZoomLevel)
let viewBoxWidth m = string (float m.ShownCycles * m.ZoomLevel)

let endCycle wsModel = wsModel.StartCycle + wsModel.ShownCycles - 1

let singleWaveWidth wsModel = 30.0 * wsModel.ZoomLevel

let button options func label = Button.button (List.append options [ Button.OnClick func ]) [ label ]

let selectedWaves (wsModel: WaveSimModel) : Map<string, Wave> = Map.filter (fun _ key -> key.Selected) wsModel.AllWaves
let selectedWavesCount (wsModel: WaveSimModel) = Map.count (selectedWaves wsModel)

let pointsToString (points: XYPos list) : string =
    List.fold (fun str (point: XYPos) ->
        str + string point.X + "," + string point.Y + " "
    ) "" points

/// Retrieve value of wave at given clock cycle as an int.
let getWaveValue (currClkCycle: int) (wave: Wave): int64 =
    List.tryItem currClkCycle wave.WaveValues
    |> function
    | Some wireData ->
        convertWireDataToInt wireData
    | None ->
        // TODO: Find better default value here
        // TODO: Should probably make it so that you can't call this function in the first place.
        printf "Trying to access index %A in wave %A. Default to 0." currClkCycle wave.DisplayName
        0

/// Make left and right x-coordinates for a clock cycle.
let makeXCoords (clkCycleWidth: float) (clkCycle: int) (transition: Transition) =
    match transition with
    | BinaryTransition _ ->
        float clkCycle * clkCycleWidth, float (clkCycle + 1) * clkCycleWidth
    | NonBinaryTransition _ ->
        // These are left-shifted by nonBinaryTransLen: doing this means that for non-binary
        // waveforms, only the transition at the start of each cycle needs to be considered,
        // rather than the transition at both the start and end of each cycle.
        float clkCycle * clkCycleWidth - Constants.nonBinaryTransLen,
        float (clkCycle + 1) * clkCycleWidth - Constants.nonBinaryTransLen

/// Make top-left, top-right, bottom-left, bottom-right coordinates for a clock cycle.
let makeCoords (clkCycleWidth: float) (clkCycle: int) (transition: Transition) : XYPos * XYPos * XYPos * XYPos =
    let xLeft, xRight = makeXCoords clkCycleWidth clkCycle transition

    let topL = {X = xLeft; Y = Constants.yTop}
    let topR = {X = xRight; Y = Constants.yTop}
    let botL = {X = xLeft; Y = Constants.yBot}
    let botR = {X = xRight; Y = Constants.yBot}

    topL, topR, botL, botR

/// TODO: Is this how to use startCycle?
/// Generate points for a binary waveform
let binaryWavePoints (clkCycleWidth: float) (startCycle: int) (index: int) (transition: BinaryTransition)  : XYPos list =
    let topL, topR, botL, botR = makeCoords clkCycleWidth (startCycle + index) (BinaryTransition transition)
    // Each match condition generates a specific transition type
    match transition with
    | ZeroToZero | OneToZero ->
        [botL; botR]
    | ZeroToOne | OneToOne ->
        [topL; topR]

/// TODO: Account for very low zoom levels.
/// TODO: Consider: If singleWaveWidth M nonBinaryTransLen, then show crosshatch.
/// TODO: Is this how to use startCycle??
/// Generate points for a non-binary waveform.
let nonBinaryWavePoints (clkCycleWidth: float) (startCycle: int) (index: int)  (transition: NonBinaryTransition) : (XYPos list * XYPos list) =
    let xLeft, _ = makeXCoords clkCycleWidth (startCycle + index) (NonBinaryTransition transition)
    let _, topR, _, botR = makeCoords clkCycleWidth (startCycle + index) (NonBinaryTransition transition)

    let crossHatchMid = {X = xLeft + Constants.nonBinaryTransLen; Y = 0.5}
    let crossHatchTop = {X = xLeft + Constants.nonBinaryTransLen * 2.; Y = Constants.yTop}
    let crossHatchBot = {X = xLeft + Constants.nonBinaryTransLen * 2.; Y = Constants.yBot}

    match transition with
    // This needs to account for different zoom levels:
    // Can probably just look at screen size and zoom level
    // And then scale the horizontal part accordingly
    // When zoomed out sufficiently and values changing fast enough,
    // The horizontal part will have length zero.
    // May need to account for odd/even clock cycle
    | Change ->
        let topStart = [crossHatchMid; crossHatchTop; topR]
        let botStart = [crossHatchMid; crossHatchBot; botR]
        (topStart, botStart)
    | Const ->
        ([topR], [botR])

/// Determine transitions for each clock cycle of a binary waveform.
/// Assumes that waveValues starts at clock cycle 0.
let calculateBinaryTransitions (waveValues: Bit list list) : BinaryTransition list =
    [List.head waveValues] @ waveValues
    |> List.pairwise
    |> List.map (fun (x, y) ->
        match x, y with
        | [Zero], [Zero] -> ZeroToZero
        | [Zero], [One] -> ZeroToOne
        | [One], [Zero] -> OneToZero
        | [One], [One] -> OneToOne
        | _ ->
            failwithf "Unrecognised transition"
    )

/// Determine transitions for each clock cycle of a non-binary waveform.
/// Assumes that waveValues starts at clock cycle 0.
let calculateNonBinaryTransitions (waveValues: Bit list list) : NonBinaryTransition list =
    // TODO: See if this will break if the clock cycle isn't 0.
    // Concat [[]] so first clock cycle always starts with Change
    [[]] @ waveValues
    |> List.pairwise
    |> List.map (fun (x, y) ->
        if x = y then
            Const
        else
            Change
    )

let isWaveSelected (waveSimModel: WaveSimModel) (name: string) : bool = waveSimModel.AllWaves[name].Selected
