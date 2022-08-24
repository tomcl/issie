module WaveSimHelpers

open Fulma
open Fable.React

open CommonTypes
open ModelType
open FileMenuView
open SimulatorTypes
open NumberHelpers

/// Determines whether a clock cycle is generated with a vertical bar at the beginning,
/// denoting that a waveform changes value at the start of that clock cycle. NB this
/// does not determine whether a waveform changes value at the end of that clock cycle.
/// TODO: Remove this since it is unnecessary. Can use WaveValues instead.
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

/// Stores information about gaps between NonBinaryTransitions.
/// Used in displayValuesOnWave
type Gap = {
    // First cycle which is Change after a Const cycle
    Start: int
    // How many Const cycles there are immediately after this Change transition
    Length: int
}

/// Groups components together in the wave selection table.
/// NB: There are fields which are commented out: these can be added back in
/// later on if we want to group those components together by type rather than
/// separately by name.
type ComponentGroup =
    | WireLabel
    | InputOutput
    | Viewers
    | Buses
    | Gates
    | MuxDemux
    | Arithmetic
    // | CustomComp
    | FFRegister
    | Memories
    | Component of string

module Constants =
    /// The horizontal length of a transition cross-hatch for non-binary waveforms
    let nonBinaryTransLen : float = 8.

    /// The height of the viewbox used for a wave's SVG. This is the same as the height
    /// of a label in the name and value columns.
    /// TODO: Combine this with WaveSimStyle.Constants.rowHeight?
    let viewBoxHeight : float = 30.0

    /// Height of a waveform
    let waveHeight : float = 0.8 * viewBoxHeight
    /// Vertical padding between top and bottom of each wave and the row it is in.
    let spacing : float = (viewBoxHeight - waveHeight) / 2.

    /// y-coordinate of the top of a waveform
    let yTop = spacing
    /// y-coordiante of the bottom of a waveform
    let yBot = waveHeight + spacing

    /// TODO: Remove this limit. This stops the waveform simulator moving past 500 clock cycles.
    let maxLastClk = 500

    /// Minimum number of visible clock cycles.
    let minCycleWidth = 5

    /// If the width of a non-binary waveform is less than this value, display a cross-hatch
    /// to indicate a non-binary wave is rapidly changing value.
    let clkCycleNarrowThreshold = 20


/// If true, then show cross-hatch only for non-binary waves when wave is changing value very fast.
let highZoom clkCycleWidth = clkCycleWidth < 2. * Constants.nonBinaryTransLen

/// Left-shift non-binary waveforms by this much.
let xShift clkCycleWidth =
    if highZoom clkCycleWidth then
        clkCycleWidth / 2.
    else Constants.nonBinaryTransLen

/// Get the current WaveSimModel used by the Model (index the map using the current sheet).
/// If no WaveSimModel for that sheet, return an empty wave sim model.
let getWSModel model : WaveSimModel =
    Map.tryFind model.WaveSimSheet model.WaveSim
    |> function
        | Some wsModel ->
            // printf "Sheet %A found in model" model.WaveSimSheet
            wsModel
        | None ->
            // printf "Sheet %A not found in model" model.WaveSimSheet
            initWSModel

/// Width of one clock cycle.
let singleWaveWidth m = float m.WaveformColumnWidth / float m.ShownCycles

/// Left-most coordinate of the SVG viewbox.
let viewBoxMinX m = string (float m.StartCycle * singleWaveWidth m)

/// Total width of the SVG viewbox.
let viewBoxWidth m = string m.WaveformColumnWidth

/// Right-most visible clock cycle.
let endCycle wsModel = wsModel.StartCycle + (wsModel.ShownCycles) - 1

/// Helper function to create Bulma buttons
let button options func label = Button.button (List.append options [ Button.OnClick func ]) [ label ]

/// List of selected waves (of type Wave)
let selectedWaves (wsModel: WaveSimModel) : Wave list = List.map (fun index -> wsModel.AllWaves[index]) wsModel.SelectedWaves

/// Convert XYPos list to string
let pointsToString (points: XYPos array) : string =
    Array.fold (fun str (point: XYPos) ->
        str + string point.X + "," + string point.Y + " "
    ) "" points

/// Retrieve value of wave at given clock cycle as an int.
let getWaveValue (currClkCycle: int) (wave: Wave): int64 =
    Array.tryItem currClkCycle wave.WaveValues
    |> function
        | Some (Data fData) ->
            convertFastDataToInt64 fData |> int64
        | _ ->
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
        // These are left-shifted by xShift: doing this means that for non-binary
        // waveforms, only the transition at the start of each cycle needs to be considered,
        // rather than the transition at both the start and end of each cycle.
        float clkCycle * clkCycleWidth - xShift clkCycleWidth,
        float (clkCycle + 1) * clkCycleWidth - xShift clkCycleWidth

/// Make top-left, top-right, bottom-left, bottom-right coordinates for a clock cycle.
let makeCoords (clkCycleWidth: float) (clkCycle: int) (transition: Transition) : XYPos * XYPos * XYPos * XYPos =
    let xLeft, xRight = makeXCoords clkCycleWidth clkCycle transition

    let topL = {X = xLeft; Y = Constants.yTop}
    let topR = {X = xRight; Y = Constants.yTop}
    let botL = {X = xLeft; Y = Constants.yBot}
    let botR = {X = xRight; Y = Constants.yBot}

    topL, topR, botL, botR

/// Generate points for a binary waveform
let binaryWavePoints (clkCycleWidth: float) (startCycle: int) (index: int) (transition: BinaryTransition)  : XYPos array =
    let topL, topR, botL, botR = makeCoords clkCycleWidth (startCycle + index) (BinaryTransition transition)
    // Each match condition generates a specific transition type
    match transition with
    | ZeroToZero | OneToZero ->
        [|botL; botR|]
    | ZeroToOne | OneToOne ->
        [|topL; topR|]

/// Generate points for a non-binary waveform.
let nonBinaryWavePoints (clkCycleWidth: float) (startCycle: int) (index: int)  (transition: NonBinaryTransition) : (XYPos array * XYPos array) =
    let xLeft, _ = makeXCoords clkCycleWidth (startCycle + index) (NonBinaryTransition transition)
    let _, topR, _, botR = makeCoords clkCycleWidth (startCycle + index) (NonBinaryTransition transition)

    let crossHatchMid, crossHatchTop, crossHatchBot =
        {X = xLeft +      xShift clkCycleWidth; Y = 0.5 * Constants.viewBoxHeight},
        {X = xLeft + 2. * xShift clkCycleWidth; Y = Constants.yTop},
        {X = xLeft + 2. * xShift clkCycleWidth; Y = Constants.yBot}

    match transition with
    | Change ->
        if highZoom clkCycleWidth then
            [|crossHatchMid; crossHatchTop|], [|crossHatchMid; crossHatchBot|]
        else
            [|crossHatchMid; crossHatchTop; topR|], [|crossHatchMid; crossHatchBot; botR|]
    | Const ->
        [|topR|], [|botR|]

/// Determine transitions for each clock cycle of a binary waveform.
/// Assumes that waveValues starts at clock cycle 0.
let calculateBinaryTransitions (waveValues: FData array) : BinaryTransition array =
    let getBit = function 
        | Data {Dat = Word bit} -> int32 bit 
        | Data {Dat = BigWord bit} -> int32 bit
        | x -> failwithf $"Malformed data: expecting single bit, not {x}"
    Array.append [|waveValues[0]|] waveValues
    |> Array.pairwise
    |> Array.map (fun (x,y) ->       
        match getBit x, getBit y with
        | 0,0 -> ZeroToZero
        | 0,1 -> ZeroToOne
        | 1,0 -> OneToZero
        | 1,1 -> OneToOne
        | _ ->
            failwithf $"Unrecognised transition {getBit x}, {getBit y}"
    )

/// Determine transitions for each clock cycle of a non-binary waveform.
/// Assumes that waveValues starts at clock cycle 0.
let calculateNonBinaryTransitions (waveValues: FData array) : NonBinaryTransition array =
    let notWaveformValue = Alg (AppendExp [])
    // TODO: See if this will break if the clock cycle isn't 0.
    // Concat [[]] so first clock cycle always starts with Change
    Array.append [|notWaveformValue|]  waveValues
    |> Array.pairwise
    |> Array.map (fun (x, y) ->
        if x = y then
            Const
        else
            Change
    )

let isWaveSelected (wsModel: WaveSimModel) (index: WaveIndexT) : bool = List.contains index wsModel.SelectedWaves
let isRamSelected (ramId: FComponentId) (wsModel: WaveSimModel) : bool = Map.containsKey ramId wsModel.SelectedRams

/// get integer from OutputPortNumber
let getInputPortNumber (ipn: InputPortNumber) : int =
    match ipn with
    | InputPortNumber pn -> pn

/// get integer from OutputPortNumber
let getOutputPortNumber (opn: OutputPortNumber) : int =
    match opn with
    | OutputPortNumber pn -> pn

type CheckBoxStyle =
    | PortItem of Wave * string
    | ComponentItem of FastComponent
    | GroupItem of ComponentGroup
    | SheetItem of string list

let getCompGroup fs wave =
    match fs.WaveComps[wave.WaveId.Id].FType with
    | Input1 _ | Output _ | Constant1 _ ->
        InputOutput
    | IOLabel ->
        WireLabel
    | Viewer _ ->
        Viewers
    | Not | And | Or | Xor | Nand | Nor | Xnor ->
        Gates
    | BusCompare _ ->
        Buses
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 | Decode4 ->
        MuxDemux
    | NbitsAdder _ | NbitsXor _ | NbitsAnd _ | NbitsNot _ | NbitSpreader _ ->
        Arithmetic
    | Custom _ ->
        Component wave.CompLabel
    | DFF | DFFE | Register _ | RegisterE _ ->
        FFRegister
    | AsyncROM1 _ | ROM1 _ | RAM1 _ | AsyncRAM1 _ ->
        Memories                
    | BusSelection _ | MergeWires | SplitWire _ ->
        failwithf "Bus select, MergeWires, SplitWire should not appear"
    | Input _ | Constant _ | AsyncROM _ | ROM _ | RAM _ ->
        failwithf "Legacy component types should not appear"



/// Name for summary field in details element.
/// NB: There are fields which are commented out: these can be added back in
/// later on if we want to group those components together by type rather than
/// separately by name.
let summaryName (ws: WaveSimModel) (cBox: CheckBoxStyle) (subSheet: string list) (waves: Wave list): ReactElement =
    let ss = String.concat "." subSheet
    match cBox with
    | PortItem (_,name) ->
        str name
    | ComponentItem fc->
        str <| ss+"."+fc.FLabel.ToUpper()
        
    | GroupItem compGroup ->
        match compGroup with
        | WireLabel -> "Wire Labels"
        | InputOutput -> "Inputs / Outputs"
        | Viewers -> "Viewers"
        | Buses -> "Buses"
        | Gates -> "Logic Gates"
        | MuxDemux -> "Multiplexers"
        | Arithmetic -> "Arithmetic"
        | FFRegister -> "Flip Flops and Registers"
        | Memories -> "RAMs and ROMs"
        | Component compLabel -> compLabel
        |> (fun name -> str $"{ss}.{name.ToUpper()}")

    | SheetItem subSheet ->
        str <| $"Subsheet {ss}"

/// convert a string to CamelCase: 
let camelCaseDottedWords (text:string) =
    let camelWord (s:string)=
        match s.Length with
        | 0 -> ""
        | 1 -> s.ToUpper()
        | _ -> s[0..0].ToUpper() + s[1..s.Length-1].ToLower()

    text.Split([|'.'|])
    |> Array.map camelWord
    |> String.concat "."

let path2fId (fastSim: FastSimulation) (path:ComponentId list) : FComponentId option=
    match path with
    | [] -> 
        None
    | p -> 
        Some <| (p[p.Length-1], p[0..p.Length-2])


let sheetIdToName (fastSim:FastSimulation) sheetId =
    sheetId
    |> path2fId fastSim
    |> function | None -> [] | Some fId -> [fastSim.WaveComps[fId].FLabel]

let sheetIdToSubsheets (fastSim:FastSimulation) (sheetId: ComponentId list) =
    match sheetId.Length with
    | 0 -> []
    | n -> [1..n] |> List.collect (fun i -> sheetId[0..i-1] |> sheetIdToName fastSim)


let subSheetsToNameReact (subSheets: string list) =
    subSheets
    |> String.concat "."
    |> camelCaseDottedWords
    |> str

let prefixOf (pre:'a list) (whole:'a list) =
    whole.Length >= pre.Length && whole[0..pre.Length-1] = pre


let wavesToIds (waves: Wave list) = 
    waves |> List.map (fun wave -> wave.WaveId)

let getCheckInfo (ws: WaveSimModel) (waves: WaveIndexT list) =
    let cMap = ws.ShowDetailMap
    match Map.tryFind  waves cMap with
    | None -> false
    | Some b -> b

let tr1 react = tr [] [ react ]
let td1 react = td [] [ react ]


    



    
