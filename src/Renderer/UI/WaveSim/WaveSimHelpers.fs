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
    /// get string in the [x:x] format given the bit limits

/// output representation of bus width
let bitLimsString (a, b) =
    match (a, b) with
    | (0, 0) -> ""
    | (msb, lsb) when msb = lsb -> sprintf "(%d)" msb
    | (msb, lsb) -> sprintf "(%d:%d)" msb lsb

let portBits n = if n < 2 then "" else $"({n-1}:0)"


/// determines how components are dispalyed in waveform selector
let getCompDetails fs wave =
    let fc = fs.WaveComps[wave.WaveId.Id]
    let label = fc.FLabel
    let descr, oneLine =
        match fc.FType with
        | Input1 _ -> "Input",true
        | Output _ -> "Output", true
        | Constant1 _ -> "What? can't happen", true
        | Viewer _ -> "Viewer", true
        | IOLabel _-> "Wire Label", true
        | Not | And | Or | Xor | Nand | Nor | Xnor -> 
            let gateType = $"{fc.FType}".ToUpper()
            $"{gateType} gate", false
        | BusCompare (width,v) -> $"Bus Compare ='{v}'", false

        | Mux2 -> "2 input multiplexer", false
        | Mux4 -> "4 input multiplexer", false
        | Mux8 -> "8 input multiplexer", false
        | Demux2 -> "2 input demultiplexer", false
        | Demux4 -> "4 input demultiplexer", false
        | Demux8 -> "8 input demultiplexer", false
        | Decode4 -> "2 line decoder", false
        | NbitsAdder n -> $"{n} bit adder",false
        | NbitsXor n -> $"{n} XOR gates",false
        | NbitsAnd n -> $"{n} AND gates",false
        | NbitsNot n -> $"{n} Not gates",false
        | NbitSpreader n -> $"1 -> {n} bits spreader",false
        | NbitsOr n -> $"{n} OR gates",false
        | Custom x -> $"({x.Name} instance)",false
        | DFF -> "D flipflip", false
        | DFFE -> "D flipflop with enaable", false
        | Register n -> $"{n} bit D register", false
        | RegisterE n -> $"{n} bit D register with enable", false
        | AsyncROM1 mem -> $"ROM  ({1 <<< mem.AddressWidth} word X {mem.WordWidth} bit) asynchronous read", false
        | ROM1 mem -> $"ROM  ({1 <<< mem.AddressWidth} word X {mem.WordWidth} bit) synchronous read", false
        | RAM1 mem -> $"RAM  ({1 <<< mem.AddressWidth} word X {mem.WordWidth} bit) synchronous read", false
        | AsyncRAM1 mem -> $"RAM  ({1 <<< mem.AddressWidth} word X {mem.WordWidth} bit) asynchronous read", false             
        | BusSelection _ | MergeWires | SplitWire _ ->
            failwithf "Bus select, MergeWires, SplitWire should not appear"
        | Input _ | Constant _ | AsyncROM _ | ROM _ | RAM _ ->
            failwithf "Legacy component types should not appear"
    match oneLine with
    | true -> $"{label}{portBits wave.Width} {descr}"
    | false -> $"{label} {descr}"

let getCompGroup fs wave =
    match fs.WaveComps[wave.WaveId.Id].FType with
    | Input1 _ | Output _ | Constant1 _ | Viewer _ | IOLabel _->
        InputOutput
    | Not | And | Or | Xor | Nand | Nor | Xnor ->
        Gates
    | BusCompare _ ->
        Buses
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8 | Decode4 ->
        MuxDemux
    | NbitsAdder _ | NbitsXor _ | NbitsAnd _ | NbitsNot _ | NbitSpreader _ | NbitsOr _->
        Arithmetic
    | Custom _ -> CustomComp
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
    match cBox with
    | PortItem (_,name) ->
        str <| camelCaseDottedWords name
    | ComponentItem fc->
        let descr = getCompDetails ws.FastSim (waves[0])
        str <| descr
        
    | GroupItem (compGroup,_) ->
        match compGroup with
        | InputOutput -> "Inputs / Outputs / Labels / Viewers"
        //| Viewers -> "Viewers"
        //| WireLabel -> "Wire Labels"
        | Buses -> "Buses"
        | Gates -> "Logic Gates"
        | MuxDemux -> "Multiplexers"
        | Arithmetic -> "Arithmetic"
        | FFRegister -> "Flip Flops and Registers"
        | Memories -> "RAMs and ROMs"
        | Component compLabel -> compLabel
        | CustomComp -> "Custom Components"
        | _ -> "What? Not used!"
        |> (fun name -> str $"""{name.ToUpper()}""")

    | SheetItem subSheet ->
        str <| $"Subsheet {camelCaseDottedWords subSheet[subSheet.Length-1]}"



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




let tr1 react = tr [] [ react ]
let td1 react = td [] [ react ]

//---------------------------Code for selector details state----------------------------------//

// It would be better to do this with one subfunction and Optics!

/// Sets or clears a subset of ShowSheetDetail
let setWaveSheetSelectionOpen (wsModel: WaveSimModel) (subSheets: string list list) (show: bool) =
    let setChange = Set.ofList subSheets
    let newSelect =
        match show with
        | false -> Set.difference wsModel.ShowSheetDetail setChange
        | true -> Set.union setChange wsModel.ShowSheetDetail
    {wsModel with ShowSheetDetail = newSelect}   

/// Sets or clears a subset of ShowComponentDetail
let setWaveComponentSelectionOpen (wsModel: WaveSimModel) (fIds: FComponentId list)  (show: bool) =
    let fIdSet = Set.ofList fIds
    let newSelect =
        match show with
        | true -> Set.union fIdSet  wsModel.ShowComponentDetail
        | false -> Set.difference wsModel.ShowComponentDetail fIdSet
    {wsModel with ShowComponentDetail = newSelect}


/// Sets or clears a subset of ShowGroupDetail
let setWaveGroupSelectionOpen (wsModel: WaveSimModel) (grps :(ComponentGroup*string list) list)  (show: bool) =
    let grpSet = Set.ofList grps
    let newSelect =
        match show with
        | true -> Set.union grpSet  wsModel.ShowGroupDetail
        | false -> Set.difference wsModel.ShowGroupDetail grpSet
    {wsModel with ShowGroupDetail = newSelect}

let setSelectionOpen (wsModel: WaveSimModel) (cBox: CheckBoxStyle) (show:bool) =
    match cBox with
    | PortItem _ -> failwithf "What? setselectionopen cannot be called from a Port"
    | ComponentItem fc -> setWaveComponentSelectionOpen wsModel [fc.fId] show
    | GroupItem (grp,subSheet) -> setWaveGroupSelectionOpen wsModel [grp,subSheet] show
    | SheetItem subSheet -> setWaveSheetSelectionOpen wsModel [subSheet] show

    



    
