module WaveGenWorker

open WorkerInterface
// open WaveSimHelpers
open NumberHelpers
open CommonTypes
open ModelType
open Thoth.Json

open Fable.React
open Fable.React.Props

module Constants =
    // Width of names column - replaced by calcNamesColWidth function

    /// Width of values column
    let valuesColWidth = 100
    let deleteSymbolWidth = 20
    let scrollBarWidth = 15

    /// Width of left margin of waveform simulator
    let leftMargin = 30
    /// Width of right margin of waveform simulator
    let rightMargin = 30

    /// Height of each row in name and value columns.
    /// Same as SVG ViewBox Height.
    let rowHeight = 30

    let colWidth = 120

    /// Width of line that separates each clock cycle.
    let clkLineWidth = 0.8
    /// Width of each waveform line.
    let lineThickness : float = 0.8
    let columnFontSize = "12px"
    let columnFontFamily = "Helvetica"
    let valueColumnFontSize = "12px"
    let valueColumnFontFamily = "Helvetica"

    let valueColumnText = 
        { DrawHelpers.defaultText with 
            FontSize = valueColumnFontSize
            FontFamily = valueColumnFontFamily}

    let fontSizeValueOnWave = "10px"
    /// Text used to display vlaues on non-binary waves
    let valueOnWaveText = { DrawHelpers.defaultText with FontSize = fontSizeValueOnWave }
    /// Whitespace padding between repeated values displayed on non-binary waves.
    let valueOnWavePadding = 75.0

    /// Border between columns and headers of waveform viewer.
    let borderProperties = "2px solid rgb(219,219,219)"

    /// Padding between name label/value label and waveform column.
    let labelPadding = 3
    /// Color for cursor and values column
    let cursorColor = "Lavender"

/// Convert XYPos list to string
let pointsToString (points: XYPos array) : string =
    Array.fold (fun str (point: XYPos) ->
        $"{str} %.1f{point.X},%.1f{point.Y} "
    ) "" points

let wavePolylineStyle points : IProp list = [
    SVGAttr.Stroke "blue"
    SVGAttr.Fill "none"
    SVGAttr.StrokeWidth Constants.lineThickness
    Points (pointsToString points)
]

let singleWaveWidth m = max 5.0 (float m.WaveformColumnWidth / float m.ShownCycles)

let genWave (waveParams: WaveGenParams) (svgValues: SVGValues) (wave: Wave) =
    let makePolyline points = 
        let points =
            points
            |> Array.concat
            |> Array.distinct
        polyline (wavePolylineStyle points) []
    
    let polylines =
        svgValues.WavePoints
        |> Array.map makePolyline

    let waveform = svg waveParams.WaveRowProps (Array.append polylines svgValues.Values)
    // printfn "%A" waveform
    // {wave with 
    //     Radix = waveParams.Radix
    //     ShownCycles = waveParams.ShownCycles
    //     StartCycle = waveParams.StartCycle
    //     CycleWidth = singleWaveWidth waveParams
    //     SVG = Some waveform}
    waveform

let onMsg (msg: {|data: {|WaveParams: string; SVGValues: string; Index: string; Wave: string|}|}) =
    printfn "hello from the worker"
    let extraCoders =
        Extra.empty
        |> Extra.withCustom encoderIprop decoderWaveIprop
        |> Extra.withCustom encoderReactElement decoderReactElement
        |> Extra.withBigInt
    let waveParams = Decode.Auto.fromString<WaveGenParams>(msg.data.WaveParams, extra = extraCoders) |> unpackJsonDecode
    let svgVals = Decode.Auto.fromString<SVGValues>(msg.data.SVGValues, extra = extraCoders) |> unpackJsonDecode
    let index = Decode.Auto.fromString<WaveIndexT>(msg.data.Index) |> unpackJsonDecode
    let wave = Decode.Auto.fromString<Wave>(msg.data.Wave, extra = extraCoders) |> unpackJsonDecode
    let outWaveform = genWave waveParams svgVals wave
    printfn "from worker: %A" (Encode.Auto.toString (0, text [(HTMLAttr.Height "2")] [str "hello"], extra = extraCoders))
    postMessage {|Idx = Encode.toString 0 index; Waveform = Encode.Auto.toString (0, outWaveform, extra = extraCoders)|}
    printfn "worker done"
    // closeWorker()

defineWorkerOnMsg onMsg
