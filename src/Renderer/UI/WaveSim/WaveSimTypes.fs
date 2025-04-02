module WaveSimTypes

//---------------------------------------------------------------------------------------//
//-----------------------Types and Constants Used Only In WaveSim------------------------//
//---------------------------------------------------------------------------------------//

// TODO: refactor any other types and constants that are only used in WaveSim into this module

// ****************************************************************************************
// NB: many of the types used in waveSim are part of the model so must be defined
// in ModelTypes.fs
// ****************************************************************************************


module Constants =
    // Width of names column - replaced by calcNamesColWidth function

    /// Width of values column
    let valuesColWidth = 100
    let deleteSymbolWidth = 20
    let scrollBarWidth = 15

    /// Width of left margin of waveform simulator
    let leftMargin = 30
    /// Width of right margin of waveform simulator
    let rightMargin = 0

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
    let valueOnWaveText = { DrawHelpers.defaultText with FontSize = "5px" } // dummy size
    /// Whitespace padding between repeated values displayed on non-binary waves.
    let valueOnWavePadding = 75.0
    /// Whitespace padding between non-binary wave values and the edge of transition.
    let valueOnWaveEdgePadding = 4.0

    /// Border between columns and headers of waveform viewer.
    let borderProperties = "2px solid rgb(219,219,219)"

    /// Padding between name label/value label and waveform column.
    let labelPadding = 3
    /// Color for cursor and values column
    let namesValuesColumnColor = "Lavender"
    let cursorColumnColor = "purple"
    let cursorColumnOpacity = 0.3

    /// <summary>Height of scrollbar, in pixels. Affects only the SVG and not the buttons.
    /// Currently set to same height as buttons.</summary>
    let softScrollBarWidth: float = 25.0

    /// <summary>Minimum width of the scrollbar thumb, in pixels.</summary>
    let scrollbarThumbMinWidth: float = 10.0

    /// height of the top half of the wave sim window (including tabs) when waveforms are displayed
    let topHalfHeight = 260.

    // helpers constants
    /// initial time running simulation without spinner to check speed (in ms)
    let initSimulationTime = 100.
    /// max estimated time to run simulation and not need a spinner (in ms)
    let maxSimulationTimeWithoutSpinner = 300.


    /// initial time making waveforms without spinner to check speed (in ms)
    let initWaveformTime = 50.
        /// max estimated time to generate new waveforms and not need a spinner (in ms)
    let maxWaveCreationTimeWithoutSpinner = 100.



    /// The horizontal length of a transition cross-hatch for non-binary waveforms
    let nonBinaryTransLen : float = 2.

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

    /// minium number of cycles on screen when zooming in
    let minVisibleCycles = 3

    /// Minimum number of visible clock cycles.
    let minCycleWidth = 5

    let zoomChangeFactor = 1.5

    /// If the width of a non-binary waveform is less than this value, display a cross-hatch
    /// to indicate a non-binary wave is rapidly changing value.
    let clkCycleNarrowThreshold = 20

    /// number of extra steps simulated beyond that used in simulation. Is this needed?
    let extraSimulatedSteps = 5 

    let infoMessage = 
        "Find ports by any part of their name. '.' = show all. '*' = show selected. '-' = collapse all"

    let outOfDateMessage = "Use refresh button to update waveforms. 'End' and then 'Start' to simulate a different sheet"

    let infoSignUnicode = "\U0001F6C8"

    let waveLegendMaxChars = 35
    let valueColumnMaxChars = 35
    let maxRamRowsDisplayed = 50
    let maxRamLocsWithSparseDisplay = 100

    let tooltipTextColour = "black"
    let tooltipBackgroundColor = "#fffdd0"
    let tooltipShadowOpacity = 0.2

