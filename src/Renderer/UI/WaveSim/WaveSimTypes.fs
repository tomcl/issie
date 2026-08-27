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

    /// How long the waveforms may lag the view before the viewer says so, in ms.
    ///
    /// Long enough that no ordinary fetch reaches it - scrolling a window of a few cycles is tens
    /// of milliseconds - and short enough that somebody looking at a wrong waveform finds out
    /// while they are still looking at it. A wait that is EXPECTED to be long has a progress bar,
    /// and this says nothing while one is up.
    let staleWaveformWarningMs = 1000.0

    /// How still the waveform viewer must be, in ms, before the run banner may show.
    ///
    /// Scrolling into cycles that have not been simulated starts a run, so without this the
    /// banner appears and vanishes on every step of a drag - noise, and about something the user
    /// is not waiting for, since the next scroll changes what is being run for anyway. Long
    /// enough that a scroll which keeps moving never shows it, short enough that one which stops
    /// on unsimulated cycles says what it is waiting for almost at once.
    let runBannerAfterScrollMs = 500
    // Width of names column - replaced by calcNamesColWidth function

    /// Width of values column
    let valuesColWidth = 100
    let deleteSymbolWidth = 20
    /// Width of the slot at the left of each wave name holding the button which shows that wave's
    /// component on the schematic. Taken out of leftMargin rather than added to the names column,
    /// so that the waveforms are no narrower for it: updateViewerWidthInWaveSim subtracts both from
    /// the same total.
    let viewSymbolWidth = 20
    let scrollBarWidth = 15

    /// How often, and for how many tries, to look for the sheet that a jump to a waveform's
    /// component has asked for. Opening a sheet is asynchronous: its load messages are delivered in
    /// one batch 300ms later by SimulationView.doBatchOfMsgsAsynch, and that batch ends with the
    /// Ctrl-W which fits the sheet to the window - so a jump has to wait for the component to
    /// appear rather than act on the next render, which would be both too early and then undone.
    let sheetLoadPollMs = 100
    let sheetLoadPollTries = 30

    /// Width of left margin of waveform simulator. What was once a 30px margin is now the
    /// view-symbol strip inside the names column (see viewSymbolWidth), which the controls and the
    /// scrollbar are indented past. All that is left to do here is keep the view buttons off the
    /// divider bar.
    let leftMargin = 3
    /// Width of right margin of waveform simulator
    let rightMargin = 0

    /// The waveform table stops this far short of the right-hand edge of the viewer:
    /// room for the scroll bar down the wave column, plus the couple of pixels of padding
    /// that updateViewerWidthInWaveSim has always allowed for. The controls above the
    /// table are indented to match, so that both end on the same line.
    let waveTableRightGap = scrollBarWidth + 8

    /// Margin carried by each button on the two control rows above the waveform table.
    let topRowButtonMargin = 5

    /// Height of each row in name and value columns.
    /// Same as SVG ViewBox Height.
    let rowHeight = 30

    /// Width of line that separates each clock cycle.
    let clkLineWidth = 0.8
    /// Width of each waveform line.
    let lineThickness : float = 0.8
    let columnFontSize = "12px"
 
    let columnFontFamily = "Helvetica"
 
    let valueColumnFontSize = "12px"
    let valueColumnFontFamily = "Helvetica"

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
    /// Fill of every other waveform row, so a trace can be followed across the column to its
    /// name and value. Lavender - namesValuesColumnColor, rgb(230,230,250) - at an opacity low
    /// enough that a blue trace and the purple cursor column both stay clearly on top of it.
    let rowBandColor = "rgba(230,230,250,0.45)"
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

    /// How long to leave a failed fetch of waveform data alone before asking again (in ms).
    ///
    /// Which waves need fetching is derived, so a failure leaves exactly the state that asks for
    /// another fetch: without this the two spin as fast as the message queue will carry them. It
    /// should never be reached - the transport waits for a sidecar that is starting, and what is
    /// left is a fault - which is why it is a plain interval and not a schedule of retries.
    let fetchRetryAfterMs = 2000.



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

    let infoSignUnicode = "\U0001F6C8"

    let waveLegendMaxChars = 35
    let valueColumnMaxChars = 35
    let maxRamRowsDisplayed = 50
    let maxRamLocsWithSparseDisplay = 100

    let tooltipTextColour = "black"
    let tooltipBackgroundColor = "#fffdd0"
    let tooltipShadowOpacity = 0.2

