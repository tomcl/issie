(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It could be put next to CommonTypes but non-UI modules should be agnostic of
    the FRP model and run independently of Fable
*)

module rec ModelType

open CommonTypes
open SimGraphTypes
open SimTypes
open TruthTableTypes
open Fable.React
open VerilogTypes
open ParameterTypes
open Optics
open Optics.Operators

module Constants =
    /// waveform simulator constant here for WSHelpers.initialWSModel reference
    /// maybe better to have this with WaveSim and parametrise initilaWSModel?
    let initialWaveformColWidth = 650 - 20 - 20 - 20 - 130 - 100


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
    | CustomComp
    | FFRegister
    | Memories
    | Component of string


/// What one openable row of the wave selector stands for, which is both how it is styled and the
/// key under which the model remembers whether it is open.
///
/// A row is a group of components within one sheet instance, or the sheet instance itself. There
/// were also cases for a single component and a single port, from when the list was nested a level
/// deeper than it is; nothing has built one since, so the rows that would have used them are gone
/// and only their styling and their summary text remained.
type CheckBoxStyle =
    | GroupItem of ComponentGroup * string list
    | SheetItem of string list

type RightTab =
    | Properties
    | Catalogue
    | Simulation
    | Build
    | Transition // hack to make a transition from Simulation to Catalog without a scrollbar artifact

type SimSubTab =
    | StepSim
    | TruthTable
    | WaveSim

type MemoryEditorData = {
    OnlyDiff : bool // Only show diffs in Memory Diff Viewer.
    Address : bigint option // Only show the specified memory address.
    Start: bigint
    NumberBase : NumberBase
}

//-----------------------Types for code editor-----------------------//



/// A text position interval in the code editor.
/// This is used to represent the start and end of a selection or an error.
/// All characters in raster scan order are considered part of the interval.
type Interval =
    {
        Start: XYPos
        End: XYPos
    }

/// Elmish Model type for a rich text code editor
type CodeEditorModel =
    {
        /// the characters in the code editor as a list of lines
        Lines: string list
        /// The errored code positions
        Errors: Interval list
        /// The current cursor position. Cursor displays between this character and the previous one.
        /// Characters are inserted at this position.
        CursorPos: XYPos
    }


let lines_ = Lens.create (fun a -> a.Lines) (fun s a -> {a with Lines = s})
let errors_ = Lens.create (fun a -> a.Errors) (fun s a -> {a with Errors = s})
let cursorPos_ = Lens.create (fun a -> a.CursorPos) (fun s a -> {a with CursorPos = s})

/// Possible messages used by editor
type EditorMsg =
    | SetCursor of int * int
    | UpdateCode of (string list -> string list)
    | SetErrors of Interval list
    | UpdateCodeEditorState of (CodeEditorModel -> CodeEditorModel)

type ImportDecision =
    | Overwrite
    | Rename

/// Possible fields that may (or may not) be used in a dialog popup.
type PopupDialogData = {
    Text : string option;
    Text2: string option;
    Int : int option;
    ImportDecisions : Map<string, ImportDecision option>
    Int2: bigint option
    Int3: bigint option
    ProjectPath: string
    MemorySetup : (int * int * InitMemData * string option) option // AddressWidth, WordWidth. 
    MemoryEditorData : MemoryEditorData option // For memory editor and viewer.
    Progress: PopupProgress option
    ConstraintTypeSel: ConstraintType option
    ConstraintIOSel: CellIO option
    ConstraintErrorMsg: string option
    NewConstraint: Constraint option
    AlgebraInputs: SimulationIO list option
    AlgebraError: SimulationError option
    VerilogCode: string option
    VerilogErrors: ErrorInfo list
    BadLabel: bool
    IntList: int list option;
    IntList2: int list option;
    DialogState: ParamBoxDialogState option
}

let text_ = Lens.create (fun a -> a.Text) (fun s a -> {a with Text = s})
let text2_ = Lens.create (fun a -> a.Text2) (fun s a -> {a with Text2 = s})
let importDecisions_ = Lens.create (fun a -> a.ImportDecisions) (fun s a -> {a with ImportDecisions = s})
let int_ = Lens.create (fun a -> a.Int) (fun s a -> {a with Int = s})
let int2_ = Lens.create (fun a -> a.Int2) (fun s a -> {a with Int2 = s})
let int3_ = Lens.create (fun a -> a.Int3) (fun s a -> {a with Int3 = s})
let projectPath_ = Lens.create (fun a -> a.ProjectPath) (fun s a -> {a with ProjectPath = s})
let memorySetup_ = Lens.create (fun a -> a.MemorySetup) (fun s a -> {a with MemorySetup = s})
let memoryEditorData_ = Lens.create (fun a -> a.MemoryEditorData) (fun s a -> {a with MemoryEditorData = s})
let progress_ = Lens.create (fun a -> a.Progress) (fun s a -> {a with Progress = s})
let constraintTypeSel_ = Lens.create (fun a -> a.ConstraintTypeSel) (fun s a -> {a with ConstraintTypeSel = s})
let constraintIOSel_ = Lens.create (fun a -> a.ConstraintIOSel) (fun s a -> {a with ConstraintIOSel = s})
let constraintErrorMsg_ = Lens.create (fun a -> a.ConstraintErrorMsg) (fun s a -> {a with ConstraintErrorMsg = s})
let newConstraint_ = Lens.create (fun a -> a.NewConstraint) (fun s a -> {a with NewConstraint = s})
let algebraInputs_ = Lens.create (fun a -> a.AlgebraInputs) (fun s a -> {a with AlgebraInputs = s})
let algebraError_ = Lens.create (fun a -> a.AlgebraError) (fun s a -> {a with AlgebraError = s})
let verilogCode_ = Lens.create (fun a -> a.VerilogCode) (fun s a -> {a with VerilogCode = s})
let verilogErrors_ = Lens.create (fun a -> a.VerilogErrors) (fun s a -> {a with VerilogErrors = s})
let badLabel_ = Lens.create (fun a -> a.BadLabel) (fun s a -> {a with BadLabel = s})
let intlist_ = Lens.create (fun a -> a.IntList) (fun s a -> {a with IntList = s})
let intlist2_ = Lens.create (fun a -> a.IntList2) (fun s a -> {a with IntList2 = s})
let paramCompSpec_ = Lens.create (fun a -> a.DialogState) (fun s a -> {a with DialogState = s})
/// Which dropdown on the renderer's own menu bar is open.
/// Edit and View hold what used to be on the Electron menus of the same names.
type TopMenu = | Closed | Project | Files | Edit | View | TransientClosed

//==========//
// Messages //
//==========//



// Messages that will be triggered on key combinations.
// KeyboardShortcutMsg was an abandoned attempt at a shortcut system: never dispatched, never
// handled, swallowed by the catch-all in Update. Superseded by KeyTypes and KeyBindings.

type UICommandType =
    | CloseProject
    | ChangeSheet
    | RenameSheet
    | ImportSheet
    | DeleteSheet
    | AddSheet
    | SaveSheet
    | StartWaveSim
    | ViewWaveSim
    | CloseWaveSim

type FileCommandType =
    | FileImportSheet
    /// argument is true to save the current project
    | FileNewProject of bool
    /// argumnet is true to save the current project
    | FileOpenProject of bool 
    | FileCloseProject
    | FileSaveOpenFile
    | FileAddFile
    | FileShowDemos of ((string * int * int) list)
    
    
//---------------------------------------------------------------
//---------------------WaveSim types-----------------------------
//---------------------------------------------------------------

/// Determines whether the user is able to see the wave viewer pane.
/// Changes value depending on the state of the circuit and whether
/// the wave simulator has been run.
type WaveSimState =
    /// If the Wave Sim has not been before
    | Empty
    /// If no project is open
    | NoProject
    /// If there is an error in the circuit diagram
    | SimError of SimulationError
    /// If there is no sequential (clocked) logic in the circuit
    | NonSequential
    /// While waiting for the fast simulator to finish running
    | Loading
    /// If there are no errors in the circuit diagram
    | Success
    /// if waveSim has been explicitly ended
    | Ended

/// <summary>Describe WaveSim's scrollbar's mouse actions' type of operation.</summary>
type ScrollbarMouseAction =
    | StartScrollbarDrag
    | InScrollbarDrag
    | ClearScrollbarDrag
    | ReleaseScrollQueue

/// Identifies which Component and Port drives a waveform.
/// Must be an Output port (Input ports cannot drive waveforms).
type DriverT = {
    DriverId: FComponentId
    Port: OutputPortNumber
}

/// A gap in a waveform.
/// Stores information about gaps between NonBinaryTransitions.
/// Used in displayValuesOnWave, and also to store hatched information.
[<Struct>]
type Gap =
    {
        // First cycle which is Change after a Const cycle
        Start: int
        // How many Const cycles there are immediately after this Change transition
        Length: int
    }

type GapStore = {
    Gaps : Gap array
    mutable NextGap : int
    mutable GapStart:int
    mutable GapEnd:int
}


/// Information required to display a waveform.
/// One waveform the viewer knows about: which signal it is, what it is called and where its data
/// lies. What it LOOKS like is not here - see WaveDrawn, which memoises that as a function of this
/// and the view. It used to be: every Wave carried its SVG and the five view settings that SVG had
/// been made under, so the model held a picture of the screen that had to be kept in step with the
/// screen, and "is this waveform up to date" was a question about the model rather than about what
/// is drawn.
/// What the .NET simulator is doing, as far as the renderer knows - and therefore what a
/// synchronous question about it can be answered with.
///
/// **This is the "is building" state that keeps everything else synchronous.** Building a design
/// and running it are the only things a simulator does that take long enough not to be answerable
/// while a frame is drawn, and both are commands the renderer issues. So they are modelled the
/// Elmish way - a message that starts one, a message that says it finished, and this in the model
/// in between - and every other question about the simulator is answered at once, with "nothing
/// yet" exactly while this says a build is in flight. See the module note on SimInterface.
///
/// It was module-level state in SidecarSession, on the argument that what another process holds
/// is not model state. It is: what the renderer BELIEVES about that process is a fact about the
/// renderer, the UI has to draw it, and a belief the view can only reach through a side channel
/// is one the view cannot draw from.
/// Identifies one asynchronous operation asked of the .NET simulator: sent with the request,
/// returned with the answer, and the key under which the model remembers what the operation was.
///
/// **Its own numbering, not the wire's correlation id.** `SidecarClient` gives every REQUEST a
/// correlation id and the sidecar echoes it, which is how a promise finds its reply. An operation
/// is not a request: a build is `sendDesign` - itself one frame per sheet - followed by `SimBuild`,
/// and a fetch is a run followed by a read. One id per request cannot name a thing made of
/// several, so this numbers the operations and `SidecarClient` goes on numbering the frames.
///
/// Unique among the operations OUTSTANDING, not for all time. An entry leaves the table when its
/// answer arrives and there are never many, so a uint32 wrapping cannot collide with anything
/// still in it.
type SeqNum = SeqNum of uint32

/// One asynchronous operation asked of the .NET simulator and not yet answered.
///
/// It carries its own parameters, which is what lets one reply message serve every operation: the
/// answer names the operation, the table says what that operation was, and the update function
/// works out what to do from the two together. Without them there would be a message type per
/// feature, each carrying the context its own handler happened to need.
type SidecarOp =
    /// elaborate the design on the sidecar and start a session
    | OpBuild of top: string * arraySize: int
    /// run the session one chunk towards this cycle, so the waveform view can be read from it.
    /// A run is a sequence of these: the answer says how far it got, and the update function
    /// issues the next unless it has arrived or the user has cancelled
    | OpRunForWaves of toCycle: int
    /// read everything the snapshot's viewports need - waves over the window, every selected
    /// RAM's rows, the panel's signals, the probe, the wanted slices - one operation because it
    /// is one promise, in order, over one connection. Carries the snapshot so the completion
    /// records what WAS fetched, not what is current by then.
    | OpFetch of FetchSnapshot
    /// run to a cycle and read back what the step panel shows there
    | OpStep of cycle: int
    /// read the value on the wire the pointer is resting on, at the cycle being shown
    | OpProbe of wave: WaveIndexT * cycle: int

/// What an operation answered. One case per operation, so that an answer cannot be handled as
/// though it were the answer to something else.
type SidecarAnswer =
    | AnsBuilt of Result<int, string>
    /// the clock the chunk reached, and whether it reached the cycle asked for
    | AnsRan of Result<int * bool, string>
    | AnsFetched of
        Result<unit, string> *
        (FComponentId * (RamView.RamKey * RamView.RamView)) list *
        (WaveIndexT * int * bigint) option
    | AnsStepped
    /// One chunk of a step-simulation run completed: where the clock was when the chunk was
    /// asked for, when it was asked (ms on the performance clock, for the speed display), and
    /// where the SIMULATOR says it now is - the fact the model's clock is set to, never
    /// incremented towards.
    | AnsSteppedTo of before: int * startedMs: float * result: Result<int * bool, string>

/// The session the sidecar holds, which is what an operation commands.
///
/// Not an operation in flight and so not in the table above: a build that has finished leaves a
/// session behind, and it is the session that the next run or read names. Whether a build is
/// RUNNING is read off the table (`ModelHelpers.sidecarIsBuilding`), so there is one place that
/// says so rather than a state here that could disagree with it.
type SidecarSessionState =
    /// nothing built: none has been asked for, or the last simulation ended
    | NoSession
    /// the sidecar holds this design, at this array size, under this session epoch, and has been
    /// run as far as this clock. The clock is here because a run is a sequence of operations and
    /// what each one reports is how far it got - so the progress of a run is model state, drawn
    /// like anything else, rather than a callback threaded down to a caller that ignored it
    | Session of top: string * arraySize: int * epoch: int * clock: int
    /// the last build failed, saying this. Kept so the UI can show it rather than retry for ever
    | SessionFailed of string

    /// The session to command, when there is one.
    member this.Epoch =
        match this with
        | Session(_, _, epoch, _) -> Some epoch
        | _ -> None

    /// Whether the sidecar already holds what a caller is about to ask for. An array size bigger
    /// than the one built for needs a new build; a smaller one does not, since the arrays it wants
    /// are inside the ones there are.
    /// How far the session has been run, or 0 when there is not one.
    member this.Clock =
        match this with
        | Session(_, _, _, clock) -> clock
        | _ -> 0

    member this.Holds(top: string, arraySize: int) =
        match this with
        | Session(built, size, _, _) -> built = top && size >= arraySize
        | _ -> false

/// Everything that could change what DATA the view needs from the .NET simulation, as ONE value
/// derived by one pure function of the model (WaveSimTop.dataViewportOf). This record's field
/// list IS the enumeration that has to be kept right, in one reviewable place: the fetch
/// decision is an equality - current viewport <> the viewport the last COMPLETED fetch was for
/// => fetch the whole current one - and no cache is ever consulted to decide. See
/// docs/dev/sidecarInvariants.md.
type DataViewport =
    { /// the session the values would come from; a rebuild changes this, so nothing fetched from
      /// an older session can satisfy the comparison
      VpEpoch: int
      /// the waveform window on screen (a zero SampleCount when no waveform view is up)
      VpWindow: WaveSlice.Window
      /// the RESOLVED waves drawn over it, address waves included, canonically ordered.
      /// Resolution state is part of the identity, so slices arriving cause a refetch.
      VpWaves: WaveIndexT list
      /// every selected memory and the key (cycle included) its rows are shown under
      VpRams: (FComponentId * RamView.RamKey) list
      /// the cycle the step panel is showing, when it is on screen. Its signal list is a
      /// function of the design, which VpEpoch already names, so it is not repeated here.
      VpPanelCycle: int option
      /// the wire the pointer rests on and the cycle its value is wanted at
      VpProbe: (WaveIndexT * int) option
      /// how many inputs have been poked into the running simulation so far. The stimulus
      /// itself is simulator state the model cannot hold, so the model counts the pokes -
      /// each poke changes this, changes the viewport, and refetches.
      VpStimulus: int }

/// Which instances' port SLICES the selector and wave reconciliation need - the structure
/// viewport beside the data one, separate because its lifetime is the build's: it only ever
/// grows under one epoch, where the data viewport moves with every scroll.
type StructureViewport =
    { SvEpoch: int
      /// canonical (distinct, sorted), so equality means set equality
      SvInstances: InstancePath list }

/// What one fetch was for - recorded on its completion as what WAS fetched, never what is
/// current by then. Each part is Some only when that viewport differed when the fetch was
/// issued.
type FetchSnapshot =
    { SnapData: DataViewport option
      SnapStructure: StructureViewport option }

type Wave = {
    /// Uniquely identifies a waveform
    WaveId: WaveIndexT
    /// Which INSTANCE of a sheet the waveform is in, as the path of custom component labels down to
    /// it - the path of custom components down to it, which is unique by construction. Not something
    /// to show anyone: for the sheet's own name use FastSimulation.getSheetNameOfInstance.
    SheetId: InstancePath
    /// The labels of the custom component instances the waveform's component sits within, outermost
    /// first, which is what tells two instances of one sheet apart when a name cannot.
    SubSheet: string list
    DisplayName: string
    /// Name shown in the waveform viewer. Not guaranteed to be unique.
    ViewerDisplayName: string
    /// Label of the component the waveform is on.
    CompLabel: string
    /// Label of the port the waveform is on.
    PortLabel: string
    /// width of the waveform's bus
    Width: int
    /// Array indexed by clock cycle to show value of wave.
    DriverIndex: DriverIndex
}



let lastClock_ = Lens.create (fun a -> a.LastClock) (fun s a -> {a with LastClock = s})
let firstClock_ = Lens.create (fun a -> a.FirstClock) (fun s a -> {a with FirstClock = s})
let fontSize_ = Lens.create (fun a -> a.FontSize) (fun s a -> {a with FontSize = s})
let fontWeight_ = Lens.create (fun a -> a.FontWeight) (fun s a -> {a with FontWeight = s})

/// Contains all information required by waveform simulator.
/// One WaveSimModel per sheet.
type WaveSimModel = {
    /// default value for cursor in waveform Simulator
    DefaultCursor : CursorType
    /// Configuration for the waveform simulator.//
    WSConfig: WSConfig
    /// temp copy of configuration used by dialog
    WSConfigDialog: WSConfig option
    /// Current state of WaveSimModel.
    State: WaveSimState
    /// What is known about each SELECTED wave: its name, its width, where its data lies, and the
    /// SVG last drawn for it.
    ///
    /// The selected waves and no others. It used to be every wave the simulation offered - one per
    /// viewable port of every INSTANCE of every sheet, which is 208,896 records on main6 of
    /// largeTest, more than the design has components, rebuilt on every scroll step and retained in
    /// every model generation React holds. What the viewer draws is at most
    /// maxAllowedViewerWaves of them; what the SELECTOR draws it now describes as it draws it,
    /// from the handful of sheet instances on screen.
    WaveDetails: Map<WaveIndexT, Wave>
    /// List of which waves are currently visible in the waveform viewer.
    SelectedWaves: WaveIndexT list
    /// Whether this simulation is still owed the selection it gets when the user has chosen
    /// nothing - see WaveSimSelect.defaultSelectedWaves.
    ///
    /// It cannot be derived, which is the whole reason it is here. "Nothing is selected because
    /// the simulation has only just started" and "nothing is selected because the user took the
    /// last wave off" are the same model otherwise, and refilling the second is a viewer the user
    /// cannot empty. Set when a simulation is STARTED and cleared as soon as anything is
    /// selected - by the default choice or by the user.
    ///
    /// It has to survive a refresh or two because of what the choice reads: the ports of the top
    /// instance, which while the .NET simulator is simulating are not known until its first slice
    /// arrives. Choosing once, at the start, therefore chose from nothing and left the viewer
    /// empty - in the mode that ships.
    DefaultSelectionPending: bool
    /// When the view the waveforms are drawn under last changed, in ms on the performance clock.
    ///
    /// Waveforms are deliberately allowed to lag: a viewer whose data comes over a wire keeps
    /// showing the last view it has while the next is fetched, because emptying itself on every
    /// scroll is what a broken viewer looks like. So "drawn for an older view" is a normal state
    /// lasting a frame or two, and it looks exactly like "drawn for a view whose data is never
    /// coming". How long it has gone on is the only thing that tells them apart.
    ///
    /// When a fetch last failed, in ms on the performance clock, or 0 if none has.
    ///
    /// A fetch is asked for whenever a wave is missing the window it is drawn over, and that is
    /// worked out afresh at the end of every update - so a fetch that fails leaves exactly the
    /// state that asks for another, and the two spin as fast as the message queue will carry them.
    /// This is the one thing that stops it: the same waves are not asked for again until a couple
    /// of seconds have passed.
    ///
    /// A timestamp rather than a flag, because the fetch must be tried again eventually, and "how
    /// long since" says that without a second thing to remember to clear.
    ///
    /// It should never be doing any work: a sidecar that is still starting up is waited for by the
    /// transport, so a failure here means a fault - a session that no longer exists, a design that
    /// would not build, a sidecar that has died - and those do not fix themselves. This is what
    /// stops one of them being asked as fast as the message queue will carry it.
    FetchFailedAtMs: float
    /// Left-most visible clock cycle.
    /// this is scaled by CycleMultiplier, and therefore not the real clock cycle
    /// for sampling zoom > 1X.
    StartCycle: int
    /// Total number of visible clock cycles.
    /// This is scaled by cycleMultiplier, and therefore not the real clock cycle
    /// for sampling zoom > 1X.
    ShownCycles: int
    /// Used for extreme zoom out. Sample waveforms every this number of cycles. Display sampled data.
    SamplingZoom: int
    /// Current highlighted clock cycle displayed in the waveform viewer.
    /// This is scaled by CycleMultiplier, and therefore not the real clock cycle
    /// for sampling zoom > 1X.
    CursorDisplayCycle: int
    /// This is the real clock cycle of the cursor which determined the
    /// values column contents. If sampling, a single highlighted waveform cycle
    /// may represent multiple real clock cycles.
    CursorExactClkCycle: int
    /// True if no number in clcock cycle box (special case).
    ClkCycleBoxIsEmpty: bool
    /// Radix in which values are being displayed in the wave simulator.
    Radix: NumberBase
    /// Width of the waveform column.
    WaveformColumnWidth: float
    /// TODO: Should this be refactored into an ActiveModal type option?
    /// If the wave selection modal is visible.
    WaveModalActive: bool
    /// If the ram selection modal is visible.
    RamModalActive: bool
    /// The component whose port selection modal is visible, if any. Set by the schematic's
    /// right-click menu, which names a component on the canvas rather than one in the simulation.
    PortSelectComp: ComponentId option
    /// Every RAM and ROM the simulation holds, with the name the selector lists it under -
    /// which is a fact about the DESIGN, worked out from it rather than from the expansion.
    RamComps: (FComponentId * string) list
    /// Map of which RAM components have been selected.
    SelectedRams: Map<FComponentId, string>
    /// If it exists this is the start location from which RAM locations are displayed.
    /// It is transient.
    /// The first component of the tuple is the text used to define the location.
    /// The second component is the actual location.
    RamStartLocation: Map<FComponentId, string * bigint>
    /// The rows each open RAM table draws, and the cycle and window they are of.
    ///
    /// Model state, not a cache beside it, because it is small - at most a hundred rows a table -
    /// and because a table redrawn from something the model does not hold is a table that does not
    /// redraw when it arrives: the view is memoised on the model, so a reply landing in a module
    /// of its own changes nothing the renderer can see. The waveform data proper is different and
    /// does live outside (WaveData): it is megabytes of typed arrays read on every render.
    ///
    /// Empty while the renderer is the one simulating - the tables read its simulation directly -
    /// and filled by SimReadRam when the .NET simulator is.
    RamRows: Map<FComponentId, RamView.RamKey * RamView.RamView>
    /// Which nodes of the design hierarchy the wave selector has open, each named by its path of
    /// design-time sheet names from the top sheet down. An open node shows its signals, and - for
    /// a sheet that more than one route reaches, whose contents are otherwise suppressed - shows
    /// the sheets inside it. At most one node per sheet name is ever in here: opening one closes
    /// the others, so both panes of the selector agree on which occurrence of a sheet is on show.
    ShowSheetDetail: Set<string list>
    /// Which instance of each sheet the wave selector is showing, keyed the same way. The value is
    /// the instance's path, which is what Wave.SheetId holds. An entry that does not name
    /// an instance inside the instance chosen at the parent node is ignored and the alphabetically
    /// first used - so choosing a different instance high in the hierarchy needs no cascade of
    /// updates to the entries below it.
    SelectedSheetInstance: Map<string list, InstancePath>
    /// What is shown in wave sim group detail elements
    ShowGroupDetail: Set<ComponentGroup * string list>
    /// The label which a user is hovering over.
    HoveredLabel: WaveIndexT option
    /// The index of the wave which the user is dragging.
    DraggedIndex: WaveIndexT option
    /// The value of SelectedWaves when the user started dragging a label.
    /// Used to restore SelectedWaves if the user drops a label in an illegal location.
    PrevSelectedWaves: WaveIndexT list option

    // Scrollbar properties:
    /// <summary>Width of scrollbar's thumb, in pixels.</summary>
    ScrollbarTbWidth: float
    /// <summary>Starting position of scrollbar's thumb, in pixels.</summary>
    ScrollbarTbPos: float
    /// <summary>Offset between scrollbar's thumb's position and cursor's position.
    /// If is Some float, scrollbar is in drag mode; otherwise scrollbar is NOT in drag.</summary>
    ScrollbarTbOffset: float option
    /// <summary>Width of scrollbar's gray background, in pixels.</summary>
    ScrollbarBkgWidth: float
    /// <summary>Number of clock cycles scrollbar's background represents.</summary>
    ScrollbarBkgRepCycs: int
    /// <summary>Counter used to coalesce scrollbar mouse actions together.
    /// If true, queue is clear and can dispatch scrollbar update.
    /// Otherwise, an update is in progress and mouse event should not be pushed onto the queue.</summary>
    ScrollbarQueueIsEmpty: bool
    /// The three filter boxes above the wave selector, each holding what the user typed in it,
    /// upper-cased. A sheet box may end in '*', which means "and everything inside it".
    SheetSearchString: string
    ComponentSearchString: string
    PortSearchString: string
    /// if true, show only the selected waves in the waveform selector
    ShowOnlySelected: bool
}
let showOnlySelected_ = Lens.create (fun a -> a.ShowOnlySelected) (fun s a -> {a with ShowOnlySelected = s})
let sheetSearchString_ = Lens.create (fun a -> a.SheetSearchString) (fun s a -> {a with SheetSearchString = s})
let wSConfig_ = Lens.create (fun a -> a.WSConfig) (fun s a -> {a with WSConfig = s})
let ramStartLocation_ = Lens.create (fun a -> a.RamStartLocation) (fun s a -> {a with RamStartLocation = s})
let ramRows_ = Lens.create (fun a -> a.RamRows) (fun s a -> {a with RamRows = s})
let wSConfigDialog_ = Lens.create (fun a -> a.WSConfigDialog) (fun s a -> {a with WSConfigDialog = s})
let defaultCursor_ = Lens.create (fun a -> a.DefaultCursor) (fun s a -> {a with DefaultCursor = s})

type DiagEl = | Comp of Component | Conn of Connection

type DragMode = DragModeOn of int | DragModeOff

type IntMode = FirstInt | SecondInt | ThirdInt

type MenuCommand =
    | MenuPrint
    | MenuSaveFile
    | MenuSaveProjectInNewFormat
    | MenuNewFile
    | MenuExit
    | MenuZoom of float
    | MenuLostFocus

type SimulationProgress =
    {
        InitialClock: int
        FinalClock: int       
    }

type PopupProgress =
    {
        Value: int
        Max: int
        Title: string
        Speed: float
    }

type TTMsg =
    | GenerateTruthTable of option<Result<SimulationData,SimulationError> * CanvasState>
    | RegenerateTruthTable
    | FilterTruthTable
    | SortTruthTable
    | DCReduceTruthTable
    | HideTTColumns
    | CloseTruthTable
    | ClearInputConstraints
    | ClearOutputConstraints
    | AddInputConstraint of Constraint
    | AddOutputConstraint of Constraint
    | DeleteInputConstraint of Constraint
    | DeleteOutputConstraint of Constraint
    | ToggleHideTTColumn of CellIO
    | ClearHiddenTTColumns
    | ClearDCMap
    | SetTTSortType of (CellIO * SortType) option
    | MoveColumn of (CellIO * MoveDirection)
    | SetIOOrder of CellIO []
    | SetTTAlgebraInputs of SimulationIO list
    | SetTTBase of NumberBase
    | SetTTGridCache of ReactElement option
    | TogglePopupAlgebraInput of (SimulationIO * SimulationData)
    | SetPopupInputConstraints of ConstraintSet option
    | SetPopupOutputConstraints of ConstraintSet option
    | SetPopupConstraintTypeSel of ConstraintType option
    | SetPopupConstraintIOSel of CellIO option
    | SetPopupConstraintErrorMsg of string option
    | SetPopupNewConstraint of Constraint option
    | StartDraggingColumn of CellIO
    | DragColumnEnter of CellIO
    | EndDraggingColumn
    | CancelDraggingColumn
    | SetPopupAlgebraInputs of SimulationIO list option
    | SetPopupAlgebraError of SimulationError option



type Msg =
    | AnyKeyPress of KeyPressInfo
    | WaveSimKeyPress of string
    | ShowExitDialog
    | Sheet of DrawModelType.SheetT.Msg
    | UpdateUISheetTrail of (string list -> string list)
    | SheetBackAction of (Msg -> unit)
    | SynchroniseCanvas
    /// Sent as the last message of every sheet load. Arms the read-only pin if the sheet just
    /// opened is a library sheet being viewed, and clears it otherwise - see
    /// ModelHelpers.pinDrawBlock. Its own message rather than part of SynchroniseCanvas because
    /// loading is not finished there: PropagateParameters still follows, and a pin armed
    /// before it would revert the parameter values the sheet is meant to be drawn at.
    | PinReadOnlyCanvas
    | JSDiagramMsg of JSDiagramMsg
    | Benchmark
    | StartSimulation of Result<SimulationData, SimulationError>
    /// Add WaveSimModel to Model.WaveSim map.
    /// String is name of current sheet.
    | AddWSModel of (string * WaveSimModel)
    /// Update the WaveSimModel of the current sheet.
    | SetWSModel of WaveSimModel
    /// Update the WaveSimModel of the specified sheet from update function
    | UpdateWSModel of (WaveSimModel -> WaveSimModel)
    /// Set the current WaveSimModel to the specified sheet
    /// and update the WaveSimModel of the specified sheet.
    /// Generate waveforms according to the current parameters
    /// of the given WaveSimModel
    /// Change the waveform viewer's state and refresh its waveforms to match. Carries a
    /// TRANSFORM of the state the message lands on, never a snapshot of the state the sender was
    /// drawn from: a snapshot written back undoes, on arrival, everything dispatched since it was
    /// taken - which is how the viewer could oscillate between two pasts.
    | GenerateWaveforms of (WaveSimModel -> WaveSimModel)
    /// The waveform viewer has not been scrolled horizontally for Constants.runBannerAfterScrollMs.
    /// Carries the serial of the scroll it was armed for, and does nothing unless that is still
    /// the last one - see Model.WaveScrollSettling.
    | WaveScrollSettled of int
    /// Generate waveforms according to the model paramerts of Wavesim
    | GenerateCurrentWaveforms
    /// A fetch of waveform data has landed, or failed. Carries nothing: what arrived is in the
    /// waveform cache, and what is still missing is worked out from it.
    /// A fetch for the .NET simulator has come back: whether the waveform data arrived, and the
    /// rows of the one RAM table that was read alongside it, if any.
    ///
    /// One message because it is one request: waves and RAM rows are fetched by a single command,
    /// in order, so that nothing has to arbitrate between two of them - see fetchWhatIsMissing.
    /// The progress-bar popup's Cancel: stop the long simulation run it is reporting, keeping
    /// everything simulated so far, with the viewer moved to the last simulated cycle.
    | CancelWaveSimulation
    /// Run, or rerun, the FastSimulation with the current state of the Canvas.
    /// Start - or restart - the waveform simulation: stop whatever simulation runs, build, then
    /// refresh. The ONE way a waveform simulation begins, and an update branch rather than a
    /// button's closure so the whole sequence runs on the model as it is when the message lands.
    | StartWaveSimulation
    | RefreshWaveSim
    /// Sets or clears ShowSheetDetail (clearing will remove all child values in the set)
    | SetWaveSheetSelectionOpen of (string list list * bool)
    /// Sets or clears GroupDetail
    | SetWaveGroupSelectionOpen of ((ComponentGroup * string list) list * bool)
    | LockTabsToWaveSim
    | UnlockTabsFromWaveSim
    | TryStartSimulationAfterErrorFix of SimSubTab
    | SetSimulationGraph of SimulationGraph  * FastSimulation
    | SetSimulationBase of NumberBase
    | IncrementSimulationClockTick of int
    | EndSimulation
    /// Clears the Model.WaveSim and Model.WaveSimSheet fields.
    | EndWaveSim
    /// An asynchronous operation has been asked of the .NET simulator under this number. It goes
    /// into the in-flight table, where anything asking whether the wire is free can see it.
    | SidecarOpStarted of SeqNum * SidecarOp
    /// That operation answered. The number says which one, the table says what it was, and the two
    /// together are what the update function acts on - which is why there is one of these rather
    /// than a message per feature.
    | SidecarReply of SeqNum * SidecarAnswer
    | TruthTableMsg of TTMsg // all the messages used by the truth table code
    | ChangeRightTab of RightTab
    | ChangeSimSubTab of SimSubTab
    | SetHighlighted of ComponentId list * ConnectionId list
    | SetSelWavesHighlighted of ConnectionId array
    | SetClipboard of CanvasState
    | SetCreateComponent of Component
    | SetProject of Project
    | UpdateProject of (Project -> Project)
    | UpdateModel of (Model -> Model)
    | DispatchDelayed of (int * Msg)
    /// The Simulation tab has noticed its circuit check is out of date. Schedules RunCircuitCheck
    /// after a delay, so that a burst of edits costs one check rather than one per edit.
    | RequestCircuitCheck
    /// Work out whether the open design builds, and store the verdict. Delayed, so by the time it
    /// arrives the design may have changed again - in which case the view simply asks once more.
    | RunCircuitCheck
    | UpdateImportDecisions of Map<string, ImportDecision option>
    | UpdateProjectWithoutSyncing of (Project->Project)
    | ShowPopup of ((Msg -> Unit) -> Model -> ReactElement)
    | ShowStaticInfoPopup of (string * ReactElement * (Msg -> Unit))
    | ClosePopup
    | SetPopupDialogText of string option
    | SetPopupDialogText2 of string option
    | SetPopupDialogBadLabel of bool
    | SetPopupDialogCode of string option
    | SetPopupDialogVerilogErrors of ErrorInfo list
    | SetPopupDialogInt of int option
    | SetPopupDialogInt2 of bigint option
    | SetPopupDialogInt3 of bigint option
    | SetPopupDialogTwoInts of (bigint option * IntMode * string option)
    | SetPopupDialogIntList of int list option
    | SetPopupDialogIntList2 of int list option
    /// Record what one property box now holds: the text as typed, and what it parsed to.
    | AddPopupDialogParamSpec of (ParamSlot * ParamBoxState)
    | ClearPopupDialogParamSpec of ParamSlot
    /// After a sheet is opened: ask the user to choose a top sheet, but only when several
    /// top-level sheets exist, none is chosen, and they disagree about the values the opened
    /// sheet displays with. Never blocks opening.
    | CheckTopSheetChoice
    /// Bring every sheet into line with what its design sets its parameters to, rewriting the
    /// values on their canvases and writing every closed sheet that changes.
    ///
    /// Sent from the events that can change what a design sets - a sheet opened or saved, a
    /// parameter or binding edited, a custom component added or deleted, a project loaded, an undo
    /// - and not from ordinary canvas edits, which cannot change a binding. The work itself is a
    /// pure recomputation (ParameterAnalysis.propagateParameterValues), so sending it twice or in
    /// an unexpected order costs time and nothing else.
    | PropagateParameters
    | SetPropertiesExtraDialogText of string option
    | SetPopupDialogMemorySetup of (int * int * InitMemData * string option) option
    | SetPopupMemoryEditorData of MemoryEditorData option
    | SetPopupProgress of PopupProgress option
    | UpdatePopupProgress of (PopupProgress -> PopupProgress)
    | SimulateWithProgressBar of SimulationProgress
    /// Start a step-simulation run towards a target cycle. The target becomes MODEL state and
    /// the update pipeline is the one thing that issues the sidecar work for it, one chunk at a
    /// time - there is no run loop to collide with anything, and cancelling is clearing the
    /// state. The renderer's own simulator keeps its synchronous chunk loop
    /// (SimulateWithProgressBar), which nothing can interleave with.
    | StartStepRun of SimulationProgress
    | SetSelectedComponentMemoryLocation of bigint * bigint
    | CloseDiagramNotification
    | SetSimulationNotification of ((Msg -> unit) -> ReactElement)
    | CloseSimulationNotification
    | CloseWaveSimNotification
    | SetFilesNotification of ((Msg -> unit) -> ReactElement)
    | CloseFilesNotification
    | SetMemoryEditorNotification of ((Msg -> unit) -> ReactElement)
    | CloseMemoryEditorNotification
    | SetPropertiesNotification of ((Msg -> unit) -> ReactElement)
    | ClosePropertiesNotification
    | SetTopMenu of TopMenu
    /// Pin the Sheet menu open, or release it. See Model.SheetMenuPinned.
    | SetSheetMenuPinned of bool
    | ReloadSelectedComponent of int
    | SetDragMode of DragMode
    /// A catalogue item has been pressed: start carrying it, drawing the given ghost at the
    /// cursor. Nothing is created - see DragPlacement.
    | StartDragPlacement of DragGhost * XYPos
    /// The cursor has moved while carrying a catalogue item, in client coordinates.
    | MoveDragPlacement of XYPos
    /// A carried catalogue item has been released over the canvas at this draw block position.
    | DropDragPlacement of XYPos
    /// Stop carrying a catalogue item, whether because it was dropped somewhere that places
    /// nothing or because the placement it started has now been made.
    | EndDragPlacement
    /// Show this folder in the project browser, opening the browser if it is not already open.
    | SetProjectBrowserFolder of string
    /// A second has passed: read the browsed folder again, and come back in another second.
    /// Does nothing once the browser has closed, which is what ends the chain.
    | TickProjectBrowser
    /// Move the project browser's keyboard selection by this many rows.
    | MoveProjectBrowserSelection of int
    /// Show the folder containing the one being browsed. Does nothing at a filesystem root.
    | GoToProjectBrowserParent
    /// Act on the selected row: a project opens, an ordinary folder is entered. Carries dispatch
    /// as FileCommand does, because opening a project is not something the update function can do
    /// with a message alone.
    | OpenProjectBrowserSelection of (Msg -> unit)
    | ChangeBuildTabVisibility
    /// Set width of right-hand pane when tab is WaveSimulator or TruthTable
    | SetViewerWidth of int
    | MenuAction of MenuCommand * (Msg -> unit)
    | DiagramMouseEvent
    | SelectionHasChanged
    | SetIsLoading of bool
    | SetRouterInteractive of bool
    | CloseApp
    | SetExitDialog of bool
    | ExecutePendingMessages of int
    | DoNothing
    | StartUICmd of UICommandType
    | FinishUICmd
    | FileCommand of FileCommandType * (Msg -> Unit)
    | ReadUserData of string
    | SetUserData of UserData
    | SetThemeUserData of DrawModelType.SymbolT.ThemeType
    | ExecCmd of Elmish.Cmd<Msg>
    | ExecFuncInMessage of (Model -> (Msg->Unit) -> Unit) * (Msg -> Unit)
    | ExecFuncAsynch of (Unit -> Elmish.Cmd<Msg>)
    | ExecCmdAsynch of Elmish.Cmd<Msg>
    | SendSeqMsgAsynch of seq<Msg>
    | ContextMenuAction of e: Browser.Types.MouseEvent
    | ContextMenuItemClick of menuType:string * item:string * dispatch: (Msg -> unit)
    | ScrollbarMouseMsg of cursor:float * action:ScrollbarMouseAction * dispatch:(Msg->unit)
    | SaveModel
    | CheckMemory
    | ChangeWaveSimMultiplier of int
    | RunAfterRender of (bool * ((Msg -> unit) -> Model -> Model))
    | CodeEditorMsg of EditorMsg


//================================//
// Componenents loaded from files //
//================================//

type Notifications = {
    FromDiagram : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromSimulation : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromWaveSim : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromFiles : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromMemoryEditor : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromProperties : ((Msg -> unit) -> Fable.React.ReactElement) option
}

let fromDiagram_ = Lens.create (fun n -> n.FromDiagram) (fun s n -> {n with FromDiagram = s})
let fromSimulation_ = Lens.create (fun n -> n.FromSimulation) (fun s n -> {n with FromSimulation = s})
let fromWaveSim_ = Lens.create (fun n -> n.FromWaveSim) (fun s n -> {n with FromWaveSim = s})
let fromFiles_ = Lens.create (fun n -> n.FromFiles) (fun s n -> {n with FromFiles = s})
let fromMemoryEditor_ = Lens.create (fun n -> n.FromMemoryEditor) (fun s n -> {n with FromMemoryEditor = s})
let fromProperties_ = Lens.create (fun n -> n.FromProperties) (fun s n -> {n with FromProperties = s})


type UserData = {
    /// Where to save the persistent app data
    UserAppDir : string option
    LastUsedDirectory: string option
    RecentProjects: string list option
    ArrowDisplay: bool
    WireType: DrawModelType.BusWireT.WireType
    Theme: DrawModelType.SymbolT.ThemeType
    }



type SpinPayload = {
    /// if false do not show progress bat screen, but still show spinner in button.
    UseProgressBar: bool
    /// text displayed with progress bar
    Name: string
    /// ToDo / Total = progress bar level
    ToDo: int
    /// ToDo / Total = progress bar level 
    Total: int
    }

type TTType = {
    /// bits associated with the maximum number of input rows allowed in a Truth Table
    BitLimit: int
    /// input constraints on truth table generation
    InputConstraints: ConstraintSet
    /// output constraints on truth table viewing
    OutputConstraints: ConstraintSet
    /// which output or viewer columns in the Truth Table should be hidden
    HiddenColumns: CellIO list
    /// by which IO and in what way is the Table being sorted
    SortType: (CellIO * SortType) option
    /// what is the display order of IOs in Table
    IOOrder: CellIO []
    /// Grid Styles for each column in the Table
    GridStyles: Map<CellIO,Props.CSSProp list>
    /// Cached CSS Grid for displaying the Truth Table
    GridCache: ReactElement option
    /// which of the Truth Table's inputs are currently algebra
    AlgebraIns: SimulationIO list
    /// The column which the user is currently dragging
    DraggedColumn: CellIO option
    /// The column which a user is hovering over while dragging
    HoveredColumn: CellIO option
    /// The value of IOOrder when the user started dragging
    PrevIOOrder: CellIO [] option
}
let gridStyles_ = Lens.create (fun a -> a.GridStyles) (fun s a -> {a with GridStyles = s})
let ioOrder_ = Lens.create (fun a -> a.IOOrder) (fun s a -> {a with IOOrder = s})
let inputConstraints_ = Lens.create (fun a -> a.InputConstraints) (fun s a -> {a with InputConstraints = s})
let outputConstraints_ = Lens.create (fun a -> a.OutputConstraints) (fun s a -> {a with OutputConstraints = s})
let hiddenColumns_ = Lens.create (fun a -> a.HiddenColumns) (fun s a -> {a with HiddenColumns = s})
let draggedColumn_ = Lens.create (fun a -> a.DraggedColumn) (fun s a -> {a with DraggedColumn = s})
let hoveredColumn_ = Lens.create (fun a -> a.HoveredColumn) (fun s a -> {a with HoveredColumn = s})
let prevIOOrder_ = Lens.create (fun a -> a.PrevIOOrder) (fun s a -> {a with PrevIOOrder = s})
let sortType_ = Lens.create (fun a -> a.SortType) (fun s a -> {a with SortType = s})
let algebraIns_ = Lens.create (fun a -> a.AlgebraIns) (fun s a -> {a with AlgebraIns = s})
let gridCache_ = Lens.create (fun a -> a.GridCache) (fun s a -> {a with GridCache = s})

type RunData = {
    ButtonSpinnerOn: bool
    FnToRun: ((Msg -> unit) -> (Model -> Model))
}

/// How the components of an in-progress drag came to exist, if they were added at all.
/// DragAndDrop is also entered when moving EXISTING components that end up overlapping, and by
/// undo snapshots, so the drag settling back to idle does not by itself mean "components added".
/// A paste is distinguished from a catalogue placement because its components must inherit the
/// parameter slots of the ones they were copied from.
type DragAddition =
    | PlacedFromCatalogue of ComponentId list
    | PastedFromClipboard of ComponentId list

/// What a catalogue drag draws following the cursor.
type DragGhost =
    /// The symbol itself, drawn by the draw block's own renderer at the component's default
    /// parameters. What the user is carrying looks like what they will get.
    ///
    /// `Clocked` is carried rather than worked out from the type because CommonTypes.isClocked
    /// answers for a custom component by finding its sheet among the project's LoadedComponents,
    /// and a library component's sheet is not in the project until it is dropped. Everything
    /// placed from the project leaves it false and is answered from the model as before; a library
    /// component answers it from the sheet read when the drag began, so the clock mark is on the
    /// ghost of a clocked component rather than appearing only once it lands.
    | GhostSymbol of ComponentType * Clocked: bool
    /// A named box, for a library component whose sheet would not read. The placement will fail
    /// too, and say why; this keeps the gesture working rather than leaving nothing to carry.
    | GhostBox of string

/// A catalogue component being dragged onto the canvas, before it exists.
///
/// Nothing is created until the drop, and where the catalogue item asks for parameters, not until
/// that popup is accepted: what follows the cursor is drawn from a symbol that is never added to
/// any model. That is what makes the gesture free to abandon - a drag ending anywhere but the
/// canvas leaves nothing behind - and it keeps the popup in front of the component exactly as it
/// is for a click.
type DragPlacement =
    /// The button is still down: `Ghost` is drawn centred on `CursorPos`, in client coordinates.
    | Dragging of Ghost: DragGhost * CursorPos: XYPos
    /// Released over the canvas at this position, in draw block coordinates. Consumed by the next
    /// InitialiseCreateComponent, which may be several messages away since a creation popup
    /// stands between the drop and the component existing.
    | DroppedAt of XYPos

/// The project browser's state while it is open.
///
/// Its own field rather than borrowed dialog text: the refresh timer has to be able to ask whether
/// the browser is still open, and a timer that answered from shared popup state would go on
/// writing into whatever dialog opened next.
type ProjectBrowserState = {
    /// The folder being shown.
    Folder: string
    /// What is in it, or why it cannot be shown.
    ///
    /// Read when the folder changes and once a second after that, in the update function rather
    /// than while rendering. A popup body runs on every message, so a view that read the disk
    /// would read it continuously - and the keyboard has to know how many rows there are before
    /// it can move between them.
    Listing: Result<FilesIO.FolderEntry list, string>
    /// The row the keyboard is on, an index into the listing. Clamped whenever the listing changes,
    /// since the folder can grow or shrink underneath it.
    Selected: int
}

/// Which half of the window the keyboard is pointing at.
type Pane = | LeftPane | RightPane

/// Whether the open design currently builds into a simulation, which is all the Simulation tab
/// needs in order to decide whether its button reads "Start Simulation" or "See Problems", and
/// whether to offer the waveform simulator.
///
/// It is here, in the model, because answering it means flattening the whole hierarchy: the tab
/// used to ask on every render, so a large design was flattened again for every frame it was
/// visible. Now it is answered once per edit, on a delay, and the last answer stands while a new
/// one is worked out - a button that is briefly a moment out of date, in exchange for an editor
/// that does not stop.
///
/// The verdict is reached by validateCircuitSimulation, which builds the graph and no
/// FastSimulation, so answering it allocates no step arrays.
type CircuitCheck = {
    /// The last verdict - Ok carrying whether the design is synchronous - together with the
    /// design it was reached from, which is what says whether it is still the current answer.
    /// None until the first check has run.
    Verdict: (Result<bool, SimulationError> * LoadedComponent list) option
    /// Set while a delayed re-check is outstanding. The view asks whenever it sees a stale
    /// verdict, and it renders many times per edit, so without this each one would schedule a check.
    /// Named CheckPending rather than Pending because Model.Pending is the mouse-drag queue, and
    /// two record fields of one name make every `{model with Pending = ...}` ambiguous.
    CheckPending: bool
}

/// The draw block state a read-only sheet is held at, captured once the sheet has finished
/// loading and written back over the live model after every message - see
/// ModelHelpers.pinDrawBlock.
///
/// Exactly the state a sheet is SAVED from, and nothing else. SymbolUpdate.extractComponent is
/// the sole path from symbols to saved state, and it ignores Symbol.Appearance, so colour,
/// opacity, port visibility and corner handles are all absent here and stay free to change:
/// selection and hover go on working on a read-only sheet at no cost. The bounding boxes are
/// here only because they are derived from the symbols, and would otherwise follow symbols that
/// did not move.
///
/// The two clipboards are here for a different reason: pinning them is what stops anything being
/// copied OUT of a library sheet, since a copy writes the clipboard and nothing else. Whatever
/// the user had copied before they looked inside survives, which is what they would expect.
type PinnedCanvas = {
    Symbols: Map<ComponentId, DrawModelType.SymbolT.Symbol>
    Ports: Map<PortId, Port>
    InputPortsConnected: Set<InputPortId>
    OutputPortsConnected: Map<OutputPortId, int>
    CopiedSymbols: Map<ComponentId, DrawModelType.SymbolT.Symbol>
    Wires: Map<ConnectionId, DrawModelType.BusWireT.Wire>
    CopiedWires: Map<ConnectionId, DrawModelType.BusWireT.Wire>
    BoundingBoxes: Map<ComponentId, BoundingBox>
}

type Model = {
    /// Which pane last received a mouse-down, and so where unmodified keys go.
    /// Set on mouse-down only - never on mouse-move - so it cannot change under the user's hand
    /// while they are typing. It replaces a flag set from the pointer's position on every mouse
    /// move, which was stale whenever the pointer had not moved and measured against the wave
    /// simulator's divider whichever tab was actually showing.
    KeyFocusPane: Pane
    /// Function to be run after rendering to update the model
    RunAfterRenderWithSpinner: RunData option
    /// User data for the application
    UserData: UserData
    /// Map of sheet name to WaveSimModel
    WaveSim : Map<string, WaveSimModel>

    /// which top-level sheet is used by wavesim
    WaveSimSheet: string option

    /// A breadcrumb-like trail of visited sheets used for UI back button
    UISheetTrail: string list

    /// If the application has a modal spinner waiting for simulation
    Spinner: (Model -> Model) option
        
    /// Draw Canvas
    Sheet: DrawModelType.SheetT.Model

    /// true during period when a sheet or project is loading
    IsLoading: bool

    /// last time check for changes was made
    LastChangeCheckTime: float

    /// top-level canvas used for current wave simulation
    LastSimulatedCanvasState: CanvasState option // reduced (without layout) canvas state
    /// used to determine whether current canvas has been saved (includes any change)
    LastDetailedSavedState: CanvasState
    /// components and connections currently selected

    CurrentSelected: Component list * Connection list
    /// component ids and connection ids previously selected (used to detect changes)
    LastSelectedIds: string list * string list
    /// last used bus width in bits - used as default in next component create dialog
    LastUsedDialogWidth: int
    /// component currently selected in properties dialog
    SelectedComponent : Component option // None if no component is selected.
    /// used during step simulation: simgraph for current clock tick
    CurrentStepSimulationStep : Result<SimulationData,SimulationError> option // None if no simulation is running.
    /// stores the generated truth table 
    CurrentTruthTable: Result<TruthTable,SimulationError> option // None if no Truth Table is being displayed.
    /// style info for the truth table
    TTConfig: TTType
    /// which of the tabbed panes is currently visible
    RightPaneTabVisible : RightTab
    /// which of the subtabs for the right pane simulation is visible
    SimSubTabVisible: SimSubTab
    /// whether the open design builds into a simulation, for the Simulation tab's buttons
    CircuitCheck: CircuitCheck
    /// components and connections which are highlighted
    Hilighted : (ComponentId list * ConnectionId list) * ConnectionId list
    /// Components and connections that have been selected and copied.
    Clipboard : CanvasState 
    /// Track the last added component
    LastCreatedComponent : Component option 
    /// used to enable "SAVE" button
    SavedSheetIsOutOfDate : bool
    /// How the components of an in-progress drag were added, if they were. Set when the drag
    /// starts and consumed when it settles; None the rest of the time.
    PendingDragAddition : DragAddition option
    /// A catalogue item being dragged onto the canvas, or the position one was dropped at. None
    /// whenever no such gesture is in flight, which is nearly always.
    DragPlacement : DragPlacement option
    /// Set while the project browser is open, and None the rest of the time - which is how its
    /// refresh timer knows to stop.
    ProjectBrowser : ProjectBrowserState option
    /// Projects whose top-sheet choice popup the user has cancelled. Cancelling opens the sheet
    /// at default parameter values; the question is not asked again for that project.
    TopSheetChoiceDeclined : Set<string>
    /// The component libraries available: names and directories only, found once at startup. No
    /// library file is opened until the user opens a library. The catalogue is a pure render
    /// function, so it cannot look for them itself.
    ComponentLibraries : ComponentLibraries.ComponentLibrary list
    /// The library the catalogue is showing instead of its own contents, if any, with the
    /// component headers read when it was opened.
    OpenLibrary : ComponentLibraries.OpenedLibrary option
    /// What the user has typed into the catalogue's search box. Empty shows the catalogue whole,
    /// with its sections closed as before; anything else shows only components whose name or
    /// tooltip contains it, with the sections holding them open.
    CatalogueSearch : string
    /// Whether the sheets of placed library components appear in the Sheets menu. False at
    /// startup and not settable from the ordinary UI: a library component is meant to be one
    /// thing, and seeing its innards listed beside the user's own sheets is confusing rather
    /// than informative. It is worth having for anyone writing a library, who does need to see
    /// what a component brought with it. The waveform simulator ignores this: library sheets
    /// are never offered there.
    /// Run simulation in the renderer instead of the .NET sidecar.
    ///
    /// DEPRECATED, and false by default: the sidecar is the simulator. It builds a simulation
    /// against the machine's memory rather than a browser heap, so it reaches cycle counts the
    /// renderer cannot, and it simulates roughly twice as fast on the designs that take long
    /// enough to care (docs/dev/sidecarSimulation.md has the measurements). The renderer's own
    /// simulator is kept for development - it is what the sidecar's results are checked against,
    /// and the only one the algebraic (FData) path and truth tables have - and is expected to be
    /// removed once the sidecar has been trusted for a while.
    ///
    /// Switchable only from Development > Simulation, and only while nothing is simulating:
    /// changing it throws away every simulation in flight, since the two backends do not share
    /// so much as a step array.
    SimulateInRenderer : bool
    /// The session the .NET simulator is believed to hold, which is what a command names.
    SidecarSession : SidecarSessionState
    /// The value the schematic probe last read, and what was asked for to get it.
    ///
    /// In the model rather than beside it because the probe is drawn from the model, and an answer
    /// arriving anywhere else would not redraw the label it belongs to. Keyed by the wave and the
    /// cycle, so a label is drawn only where the value on screen is the value that was asked for -
    /// which is what stops the previous wire's value appearing under the pointer on this one.
    ProbeRead : (WaveIndexT * int * bigint) option
    /// Every asynchronous operation asked of the .NET simulator and not yet answered, by the
    /// number it was asked under.
    ///
    /// **This is what makes synchronous interrogation safe.** A synchronous operation may go out
    /// only when this is empty, and two synchronous operations cannot overlap, because one runs to
    /// completion on the renderer's single thread before anything else runs. Those two facts
    /// together are why nothing here needs a priority queue, a dispatch lock, or preemption: a
    /// synchronous operation never waits behind an asynchronous one, because it is never issued
    /// while one exists.
    SidecarInFlight : Map<SeqNum, SidecarOp>
    /// When the .NET simulator's last build reply landed, ms on the performance clock. With
    /// SidecarRunEndedMs, what the stale-waveform warning times itself from: the warning must not
    /// count time spent building or simulating - both have their own feedback - so its clock
    /// starts at whichever came last of the view changing and these two ending. Zero when no such
    /// thing has happened, which max then ignores.
    SidecarBuildEndedMs : float
    /// When the last run chunk's reply landed. Stamped per chunk: while the loop runs the warning
    /// is suppressed anyway, so the stamp that matters is the final one - the loop's end.
    SidecarRunEndedMs : float
    /// The cycle a step-simulation run is heading for, when one is - with where it started, for
    /// the progress bar. Model state, not a loop: after every message the update pipeline sees
    /// this set, the wire free and the clock short of the target, and issues ONE chunk. Cleared
    /// when the clock arrives, when the progress popup is closed (which is what Cancel is), and
    /// when the simulation ends.
    StepRunTarget : SimulationProgress option
    /// The data viewport the last COMPLETED fetch was for - None until one completes under the
    /// current session. The fetch decision is `dataViewportOf model <> FetchedData`; nothing
    /// clears this on rebuild because the epoch inside the viewport makes a rebuild an
    /// inequality (EndSimulation clears it as hygiene only).
    FetchedData : DataViewport option
    /// the structure viewport the last completed fetch covered, likewise
    FetchedStructure : StructureViewport option
    /// The snapshot of a fetch that FAILED: not reissued while the current snapshot still
    /// equals it - reset is inequality itself, so a changed viewport tries again and an
    /// unchanged one does not retry at wire speed.
    FailedFetch : FetchSnapshot option
    /// pokes into the running simulation so far - see DataViewport.VpStimulus
    StimulusGeneration : int
    /// The data viewport as the checks last derived it, with when it last CHANGED. The pair
    /// exists for one question the derivation alone cannot answer - how long has THIS viewport
    /// been waiting to be served - which is what the stale banner is about: it shows when the
    /// viewport is still unserved (differs from FetchedData; an error reply never updates that,
    /// so an errored fetch counts as still outstanding) for longer than any healthy fetch
    /// takes, measured from this stamp. A viewport change - a scroll, a hover - resets the
    /// clock, so starting a fetch can never look stale for the frames it is in flight.
    CurrentViewport : DataViewport option
    ViewportChangedAtMs : float
    /// True from a horizontal scroll of the waveform viewer until it has been still for
    /// Constants.runBannerAfterScrollMs, carrying the serial number of the scroll that set it.
    /// The run banner shows nothing while it holds - scrolling into cycles that have not been
    /// simulated starts a run every time, and a banner that flashes on each step of a drag is
    /// noise. The serial is what makes the wait restartable: the WaveScrollSettled message
    /// carries the serial it was armed for, so the timer from an earlier step of a drag cannot
    /// put the banner back up under a later one.
    WaveScrollSettling : bool
    WaveScrollSerial : int
    ShowLibrarySheets : bool
    /// The library sheets the user has asked to look inside, by name. A library component is an
    /// abstraction and its sheet is not normally reachable at all, but understanding how one
    /// works is a fair thing to want, so an instance's right-click menu can open its sheet
    /// read-only. Held here rather than on the sheet's CCForm because Form is written to the
    /// .dgm on every save: this state is meant to last only as long as the project is open, and
    /// storing it in the file would both outlive that and let the Sheets menu's Unlock turn a
    /// library sheet into an ordinary editable one.
    OpenedLibrarySheets : Set<string>
    /// What the open sheet's draw block is pinned to, when it is one of the above and has
    /// finished loading. None for every ordinary sheet, and while a read-only one is still
    /// loading - loading legitimately changes the canvas, recomputing symbol sizes, rerouting
    /// wires whose ports moved and centring the circuit, so the pin cannot be armed until that
    /// has settled. See ModelHelpers.pinDrawBlock.
    ReadOnlyBaseline : PinnedCanvas option
    /// the project contains, as loadable components, the state of each of its sheets
    CurrentProj : Project option
    /// function to create popup pane if present
    PopupViewFunc : ((Msg -> Unit) -> Model -> Fable.React.ReactElement) option
    /// function to create spinner popup pane if present (overrides otehr popups)
    SpinnerPayload : SpinPayload option
    /// data to populate popup (may not all be used)
    PopupDialogData : PopupDialogData
    /// record containing functions that create react elements of notifications
    Notifications : Notifications
    /// State of menus for sheets, projects etc
    TopMenuOpenState : TopMenu
    /// The Sheet menu is normally dismissed by clicking anywhere outside it - a sheet in it, or
    /// the schematic. Pinned, it stays open until the pin is clicked again, so that a design can
    /// be navigated with the hierarchy in view. Sheets fitted while it is pinned are sized to the
    /// canvas beside it rather than to the whole canvas - see Sheet.getWindowParasToFitBox.
    SheetMenuPinned : bool
    /// used to determine whether mouse is currently dragging the divider, or used normally
    DividerDragMode: DragMode
    /// viewer width in pixels altered by dragging the divider
    WaveSimViewerWidth: int
    /// if true highlight connections from wavesim editor
    ConnsOfSelectedWavesAreHighlighted: bool
    /// Contains a list of pending messages
    Pending: Msg list
    /// Bad way to tidy up the messy UI commands - better - do them all in just one message!
    UIState: UICommandType Option
    /// if true the "build" tab appears on the RHS
    BuildVisible: bool
    CodeEditorState: CodeEditorModel option
} 

    with member this.WaveSimOrCurrentSheet =
            match this.WaveSimSheet, this.CurrentProj with
            | None, Some {OpenFileName = name} -> name
            | Some name, _ -> name
            | None, None -> failwithf "What? Project is not open cannot guess sheet!"

let waveSimSheet_ = Lens.create (fun a -> a.WaveSimSheet) (fun s a -> {a with WaveSimSheet = s})
let waveSim_ = Lens.create (fun a -> a.WaveSim) (fun s a -> {a with WaveSim = s})
let codeEditorState_ = Lens.create (fun a -> a.CodeEditorState) (fun s a -> {a with CodeEditorState = s})

let runAfterRender_ = Lens.create (fun a -> a.RunAfterRenderWithSpinner) (fun s a -> {a with RunAfterRenderWithSpinner = s})
let rightPaneTabVisible_ = Lens.create (fun a -> a.RightPaneTabVisible) (fun s a -> {a with RightPaneTabVisible = s})
let simSubTabVisible_ = Lens.create (fun a -> a.SimSubTabVisible) (fun s a -> {a with SimSubTabVisible = s})
let circuitCheck_ = Lens.create (fun a -> a.CircuitCheck) (fun s a -> {a with CircuitCheck = s})
let verdict_ = Lens.create (fun (a: CircuitCheck) -> a.Verdict) (fun s (a: CircuitCheck) -> {a with Verdict = s})
let checkPending_ = Lens.create (fun (a: CircuitCheck) -> a.CheckPending) (fun s (a: CircuitCheck) -> {a with CheckPending = s})
let buildVisible_ = Lens.create (fun a -> a.BuildVisible) (fun s a -> {a with BuildVisible = s})
let sheetMenuPinned_ = Lens.create (fun a -> a.SheetMenuPinned) (fun s a -> {a with SheetMenuPinned = s})
let popupViewFunc_ = Lens.create (fun a -> a.PopupViewFunc) (fun s a -> {a with PopupViewFunc = s})

let sheet_ = Lens.create (fun a -> a.Sheet) (fun s a -> {a with Sheet = s})
let tTType_ = Lens.create (fun a -> a.TTConfig) (fun s a -> {a with TTConfig = s})
let currentStepSimulationStep_ = Lens.create (fun a -> a.CurrentStepSimulationStep) (fun s a -> {a with CurrentStepSimulationStep = s})
let currentTruthTable_ = Lens.create (fun a -> a.CurrentTruthTable) (fun s a -> {a with CurrentTruthTable = s})
let popupDialogData_ = Lens.create (fun a -> a.PopupDialogData) (fun p a -> {a with PopupDialogData = p})
let selectedComponent_ = Lens.create (fun a -> a.SelectedComponent) (fun s a -> {a with SelectedComponent = s})
let userData_ = Lens.create (fun a -> a.UserData) (fun s a -> {a with UserData = s})
let uISheetTrail_ = Lens.create (fun a -> a.UISheetTrail) (fun s a -> {a with UISheetTrail = s})
let savedSheetIsOutOfDate_ = Lens.create (fun a -> a.SavedSheetIsOutOfDate) (fun s a -> {a with SavedSheetIsOutOfDate = s})
let pendingDragAddition_ = Lens.create (fun a -> a.PendingDragAddition) (fun s a -> {a with PendingDragAddition = s})
let dragPlacement_ = Lens.create (fun a -> a.DragPlacement) (fun s a -> {a with DragPlacement = s})
let projectBrowser_ = Lens.create (fun a -> a.ProjectBrowser) (fun s a -> {a with ProjectBrowser = s})
let topSheetChoiceDeclined_ = Lens.create (fun a -> a.TopSheetChoiceDeclined) (fun s a -> {a with TopSheetChoiceDeclined = s})
let openLibrary_ = Lens.create (fun a -> a.OpenLibrary) (fun s a -> {a with OpenLibrary = s})
let catalogueSearch_ = Lens.create (fun a -> a.CatalogueSearch) (fun s a -> {a with CatalogueSearch = s})
let simulateInRenderer_ = Lens.create (fun a -> a.SimulateInRenderer) (fun s a -> {a with SimulateInRenderer = s})
let sidecarSession_ = Lens.create (fun a -> a.SidecarSession) (fun s a -> {a with SidecarSession = s})
let probeRead_ = Lens.create (fun a -> a.ProbeRead) (fun s a -> {a with ProbeRead = s})
let sidecarInFlight_ = Lens.create (fun a -> a.SidecarInFlight) (fun s a -> {a with SidecarInFlight = s})
let sidecarBuildEndedMs_ = Lens.create (fun a -> a.SidecarBuildEndedMs) (fun s a -> {a with SidecarBuildEndedMs = s})
let sidecarRunEndedMs_ = Lens.create (fun a -> a.SidecarRunEndedMs) (fun s a -> {a with SidecarRunEndedMs = s})
let stepRunTarget_ = Lens.create (fun a -> a.StepRunTarget) (fun s a -> {a with StepRunTarget = s})
let fetchedData_ = Lens.create (fun a -> a.FetchedData) (fun s a -> {a with FetchedData = s})
let fetchedStructure_ = Lens.create (fun a -> a.FetchedStructure) (fun s a -> {a with FetchedStructure = s})
let failedFetch_ = Lens.create (fun a -> a.FailedFetch) (fun s a -> {a with FailedFetch = s})
let stimulusGeneration_ = Lens.create (fun a -> a.StimulusGeneration) (fun s a -> {a with StimulusGeneration = s})
let currentViewport_ = Lens.create (fun a -> a.CurrentViewport) (fun s a -> {a with CurrentViewport = s})
let viewportChangedAtMs_ = Lens.create (fun a -> a.ViewportChangedAtMs) (fun s a -> {a with ViewportChangedAtMs = s})
let waveScrollSettling_ = Lens.create (fun a -> a.WaveScrollSettling) (fun s a -> {a with WaveScrollSettling = s})
let waveScrollSerial_ = Lens.create (fun a -> a.WaveScrollSerial) (fun s a -> {a with WaveScrollSerial = s})
let showLibrarySheets_ = Lens.create (fun a -> a.ShowLibrarySheets) (fun s a -> {a with ShowLibrarySheets = s})
let openedLibrarySheets_ = Lens.create (fun a -> a.OpenedLibrarySheets) (fun s a -> {a with OpenedLibrarySheets = s})
let readOnlyBaseline_ = Lens.create (fun a -> a.ReadOnlyBaseline) (fun s a -> {a with ReadOnlyBaseline = s})

let currentProj_ = Lens.create (fun a -> a.CurrentProj) (fun s a -> {a with CurrentProj = s})
let openLoadedComponentOfModel_ = currentProj_ >-> Optics.Option.value_ >?> openLoadedComponent_
let notifications_ = Lens.create (fun a -> a.Notifications) (fun s a -> {a with Notifications = s})
let project_ = Lens.create (fun a -> Option.get (a.CurrentProj)) (fun s a -> {a with CurrentProj = Some s})
let projectOpt_ = Prism.create (fun a -> a.CurrentProj) (fun s a -> {a with CurrentProj =  a.CurrentProj |> Option.map (fun _ -> s)})
let ldcM = project_ >-> loadedComponents_
let ldcOptM = projectOpt_ >?> loadedComponents_
let nameM = project_ >-> openFileName_
let nameOptM = projectOpt_ >?> openFileName_

/// Update the WaveSimModel of the current waveSim sheet.
let putWaveSim (wsm: WaveSimModel) (model: Model) =
    match model.WaveSimSheet with
    | None -> model
    | Some sheet -> 
        model
        |> Optic.map waveSim_ (Map.add sheet wsm)






