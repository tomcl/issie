module rec DrawModelType
#nowarn "40"

open Fable.Core
open CommonTypes
open DrawHelpers
open Fable.React
open Optics
open Node.ChildProcess
open Operators


//--------------------------COMMON TYPES------------------------------//

/// Static 1D data defining position range within which the currently moved thing will "snap"
/// to fixed point (Snap).
type SnapData = {
    UpperLimit: float
    LowerLimit: float
    Snap: float
    /// DisplayLine may not be the same as Snap because when two symbols snap together the
    /// displayed line must go through the centre of each symbol, whereas the TopLeft 
    /// coordinate is the one which is snapped
    IndicatorPos: float
    }

/// Dynamic data used when a symbol is being snapped
type Snap = {
    UnSnapPosition: float
    SnapPosition: float
    SnapIndicatorPos: float
}
// lenses to access fields in above types
let unSnapPositon_ = Lens.create (fun s -> s.UnSnapPosition) (fun u s -> {s with UnSnapPosition = u})
let snapPosition_ = Lens.create (fun s -> s.SnapPosition) (fun u s -> {s with SnapPosition = u})
let snapIndicatorPos_ = Lens.create (fun s -> s.SnapIndicatorPos) (fun u s -> {s with SnapIndicatorPos = u})

/// All the 1D data needed to manage snapping of a moving symbol
type SnapInfo = {
    /// static data - set of "snap" positions
    SnapData: SnapData array 
    /// dynamic data - present if symbol is currently snapped
    SnapOpt: Snap option 
    }

// lenses to access fields in the above types
let snapData_ = Lens.create (fun inf -> inf.SnapData) (fun s inf -> {inf with SnapData = s})
let snapOpt_ = Lens.create (fun inf -> inf.SnapOpt) (fun s inf -> {inf with SnapOpt = s})

type SnapXY = {SnapX: SnapInfo; SnapY: SnapInfo}
let snapX_ = Lens.create (fun xy -> xy.SnapX) (fun s xy -> {xy with SnapX = s})
let snapY_ = Lens.create (fun xy -> xy.SnapY) (fun s xy -> {xy with SnapY = s})


/// ---------- SYMBOL TYPES ----------
module SymbolT =
    open Optics.Operators

    /// Represents the orientation of a wire segment or symbol flip
    [<StringEnum>]
    type FlipType =  FlipHorizontal | FlipVertical
    [<StringEnum>]
    //Used in scaling in SmartRotate
    //HLP23: AUTHOR Ismagilov
    type ScaleType = ScaleUp | ScaleDown
    /// Wraps around the input and output port id types

    /// A port named together with the side of a connection it is: the undirected id is
    /// PortId, which this carries inside InputPortId or OutputPortId.
    type DirectedPortId = | InputId of InputPortId | OutputId of OutputPortId

    /// data structures defining where ports are put on symbol boundary
    /// strings here are used for port ids
    type PortMaps =
        {     
            /// Maps edge to list of ports on that edge, in correct order
            Order: Map<Edge, PortId list>
            /// Maps the port ids to which side of the component the port is on
            Orientation: Map<PortId, Edge>
        }

    let order_ = Lens.create (fun a -> a.Order) (fun s a -> {a with Order = s})
    let orientation_ = Lens.create (fun a -> a.Orientation) (fun s a -> {a with Orientation = s})

    /// data here changes how the symbol looks but has no other effect
    type ShowPorts = | ShowInput | ShowOutput | ShowBoth | ShowBothForPortMovement | ShowNone | ShowOneTouching of Port | ShowOneNotTouching of Port | ShowTarget  
    
    // HLP23 AUTHOR: BRYAN TAN
    type ShowCorners = | ShowAll | DontShow
    type Annotation = ScaleButton | RotateButton of Rotation
    type AppearanceT =
        {
            // During various operations the ports on a symbol (input, output, or both types)
            // are highlighted as circles. This D.U. controls that.
            ShowPorts: ShowPorts
            ShowCorners: ShowCorners
            // appears not to be used now.
            HighlightLabel: bool
            /// symbol color is determined by symbol selected / not selected, or if there are errors.
            Colour: string
            /// translucent symbols are used uring symbol copy operations.
            Opacity: float  
        }

    /// This defines the colors used in teh drawblack, and therfore also the symbol color.
    type ThemeType =
        |White
        |Light
        |Colourful

    let showPorts_ = Lens.create (fun a -> a.ShowPorts) (fun s a -> {a with ShowPorts = s})
    // let showOutputPorts_ = Lens.create (fun a -> a.ShowOutputPorts) (fun s a -> {a with ShowOutputPorts = s})
    let showCorners_ = Lens.create (fun a -> a.ShowCorners) (fun s a -> {a with ShowCorners = s})
    let highlightLabel_ = Lens.create (fun a -> a.HighlightLabel) (fun s a -> {a with HighlightLabel = s})
    let colour_ = Lens.create (fun a -> a.Colour) (fun s a -> {a with Colour = s})
    let opacity_ = Lens.create (fun a -> a.Opacity) (fun s a -> {a with Opacity = s})


    /// Represents a symbol, that contains a component and all the other information needed to render it
    type Symbol =
        {
            /// Coordinates of the symbol's top left corner
            /// on component save/restore this is also X,Y on Component type.
            Pos: XYPos

            /// Initial coordinates of the symbol's centre when ScaleButton is pressed
            CentrePos: XYPos

            /// symbol's centre to the selected components' boundingBox centre when ScaleButton is pressed
            OffsetFromBBCentre: XYPos
        
            /// Width of the wires connected to input ports 0 & 1
            /// This is needed on the symbol only for  bus splitter and bus merge symbols
            /// These display the bit numbers of their connections.
            /// wire widths are calulated by width inference so these values are transient and
            /// need not be stored.
            InWidth0: int option
            InWidth1: int option

            /// for MergeN later
            InWidths: int option list option

            /// the following fields define the position and size of the component label.
            /// labels will rotate when the symbol is rotated.
            /// labels can be manually adjusted, if not position is as default (for given rotation)
            LabelBoundingBox: BoundingBox
            LabelHasDefaultPos: bool
            LabelRotation: Rotation option
        
            /// this filed contains transient information that alters the appearance of the symbol
            Appearance: AppearanceT

            /// This, for convenience, is a copy of the component Id string, used as Id for symbol
            /// Thus symbol Id = component Id.
            /// It is unique within one design sheet.
            Id : ComponentId  

            /// This is the electrical component.
            /// When the component is loaded into draw block the position is kept as Pos field in symbol
            /// However H & W remain important, as the height and width of the symbol. This is anomalous.
            /// It would make sure sense for all geometric info to be in fields on the symbol.
            /// However X,Y,H,W are used (to some extent) in non-draw-block Issie code.
            /// NB HScale, VScale modify V, H
            /// The symbol's component, at the values it is drawn AND saved at. Parameter values
            /// are resolved into it by ParameterAnalysis.propagateParameterValues before it gets
            /// here, so there is one set of values rather than a displayed set over a stored one.
            Component : Component

            /// Use Some Annotation for visible (and clickable) objects on screen
            /// In this case Component is a dummy used only to provide expected H & V
            Annotation: Annotation option
            
            /// transient field to show if ports are being dragged in teh UI.
            Moving: bool
            /// determines whetehr the symbol or its contents (it it is a custom component) contain any clo9cked logic.
            /// used to display a clokc symbol
            IsClocked: bool
            /// determines whether symbol is rorated (in 90 degree increments) or flipped (reflected).
            STransform: STransform
            ReversedInputPorts: bool option

            /// These maps contain the order (on and edge), and the symbol edge, of each port.
            /// Edge positions are known from the component XYPos and H (height), W (width).
            /// Ports are located as fixed equidistant positions along each component edge dependent on number of ports.
            /// Therefore port position can be calculated from these maps and XYPos, H, W.
            PortMaps : PortMaps

            /// HScale & VScale modify default W (width)  and H (height) respectively if not None. They are changed by symbol property box.
            /// Horizontal symbol dimension = HScale*W etc
            /// They are currently used only on Custom Components and will not work on other omponents.
            HScale : float option
            /// HScale & VScale modify W and H respectively if not None.
            /// Vertical symbol dimension = VScale*H etc
            /// They are currently used only on Custom Components and will not work on other omponents.

            VScale : float option

            /// Option to represent a port that is being moved, if it's some, it contains the moving port's Id and its current position.
            /// dynamic info used in port move operation.
            MovingPort: Option<{|PortId: PortId; CurrPos: XYPos|}>
            /// dynamic info used in port move operation
            MovingPortTarget: (XYPos*XYPos) option

        }

    let appearance_ = Lens.create (fun a -> a.Appearance) (fun s a -> {a with Appearance = s})
    let moving_ = Lens.create (fun a -> a.Moving) (fun s a -> {a with Moving = s})
    let labelBoundingBox_ = Lens.create (fun a -> a.LabelBoundingBox) (fun s a -> {a with LabelBoundingBox = s})
    let portMaps_ = Lens.create (fun a -> a.PortMaps) (fun s a -> {a with PortMaps = s})
    let movingPort_ = Lens.create (fun a -> a.MovingPort) (fun s a -> {a with MovingPort = s})
    let movingPortTarget_ = Lens.create (fun a -> a.MovingPortTarget) (fun s a -> {a with MovingPortTarget = s})
    let component_ = Lens.create (fun a -> a.Component) (fun s a -> {a with Component = s})
    let posOfSym_ = Lens.create (fun a -> a.Pos) (fun s a -> {a with Pos = s})
    let getScaleF = Option.defaultValue 1.
    let scaleF_ = Lens.create
                    (fun sym -> {X= getScaleF sym.HScale; Y=getScaleF sym.VScale})
                    (fun sf sym -> {sym with HScale = Some sf.X; VScale = Some sf.Y})


    /// Represents all the symbols and ports on the sheet
    type Model = {
        Symbols: Map<ComponentId, Symbol>

        /// All the symbols currently on the clipboard
        CopiedSymbols: Map<ComponentId, Symbol>

        /// Contains all the input and output ports in the model (currently rendered)
        Ports: Map<PortId, Port>

        /// Contains all the inputports that have a wire connected to them.
        /// If a port is in the set, it is connected, otherwise it is not
        InputPortsConnected:  Set<InputPortId>

        /// Represents the number of wires connected to each output port in the model
        OutputPortsConnected: Map<OutputPortId, int>

        Theme: ThemeType

        HintPane: string list option
        }

    //----------------------------Message Type-----------------------------------//

    /// The different messages coming from sheet, normally represent events
    type Msg =
        | MouseMsg of MouseT
        | AddSymbol of (LoadedComponent list) * pos:XYPos * compType:ComponentType * lbl: string
        | CopySymbols of ComponentId list
        | DeleteSymbols of sIds:ComponentId list
        | ShowAllInputPorts | ShowAllOutputPorts | DeleteAllPorts
        | MoveSymbols of compList: ComponentId list * move: XYPos
        | MoveLabel of compId: ComponentId * move: XYPos
        | ShowPorts of ComponentId list
        | ShowCustomOnlyPorts of ComponentId list
        | SelectSymbols of ComponentId list// Issie interface
        | SymbolsHaveError of sIds: ComponentId list
        | ChangeLabel of sId : ComponentId * newLabel : string
        | PasteSymbols of sIds: ComponentId list
        | ColorSymbols of compList : ComponentId list * colour : HighLightColor
        | ErrorSymbols of errorIds: ComponentId list * selectIds: ComponentId list * isDragAndDrop: bool
        | ChangeNumberOfBits of compId:ComponentId * NewBits:int 
        | ChangeLsb of compId: ComponentId * NewBits:bigint 
        | ChangeInputValue of compId: ComponentId * newVal: bigint
        | ChangeScale of compId:ComponentId * newScale:float * whichScale:ScaleAdjustment
        | ChangeConstant of compId: ComponentId * NewBits:bigint * NewText:string
        | ChangeBusCompare of compId: ComponentId * NewBits:bigint * NewText:string
        | ChangeReversedInputs of compId: ComponentId
        | ChangeAdderComponent of compId: ComponentId * oldComp: Component * newComp: ComponentType
        | ChangeCustom of compId: ComponentId * oldComp: Component * newComp: ComponentType
        | ChangeCounterComponent of compId: ComponentId * oldComp: Component * newComp: ComponentType
        | ResetModel // For Issie Integration
        | LoadComponents of  LoadedComponent list * Component list // For Issie Integration
        | WriteMemoryLine of ComponentId * bigint * bigint // For Issie Integration 
        | WriteMemoryType of ComponentId * ComponentType
        | UpdateMemory of ComponentId * (Memory1 -> Memory1)
        | RotateLeft of compList : ComponentId list * Rotation
        | RotateAntiClockAng of compList : ComponentId list * Rotation
        | Flip of compList: ComponentId list * orientation: FlipType
        /// Taking the input and..
        | MovePort of portId: PortId * move: XYPos
        | MovePortDone of portId: PortId * move: XYPos
        // HLP23 AUTHOR: BRYAN TAN
        | ShowCustomCorners of compList: ComponentId list
        | HideCustomCorners of compList: ComponentId list
        | ResizeSymbol of compId: ComponentId * corner: XYPos * move: XYPos // corner XYPos of clicked corner. move: XYPos of mouse
        | ResizeSymbolDone of compId: ComponentId * resetSymbol: Symbol option * corner: XYPos * move: XYPos
        | SaveSymbols
        | SetTheme of ThemeType
             //------------------------Sheet interface message----------------------------//
        | UpdateBoundingBoxes

    
    let symbols_ = Lens.create (fun m -> m.Symbols) (fun s m -> {m with Symbols = s})
    let ports_ = Lens.create (fun m -> m.Ports) (fun w m -> {m with Ports = w})
    let symbolOf_ k = symbols_ >-> Map.valueForce_ "What? Symbol id lookup in model failed" k
    let hintPane_ = Lens.create (fun m -> m.HintPane) (fun s m -> {m with HintPane = s})


    //------------------------------------------------------------------------//
    //------------------------------BusWire Types-----------------------------//
    //------------------------------------------------------------------------//
    
module BusWireT =

    [<StringEnum>]
    type Orientation = | Vertical | Horizontal
    
    ///
    type SnapPosition = High | Mid | Low
    
    /// Represents how wires are rendered
    type WireType = Radial | Modern | Jump
    
    /// Represents how a wire segment is currently being routed

    [<StringEnum>]
    type RoutingMode = Manual | Auto
    
    /// Used to represent a segment in a wire
    type Segment = 
        {
            Index: int
            Length : float
            WireId: ConnectionId
            /// List of offsets along a segment where jumps or intersects occur. Matches the sign of Length. Only used on horizontal segments.
            IntersectOrJumpList: float list
            Draggable : bool
            Mode : RoutingMode
        }
        with
            /// get SegmentID id for segment
            member inline this.GetId = this.Index,this.WireId
            /// return true if segment length is 0 to within FP tolerance
            member inline this.IsZero = abs this.Length < XYPos.epsilon
    
    /// Add absolute vertices to a segment
    type ASegment = {
            Start: XYPos
            End: XYPos
            Segment: Segment
        }
        with
            /// get SegmentID id for segment
            member inline this.GetId = this.Segment.Index,this.Segment.WireId
            /// return true if segment length is 0 to within FP tolerance
            member inline this.IsZero = abs this.Segment.Length < XYPos.epsilon

            member inline this.Orientation =
                            let delta = this.Start - this.End
                            if abs delta.X > abs delta.Y then Horizontal else Vertical

    
    type Wire =
        {
            WId: ConnectionId 
            InputPort: InputPortId
            OutputPort: OutputPortId
            Color: HighLightColor
            Width: int
            Segments: list<Segment>
            StartPos : XYPos
            InitialOrientation : Orientation
        }

    let segments_ = Lens.create (fun m -> m.Segments) (fun s m -> {m with Segments = s})
    let mode_ = Lens.create (fun m -> m.Mode) (fun s m -> {m with Mode = s})
   
    
    /// Defines offsets used to render wire width text
    type TextOffset =
        static member yOffset = 7.
        static member xOffset = 1.
        static member xLeftOffset = 20.
    
    type Model =
        {
            Symbol: SymbolT.Model
            Wires: Map<ConnectionId, Wire>
            CopiedWires: Map<ConnectionId, Wire> 
            SelectedSegment: SegmentId list
            LastMousePos: XYPos
            ErrorWires: list<ConnectionId>
            Notifications: Option<string>
            Type : WireType
            ArrowDisplay: bool
            SnapToNet: bool
        }
    
    //----------------------------Message Type-----------------------------------//
    
    /// BusWire messages: see BusWire.update for more info
    type Msg =
        | Symbol of SymbolT.Msg // record containing messages from Symbol module
        | AddWire of (InputPortId * OutputPortId) // add a new wire between these ports to the model
        | BusWidths
        | CopyWires of list<ConnectionId>
        | DeleteWires of list<ConnectionId>
        | DeleteWiresWithPort of (Port option) list
        | SelectWires of list<ConnectionId>
        | UpdateWires of list<ComponentId> * XYPos
        | UpdateSymbolWires of ComponentId
        | DragSegment of SegmentId list * MouseT
        | CoalesceWire of ConnectionId
        | ColorWires of list<ConnectionId> * HighLightColor
        | ErrorWires of list<ConnectionId>
        | ResetJumps of list<ConnectionId>
        | MakeJumps of separate: bool * conns: list<ConnectionId>
        /// The end of a symbol move: every wire not routed by hand is re-routed from scratch and
        /// the whole sheet separated, so a drag leaves exactly the layout "redraw floating wires"
        /// would - nothing for a redraw to improve. Wires shaped by the moved symbol's OLD position
        /// (an obstacle detour, a branch point) are not connected to it, so re-routing only the
        /// moved symbol's wires left those behind.
        | RerouteAllFloatingWires
        | UpdateWireDisplayType of WireType
        | ToggleArrowDisplay
        | ResetModel // For Issie Integration
        | LoadConnections of list<Connection> // For Issie Integration
        | UpdateConnectedWires of list<ComponentId> // rotate each symbol separately. TODO - rotate as group? Custom comps do not rotate
        | RerouteWire of PortId
        | ToggleSnapToNet

    let symbol_ = Lens.create (fun m -> m.Symbol) (fun w m -> {m with Symbol = w})
    let wires_ = Lens.create (fun m -> m.Wires) (fun w m -> {m with Wires = w})
    let wireOf_ k = wires_ >-> Map.valueForce_ "What? Symbol id lookup in model failed" k
    let symbolOf_ k = symbol_ >-> SymbolT.symbolOf_ k

module SheetT =

    // HLP 23: AUTHOR Khoury & Ismagilov
    // Types needed for scaling box
    type ScalingBox = {
        ScaleButton: SymbolT.Symbol 
        RotateDeg90Button: SymbolT.Symbol 
        RotateDeg270Button: SymbolT.Symbol 
        ScalingBoxBound: BoundingBox
        ButtonList: ComponentId list
    }

  

    /// Used to keep mouse movement (AKA velocity) info as well as position
    type XYPosMov = {
        Pos: XYPos
        Move: XYPos
        }

    let move_ = Lens.create (fun m -> m.Move) (fun w m -> {m with Move = w})
    let pos_ = Lens.create (fun m -> m.Pos) (fun w m -> {m with Pos = w})

    /// Used to keep track of what the mouse is on
    type MouseOn =
        | Label of ComponentId
        | InputPort of InputPortId * XYPos
        | OutputPort of OutputPortId * XYPos
        | Component of ComponentId
        | Connection of ConnectionId
        | ComponentCorner of ComponentId * XYPos * int
        | Canvas

    /// What a right-click has made draggable on one custom component.
    ///
    /// These used to be reached by holding Ctrl, which meant the affordance appeared on every
    /// custom component at once, vanished if the key was released or the window lost focus, and
    /// was undiscoverable. A mode entered from the component's own menu applies to that component
    /// alone and stays until the user clicks away.
    type SymbolEditMode =
        /// drag a port along the symbol's edges
        | EditPorts
        /// drag a corner to resize the symbol
        | EditSize

    /// Keeps track of the current action that the user is doing
    type CurrentAction =
        | Selecting
        | InitialiseMoving of ComponentId // In case user clicks on a component and never drags the mouse then we'll have saved the component that the user clicked on to reset any multi-selection to that component only.
        | InitialiseMovingLabel of ComponentId
        | MovingSymbols
        | MovingLabel
        | DragAndDrop
        | Panning of offset: XYPos // panning sheet using shift/drag, offset = (initials) ScreenScrollPos + (initial) ScreenPage
        | MovingWire of SegmentId list // Sends mouse messages on to BusWire
        | ConnectingInput of InputPortId // When trying to connect a wire from an input
        | ConnectingOutput of OutputPortId // When trying to connect a wire from an output
        | Scrolling // For Automatic Scrolling by moving mouse to edge to screen
        | Idle
        | Scaling
        // ------------------------------ Issie Actions ---------------------------- //
        // (ComponentId -> Unit) is function used to add created component to parameter slots
        | InitialisedCreateComponent of LoadedComponent list * ComponentType * string * (ComponentId -> Unit) option
        | MovingPort of portId: PortId//?? should it have the port id?
        | ResizingSymbol of ComponentId * XYPos

    type UndoAction =
        | MoveBackSymbol of ComponentId List * XYPos
        | UndoPaste of ComponentId list





    /// For Keyboard messages.
    /// These name draw block *actions*, not keys - KeyTypes decides which chord reaches which.
    /// AltC, AltV, AltZ and AltShiftZ used to be here with no dispatcher, and CtrlS with a handler
    /// that added a square for a demo.
    type KeyboardMsg =
        | CtrlC | CtrlV | CtrlZ | CtrlY | CtrlA | CtrlW | ZoomIn | ZoomOut | DEL | ESC

    type WireTypeMsg =
        | Jump | Radiussed | Modern

    type IssieInterfaceMsg =
        | ToggleArrows

    /// Possible fields that may (or may not) be used in a dialog popup.
    type PopupDialogData = {
        Text : string option;
        Int : int option;
        Int2: bigint option
    }

    type Arrange = | AlignSymbols | DistributeSymbols

    /// Which way the copies of a pasted array run. A fragment is arrayed across its short axis, so
    /// that the copies stack up beside each other rather than end to end: a row of gates laid out
    /// left to right becomes a vertical array, and a column of them a horizontal one.
    ///
    /// Prefixed because CommonTypes.Orientation is Horizontal | Vertical and is used unqualified
    /// all over the draw block - two of these cases named the same would shadow it.
    type ArrayDirection = | ArrayVertical | ArrayHorizontal

    type CompilationStage =
        | Completed of int
        | InProgress of int
        | Failed
        | Queued
    
    type CompilationStageLabel =
        | Synthesis
        | PlaceAndRoute
        | Generate
        | Upload

    type CompileStatus = {
        Synthesis: CompilationStage;
        PlaceAndRoute: CompilationStage;
        Generate: CompilationStage;
        Upload: CompilationStage;
    }

    type Msg =
        | Wire of BusWireT.Msg
        | SetWireModel of BusWireT.Model
        | KeyPress of KeyboardMsg
        | ToggleGrid
        | KeepZoomCentered of XYPos
        | ApplyPendingZoomCenter
        | PreciseZoom of float
        | MouseMsg of MouseT
        | MouseMsgOrig of Browser.Types.MouseEvent * MouseOp * float
        | UpdateBoundingBoxes
        | UpdateSingleBoundingBox of ComponentId
        | UpdateScrollPos of XYPos
        | UpdateScrollPosFromCanvas of XYPos
        | AddNotConnected of (LoadedComponent list) * port:Port * pos:XYPos * rotation:Rotation
        | CheckAutomaticScrolling
        | DoNothing
        // ------------------- Popup Dialog Management Messages----------------------//
        | ShowPopup of ((Msg -> Unit) -> PopupDialogData -> ReactElement)
        | ClosePopup
        | SetPopupDialogText of string option
        | SetPopupDialogInt of int option
        // ------------------- Issie Interface Messages ----------------------
        // Need to initialise for drag-and-drop
        // (ComponentId -> Unit) is function used to add created component to parameter slots
        | InitialiseCreateComponent of LoadedComponent list * ComponentType * string * (ComponentId -> Unit) option
        | FlushCommandStack
        | ResetModel
        | UpdateSelectedWires of ConnectionId list * bool
        | ColourSelection of compIds : ComponentId list * connIds : ConnectionId list * colour : HighLightColor
        | PortMovementStart
        | PortMovementEnd
        | PanModeStart
        | PanModeEnd
        | ResetSelection
        | ToggleNet of CanvasState //This message does nothing in sheet, but will be picked up by the update function
        | SelectWires of ConnectionId list
        | SetSpinner of bool
        | Rotate of Rotation
        | Flip of SymbolT.FlipType
        | Arrangement of Arrange
        /// Paste what was copied as an array of `copies` copies stepped along `direction`, the
        /// labels of each suffixed with its number counting from `firstSuffix`. Placed like an
        /// ordinary paste: it arrives following the mouse.
        | PasteArray of direction: ArrayDirection * copies: int * firstSuffix: int
        | RotateLabels
        | WireType of WireTypeMsg
        | IssieInterface of IssieInterfaceMsg
        | MovePort of MouseT //different from mousemsg because ctrl pressed too
        | SaveSymbols
        // ------------------- Compilation and Debugging ----------------------
        | StartCompiling of path: string * name: string * profile: Verilog.CompilationProfile
        | StartCompilationStage of CompilationStageLabel * path: string * name: string * profile: Verilog.CompilationProfile
        | StopCompilation
        | TickCompilation of string
        | FinishedCompilationStage
        | DebugSingleStep
        | DebugStepAndRead of parts: int 
        | DebugRead of parts: int 
        | OnDebugRead of data: int * viewer: int
        | DebugConnect
        | DebugDisconnect
        | DebugUpdateMapping of string array
        | DebugContinue
        | DebugPause
        | SetDebugDevice of string
        | ReorderPorts
        | TestSmartChannel
        | TestPortPosition
        | ToggleSnapToNet
        | BeautifySheet
        | SheetBatch of Msg list
        | ExecFuncInSheetMessage of (SheetT.Model -> Unit)

    type ReadLog = | ReadLog of int

    type DebugState =
        | NotDebugging
        | Paused
        | Running
    
    type ScalingDirection = ScaleUp | ScaleDown

    type Model = {
        Wire: BusWireT.Model
        // function to create popup pane if present
        PopupViewFunc : ((Msg -> Unit) -> PopupDialogData -> Fable.React.ReactElement) option
        // data to populate popup (may not all be used)
        PopupDialogData : PopupDialogData
        BoundingBoxes: Map<ComponentId, BoundingBox>
        LastValidBoundingBoxes: Map<ComponentId, BoundingBox>
        SelectedLabel: ComponentId option
        SelectedComponents: ComponentId List
        SelectedWires: ConnectionId list
        NearbyComponents: ComponentId list
        /// The wire the mouse is resting on, when it is resting on one and nothing is being
        /// dragged. Recorded so that the value it carries can be shown beside the cursor while a
        /// simulation is running - which is the draw block's only interest in it: the value itself
        /// is worked out by the UI layer, which is the only part that knows a simulation exists.
        HoveredWire: ConnectionId option
        ErrorComponents: ComponentId list
        DragToSelectBox: BoundingBox
        ConnectPortsLine: XYPos * XYPos // Visual indicator for connecting ports, defines two vertices to draw a line in-between.
        /// PortId 0 = none. Keeps track of if a target port has been found for connecting two wires in-between.
        TargetPortId: PortId
        Action: CurrentAction
        ShowGrid: bool // Always true at the moment, kept in-case we want an optional grid
        //Theme: ThemeType
        CursorType: CursorType
        LastCursorType: CursorType
        LastValidPos: XYPos
        // HLP23 AUTHOR: BRYAN TAN
        LastValidSymbol: SymbolT.Symbol option
        /// The custom component whose ports or size the user is currently able to drag, and which
        /// of the two. Entered from that component's right-click menu and left until the user
        /// clicks away, so the affordance stays put rather than depending on a key being held.
        /// None is the ordinary state, which is why there is no case for it: a mode without a
        /// symbol to apply to would mean nothing.
        SymbolEdit: (ComponentId * SymbolEditMode) option
        SnapSymbols: SnapXY
        SnapSegments: SnapXY
        /// how X,Y coordinates throughout draw block are scaled into screen pixels.
        /// All unscaled dimensions (screen pixels) have Screen prepended to name.
        Zoom: float
        /// the size of teh canvas in DrawBlock units
        CanvasSize: float // how large is the circuit canvas - can be changed dynamically
        TmpModel: Model Option // Stored for Redo/Undo
        ScalingTmpModel: Model Option // Stored to save expandable scaling model
        UndoList: Model List
        RedoList: Model List
        AutomaticScrolling: bool // True if mouse is near the edge of the screen and is currently scrolling. This improved performance for manual scrolling with mouse wheel (don't check for automatic scrolling if there is no reason to)
        /// html scrolling position: this is in screen pixels, draw block X,Y values are 1/model.Zoom of this
        ScreenScrollPos: XYPos // copies HTML canvas scrolling position: (canvas.scrollLeft,canvas.scrollTop)
        /// Screen centre to restore after React has committed a zoom resize.
        PendingZoomCenter: XYPos option
        /// this is Drawblock X,Y values
        LastMousePos: XYPos // For Symbol Movement
        ScalingBoxCentrePos: XYPos
        InitMouseToScalingBoxCentre: XYPos
        ScrollingLastMousePos: XYPosMov // For keeping track of mouse movement when scrolling. Can't use LastMousePos as it's used for moving symbols (won't be able to move and scroll symbols at same time)
        LastMousePosForSnap: XYPos
        MouseCounter: int
        CtrlKeyDown : bool
        /// Whether the space bar is held. Space+drag pans the schematic, as it does in every
        /// drawing application; Shift+drag does the same and is kept. Held state rather than a
        /// chord, so it is tracked beside CtrlKeyDown rather than in the shortcut table.
        SpaceKeyDown : bool
        PrevWireSelection : ConnectionId list
        /// Whether to offer "paste array" on the canvas: something has been copied and not yet
        /// pasted. The feature is worth surfacing at the moment it applies - between copying and
        /// pasting - and not worth a permanent button, so the offer is withdrawn once a paste of
        /// either kind has happened. The Edit menu keeps it available either way.
        OfferPasteArray : bool
        ScalingBox: ScalingBox Option
        Compiling: bool
        CompilationStatus: CompileStatus
        /// The FPGA build running now, named by the id main gave it. This used to be the
        /// ChildProcess itself, which a renderer without node integration has no way to hold - main
        /// owns the process and answers questions about it by id.
        CompilationJob: string option
        DebugState: DebugState
        DebugData: int list
        DebugMappings: string array
        DebugIsConnected: bool
        DebugDevice: string option
        }
    
    let wire_ = Lens.create (fun m -> m.Wire) (fun w m -> {m with Wire = w})
    let selectedComponents_ = Lens.create (fun m -> m.SelectedComponents) (fun sc m -> {m with SelectedComponents = sc})
    let selectedWires_ = Lens.create (fun m -> m.SelectedWires) (fun sw m -> {m with SelectedWires = sw})
    let boundingBoxes_ = Lens.create (fun m -> m.BoundingBoxes) (fun bb m -> {m with BoundingBoxes = bb})
    // let Action_ = Lens.create (fun m -> m.Action) (fun act m -> {m with Action = act})
    // let tmpModel_ = Lens.create (fun m -> m.TmpModel) (fun tmp m -> {m with TmpModel = tmp})

    let wires_ = wire_ >-> BusWireT.wires_
    let wireOf_ k = wires_ >-> Map.valueForce_ "What? Wire id lookup in model failed" k
    let symbol_ = wire_ >-> BusWireT.symbol_
    let symbols_ = wire_ >-> BusWireT.symbol_ >-> SymbolT.symbols_
    let symbolOf_ k = symbol_ >-> SymbolT.symbolOf_ k
    let scrollingLastMousePos_ = Lens.create (fun m -> m.ScrollingLastMousePos) (fun w m -> {m with ScrollingLastMousePos = w})
    let lastMousePos_ = Lens.create (fun m -> m.LastMousePos) (fun w m -> {m with LastMousePos = w})
    let screenScrollPos_ = Lens.create (fun m -> m.ScreenScrollPos) (fun w m -> {m with ScreenScrollPos = w})
    let lastMousePosForSnap_ = Lens.create (fun m -> m.LastMousePosForSnap) (fun w m -> {m with LastMousePosForSnap = w})
    let canvasSize_ = Lens.create (fun m -> m.CanvasSize) (fun w m -> {m with CanvasSize = w})

    let zoom_ = Lens.create (fun m -> m.Zoom) (fun w m -> {m with Zoom = w})

    let scalingBox_ = Lens.create (fun m -> m.ScalingBox) (fun w m -> {m with ScalingBox = w})
    let symbolEdit_ = Lens.create (fun m -> m.SymbolEdit) (fun w m -> {m with SymbolEdit = w})
