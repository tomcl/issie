module DrawModelType


open CommonTypes
open DrawHelpers
open Fable.React
open Fable.React.Props
open Elmish
open Optics
open Node.ChildProcess

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
    type FlipType =  FlipHorizontal | FlipVertical
    type RotationType = RotateClockwise | RotateAntiClockwise

    /// Wraps around the input and output port id types
    type PortId = | InputId of InputPortId | OutputId of OutputPortId

    /// data structures defining where ports are put on symbol boundary
    /// strings here are used for port ids
    type PortMaps =
        {     
            /// Maps edge to list of ports on that edge, in correct order
            Order: Map<Edge, string list>
            /// Maps the port ids to which side of the component the port is on
            Orientation: Map<string, Edge>
        }

    let order_ = Lens.create (fun a -> a.Order) (fun s a -> {a with Order = s})
    let orientation_ = Lens.create (fun a -> a.Orientation) (fun s a -> {a with Orientation = s})

    /// data here changes how the symbol looks but has no other effect
    type ShowPorts = | ShowInput | ShowOutput | ShowBoth | ShowBothForPortMovement | ShowNone | ShowOneTouching of Port | ShowOneNotTouching of Port | ShowTarget  
    
    type AppearanceT =
        {
            // During various operations the ports on a symbol (input, output, or both types)
            // are highlighted as circles. This D.U. controls that.
            ShowPorts: ShowPorts
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
    let highlightLabel_ = Lens.create (fun a -> a.HighlightLabel) (fun s a -> {a with HighlightLabel = s})
    let colour_ = Lens.create (fun a -> a.Colour) (fun s a -> {a with Colour = s})
    let opacity_ = Lens.create (fun a -> a.Opacity) (fun s a -> {a with Opacity = s})


    /// Represents a symbol, that contains a component and all the other information needed to render it
    type Symbol =
        {
            /// Coordinates of the symbol's top left corner
            /// on component save/restore this is also X,Y on Component type.
            Pos: XYPos
        
            /// Width of the wires connected to input ports 0 & 1
            /// This is needed on the symbol only for  bus splitter and bus merge symbols
            /// These display the bit numbers of their connections.
            /// wire widths are calulated by width inference so these values are transient and
            /// need not be stored.
            InWidth0: int option
            InWidth1: int option

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
            /// NB HScale, VScale modify H,
            Component : Component  
            
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
            MovingPort: Option<{|PortId:string; CurrPos: XYPos|}>
            /// dynamic info used in port move operation
            MovingPortTarget: (XYPos*XYPos) option

        }

    let appearance_ = Lens.create (fun a -> a.Appearance) (fun s a -> {a with Appearance = s})
    let portMaps_ = Lens.create (fun a -> a.PortMaps) (fun s a -> {a with PortMaps = s})
    let movingPort_ = Lens.create (fun a -> a.MovingPort) (fun s a -> {a with MovingPort = s})
    let movingPortTarget_ = Lens.create (fun a -> a.MovingPortTarget) (fun s a -> {a with MovingPortTarget = s})
    let component_ = Lens.create (fun a -> a.Component) (fun s a -> {a with Component = s})


    /// Represents all the symbols and ports on the sheet
    type Model = {
        Symbols: Map<ComponentId, Symbol>

        /// All the symbols currently on the clipboard
        CopiedSymbols: Map<ComponentId, Symbol>

        /// Contains all the input and output ports in the model (currently rendered)
        Ports: Map<string, Port>

        /// Contains all the inputports that have a wire connected to them.
        /// If a port is in the set, it is connected, otherwise it is not
        InputPortsConnected:  Set<InputPortId>

        /// Represents the number of wires connected to each output port in the model
        OutputPortsConnected: Map<OutputPortId, int>

        Theme: ThemeType
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
        | ChangeLsb of compId: ComponentId * NewBits:int64 
        | ChangeInputValue of compId: ComponentId * newVal: int
        | ChangeScale of compId:ComponentId * newScale:float * whichScale:ScaleAdjustment
        | ChangeConstant of compId: ComponentId * NewBits:int64 * NewText:string
        | ChangeBusCompare of compId: ComponentId * NewBits:uint32 * NewText:string
        | ChangeReversedInputs of compId: ComponentId
        | ChangeAdderComponent of compId: ComponentId * oldComp: Component * newComp: ComponentType
        | ChangeCounterComponent of compId: ComponentId * oldComp: Component * newComp: ComponentType
        | ResetModel // For Issie Integration
        | LoadComponents of  LoadedComponent list * Component list // For Issie Integration
        | WriteMemoryLine of ComponentId * int64 * int64 // For Issie Integration 
        | WriteMemoryType of ComponentId * ComponentType
        | UpdateMemory of ComponentId * (Memory1 -> Memory1)
        | RotateLeft of compList : ComponentId list * RotationType
        | Flip of compList: ComponentId list * orientation: FlipType
        /// Taking the input and..
        | MovePort of portId: string * move: XYPos
        | MovePortDone of portId: string * move: XYPos
        | SaveSymbols
        | SetTheme of ThemeType
             //------------------------Sheet interface message----------------------------//
        | UpdateBoundingBoxes

    
    let symbols_ = Lens.create (fun m -> m.Symbols) (fun s m -> {m with Symbols = s})
    let ports_ = Lens.create (fun m -> m.Ports) (fun w m -> {m with Ports = w})
    let symbolOf_ k = symbols_ >-> Map.valueForce_ "What? Symbol id lookup in model failed" k


        //------------------------------------------------------------------------//
    //------------------------------BusWire Types-----------------------------//
    //------------------------------------------------------------------------//
    
module BusWireT =

    type Orientation = | Vertical | Horizontal
    
    ///
    type SnapPosition = High | Mid | Low
    
    /// Represents how wires are rendered
    type WireType = Radial | Modern | Jump
    
    /// Represents how a wire segment is currently being routed
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
            member inline this.GetId() = this.Index,this.WireId
            /// return true if segment length is 0 to within FP tolerance
            member inline this.IsZero() = abs this.Length < XYPos.epsilon
    
    /// Add absolute vertices to a segment
    type ASegment = {
            Start: XYPos
            End: XYPos
            Segment: Segment
        }
        with
            /// get SegmentID id for segment
            member inline this.GetId() = this.Segment.Index,this.Segment.WireId
            /// return true if segment length is 0 to within FP tolerance
            member inline this.IsZero() = abs this.Segment.Length < XYPos.epsilon

    
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
            SelectedSegment: SegmentId option
            LastMousePos: XYPos
            ErrorWires: list<ConnectionId>
            Notifications: Option<string>
            Type : WireType
            ArrowDisplay: bool
        }
    
    //----------------------------Message Type-----------------------------------//
    
    /// BusWire messages: see BusWire.update for more info
    type Msg =
        | Symbol of SymbolT.Msg // record containing messages from Symbol module
        | AddWire of (InputPortId * OutputPortId) // add a new wire between these ports to the model
        | BusWidths
        | CopyWires of list<ConnectionId>
        | DeleteWires of list<ConnectionId>
        | DeleteWiresOnPort of (Port option) list
        | SelectWires of list<ConnectionId>
        | UpdateWires of list<ComponentId> * XYPos
        | UpdateSymbolWires of ComponentId
        | DragSegment of SegmentId * MouseT
        | CoalesceWire of ConnectionId
        | ColorWires of list<ConnectionId> * HighLightColor
        | ErrorWires of list<ConnectionId>
        | ResetJumps of list<ConnectionId>
        | MakeJumps of list<ConnectionId>
        | UpdateWireDisplayType of WireType
        | ToggleArrowDisplay
        | ResetModel // For Issie Integration
        | LoadConnections of list<Connection> // For Issie Integration
        | UpdateConnectedWires of list<ComponentId> // rotate each symbol separately. TODO - rotate as group? Custom comps do not rotate
        | RerouteWire of string

    open Optics
    open Operators
    let symbol_ = Lens.create (fun m -> m.Symbol) (fun w m -> {m with Symbol = w})
    let wires_ = Lens.create (fun m -> m.Wires) (fun w m -> {m with Wires = w})
    let wireOf_ k = wires_ >-> Map.valueForce_ "What? Symbol id lookup in model failed" k
    let symbolOf_ k = symbol_ >-> SymbolT.symbolOf_ k

module SheetT =

    /// Used to keep mouse movement (AKA velocity) info as well as position
    type XYPosMov = {
        Pos: XYPos
        Move: XYPos
        }

    let move_ = Lens.create (fun m -> m.Move) (fun w m -> {m with Move = w})
    let pos_ = Lens.create (fun m -> m.Pos) (fun w m -> {m with Pos = w})

    /// Used to keep track of what the mouse is on
    type MouseOn =
        | Label of CommonTypes.ComponentId
        | InputPort of CommonTypes.InputPortId * XYPos
        | OutputPort of CommonTypes.OutputPortId * XYPos
        | Component of CommonTypes.ComponentId
        | Connection of CommonTypes.ConnectionId
        | Canvas

    /// Keeps track of the current action that the user is doing
    type CurrentAction =
        | Selecting
        | InitialiseMoving of CommonTypes.ComponentId // In case user clicks on a component and never drags the mouse then we'll have saved the component that the user clicked on to reset any multi-selection to that component only.
        | InitialiseMovingLabel of CommonTypes.ComponentId
        | MovingSymbols
        | MovingLabel
        | DragAndDrop
        | Rotating
        | Panning of offset: XYPos // panning sheet using shift/drag, offset = (initials) ScreenScrollPos + (initial) ScreenPage
        | MovingWire of SegmentId // Sends mouse messages on to BusWire
        | ConnectingInput of CommonTypes.InputPortId // When trying to connect a wire from an input
        | ConnectingOutput of CommonTypes.OutputPortId // When trying to connect a wire from an output
        | Scrolling // For Automatic Scrolling by moving mouse to edge to screen
        | Idle
        // ------------------------------ Issie Actions ---------------------------- //
        | InitialisedCreateComponent of LoadedComponent list * ComponentType * string
        | MovingPort of portId: string//?? should it have the port id?

    type UndoAction =
        | MoveBackSymbol of CommonTypes.ComponentId List * XYPos
        | UndoPaste of CommonTypes.ComponentId list



    /// Keeps track of what cursor to show
    type CursorType =
        | Default
        | ClickablePort
        | NoCursor
        | Spinner
        | GrabWire
        | GrabLabel
        | GrabSymbol
        | Grabbing
    with
        member this.Text() = 
            match this with
            | Default -> "default"
            | ClickablePort -> "move"
            | NoCursor -> "none"
            | Spinner -> "wait"
            | GrabWire -> "crosshair"
            | GrabSymbol -> "cell"
            | GrabLabel -> "grab"
            | Grabbing -> "grabbing"



    /// For Keyboard messages
    type KeyboardMsg =
        | CtrlS | CtrlC | CtrlV | CtrlZ | CtrlY | CtrlA | CtrlW | AltC | AltV | AltZ | AltShiftZ | ZoomIn | ZoomOut | DEL | ESC

    type WireTypeMsg =
        | Jump | Radiussed | Modern

    type IssieInterfaceMsg =
        | ToggleArrows

    /// Possible fields that may (or may not) be used in a dialog popup.
    type PopupDialogData = {
        Text : string option;
        Int : int option;
        Int2: int64 option
    }

    type Arrange = | AlignSymbols | DistributeSymbols

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
        | KeyPress of KeyboardMsg
        | ToggleGrid
        | KeepZoomCentered of XYPos
        | MouseMsg of MouseT
        | UpdateBoundingBoxes
        | UpdateSingleBoundingBox of ComponentId
        | UpdateScrollPos of XYPos
        | UpdateScrollPosFromCanvas of dispatch: ( Msg -> Unit)
        | ManualKeyUp of string // For manual key-press checking, e.g. CtrlC
        | ManualKeyDown of string // For manual key-press checking, e.g. CtrlC
        | CheckAutomaticScrolling
        | DoNothing
        // ------------------- Popup Dialog Management Messages----------------------//
        | ShowPopup of ((Msg -> Unit) -> PopupDialogData -> ReactElement)
        | ClosePopup
        | SetPopupDialogText of string option
        | SetPopupDialogInt of int option
        // ------------------- Issie Interface Messages ----------------------
        | InitialiseCreateComponent of LoadedComponent list * ComponentType * string // Need to initialise for drag-and-drop
        | FlushCommandStack
        | ResetModel
        | UpdateSelectedWires of ConnectionId list * bool
        | ColourSelection of compIds : ComponentId list * connIds : ConnectionId list * colour : HighLightColor
        | PortMovementStart
        | PortMovementEnd
        | ResetSelection
        | ToggleNet of CanvasState //This message does nothing in sheet, but will be picked up by the update function
        | SelectWires of ConnectionId list
        | SetSpinner of bool
        | Rotate of SymbolT.RotationType
        | Flip of SymbolT.FlipType
        | Arrangement of Arrange
        | RotateLabels
        | WireType of WireTypeMsg
        | IssieInterface of IssieInterfaceMsg
        | MovePort of MouseT //different from mousemsg because ctrl pressed too
        | SaveSymbols
        // ------------------- Compilation and Debugging ----------------------
        | StartCompiling of path: string * name: string * profile: Verilog.CompilationProfile
        | StartCompilationStage of CompilationStageLabel * path: string * name: string * profile: Verilog.CompilationProfile
        | StopCompilation
        | TickCompilation of float
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
        | TestPortReorder
        | TestSmartChannel
        | TestPortPosition


    type ReadLog = | ReadLog of int

    type DebugState =
        | NotDebugging
        | Paused
        | Running

    type Model = {
        Wire: BusWireT.Model
        // function to create popup pane if present
        PopupViewFunc : ((Msg -> Unit) -> PopupDialogData -> Fable.React.ReactElement) option
        // data to populate popup (may not all be used)
        PopupDialogData : PopupDialogData
        BoundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>
        LastValidBoundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>
        SelectedLabel: CommonTypes.ComponentId option
        SelectedComponents: CommonTypes.ComponentId List
        SelectedWires: CommonTypes.ConnectionId list
        NearbyComponents: CommonTypes.ComponentId list
        ErrorComponents: CommonTypes.ComponentId list
        DragToSelectBox: BoundingBox
        ConnectPortsLine: XYPos * XYPos // Visual indicator for connecting ports, defines two vertices to draw a line in-between.
        TargetPortId: string // Keeps track of if a target port has been found for connecting two wires in-between.
        Action: CurrentAction
        ShowGrid: bool // Always true at the moment, kept in-case we want an optional grid
        //Theme: ThemeType
        CursorType: CursorType
        LastValidPos: XYPos
        SnapSymbols: SnapXY
        SnapSegments: SnapXY
        CurrentKeyPresses: Set<string> // For manual key-press checking, e.g. CtrlC
        /// how X,Y coordinates throughout draw block are scaled into screen pixels.
        /// All unscaled dimensions (screen pixels) have Screen prepended to name.
        Zoom: float
        /// the size of teh canvas in DrawBlock units
        CanvasSize: float // how large is the circuit canvas - can be changed dynamically
        TmpModel: Model Option
        UndoList: Model List
        RedoList: Model List
        AutomaticScrolling: bool // True if mouse is near the edge of the screen and is currently scrolling. This improved performance for manual scrolling with mouse wheel (don't check for automatic scrolling if there is no reason to)
        /// html scrolling position: this is in screen pixels, draw block X,Y values are 1/model.Zoom of this
        ScreenScrollPos: XYPos // copies HTML canvas scrolling position: (canvas.scrollLeft,canvas.scrollTop)
        /// this is Drawblock X,Y values
        LastMousePos: XYPos // For Symbol Movement
        ScrollingLastMousePos: XYPosMov // For keeping track of mouse movement when scrolling. Can't use LastMousePos as it's used for moving symbols (won't be able to move and scroll symbols at same time)
        LastMousePosForSnap: XYPos
        MouseCounter: int
        CtrlKeyDown : bool
        ScrollUpdateIsOutstanding: bool
        PrevWireSelection : ConnectionId list
        Compiling: bool
        CompilationStatus: CompileStatus
        CompilationProcess: ChildProcess option
        DebugState: DebugState
        DebugData: int list
        DebugMappings: string array
        DebugIsConnected: bool
        DebugDevice: string option
        }
    
    open Operators
    let wire_ = Lens.create (fun m -> m.Wire) (fun w m -> {m with Wire = w})
    let selectedComponents_ = Lens.create (fun m -> m.SelectedComponents) (fun sc m -> {m with SelectedComponents = sc})
    let selectedWires_ = Lens.create (fun m -> m.SelectedWires) (fun sw m -> {m with SelectedWires = sw})
    let boundingBoxes_ = Lens.create (fun m -> m.BoundingBoxes) (fun bb m -> {m with BoundingBoxes = bb})

    let wires_ = wire_ >-> BusWireT.wires_
    let symbol_ = wire_ >-> BusWireT.symbol_
    let symbols_ = wire_ >-> BusWireT.symbol_ >-> SymbolT.symbols_
    let symbolOf_ k = symbol_ >-> SymbolT.symbolOf_ k

    let scrollingLastMousePos_ = Lens.create (fun m -> m.ScrollingLastMousePos) (fun w m -> {m with ScrollingLastMousePos = w})
    let lastMousePos_ = Lens.create (fun m -> m.LastMousePos) (fun w m -> {m with LastMousePos = w})
    let screenScrollPos_ = Lens.create (fun m -> m.ScreenScrollPos) (fun w m -> {m with ScreenScrollPos = w})
    let lastMousePosForSnap_ = Lens.create (fun m -> m.LastMousePosForSnap) (fun w m -> {m with LastMousePosForSnap = w})
    let canvasSize_ = Lens.create (fun m -> m.CanvasSize) (fun w m -> {m with CanvasSize = w})


