module DrawModelType


open CommonTypes
open DrawHelpers
open Fable.React
open Fable.React.Props
open Elmish

//--------------------------COMMON TYPES------------------------------//

type SnapData = {
    UpperLimit: float
    LowerLimit: float
    Snap: float
    }

type Snap = {
    UnSnapPosition: float
    SnapPosition: float
}

type SnapInfo = {
    SnapData: SnapData array
    SnapOpt: Snap option
    }

type SnapXY = {SnapX: SnapInfo; SnapY: SnapInfo}


/// ---------- SYMBOL TYPES ----------
module SymbolT =

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

    /// data here changes how the symbol looks but has no other effect
    type AppearanceT =
        {
            ShowInputPorts: bool
            ShowOutputPorts: bool
            HighlightLabel: bool
            Colour: string
            Opacity: float       
        }

    /// Represents a symbol, that contains a component and all the other information needed to render
    type Symbol =
        {
            /// Coordinates of the symbol's top left corner
            Pos: XYPos
        
            /// Width of the input port 0
            InWidth0: int option

            /// Width of the output port 1
            InWidth1: int option

            LabelBoundingBox: BoundingBox
            LabelHasDefaultPos: bool
        

            Appearance: AppearanceT

            Id : ComponentId       
            Component : Component                 

            Moving: bool
            IsClocked: bool
            STransform: STransform

            PortMaps : PortMaps

            /// Option to represent a port that is being moved, if it's some, it contains the moving port's Id and its current position.
            MovingPort: Option<{|PortId:string; CurrPos: XYPos|}>

        }

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
        | SelectSymbols of ComponentId list// Issie interface
        | SymbolsHaveError of sIds: ComponentId list
        | ChangeLabel of sId : ComponentId * newLabel : string
        | PasteSymbols of sIds: ComponentId list
        | ColorSymbols of compList : ComponentId list * colour : HighLightColor
        | ErrorSymbols of errorIds: ComponentId list * selectIds: ComponentId list * isDragAndDrop: bool
        | ChangeNumberOfBits of compId:ComponentId * NewBits:int 
        | ChangeLsb of compId: ComponentId * NewBits:int64 
        | ChangeConstant of compId: ComponentId * NewBits:int64 * NewText:string
        | ResetModel // For Issie Integration
        | LoadComponents of  LoadedComponent list * Component list // For Issie Integration
        | WriteMemoryLine of ComponentId * int64 * int64 // For Issie Integration 
        | WriteMemoryType of ComponentId * ComponentType
        | RotateLeft of compList : ComponentId list * RotationType
        | Flip of compList: ComponentId list * orientation: FlipType
        | MovePort of portId: string * move: XYPos
        | MovePortDone of portId: string * move: XYPos
        | SaveSymbols
             //------------------------Sheet interface message----------------------------//
        | UpdateBoundingBoxes


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
    
        with member this.getId() = this.Index,this.WireId
    
    /// Add absolute vertices to a segment
    type ASegment = {
            Start: XYPos
            End: XYPos
            Segment: Segment
        }
    
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
        }
    
    //----------------------------Message Type-----------------------------------//
    
    /// BusWire messages: see BusWire.update for more info
    type Msg =
        | Symbol of SymbolT.Msg // record containing messages from Symbol module
        | AddWire of (InputPortId * OutputPortId) // add a new wire between these ports to the model
        | BusWidths
        | CopyWires of list<ConnectionId>
        | DeleteWires of list<ConnectionId>
        | SelectWires of list<ConnectionId>
        | UpdateWires of list<ComponentId> * XYPos
        | UpdateSymbolWires of ComponentId
        | DragSegment of SegmentId * MouseT
        | ColorWires of list<ConnectionId> * HighLightColor
        | ErrorWires of list<ConnectionId>
        | ResetJumps of list<ConnectionId>
        | MakeJumps of list<ConnectionId>
        | UpdateWireDisplayType of WireType
        | ResetModel // For Issie Integration
        | LoadConnections of list<Connection> // For Issie Integration
        | UpdateConnectedWires of list<ComponentId> // rotate each symbol separately. TODO - rotate as group? Custom comps do not rotate
        | RerouteWire of string

module SheetT =

    /// Used to keep mouse movement (AKA velocity) info as well as position
    type XYPosMov = {
        Pos: XYPos
        Move: XYPos
        }

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
        | Grab
        | Grabbing
    with
        member this.Text() = 
            match this with
            | Default -> "default"
            | ClickablePort -> "move"
            | NoCursor -> "none"
            | Spinner -> "wait"
            | Grab -> "crosshair"
            | Grabbing -> "grabbing"



    /// For Keyboard messages
    type KeyboardMsg =
        | CtrlS | CtrlC | CtrlV | CtrlZ | CtrlY | CtrlA | CtrlW | AltC | AltV | AltZ | AltShiftZ | ZoomIn | ZoomOut | DEL | ESC 

    type WireTypeMsg =
        | Jump | Radiussed | Modern

    /// Possible fields that may (or may not) be used in a dialog popup.
    type PopupDialogData = {
        Text : string option;
        Int : int option;
        Int2: int64 option
    }


    type Msg =
        | Wire of BusWireT.Msg
        | KeyPress of KeyboardMsg
        | ToggleGrid
        | KeepZoomCentered of XYPos
        | MouseMsg of MouseT
        | UpdateBoundingBoxes
        | UpdateSingleBoundingBox of ComponentId
        | UpdateLabelBoundingBoxes
        | UpdateSingleLabelBoundingBox of ComponentId
        | UpdateScrollPos of X: float * Y: float
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
        | ToggleSelectionOpen
        | ToggleSelectionClose
        | ResetSelection
        | SetWaveSimMode of bool
        | ToggleNet of CanvasState //This message does nothing in sheet, but will be picked up by the update function
        | SelectWires of ConnectionId list
        | SetSpinner of bool
        | Rotate of SymbolT.RotationType
        | Flip of SymbolT.FlipType
        | WireType of WireTypeMsg
        | MovePort of MouseT //different from mousemsg because ctrl pressed too
        | SaveSymbols

    type Model = {
        Wire: BusWireT.Model
        // function to create popup pane if present
        PopupViewFunc : ((Msg -> Unit) -> PopupDialogData -> Fable.React.ReactElement) option
        // data to populate popup (may not all be used)
        PopupDialogData : PopupDialogData
        BoundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>
        LastValidBoundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>
        LabelBoundingBoxes: Map<CommonTypes.ComponentId, BoundingBox>
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
        CursorType: CursorType
        LastValidPos: XYPos
        SnapSymbols: SnapXY
        SnapSegments: SnapXY
        CurrentKeyPresses: Set<string> // For manual key-press checking, e.g. CtrlC
        Zoom: float
        TmpModel: Model Option
        UndoList: Model List
        RedoList: Model List
        AutomaticScrolling: bool // True if mouse is near the edge of the screen and is currently scrolling. This improved performance for manual scrolling with mouse wheel (don't check for automatic scrolling if there is no reason to)
        ScrollPos: XYPos // copies HTML canvas scrolling position: (canvas.scrollLeft,canvas.scrollTop)
        LastMousePos: XYPos // For Symbol Movement
        ScrollingLastMousePos: XYPosMov // For keeping track of mouse movement when scrolling. Can't use LastMousePos as it's used for moving symbols (won't be able to move and scroll symbols at same time)
        LastMousePosForSnap: XYPos
        MouseCounter: int
        Toggle : bool
        IsWaveSim : bool
        PrevWireSelection : ConnectionId list
        }
    

