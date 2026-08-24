(*
These are types used throughout the application
*)

module CommonTypes

module Constants =
    let equalityCheckTolerance = 0.0001
    let labelPosTolerance = 0.00001
    /// Max width of an Issie bus. There is no real need for any restriction,
    /// since all code is bigint based, but this is a reasonable limit.
    /// There are performance & UI issues for very large busses.
    ///
    /// Here rather than in NumberHelpers, which is compiled after ComponentSlots: the bound a
    /// width must satisfy is part of what a slot IS, and ComponentSlots.constraintsFor is where
    /// that now lives. NumberHelpers.Constants.maxIssieBusWidth aliases this one.
    let maxIssieBusWidth = 16384


open Fable.Core               
open Optics
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

/// Position on SVG canvas
/// Positions can be added, subtracted, scaled using overloaded +,-, *  operators
/// currently these custom operators are not used in Issie - they should be!
type XYPos =
    {
        X : float
        Y : float
    }

    static member inline zero: XYPos = {X=0.; Y=0.}
    
    /// allowed tolerance when comparing positions with floating point errors for equality
    static member inline epsilon = 0.0000001
    
    /// Add postions as vectors (overlaoded operator)
    static member inline ( + ) (left: XYPos, right: XYPos) =
        { X = left.X + right.X; Y = left.Y + right.Y }
    
    /// Subtract positions as vectors (overloaded operator)
    static member inline ( - ) (left: XYPos, right: XYPos) =
        { X = left.X - right.X; Y = left.Y - right.Y }
    
    /// Scale a position by a number (overloaded operator).
    static member inline ( * ) (pos: XYPos, scaleFactor: float) =
        { X = pos.X*scaleFactor; Y = pos.Y * scaleFactor }
    
    /// Compare positions as vectors. Comparison is approximate so 
    /// it will work even with floating point errors. New infix operator.
    static member inline ( =~ ) (left: XYPos, right: XYPos) =
        abs (left.X - right.X) <= XYPos.epsilon && abs (left.Y - right.Y) <= XYPos.epsilon
    
let inline euclideanDistance (pos1: XYPos) (pos2:XYPos) = 
    let vec = pos1 - pos2
    sqrt(vec.X**2 + vec.Y**2)
    
/// example use of comparison operator: note that F# type inference will not work without at least
/// one of the two operator arguments having a known XYPos type.
let private testXYPosComparison a  (b:XYPos) = 
    a =~ b

/// display XYPos as string nicely for debugging
let pXY ({X=x;Y=y}:XYPos) =
    if max (abs x) (abs y) > 20. then
        $"(%.0f{x},%.0f{y})"
    else
        $"(%.2f{x},%.2f{y})" 


//==========================================//
// Canvas state mapped to f# data structure //
//==========================================//

/// Specify the type of a port in a Component.
type PortType = Input | Output

(*
Note on Ports. Ports are used throughout Issie to represent I/Os of components.
Because a design sheet can be instantiated as a component they can also represent I/Os of a sheet.

1. Port records are used on both connections and components, a connection
    source or target port will have port Id matching that of the port on the
    component it connects to. All ports also specify the componentId of the
    component they are on (HostID).
2. Port records on connections do NOT have port numbers, note this means that connection ports
    cannot be the same as the corresponding component port.
3. Port numbers on components are contiguous from 0 separtely for input
    and output ports.
4. Port numbers must match with the index of the port in the corresponding component
    InputPorts or OutputPorts list
5. For custom components port numbers match index of the port in InputPortNames,OutputPortNames
6. For symbols port numbers determine the vertical order in which ports are displayed.
7. Thus when changing the order of number of I/Os on a custom component port numbers can be changed
    as long as port lists and port name lists are similarly re-ordered.
8. In the simulation port numbers are not relevant for custom comps - connections match port names with the 
    sheet input or output component for the port
9. In the simulation port numbers matter for all other ports: the simulator defines operation based on them.
10.In model.Symbol ports are kept in a single global map, including port numbers. If port numbers are permuted on
    custom components the port numbers in this map must be changed. However this will normally happen since
    model.Symbol symbols and ports are changed at the same time by AddSymbol or deleteSymbol or LoadComponents
    messages.
*)


/// A component I/O.
///
/// Id (like any other Id) is a string generated with 32 random hex charactes,
/// so it is (practically) globally unique. These Ids are used 
/// to uniquely refer to ports and components. They are generated via uuid().
///
/// PortNumber is used to identify which port is which on a component, contiguous from 0
/// separately for inputs and outputs. See comments above type definition for details
///
/// HostId is the unique Id of the component where the port is. For example,
/// all three ports on the same And component will have the same HostId.
type Port = {
    Id : int
    // For example, an And would have input ports 0 and 1, and output port 0.
    // If the port is used in a Connection record as Source or Target, the Number is None. 
    PortNumber : int option
    PortType : PortType 
    HostId : int
}

    
type PortId = | PortId of int

// NB - this.Text() is not currently used.

/// This width is for wire displaying, >8 buswires displayed with 8px thickness. Actual size stored in Port type
type Width = One | Two | Three | Four | Five | Six | Seven | Eight
with
    member this.Text() = // the match statement is used for performance
        match this with
        | One -> "1px"
        | Two -> "2px"
        | Three -> "3px"
        | Four -> "4px"
        | Five -> "5px"
        | Six -> "6px"
        | Seven -> "7px"
        | Eight -> "8px"
            
            
/// Type to specify the origin of a custom component
type CCForm =
    |User
    /// A sheet materialised into the project from a shipped component library. The sheet is named
    /// L<n>_<CompName> so that it cannot clash with a user sheet, and that name is what the user
    /// sees on the canvas and in the waveform simulator; the library and component names are kept
    /// here so the catalogue can show the real name and the origin stays recoverable.
    /// Library sheets are hidden from the Sheets menu, and are reached only by asking to view one
    /// from an instance's right-click menu - which opens it read-only, and only until the project
    /// is closed (Model.OpenedLibrarySheets). They are otherwise ordinary sheets: parameter
    /// analysis, width inference and simulation all see them.
    |Library of LibName: string * CompName: string
    |ProtectedTopLevel
    |ProtectedSubSheet
    |Verilog of string


/// Name identifies the LoadedComponent used.
/// The labels define legends on symbol designating inputs or outputs: and are the names of the Input or Output components of the CC sheet.
/// Label strings are unique per CustomComponent.
/// Label position in list determines inputPortNumber or outputPortNumber of label.
/// Multiple CustomComponent instances are differentiated by Component data.
type CustomComponentType = {
    Name: string
    // Tuples with (label * connection width).
    InputLabels: (string * int) list
    OutputLabels: (string * int) list
    Form : CCForm option
    ParameterBindings: ParameterTypes.ParamBindings option
    Description : string option
}

/// Note that any memory addresses which have not been explicitly set when printing
/// out memory data.
type Memory = {
    // How many bits the address should have.
    // The memory will have 2^AddressWidth memory locations.
    AddressWidth : int 
    // How wide each memory word should be, in bits.
    WordWidth : int
    /// Sparse representation: elements not in Map are assumed zero
    Data : Map<bigint,bigint>
}

   
type InitMemData = 
    | FromData // old method (from data field)
    | FromFile of string // FromFile fName => read a file fName.ram for data
    | ToFile of string // ToFile fName => write data to a file fName.ram
    | ToFileBadName of string // as ToFile but the name does not validate
    | UnsignedMultiplier
    | SignedMultiplier


type Memory1 = {
/// Is the data initialised from a file name.ram in the project directory, or some other way?
Init: InitMemData
/// How many bits the address should have.
/// The memory will have 2^AddressWidth memory locations.
AddressWidth : int 
/// How wide each memory word should be, in bits.
WordWidth : int
/// Sparse represnetation: addresses not in map contain zero
Data : Map<bigint,bigint>
/// Comments written against locations in a .ram file, as "0 10 // what this word is for".
/// Addresses with no comment are absent. Optional so that designs saved before comments
/// existed still load: a missing field reads back as None.
Comments : Map<bigint,string> option
}

    
type ShiftComponentType =
    |LSL
    |LSR
    |ASR

/// Number of bits needed on a shifter's SHIFT input to express every shift
/// amount 0 .. busWidth-1. Must be recomputed whenever the bus width changes.
///
/// This is ceil(log2 busWidth), clamped to 1 so that a one-bit bus still has a shift input. It
/// used to count the bits of busWidth-1 itself; ParameterTypes.clog2 is the same count, exported
/// because `clog2` in a parameter expression must mean exactly what the SHIFT input does. The one
/// difference is at busWidth <= 0, where counting the bits of -1 shifted for ever.
let shifterWidthFor (busWidth: int) =
    max 1 (int (ParameterTypes.clog2 (bigint busWidth)))

[<StringEnum>]
type GateComponentType =
    | And
    | Or
    | Xor
    | Nand
    | Nor
    | Xnor

/// Option of this qualifies NBitsXOr to allow many different components
/// None => Xor
/// TODO to reduce technical debt: 
///     Rename NbitsXor as NBitsCustom, put all the Nbits ops into this D.U.
///     Change catalog entries for all NBits ops to use NBitsCustom, alter load to remain compatibility.
type NBitsArithmetic =
    | Multiply
    //Divide   uncomment or add new cases to implement additional N bit operations. (match warnings will show what must be added)
    //Modulo
    
// Each case contains the data needed to define a digital component of given Type
// Used to read .dgm files, which may contain legacy ComponentType D.U. cases no longer used
// Any NEW case added here must also be added (with identical from) to JSONComponentType
// Cases DELETED here, should be kept in JSONComponentType, with a conversion added to convert the
// deleted case into a case here which still exists.
type ComponentType =
    // Legacy component: to be deleted
    | Input1 of BusWidth: int * DefaultValue: bigint option
    | Output of BusWidth: int
    | Viewer of BusWidth: int
    | IOLabel
    | NotConnected
    | BusCompare1 of BusWidth: int * CompareValue: bigint * DialogTextValue: string
    | BusSelection of OutputWidth: int * OutputLSBit: int
    | Constant1 of Width: int * ConstValue: bigint * DialogTextValue: string
    | Not | Decode4
    | GateN of GateType: GateComponentType * NumInputs: int
    | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8
    | NbitsAdder of BusWidth: int | NbitsAdderNoCin of BusWidth: int 
    | NbitsAdderNoCout of BusWidth: int | NbitsAdderNoCinCout of BusWidth: int 
    | NbitsXor of BusWidth:int * ArithmeticOp: NBitsArithmetic option
    | NbitsAnd of BusWidth: int 
    | NbitsNot of BusWidth: int
    | NbitsOr of BusWidth: int | NbitSpreader of BusWidth: int
    | Custom of CustomComponentType // schematic sheet used as component
    | MergeWires | SplitWire of BusWidth: int // int is bus width
    | MergeN of NumInputs: int
    | SplitN of NumInputs: int * OutputWdiths: int list * OutputLSBits: int list
    // DFFE is a DFF with an enable signal.
    // No initial state for DFF or Register? Default 0.
    | DFF | DFFE | Register of BusWidth: int | RegisterE of BusWidth: int
    | Counter of BusWidth:int | CounterNoLoad of BusWidth:int
    | CounterNoEnable of BusWidth:int | CounterNoEnableLoad of BusWidth:int
    | AsyncROM1 of Memory1 | ROM1 of Memory1 | RAM1 of Memory1 | AsyncRAM1 of Memory1
    // legacy components - to be deleted
    | AsyncROM of Memory | ROM of Memory | RAM of Memory
    | Shift of BusWidth: int * ShifterWidth: int * ShiftType: ShiftComponentType
    // legacy cases to be deleted?
    | BusCompare of BusWidth: int * CompareValue: bigint
    | Input of BusWidth: int
    | Constant of Width: int * ConstValue: bigint 




/// Active pattern which matches 2-input gate component types.
/// NB - NOT gates are not included here.
let (|IsBinaryGate|NotBinaryGate|) cType =
    match cType with
        | GateN (_, n) when n = 2 -> IsBinaryGate
        | _ -> NotBinaryGate
    
let inline isNegated gateType =
    match gateType with
    | Nand | Nor | Xnor -> true
    | And | Or | Xor -> false
    
let (|IsGate|NoGate|) cType =
    match cType with
    | GateN _ -> IsGate
    | _ -> NoGate

/// get memory component type constructor
/// NB only works with new-style memory components
let getMemType (cType: ComponentType) =
    match cType with
    | RAM1 _ -> RAM1
    | AsyncRAM1 _ -> AsyncRAM1
    | ROM1 _ -> ROM1
    | AsyncROM1 _ -> AsyncROM1
    | _ -> failwithf $"Can't get memory type from {cType}"

let (|Memory|_|) (typ:ComponentType) =
    match typ with
    | RAM1 mem 
    | AsyncRAM1 mem
    | ROM1 mem
    | AsyncROM1 mem -> Some mem
    | _ -> None

let (|MemoryAndType|_|) (typ:ComponentType) =
    match typ with
    | RAM1 mem -> Some(RAM1, mem)
    | AsyncRAM1 mem -> Some(AsyncRAM1,mem)
    | ROM1 mem -> Some(ROM1, mem)
    | AsyncROM1 mem -> Some(AsyncROM1, mem)
    | _ -> None


// --------------- Types needed for symbol ---------------- //
/// Represents the rotation of a symbol in degrees, Degree0 is the default symbol rotation.
/// Angle is anticlockwise
   
type Rotation = | Degree0 | Degree90 | Degree180 | Degree270
    
/// Stores the rotation and the flip of the symbol, flipped false by default
type STransform = {Rotation: Rotation; flipped: bool}
    
/// Represents the sides of a component

type Edge =
    | Top
    | Bottom
    | Left
    | Right
        
    /// HLP23: AUTHOR dgs119
    member this.Opposite =
        match this with
        | Top -> Bottom
        | Bottom -> Top
        | Left -> Right
        | _ -> Left

/// Holds possible directions to sort ports.
/// HLP23: AUTHOR dgs119
    
type Direction =
    | Clockwise
    | AntiClockwise

    member this.Opposite =
        match this with
        | Clockwise -> AntiClockwise
        | _ -> Clockwise

type BoundingBox = {
    /// Top left corner of the bounding box
    TopLeft: XYPos
    /// Width
    W: float
    /// Height
    H: float
}
    with member this.Centre() = this.TopLeft + {X=this.W/2.; Y=this.H/2.}


let topLeft_ = Lens.create (fun a -> a.TopLeft) (fun s a -> {a with TopLeft = s})

[<StringEnum>]
type ScaleAdjustment =
    | Horizontal
    | Vertical
    
type SymbolInfo = {
    LabelBoundingBox: BoundingBox option
    LabelRotation: Rotation option
    STransform: STransform
    ReversedInputPorts: bool option
    PortOrientation: Map<int, Edge>
    PortOrder: Map<Edge, int list>
    HScale: float option
    VScale: float option
}



let portOrder_ = Lens.create (fun c -> c.PortOrder) (fun n c -> {c with PortOrder = n})
let portOrientation_ = Lens.create (fun c -> c.PortOrientation) (fun n c -> {c with PortOrientation = n})


let getSTransformWithDefault (infoOpt: SymbolInfo option) =
    match infoOpt with
    | None ->{Rotation=Degree0; flipped=false}
    | Some inf -> inf.STransform

// ---------------------------------------------------------------------------------------------
// The Simple design types: the wire format for sending a design to the dotnet sidecar, before
// parameter resolution and simulation-graph expansion. Deliberately minimal and electrical-only:
// geometry is gone, ports exist only as numbers on connection endpoints, and ids are the dense
// whole-design integers the id reducer (Helpers.RegenerateIds.reduceLoadedComponents) produces -
// so the receiving side can use them as array indices. These types are the CONTRACT, not the
// .NET simulator's working types: that side derives whatever richer structures it wants.
//
// Everything here must stay within what SimpleJsonDotNet.tryDeserialise can read back from the
// vendored SimpleJson encoding: records, DU cases, option, list, bigint, Map (structural keys
// included). No tuples, no floats, no int64/Guid/Set - SimpleDesignTests pins this.
// ---------------------------------------------------------------------------------------------

/// Reduced version of Component: id, type and label are all a simulation needs. A custom
/// component's port names and instance parameter bindings ride inside TypeS
/// (CustomComponentType.InputLabels/OutputLabels/ParameterBindings), where port number n is by
/// definition index n of the label list - so port names are never duplicated here.
type SimpleComponent = {
    /// the reduced id, parsed to int
    CompId : int
    TypeS : ComponentType
    Label : string
}

/// Reduced version of Connection: two endpoints, each a component id and a port NUMBER
/// (output-side number at the source, input-side number at the destination).
type SimpleConnection = {
    /// reduced connection id, kept so results and errors can be mapped back
    ConnId : int
    SrcComp : int
    SrcPort : int
    DestComp : int
    DestPort : int
}

/// One sheet of a design. Sheet IO *order* is not carried (in Issie it derives from component
/// geometry, which is gone): the receiving side recovers ordering from any instance's TypeS
/// labels, and the top sheet needs none.
type SimpleSheet = {
    /// how Custom instances on other sheets name this one
    SheetName: string
    Components : SimpleComponent list
    Connections : SimpleConnection list
    /// the sheet's parameter declarations - LCParameterSlots.DefaultBindings
    DefaultBindings : ParameterTypes.ParamDefinitions
    /// the sheet's parameterised slots - LCParameterSlots.ParamSlots, with every
    /// ParamSlot.CompId being a reduced-int string, so int slot.CompId matches CompId above
    ParamSlots : ParameterTypes.ComponentSlotExpr
}

/// A whole design: the sheets and which one is the top.
type SimpleDesign = {
    TopSheet : string
    Sheets : SimpleSheet list
}


/// JSComponent mapped to F# record.
/// Id uniquely identifies the component within a sheet.
/// Label is optional descriptor displayed on schematic.
type Component = {
    Id : int
    Type : ComponentType
    /// All components have a label that may be empty: label is not unique
    Label : string 
    // position on this list determines inputPortNumber
    InputPorts : Port list 
    /// position in this list determines OutputPortNumber
    OutputPorts : Port list 
    X : float
    Y : float
    /// Height
    H : float
    /// Width
    W : float
    /// Field used only when sheet is saved from Draw Block: Symbol info is copied here
    /// This field is not uptodate when symbol is being edited in Draw Block
    SymbolInfo : SymbolInfo option
    /// Information about parameter expressions that may override conponent slot values
    SlotInfo : ParameterTypes.ComponentSlotExpr option
}

with
    member this.getPort (PortId portId: PortId) = 
        List.tryFind (fun (port:Port) -> port.Id = portId ) (this.InputPorts @ this.OutputPorts)

    /// Equality function for components, includes all geometry except component position
    member c1.isSame(c2: Component) =
        c1.Id = c2.Id && c1.Type = c2.Type && c1.Label = c2.Label && c1.SlotInfo = c2.SlotInfo &&
        c1.InputPorts = c2.InputPorts && c1.OutputPorts = c2.OutputPorts &&
        match c1.SymbolInfo,  c2.SymbolInfo with
        | Some s1, Some s2->
            let dx = c1.X - c2.X
            let dy = c1.Y - c2.Y
            // check if label positions are equal
            let labelPosEq =
                match s1.LabelBoundingBox, s2.LabelBoundingBox with
                | Some l1, Some l2 -> (l1.TopLeft.X - l2.TopLeft.X - dx)**2. + (l1.TopLeft.Y - l2.TopLeft.Y - dy)**2. < Constants.labelPosTolerance
                | None, None -> true
                | _ -> false
            s1.HScale = s2.HScale && s1.VScale = s2.VScale && s1.LabelRotation = s2.LabelRotation &&
            s1.PortOrder = s2.PortOrder &&
            s1.PortOrientation = s2.PortOrientation && s1.ReversedInputPorts = s2.ReversedInputPorts &&
            labelPosEq
        | None, None -> true
        | _ -> false

     
     
let type_ = Lens.create (fun c -> c.Type) (fun n c -> {c with Type = n})
let inputPorts_ = Lens.create (fun c -> c.InputPorts) (fun n c -> {c with InputPorts = n})
let outputPorts_ = Lens.create (fun c -> c.OutputPorts) (fun n c -> {c with OutputPorts = n})
let h_ = Lens.create (fun c -> c.H) (fun n c -> {c with H= n})
let w_ = Lens.create (fun c -> c.W) (fun n c -> {c with W= n})
let slotInfo_ = Lens.create (fun c -> c.SlotInfo) (fun n c -> {c with SlotInfo = n})


/// JSConnection mapped to F# record.
/// Id uniquely identifies connection globally and is used by library.
type Connection = {
    Id : int
    Source : Port
    Target : Port
    Vertices : (float * float * bool) list
}

/// F# data describing the contents of a single schematic sheet.
type CanvasState = Component list * Connection list

    

/// reduced version of CanvasState for electrical comparison, all geometry removed, components ordered
type ReducedCanvasState = | ReducedCanvasState of CanvasState

let unreduced (ReducedCanvasState(rComps,rConns)) = rComps,rConns



//===================================================================================================//
//                                         LEGACY TYPES                                              //
//===================================================================================================//

//------------------------START of ComponentType Conversion------------------------------------------//
//------------------------Used when component efinitions are upgraded

module JSONComponent =

    /// Used only to read/write .dgm files, which may contain legacy ComponentType D.U. cases no longer used
    /// Any NEW case added to ComponentType must also be added here
    /// Cases DELETED from ComponentType should remain here, with a conversion added.
    type ComponentType =
        // Legacy component: to be deleted
        //-----The cases here must be identical, and same order, as the main ComponentType (just copy the code!)----//
        // This allows unboxing to implement JSONComponent.Component <--> Component type conversion
        | Input1 of BusWidth: int * DefaultValue: bigint option
        | Output of BusWidth: int
        | Viewer of BusWidth: int
        | IOLabel
        | NotConnected
        | BusCompare1 of BusWidth: int * CompareValue: bigint * DialogTextValue: string
        | BusSelection of OutputWidth: int * OutputLSBit: int
        | Constant1 of Width: int * ConstValue: bigint * DialogTextValue: string
        | Not | And | Or | Xor | Nand | Nor | Xnor | Decode4
        | GateN of GateType: GateComponentType * NumInputs: int
        | Mux2 | Mux4 | Mux8 | Demux2 | Demux4 | Demux8
        | NbitsAdder of BusWidth: int | NbitsAdderNoCin of BusWidth: int 
        | NbitsAdderNoCout of BusWidth: int | NbitsAdderNoCinCout of BusWidth: int 
        | NbitsXor of BusWidth:int * ArithmeticOp: NBitsArithmetic option
        | NbitsAnd of BusWidth: int 
        | NbitsNot of BusWidth: int
        | NbitsOr of BusWidth: int | NbitSpreader of BusWidth: int
        | Custom of CustomComponentType // schematic sheet used as component
        | MergeWires | SplitWire of BusWidth: int // int is bus width
        | MergeN of NumInputs: int
        | SplitN of NumInputs: int * OutputWdiths: int list * OutputLSBits: int list
        // DFFE is a DFF with an enable signal.
        // No initial state for DFF or Register? Default 0.
        | DFF | DFFE | Register of BusWidth: int | RegisterE of BusWidth: int
        | Counter of BusWidth:int | CounterNoLoad of BusWidth:int
        | CounterNoEnable of BusWidth:int | CounterNoEnableLoad of BusWidth:int
        | AsyncROM1 of Memory1 | ROM1 of Memory1 | RAM1 of Memory1 | AsyncRAM1 of Memory1
        // legacy components - to be deleted
        | AsyncROM of Memory | ROM of Memory | RAM of Memory
        | Shift of BusWidth: int * ShifterWidth: int * ShiftType: ShiftComponentType
        //---------------Legacy cases not in the Issie ComponentType here-------------------//
        | BusCompare of BusWidth: int * CompareValue: bigint
        | Input of BusWidth: int
        | Constant of Width: int * ConstValue: bigint 



    /// The FILE form of a Port. Saved .dgm files hold ids as strings - uuids in old files,
    /// integers written as strings in new ones - while the in-memory types hold integers; the
    /// converters below are where one becomes the other. Field names match the in-memory type
    /// exactly, so the on-disk JSON is unchanged by the int move.
    type Port = {
        Id : string
        PortNumber : int option
        PortType : PortType
        HostId : string
    }

    /// The FILE form of SymbolInfo: port ids (map keys and PortOrder values) as strings.
    type SymbolInfo = {
        LabelBoundingBox: BoundingBox option
        LabelRotation: Rotation option
        STransform: STransform
        ReversedInputPorts: bool option
        PortOrientation: Map<string, Edge>
        PortOrder: Map<Edge, string list>
        HScale: float option
        VScale: float option
    }

    /// Like Component, but with legacy cases added to ComponentType
    /// Used only to read/write JSON versions of circuits
    type Component = {
        Id : string
        Type : ComponentType // This is JSONComponent.ComponentType!
        Label : string // All components have a label that may be empty.
        InputPorts : Port list // position on this list determines inputPortNumber
        OutputPorts : Port list // position in this lits determines OutputPortNumber
        SlotInfo : ParameterTypes.JSONParams.ComponentSlotExpr option
        X : float
        Y : float
        H : float
        W : float
        SymbolInfo : SymbolInfo option
    }

    /// The FILE form of a Connection: ids as strings.
    type Connection = {
        Id : string
        Source : Port
        Target : Port
        Vertices : (float * float * bool) list
    }

/// Transforms JSON components (parsed from JSON) to current components: legacy ComponentType
/// cases are upgraded, and the file's string ids become integers through the mapping functions
/// the loader supplies - which is where a uuid in an old file gets its integer allocated.
let convertFromJSONComponent (mapCompId: string -> int) (mapPortId: string -> int) (comp: JSONComponent.Component) : Component =
    let newType (ct: JSONComponent.ComponentType) : ComponentType = 
        match ct with
        | JSONComponent.ComponentType.Input1 (a,b) -> Input1 (a,b)
        | JSONComponent.ComponentType.Output x -> Output x
        | JSONComponent.ComponentType.Viewer x -> Viewer x
        | JSONComponent.ComponentType.IOLabel -> IOLabel
        | JSONComponent.ComponentType.NotConnected -> NotConnected
        | JSONComponent.ComponentType.BusCompare1 (a,b,c) -> BusCompare1 (a,b,c)
        | JSONComponent.ComponentType.BusSelection (a,b) -> BusSelection (a,b)
        | JSONComponent.ComponentType.Constant1 (a,b,c) -> Constant1 (a,b,c)
        | JSONComponent.ComponentType.Not -> Not
        | JSONComponent.ComponentType.And -> GateN (And, 2)
        | JSONComponent.ComponentType.Or -> GateN (Or, 2)
        | JSONComponent.ComponentType.Xor -> GateN (Xor, 2)
        | JSONComponent.ComponentType.Nand -> GateN (Nand, 2)
        | JSONComponent.ComponentType.Nor -> GateN (Nor, 2)
        | JSONComponent.ComponentType.Xnor -> GateN (Xnor, 2)
        | JSONComponent.ComponentType.GateN (gateType, n) -> GateN (gateType, n)
        | JSONComponent.ComponentType.Decode4 -> Decode4
        | JSONComponent.ComponentType.Mux2 -> Mux2
        | JSONComponent.ComponentType.Mux4 -> Mux4
        | JSONComponent.ComponentType.Mux8 -> Mux8
        | JSONComponent.ComponentType.Demux2 -> Demux2
        | JSONComponent.ComponentType.Demux4 -> Demux4
        | JSONComponent.ComponentType.Demux8 -> Demux8
        | JSONComponent.ComponentType.NbitsAdder x -> NbitsAdder x
        | JSONComponent.ComponentType.NbitsAdderNoCin x -> NbitsAdderNoCin x
        | JSONComponent.ComponentType.NbitsAdderNoCout x -> NbitsAdderNoCout x
        | JSONComponent.ComponentType.NbitsAdderNoCinCout x -> NbitsAdderNoCinCout x
        | JSONComponent.ComponentType.NbitsXor (a,b) -> NbitsXor (a,b)
        | JSONComponent.ComponentType.NbitsAnd x -> NbitsAnd x
        | JSONComponent.ComponentType.NbitsNot x -> NbitsNot x
        | JSONComponent.ComponentType.NbitsOr x -> NbitsOr x
        | JSONComponent.ComponentType.NbitSpreader x -> NbitSpreader x
        | JSONComponent.ComponentType.Custom x -> Custom x // schematic sheet used as component
        | JSONComponent.ComponentType.MergeWires -> MergeWires
        | JSONComponent.ComponentType.MergeN x -> MergeN x
        | JSONComponent.ComponentType.SplitWire x -> SplitWire x // int is bus width
        | JSONComponent.ComponentType.SplitN (a, b, c) -> SplitN (a, b, c)
        | JSONComponent.ComponentType.DFF -> DFF
        | JSONComponent.ComponentType.DFFE -> DFFE
        | JSONComponent.ComponentType.Register x -> Register x
        | JSONComponent.ComponentType.RegisterE x -> RegisterE x 
        | JSONComponent.ComponentType.Counter x -> Counter x
        | JSONComponent.ComponentType.CounterNoLoad x -> CounterNoLoad x
        | JSONComponent.ComponentType.CounterNoEnable x -> CounterNoEnable x
        | JSONComponent.ComponentType.CounterNoEnableLoad x -> CounterNoEnableLoad x
        | JSONComponent.ComponentType.AsyncROM1 x -> AsyncROM1 x
        | JSONComponent.ComponentType.ROM1 x -> ROM1 x
        | JSONComponent.ComponentType.RAM1 x -> RAM1 x
        | JSONComponent.ComponentType.AsyncRAM1 x -> AsyncRAM1 x
        // legacy components - to be deleted
        | JSONComponent.ComponentType.AsyncROM x -> AsyncROM x
        | JSONComponent.ComponentType.ROM x -> ROM x
        | JSONComponent.ComponentType.RAM x -> RAM x
        | JSONComponent.ComponentType.Shift (a,b,c) -> Shift (a,b,c)
        //-----------------------Changes are made in these conversions---------------------------//
        | JSONComponent.Constant(w,v) -> Constant1(w,v,sprintf "%A" v)
        | JSONComponent.Input n -> Input1(n, None)
        | JSONComponent.BusCompare(w,v) -> BusCompare1(w,v, sprintf "%A" v)
    let newPort (port: JSONComponent.Port) : Port =
        { Id = mapPortId port.Id
          PortNumber = port.PortNumber
          PortType = port.PortType
          HostId = mapCompId port.HostId }

    let newSymbolInfo (info: JSONComponent.SymbolInfo) : SymbolInfo =
        { LabelBoundingBox = info.LabelBoundingBox
          LabelRotation = info.LabelRotation
          STransform = info.STransform
          ReversedInputPorts = info.ReversedInputPorts
          PortOrientation =
            info.PortOrientation |> Map.toList |> List.map (fun (id, edge) -> mapPortId id, edge) |> Map.ofList
          PortOrder = info.PortOrder |> Map.map (fun _ ids -> List.map mapPortId ids)
          HScale = info.HScale
          VScale = info.VScale }

    // explicit construction, not unbox: the records only share a JS runtime representation,
    // and this code also runs under dotnet where unboxing between them is an invalid cast
    { Id = mapCompId comp.Id
      Type = newType comp.Type
      Label = comp.Label
      InputPorts = List.map newPort comp.InputPorts
      OutputPorts = List.map newPort comp.OutputPorts
      SlotInfo = comp.SlotInfo |> Option.map (ParameterTypes.slotsOfJson mapCompId)
      X = comp.X
      Y = comp.Y
      H = comp.H
      W = comp.W
      SymbolInfo = Option.map newSymbolInfo comp.SymbolInfo }

/// A file connection to a live one, through the same id mappings.
let convertFromJSONConnection
    (mapConnId: string -> int)
    (mapCompId: string -> int)
    (mapPortId: string -> int)
    (conn: JSONComponent.Connection)
    : Connection =
    let newPort (port: JSONComponent.Port) : Port =
        { Id = mapPortId port.Id
          PortNumber = port.PortNumber
          PortType = port.PortType
          HostId = mapCompId port.HostId }

    { Id = mapConnId conn.Id
      Source = newPort conn.Source
      Target = newPort conn.Target
      Vertices = conn.Vertices }

/// Transforms normal Components into JSON Components which can be saved.
/// This is always an identity transformation since the normal ComponentType
/// muts be strict subset of teh JSON ComponentType.
/// unboxing is ok here because we do not use equality in the conversion to JSON.
let convertToJSONComponent (comp: Component) : JSONComponent.Component =
    let newType =
        match comp.Type with
        | Input1 (a, b) -> JSONComponent.ComponentType.Input1 (a, b)
        | Output w -> JSONComponent.ComponentType.Output w
        | Viewer w -> JSONComponent.ComponentType.Viewer w
        | IOLabel -> JSONComponent.ComponentType.IOLabel
        | NotConnected -> JSONComponent.ComponentType.NotConnected
        | BusCompare1 (w, v, d) -> JSONComponent.ComponentType.BusCompare1 (w, v, d)
        | BusSelection (w, b) -> JSONComponent.ComponentType.BusSelection (w, b)
        | Constant1 (w, v, d) -> JSONComponent.ComponentType.Constant1 (w, v, d)
        | Not -> JSONComponent.ComponentType.Not
        | Decode4 -> JSONComponent.ComponentType.Decode4
        | GateN (t, n) -> JSONComponent.ComponentType.GateN (t, n)
        | Mux2 -> JSONComponent.ComponentType.Mux2
        | Mux4 -> JSONComponent.ComponentType.Mux4
        | Mux8 -> JSONComponent.ComponentType.Mux8
        | Demux2 -> JSONComponent.ComponentType.Demux2
        | Demux4 -> JSONComponent.ComponentType.Demux4
        | Demux8 -> JSONComponent.ComponentType.Demux8
        | NbitsAdder w -> JSONComponent.ComponentType.NbitsAdder w
        | NbitsAdderNoCin w -> JSONComponent.ComponentType.NbitsAdderNoCin w
        | NbitsAdderNoCout w -> JSONComponent.ComponentType.NbitsAdderNoCout w
        | NbitsAdderNoCinCout w -> JSONComponent.ComponentType.NbitsAdderNoCinCout w
        | NbitsXor (w, op) -> JSONComponent.ComponentType.NbitsXor (w, op)
        | NbitsAnd w -> JSONComponent.ComponentType.NbitsAnd w
        | NbitsNot w -> JSONComponent.ComponentType.NbitsNot w
        | NbitsOr w -> JSONComponent.ComponentType.NbitsOr w
        | NbitSpreader w -> JSONComponent.ComponentType.NbitSpreader w
        | Custom t -> JSONComponent.ComponentType.Custom t // schematic sheet used as component
        | MergeWires -> JSONComponent.ComponentType.MergeWires
        | MergeN x -> JSONComponent.ComponentType.MergeN x
        | SplitWire w -> JSONComponent.ComponentType.SplitWire w // int is bus width
        | SplitN (a, b, c) -> JSONComponent.ComponentType.SplitN (a, b, c)
        // DFFE is a DFF with an enable signal.
        // No initial state for DFF or Register? Default 0.
        | DFF -> JSONComponent.ComponentType.DFF
        | DFFE -> JSONComponent.ComponentType.DFFE
        | Register w -> JSONComponent.ComponentType.Register w
        | RegisterE w -> JSONComponent.ComponentType.RegisterE w
        | Counter w -> JSONComponent.ComponentType.Counter w
        | CounterNoLoad w -> JSONComponent.ComponentType.CounterNoLoad w
        | CounterNoEnable w -> JSONComponent.ComponentType.CounterNoEnable w
        | CounterNoEnableLoad w -> JSONComponent.ComponentType.CounterNoEnableLoad w
        | AsyncROM1 m -> JSONComponent.ComponentType.AsyncROM1 m
        | ROM1 m -> JSONComponent.ComponentType.ROM1 m
        | RAM1 m -> JSONComponent.ComponentType.RAM1 m
        | AsyncRAM1 m -> JSONComponent.ComponentType.AsyncRAM1 m
        // legacy components - to be deleted
        | AsyncROM m -> JSONComponent.ComponentType.AsyncROM m
        | ROM m -> JSONComponent.ComponentType.ROM m
        | RAM m -> JSONComponent.ComponentType.RAM m
        | Shift (w1, w2, t) -> JSONComponent.ComponentType.Shift (w1, w2, t)
        // legacy cases to be deleted?
        | BusCompare (w, v) -> JSONComponent.ComponentType.BusCompare (w, v)
        | Input w -> JSONComponent.ComponentType.Input w
        | Constant (w, v) -> JSONComponent.ComponentType.Constant (w, v)
    let jsonPort (port: Port) : JSONComponent.Port =
        { Id = string port.Id
          PortNumber = port.PortNumber
          PortType = port.PortType
          HostId = string port.HostId }

    let jsonSymbolInfo (info: SymbolInfo) : JSONComponent.SymbolInfo =
        { LabelBoundingBox = info.LabelBoundingBox
          LabelRotation = info.LabelRotation
          STransform = info.STransform
          ReversedInputPorts = info.ReversedInputPorts
          PortOrientation =
            info.PortOrientation |> Map.toList |> List.map (fun (id, edge) -> string id, edge) |> Map.ofList
          PortOrder = info.PortOrder |> Map.map (fun _ ids -> List.map string ids)
          HScale = info.HScale
          VScale = info.VScale }

    // explicit construction, not unbox: the records only share a JS runtime representation,
    // and this code also runs under dotnet where unboxing between them is an invalid cast
    { Id = string comp.Id
      Type = newType
      Label = comp.Label
      InputPorts = List.map jsonPort comp.InputPorts
      OutputPorts = List.map jsonPort comp.OutputPorts
      SlotInfo = comp.SlotInfo |> Option.map ParameterTypes.slotsToJson
      X = comp.X
      Y = comp.Y
      H = comp.H
      W = comp.W
      SymbolInfo = Option.map jsonSymbolInfo comp.SymbolInfo }

/// A live connection to its file form, ids written as decimal strings.
let convertToJSONConnection (conn: Connection) : JSONComponent.Connection =
    let jsonPort (port: Port) : JSONComponent.Port =
        { Id = string port.Id
          PortNumber = port.PortNumber
          PortType = port.PortType
          HostId = string port.HostId }

    { Id = string conn.Id
      Source = jsonPort conn.Source
      Target = jsonPort conn.Target
      Vertices = conn.Vertices }

//---------------------------------------------------------------------------------------------------------------//
//--------------------------END OF ComponentType CONVERSION - used when upgarding Component definitions----------//
//---------------------------------------------------------------------------------------------------------------//



// OLDER LEGACY TYPES, for VERY OLD Circuit compatibility

module LegacyCanvas =
    /// JSComponent mapped to F# record.
    /// Id uniquely identifies the component within a sheet.
    /// Label is optional descriptor displayed on schematic.
    type LegacyComponent = {
        Id : string
        Type : JSONComponent.ComponentType
        Label : string // All components have a label that may be empty.
        InputPorts : JSONComponent.Port list // position on this list determines inputPortNumber
        OutputPorts : JSONComponent.Port list // position in this lits determines OutputPortNumber
        X : float
        Y : float
        H : float
        W : float
    }

    /// JSConnection mapped to F# record.
    /// Id uniquely identifies connection globally and is used by library.
    type LegacyConnection = {
        Id : string
        Source : JSONComponent.Port
        Target : JSONComponent.Port
        Vertices : (float * float) list
    }

    /// F# data describing the contents of a single schematic sheet.
    type LegacyCanvasState = LegacyComponent list * LegacyConnection list



        

            
            
// This code is for VERY OLD circuits...
let legacyTypesConvert (lComps, lConns) =
    let convertConnection (c:LegacyCanvas.LegacyConnection) : JSONComponent.Connection =
        {
            Id=c.Id; 
            Source=c.Source;
            Target=c.Target;
            Vertices = 
                c.Vertices
                |> List.map (function 
                    | (x,y) when x >= 0. && y >= 0. -> (x,y,false)
                    | (x,y) -> (abs x, abs y, true))
        }
    let convertComponent (comp:LegacyCanvas.LegacyComponent) : JSONComponent.Component =

        {
            Id = comp.Id
            Type = comp.Type
            Label = comp.Label // All components have a label that may be empty.
            InputPorts = comp.InputPorts // position on this list determines inputPortNumber
            OutputPorts = comp.OutputPorts // position in this lits determines OutputPortNumber
            SlotInfo = None
            X = comp.X
            Y = comp.Y
            H = comp.H
            W = comp.W
            SymbolInfo = None
                    
        }
    let comps = List.map convertComponent lComps
    let conns = List.map convertConnection lConns
    (comps,conns)


//=========================================================================================================//
//-------------------------------------MISCELLANEOUS------------------------------------------------------ //
//=========================================================================================================//

///unconfigured replaces Some -1, Error replaces None, Configured of int replaces Some (positive int)
type WireWidth = | Configured of int | Unconfigured | ErrorWidth

type NumberBase = | Hex | Dec | Bin | SDec

/// Colors to highlight components
/// Case name is used as HTML color name.
/// See JSHelpers.getColorString
/// lots of colors can be added, see https://www.w3schools.com/colors/colors_names.asp
/// The Text() method converts it to the correct HTML string
/// Where speed matters the color must be added as a case in the match statement
type HighLightColor = Red | Blue | Yellow | Green | Orange | Grey | White | Purple | DarkSlateGrey | Thistle | Brown |SkyBlue
with 
    member this.Text() = // the match statement is used for performance
        match this with
        | Red -> "Red"
        | Blue -> "Blue"
        | SkyBlue -> "Skyblue"
        | Yellow -> "Yellow"
        | Green -> "Green"
        | Grey -> "Grey"
        | Purple -> "Purple"
        | DarkSlateGrey -> "darkslategrey"
        | Thistle -> "thistle"
        | c -> sprintf "%A" c
            
            

// Why none of the id types here are [<Struct>], although they look like textbook cases for it.
//
// Because F# Map BOXES a struct key on every comparison. Measured directly (.NET, 200,000
// Map.containsKey lookups into a 10,000-entry map, keys built outside the timed loop, so the
// only thing measured is the comparison):
//
//     raw int key                                 11.1 ms      0 MB
//     RefId of int        (reference DU, as here) 26.5 ms      0 MB
//     [<Struct>] StructId of int                  46.7 ms    113 MB
//
// That is about 570 bytes per lookup, roughly 44 bytes on each of the ~13 comparisons a tree of
// that size needs - both operands boxed every time. It is also SLOWER than the reference wrapper,
// not faster. The reason is that Map does not reach IComparable<'T> through a devirtualised
// constrained call: it takes its comparer from LanguagePrimitives.FastGenericComparer<'T>, which
// has hard-coded fast paths for genuine primitives and otherwise falls back to a path taking obj.
// A struct wrapper gets neither the primitive fast path nor the reference type's property of
// already being an object.
//
// The same effect is visible in the application. Allocation per build of the 3cpu demo, from
// SimLog's AllocMb, which repeats to within 0.1%:
//
//     nothing struct (as here)                    270.26 MB
//     port NUMBERS struct - never Map keys        270.44 MB   no change
//     ComponentId struct - the dominant Map key   275.38 MB   +1.9%
//     all seven struct                            278.11 MB   +2.9%
//
// So: [<Struct>] is free for an id that never becomes a Map key, and costs allocation and time
// for one that does. Every id below is Map-key material. Do not add it without measuring.
//
// The table also prices the wrappers themselves: a raw int key is 2.4x faster than the reference
// DU. That is the standing cost of type-safe ids in an F# Map, it is already being paid, and
// [<Struct>] does not recover it.

// The next types are not strictly necessary, but help in understanding what is what.
// Used consistently they provide type protection that greatly reduces coding errors

/// Unique integer id of a component. Unique across the whole DESIGN - the one id namespace
/// with a global invariant, allocated densely from 1 by Helpers.IdAllocator so a design's
/// components can index arrays directly. 0 and negatives are sentinels, never allocated.
[<Erase>]
type ComponentId = | ComponentId of int

let componentIdEncoder (cid: ComponentId) =
    match cid with
    | ComponentId n -> Encode.int n

let componentIdDecoder: Decoder<ComponentId> =
    Decode.int |> Decode.map ComponentId

/// The DESIGN-time name of a sheet.
///
/// A sheet's name and the name of one INSTANCE of that sheet are different things which have had
/// the same type - bare string - for as long as the waveform simulator has existed. That is why
/// SimTypes and ModelType each carry a long comment warning about the confusion, and why
/// FastCreate needs a collision hack for it. Wrapping the design-time one is half of telling
/// them apart; SimSheetId below is the other half.
///
/// Wrapped at the simulator interface only. It is deliberately NOT pushed into
/// LoadedComponent.Name, CustomComponentType.Name or SimpleSheet.SheetName: those cross the .dgm
/// persistence boundary and the SimpleJsonDotNet wire boundary, and [<Erase>] does not mean the
/// same thing under Fable as under .NET.
[<Erase>]
type SheetName = | SheetName of string

/// The chain of custom-component instances from the simulated top sheet down to one instance,
/// root first - so it names one ELABORATED copy of a sheet.
///
/// These are design-time ComponentIds, unique across the design, so a path is stable under
/// relabelling and means the same thing whichever side computed it. It is not a new value: the
/// simulator already builds exactly this as FastComponent.AccessPath (`ap @ [cid]` in
/// FastCreate) and the design side already builds exactly this as SheetTree.SheetAccessPath
/// (`accessPath @ [inst.InstId]` in MenuHelpers). This gives it a name.
[<Erase>]
type InstancePath = | InstancePath of ComponentId list

/// A path as a person reads it: the labels of the custom components passed through, root first.
///
/// DISPLAY ONLY, never an identity. A shown path may be shortened where that is unambiguous -
/// which is a rendering decision, and must not reach anything that compares paths.
[<Erase>]
type LabelPath = | LabelPath of string list

/// Unique identifier for a fast component.
/// The list is the access path, a list of all the containing custom components 
/// from the top sheet of the simulation (root first)
type SimComponentId = ComponentId * ComponentId list

/// The old name for SimComponentId, kept while the ~70 sites that destructure it as a bare tuple
/// are still doing so.
///
/// Both are abbreviations of the same tuple today, so this costs nothing and changes nothing.
/// Making the identity a tagged type is one line here - `[<Erase>] type SimComponentId =
/// SimComponentId of ComponentId * ComponentId list` - and it was measured, on this branch, to
/// break 71 sites, 38 of them in FastCreate and FastExtract. THAT is the reason to wait: it is
/// worth doing with the change that needs it (the per-instance port enumeration, which factors a
/// predicate out of FastCreate anyway) rather than as a sweep of the simulator core which buys
/// nothing on its own.
///
/// When it is done, tag it [<Erase>] and NOT [<Struct>]: this is the key type of fs.FComps and
/// the note above the id types prices what a struct key costs in an F# Map. A plain reference
/// wrapper is what the rest of them are and what this should be.
type FComponentId = SimComponentId

// An instance of a sheet in a running simulation is named by its InstancePath and nothing more,
// so no separate type for it: a wrapper carrying exactly the same information would be a layer to
// unwrap at every use and a second name for one idea.

/// Unique integer id of a connection, unique within its SHEET only - nothing resolves a
/// connection id outside the sheet it belongs to (error highlighting is sheet-guarded).
[<Erase>]
type ConnectionId     = | ConnectionId of int

/// type to uniquely identify a segment
type SegmentId      = int * ConnectionId


/// Human-readable name of component as displayed on sheet.
/// For I/O/labelIO components a width indication eg (7:0) is also displayed, but NOT included here
[<Erase>]
type ComponentLabel   = | ComponentLabel of string

/// Integer id of a component port, unique within its SHEET.
/// Connection ports and connected component ports have the same port Id
/// InputPortId and OutputPortID wrap the id to distinguish component
/// inputs and outputs some times (e.g. in simulation)
[<Erase>]
type InputPortId      = | InputPortId of int

/// Integer id of a component port, unique within its SHEET.
/// Connection ports and connected component ports have the same port Id
/// InputPortId and OutputPortID wrap the id to distinguish component
/// inputs and outputs some times (e.g. in simulation)
[<Erase>]
type OutputPortId     = | OutputPortId of int

/// Port numbers are sequential unique with port lists.
/// Inputs and Outputs are both numberd from 0 up.
[<Erase>]
type InputPortNumber  = | InputPortNumber of int

/// Port numbers are sequential unique with port lists.
/// Inputs and Outputs are both numberd from 0 up.
[<Erase>]
type OutputPortNumber = | OutputPortNumber of int

(*---------------------------Types for wave Simulation----------------------------------------*)

(*-----------------------------------------------------------------------------*)
// Types used within waveform Simulation code, and for saved wavesim configuartion

    
/// Uniquely identifies a wave by the component it comes from, and the port on which that
/// wave is from. Two waves can be identical but have a different index (e.g. a wave with
/// PortType Input must be driven by another wave of PortType Output).
type WaveIndexT = {
    SimArrayIndex: int
    Id: FComponentId
    PortType: PortType
    PortNumber: int
}

/// The stable NAME of a signal: which port of which component of which instance.
///
/// This is WaveIndexT without SimArrayIndex, and that omission is the point. SimArrayIndex is a
/// step-array index handed out by one build of one simulation and means nothing outside it,
/// which is why a selection has to be re-resolved by the other three fields whenever the
/// simulation is rebuilt. A SignalId survives a rebuild by construction, so it is what the model
/// should hold.
type SignalId =
    { SigComp: FComponentId
      SigPortType: PortType
      SigPort: int }

/// A simulator's handle for reading one signal: dense within one build, meaningless outside it,
/// and issued by whichever simulator is running.
///
/// Never stored in the model. The renderer asks the simulator to turn SignalIds into handles
/// after each build and quotes handles back when reading data. Keeping it distinct from SignalId
/// is what stops a handle from one simulation being used to read another - which is exactly the
/// mistake an exposed SimArrayIndex makes possible.
[<Erase>]
type SignalHandle = | SignalHandle of int

/// Bumped by every simulation build. Every cached entry and every reply in flight carries the
/// epoch it belongs to, so an answer from a superseded build is discarded rather than displayed.
[<Erase>]
type SimEpoch = | SimEpoch of int

type WSConfig = {
    /// This is the last clock cycle number possibly needed by a waveform simulation
    LastClock: int
    /// currently this is always 0
    /// TODO (maybe): implement simulation windows allowing this to be non-zero
    FirstClock: int
    /// The size of the waveform sdispaly font
    FontSize: int
    /// The weight of the waveform display font: 300 = normal, 600 = bold.
    FontWeight: int
}

/// The zoom multipliers the waveform viewer can sample at. Zooming out samples every Nth cycle,
/// so the step arrays carry a margin past WSConfig.LastClock of up to the largest multiplier,
/// plus a few overflow steps - ModelHelpers.waveSimRequiredArraySize computes the exact size a
/// configuration implies. These live here, beside WSConfig, because the simulator's memory check
/// (FastCreate) and the configuration dialog sit on opposite sides of the UI boundary and must
/// agree about what a configuration costs: each once had its own idea of the margin, so a last
/// clock the dialog allowed could be refused the moment the simulation was built.
let waveSimMultipliers = [1; 2; 5; 10; 20; 50; 100; 200; 500; 1000]

/// Extra simulation steps the waveform viewer may run past the last sampled cycle.
let waveSimStepsOverflow = 3

/// The most step-array cycles a WSConfig.LastClock can imply beyond LastClock itself: the
/// worst-case zoom margin plus the overflow steps. The largest configurable last clock and the
/// largest array that fits differ by exactly this.
let waveSimMaxArrayMargin = waveSimStepsOverflow + List.max waveSimMultipliers
    


/// Info saved by Wave Sim.
/// This info is not necessarilu uptodate with deletions or additions in the Diagram.
/// The wavesim code processing this will not fail if non-existent nets are referenced.
type SavedWaveInfo = {
    /// Waves which are selected to be shown in the waveform viewer
    SelectedWaves: WaveIndexT list option
    /// Radix in which values are displayed in the wave simulator
    Radix: NumberBase option
    /// Width of the waveform column
    WaveformColumnWidth: float option
    /// RAMs which are selected to be shown in the RAM tables
    SelectedRams: Map<ComponentId, string> option
    SelectedFRams: Map<FComponentId, string> option
    /// configuration options for waveform simulator
    WSConfig: WSConfig option

    /// The below fields are legacy values and no longer used.
    ClkWidth: float option
    Cursor: uint32 option
    LastClk: uint32 option
    DisplayedPortIds: string array option
}

/// Info regarding sheet saved in the .dgm file
type SheetInfo = {
    Form: CCForm option
    Description: string option
    ParameterDefinitions: ParameterTypes.ParameterDefs option
    /// True on the sheet the user has chosen as the current top of the design for display
    /// purposes. View state, not semantics: it changes what the editor displays, never what
    /// anything means. Optional so files saved by older Issie versions load unchanged.
    IsTopSheet: bool option
}

// ---------------------------------------------------------------------------------------------
// The FILE forms of the wave-viewer selection and the sheet info. Saved .dgm files hold every
// id as a string (uuids in old files, integers written as strings in new ones); the in-memory
// types above hold integers. Field names match the in-memory types exactly, so the on-disk
// JSON is unchanged. The converters take the loader's id mapping; a wave selection can name
// components on OTHER sheets (its access path), so the mapping the loader passes must cover
// the whole design - an id it cannot map becomes 0, a dangling reference the wave simulator
// already tolerates by lookup-miss, exactly as a stale uuid was.
// ---------------------------------------------------------------------------------------------

module JSONWave =

    type WaveIndexT = {
        SimArrayIndex: int
        Id: string * string list
        PortType: PortType
        PortNumber: int
    }

    type SavedWaveInfo = {
        SelectedWaves: WaveIndexT list option
        Radix: NumberBase option
        WaveformColumnWidth: float option
        SelectedRams: Map<string, string> option
        SelectedFRams: Map<string * string list, string> option
        WSConfig: WSConfig option
        ClkWidth: float option
        Cursor: uint32 option
        LastClk: uint32 option
        DisplayedPortIds: string array option
    }

    type SheetInfo = {
        Form: CCForm option
        Description: string option
        ParameterDefinitions: ParameterTypes.JSONParams.ParameterDefs option
        IsTopSheet: bool option
    }

let private fCompIdToJson ((ComponentId cid, path): FComponentId) : string * string list =
    string cid, path |> List.map (fun (ComponentId id) -> string id)

let private fCompIdOfJson (mapCompId: string -> int) ((cid, path): string * string list) : FComponentId =
    ComponentId(mapCompId cid), path |> List.map (mapCompId >> ComponentId)

let waveInfoToJson (wi: SavedWaveInfo) : JSONWave.SavedWaveInfo =
    { SelectedWaves =
        wi.SelectedWaves
        |> Option.map (List.map (fun w ->
            ({ SimArrayIndex = w.SimArrayIndex
               Id = fCompIdToJson w.Id
               PortType = w.PortType
               PortNumber = w.PortNumber }: JSONWave.WaveIndexT)))
      Radix = wi.Radix
      WaveformColumnWidth = wi.WaveformColumnWidth
      SelectedRams =
        wi.SelectedRams
        |> Option.map (Map.toList >> List.map (fun (ComponentId cid, v) -> string cid, v) >> Map.ofList)
      SelectedFRams =
        wi.SelectedFRams
        |> Option.map (Map.toList >> List.map (fun (fid, v) -> fCompIdToJson fid, v) >> Map.ofList)
      WSConfig = wi.WSConfig
      ClkWidth = wi.ClkWidth
      Cursor = wi.Cursor
      LastClk = wi.LastClk
      DisplayedPortIds = wi.DisplayedPortIds }

let waveInfoOfJson (mapCompId: string -> int) (wi: JSONWave.SavedWaveInfo) : SavedWaveInfo =
    { SelectedWaves =
        wi.SelectedWaves
        |> Option.map (List.map (fun (w: JSONWave.WaveIndexT) ->
            { SimArrayIndex = w.SimArrayIndex
              Id = fCompIdOfJson mapCompId w.Id
              PortType = w.PortType
              PortNumber = w.PortNumber }))
      Radix = wi.Radix
      WaveformColumnWidth = wi.WaveformColumnWidth
      SelectedRams =
        wi.SelectedRams
        |> Option.map (Map.toList >> List.map (fun (cid, v) -> ComponentId(mapCompId cid), v) >> Map.ofList)
      SelectedFRams =
        wi.SelectedFRams
        |> Option.map (Map.toList >> List.map (fun (fid, v) -> fCompIdOfJson mapCompId fid, v) >> Map.ofList)
      WSConfig = wi.WSConfig
      ClkWidth = wi.ClkWidth
      Cursor = wi.Cursor
      LastClk = wi.LastClk
      DisplayedPortIds = wi.DisplayedPortIds }

let sheetInfoToJson (si: SheetInfo) : JSONWave.SheetInfo =
    { Form = si.Form
      Description = si.Description
      ParameterDefinitions = si.ParameterDefinitions |> Option.map ParameterTypes.paramDefsToJson
      IsTopSheet = si.IsTopSheet }

let sheetInfoOfJson (mapCompId: string -> int) (si: JSONWave.SheetInfo) : SheetInfo =
    { Form = si.Form
      Description = si.Description
      ParameterDefinitions = si.ParameterDefinitions |> Option.map (ParameterTypes.paramDefsOfJson mapCompId)
      IsTopSheet = si.IsTopSheet }

(*--------------------------------------------------------------------------------------------------*)

/// Static data describing a schematic sheet loaded as a custom component.
/// Every sheet is always identified with a file from which it is loaded/saved. 
/// Name is human readable (and is the filename - without extension) and identifies sheet.
/// File path is the sheet directory and name (with extension).
/// InputLabels, OutputLabels are the I/O connections.
/// The I/O connection integers are bus widths.
/// The I/O connection strings are human readable. The strings are guaranteed
/// to be unique in the I/O connection list. I.e. An input label may be the same
/// as an output label, but two input (or output) labels cannot be the same.
/// The position in the I/O connections list is important as it implicitly
/// indicates the port number. For example, the first element in the InputLabels
/// list is related to the Component's Port with PortNumber 0.
/// Two instances of a loaded component have the same LoadedComponent data.
type LoadedComponent = {
    /// File name without extension = sheet name
    Name: string
    /// When the component was last saved
    TimeStamp: System.DateTime 
    /// Complete file path, including name and dgm extension
    FilePath : string
    /// Info on WaveSim settings
    WaveInfo: SavedWaveInfo option
    /// F# equivalent of Diagram components and connections including layout
    CanvasState : CanvasState
    /// Input port names, and port numbers in any created custom component
    InputLabels : (string * int) list
    /// Output port names, and port numbers in any created custom component
    OutputLabels : (string * int) list
    LCParameterSlots: ParameterTypes.ParameterDefs option
    Form : CCForm option
    /// If component needs saving to disk
    LoadedComponentIsOutOfDate: bool
    Description: string option
    /// True on the sheet chosen as the current top of the design for display purposes.
    /// View state persisted in the sheet's file; never affects elaboration.
    IsTopSheet: bool
}

open Optics.Operators

let formOpt_ = Lens.create (fun a -> a.Form) (fun s a -> match s with | None -> a | Some s -> {a with Form = Some s})
let canvasState_ = Lens.create (fun a -> a.CanvasState) (fun s a -> {a with CanvasState = s})
let loadedComponentIsOutOfDate_ = Lens.create (fun a -> a.LoadedComponentIsOutOfDate) (fun s a -> {a with LoadedComponentIsOutOfDate = s})
let componentsState_ = canvasState_ >-> Optics.fst_
let lcParameterSlots_ = Prism.create (fun a -> a.LCParameterSlots) (fun s a -> {a with LCParameterSlots = Some s})

/// A sheet's parameter data as though it were always there: a sheet that declares none reads as
/// empty declarations and no slots, and writing to it creates the record.
///
/// A LENS, where lcParameterSlots_ above is a prism, and that is the whole point. Composing a
/// prism onto a prism gives an optic whose SETTER does nothing when the outer get returns None
/// (Optics.fs), so `Optic.set` through lcParameterSlots_ silently dropped every write to a sheet
/// that had no parameter data yet - which is exactly the sheet a first declaration is being
/// written to. That hole is why ParameterView grew a second, hand-written path for the None case.
let lcParameterDefs_ =
    Lens.create
        (fun a ->
            a.LCParameterSlots
            |> Option.defaultValue {ParameterTypes.DefaultBindings = Map.empty; ParameterTypes.ParamSlots = Map.empty})
        (fun s a -> {a with LCParameterSlots = Some s})

let isTopSheet_ = Lens.create (fun a -> a.IsTopSheet) (fun s a -> {a with IsTopSheet = s})

/// Whether a component is clocked in itself - without looking inside a custom component, which is
/// a question about the sheet it names rather than about the component.
let isClockedPrimitive (compType: ComponentType) =
    match compType with
    | DFF | DFFE | Register _ | RegisterE _ | RAM _ | ROM _
    | Counter _ | CounterNoEnable _ | CounterNoLoad _ | CounterNoEnableLoad _ -> true
    | _ -> false

/// Which SHEETS of a design hold something clocked, at any depth.
///
/// A fact about a sheet and not about any instance of it: a sheet's components are the same
/// wherever it is placed, and parameters change widths rather than kinds. So it is worked out once
/// for a design, and every symbol drawn on it is then a set lookup.
///
/// Settled in one pass over sheets ordered so that each comes after everything inside it. Working
/// it out per component instead re-walked the whole subtree for every custom component drawn -
/// twice, since a symbol asks once for its colour and once for its own record - and that walk
/// follows every ROUTE through the design rather than every sheet, so a design whose sheets
/// instantiate one another costs paths rather than sheets.
let clockedSheets (ldcs: LoadedComponent list) : Set<string> =
    let byName = ldcs |> List.map (fun ldc -> ldc.Name, ldc) |> Map.ofList

    let subSheetsOf name =
        match Map.tryFind name byName with
        | None -> []
        | Some ldc ->
            fst ldc.CanvasState
            |> List.choose (fun comp ->
                match comp.Type with
                | Custom ct -> Some ct.Name
                | _ -> None)
            |> List.distinct

    /// Every sheet, ordered so that a sheet comes after all the sheets it instantiates. The
    /// reverse of a parents-first walk; a sheet reached several ways is placed once, and the
    /// visited set is also what stops a design that wrongly contains a cycle from hanging.
    let sorted =
        let rec walk (seen: Set<string>, acc) name =
            if Set.contains name seen then
                seen, acc
            else
                let seen, acc = ((Set.add name seen, acc), subSheetsOf name) ||> List.fold walk
                seen, name :: acc

        ((Set.empty, []), ldcs |> List.map (fun ldc -> ldc.Name))
        ||> List.fold walk
        |> snd
        |> List.rev

    (Set.empty, sorted)
    ||> List.fold (fun clocked name ->
        let holdsSomethingClocked =
            match Map.tryFind name byName with
            | None -> false
            | Some ldc ->
                fst ldc.CanvasState
                |> List.exists (fun comp ->
                    match comp.Type with
                    | Custom ct -> Set.contains ct.Name clocked
                    | compType -> isClockedPrimitive compType)

        if holdsSomethingClocked then Set.add name clocked else clocked)

/// Whether a component is clocked, given which sheets of the design are.
let isClockedGiven (clocked: Set<string>) (comp: Component) =
    match comp.Type with
    | Custom ct -> Set.contains ct.Name clocked
    | compType -> isClockedPrimitive compType

/// Returns true if a component is clocked.
///
/// For ONE component. Anything asking about many should work out `clockedSheets` once and use
/// `isClockedGiven` - which is what the draw block does, since it asks about every symbol on a
/// sheet twice over.
let isClocked (_visitedSheets: string list) (ldcs: LoadedComponent list) (comp: Component) =
    match comp.Type with
    | Custom _ -> isClockedGiven (clockedSheets ldcs) comp
    | compType -> isClockedPrimitive compType

/// Type for an open project which represents a complete design.
/// ProjectPath is directory containing project files.
/// OpenFileName is name of file from which current schematic sheet is loaded/saved, without extension or path
/// LoadedComponents contains the list of schematic sheets, each as a component, one per sheet.
type Project = {
    /// directory which contains the project files
    ProjectPath : string
    /// name of viewed sheet (Form: User) (without extension)
    OpenFileName : string
    /// name of sheet performing operation on (e.g.: when Verilog Editor is open)
    WorkingFileName : string option
    /// componnets have one-one correspondence with files
    LoadedComponents : LoadedComponent list
    }

        

let loadedComponents_ = Lens.create (fun a -> a.LoadedComponents) (fun s a -> {a with LoadedComponents = s})

let openLoadedComponent_ = 
    Lens.create 
        (fun a -> List.find (fun lc -> lc.Name = a.OpenFileName) a.LoadedComponents) 
        (fun lc' a -> {a with LoadedComponents = List.map (fun lc -> if lc.Name = a.OpenFileName then lc' else lc) a.LoadedComponents})

let openFileName_ = Lens.create (fun a -> a.OpenFileName) (fun s a -> {a with OpenFileName = s})
let workingFileName_ = Lens.create (fun a -> a.WorkingFileName) (fun s a -> {a with WorkingFileName = s})

let loadedComponentOf_ (name:string) = 
    Lens.create 
        (fun a -> List.find (fun lc -> lc.Name = name) a.LoadedComponents) 
        (fun lc' a -> {a with LoadedComponents = List.map (fun lc -> if lc.Name = name then lc' else lc) a.LoadedComponents})


/// Value set to None if the connection width could not be inferred.
type ConnectionsWidth = Map<ConnectionId, int option>

/// Documents user circuit error found during connection width inference
type WidthInferError = {
    Msg : string
    ConnectionsAffected : ConnectionId list // A list of connection Ids.
}


/// Messages sent from draw block
type JSDiagramMsg =
    | InitCanvas of CanvasState // Has to be dispatched only once.
    | SelectComponent of Component
    | UnselectComponent of unit
    | InferWidths of unit
    | SetHasUnsavedChanges of bool

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
    | ResizeNESW
    | ResizeNWSE
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
        | ResizeNESW -> "nesw-resize"   
        | ResizeNWSE -> "nwse-resize"

/// Type capturing global key press information
/// TODO: use this consistently, with global key listener,
/// throughout Issie.
type KeyPressInfo = {
    /// true if shift key is down
    ShiftKey: bool
    /// true if control key is down
    ControlKey: bool
    /// true if alt key is down
    AltKey: bool
    /// true if meta key is down
    MetaKey: bool
    /// the string representing the key itself
    KeyString: string
}
