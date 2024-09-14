(*
These are types used throughout the application
*)

module CommonTypes

module Constants =
    let equalityCheckTolerance = 0.0001
    let labelPosTolerance = 0.00001


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
    Id : string
    // For example, an And would have input ports 0 and 1, and output port 0.
    // If the port is used in a Connection record as Source or Target, the Number is None. 
    PortNumber : int option
    PortType : PortType 
    HostId : string
}

    
type PortId = | PortId of string

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
    |Library
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
} 

    
type ShiftComponentType =
    |LSL
    |LSR
    |ASR
    
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
    PortOrientation: Map<string, Edge>
    PortOrder: Map<Edge, string list>
    HScale: float option
    VScale: float option
}



let portOrder_ = Lens.create (fun c -> c.PortOrder) (fun n c -> {c with PortOrder = n})
let portOrientation_ = Lens.create (fun c -> c.PortOrientation) (fun n c -> {c with PortOrientation = n})


let getSTransformWithDefault (infoOpt: SymbolInfo option) =
    match infoOpt with
    | None ->{Rotation=Degree0; flipped=false}
    | Some inf -> inf.STransform


/// JSComponent mapped to F# record.
/// Id uniquely identifies the component within a sheet.
/// Label is optional descriptor displayed on schematic.
type Component = {
    Id : string
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
}

with
    member this.getPort (PortId portId: PortId) = 
        List.tryFind (fun (port:Port) -> port.Id = portId ) (this.InputPorts @ this.OutputPorts)

    /// Equality function for components, includes all geometry except component position
    member c1.isSame(c2: Component) =
        c1.Id = c2.Id && c1.Type = c2.Type && c1.Label = c2.Label &&
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


/// JSConnection mapped to F# record.
/// Id uniquely identifies connection globally and is used by library.
type Connection = {
    Id : string
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



    /// Like Component, but with legacy cases added to ComponentType
    /// Used only to read/write JSON versions of circuits
    type Component = {
        Id : string
        Type : ComponentType // This is JSONComponent.ComponentType!
        Label : string // All components have a label that may be empty.
        InputPorts : Port list // position on this list determines inputPortNumber
        OutputPorts : Port list // position in this lits determines OutputPortNumber
        X : float
        Y : float
        H : float
        W : float
        SymbolInfo : SymbolInfo option
    }

/// Transforms JSON components (parsed from JSON)  to current components
/// Normally this means converting legacy JSON component types into new ones.
/// However it could in principle be more radical.
/// The default transform unboxes the value which works when there is no change in the JS value
/// representation
let convertFromJSONComponent (comp: JSONComponent.Component) : Component =
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
    {unbox comp with Type = newType comp.Type}

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
    {unbox comp with Type = newType}

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
        InputPorts : Port list // position on this list determines inputPortNumber
        OutputPorts : Port list // position in this lits determines OutputPortNumber
        X : float
        Y : float
        H : float
        W : float
    }

    /// JSConnection mapped to F# record.
    /// Id uniquely identifies connection globally and is used by library.
    type LegacyConnection = {
        Id : string
        Source : Port
        Target : Port
        Vertices : (float * float) list
    }

    /// F# data describing the contents of a single schematic sheet.
    type LegacyCanvasState = LegacyComponent list * LegacyConnection list



        

            
            
// This code is for VERY OLD circuits...
let legacyTypesConvert (lComps, lConns) =
    let convertConnection (c:LegacyCanvas.LegacyConnection) : Connection =
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
            
            

// The next types are not strictly necessary, but help in understanding what is what.
// Used consistently they provide type protection that greatly reduces coding errors

/// SHA hash unique to a component - common between JS and F#
[<Erase>]
type ComponentId = | ComponentId of string

let componentIdEncoder (cid: ComponentId) =
    match cid with
    | ComponentId s -> Encode.string s

let componentIdDecoder: Decoder<ComponentId> =
    Decode.index 0 Decode.string
    |> Decode.andThen (fun caseName ->
        match caseName with
        | "ComponentId" ->
            Decode.index 1 Decode.string
            |> Decode.andThen (fun id -> Decode.succeed (ComponentId id))
        | invalid -> Decode.fail (sprintf "Invalid case name: %s" invalid))

/// Unique identifier for a fast component.
/// The list is the access path, a list of all the containing custom components 
/// from the top sheet of the simulation (root first)
type FComponentId = ComponentId * ComponentId list

/// SHA hash unique to a connection - common between JS and F#
[<Erase>]
type ConnectionId     = | ConnectionId of string

/// type to uniquely identify a segment
type SegmentId      = int * ConnectionId


/// Human-readable name of component as displayed on sheet.
/// For I/O/labelIO components a width indication eg (7:0) is also displayed, but NOT included here
[<Erase>]
type ComponentLabel   = | ComponentLabel of string

/// SHA hash unique to a component port - common between JS and F#.
/// Connection ports and connected component ports have the same port Id
/// InputPortId and OutputPortID wrap the hash to distinguish component
/// inputs and outputs some times (e.g. in simulation)
[<Erase>]
type InputPortId      = | InputPortId of string

/// SHA hash unique to a component port - common between JS and F#.
/// Connection ports and connected component ports have the same port Id
/// InputPortId and OutputPortID wrap the hash to distinguish component
/// inputs and outputs some times (e.g. in simulation)
[<Erase>]
type OutputPortId     = | OutputPortId of string

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
}

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
    Form : CCForm option
    /// If component needs saving to disk
    LoadedComponentIsOutOfDate: bool
    Description: string option
}

open Optics.Operators

let formOpt_ = Lens.create (fun a -> a.Form) (fun s a -> match s with | None -> a | Some s -> {a with Form = Some s})
let canvasState_ = Lens.create (fun a -> a.CanvasState) (fun s a -> {a with CanvasState = s})
let loadedComponentIsOutOfDate_ = Lens.create (fun a -> a.LoadedComponentIsOutOfDate) (fun s a -> {a with LoadedComponentIsOutOfDate = s})
let componentsState_ = canvasState_ >-> Optics.fst_


/// Returns true if a component is clocked
let rec isClocked (visitedSheets: string list) (ldcs: LoadedComponent list) (comp: Component) =
    match comp.Type with
    | Custom ct ->
        let ldcOpt =
            ldcs
            |> List.tryFind (fun ldc -> ldc.Name = ct.Name)
        match ldcOpt, List.contains ct.Name visitedSheets with
        | _, true -> false
        | None, _ -> false
        | Some ldc, _ ->
            let (comps, _) = ldc.CanvasState
            List.exists (isClocked (ct.Name :: visitedSheets) ldcs) comps
                        

                            
    | DFF | DFFE | Register _ | RegisterE _ | RAM _ | ROM _
    | Counter _ |CounterNoEnable _ | CounterNoLoad _  |CounterNoEnableLoad _ ->
        true
    | _ -> false

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
