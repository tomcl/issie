module CommonTypes
    open Fable.Core

    //=================================================//
    // Faster version of Map for objects with SHA hash //
    //=================================================//

    
    type HMap<'T> = 
        | Tree of HMap<'T> array
        | Found of 'T
        | Empty

    let inline initHArray() = Array.create 16 Empty

    let charCodeF = int 'a' 
    let charCode0 = int '0' 

    let inline getHashDigit n (h:string) =
        let ch = h.[n]
        if System.Char.IsDigit ch then
            int ch - charCode0          
        elif System.Char.IsLetter ch then
            int ch - charCodeF + 10
        elif ch = '-' then 0           
        else
            failwithf "Hash to digit conversion failed on n={n}, char={ch}"

    let getFastHash (sha:string) =
        sha 
        |> Seq.toArray 
        |> Array.mapi (fun i _ -> getHashDigit i sha)

    let getFastHItem (x,_,_) = x

    let getFastSHA (sha:string, x:int array, _)  n = x.[n]

    let inline copyUpdate n item arr =
        Array.init 
            (Array.length arr) 
            (fun i -> if i = n then item else Empty)

    let hMapAdd (getFastEq: 'T -> string) (getSHA: 'T -> int -> int) (item: 'T) (hm: HMap<'T>) =
        let hash = getSHA item
        let rec hAdd shaIndex hm =
            match hm with
            | Empty -> Found item
            | Found item' ->
                let hash' = getSHA item'
                if getFastEq item' = getFastEq item then  
                    Found item
                else
                    let h = hash shaIndex
                    let h' = hash' shaIndex
                    let arr = initHArray()
                    if h <> h' then
                        arr.[h] <- Found item 
                        arr.[h'] <- Found item'
                        Tree arr
                    else 
                        arr.[h'] <- hAdd (shaIndex+1) (Found item')
                        Tree arr
            | Tree arr ->
                let h = hash shaIndex
                let hm' = hAdd (shaIndex+1) arr.[h]
                Tree <| copyUpdate h hm' arr
        hAdd 0 hm  


    let hMapAddMutate (getFastEq: 'T -> string) (getSHA: 'T -> int -> int) (item: 'T) (hm: HMap<'T>) =
        let hash = getSHA item
        let rec hAdd shaIndex hm =
            match hm with
            | Empty -> Found item
            | Found item' ->
                let hash' = getSHA item'
                if getFastEq item = getFastEq item' then  
                    Found item
                else
                    let arr = initHArray()
                    let h = hash shaIndex
                    let h' = hash' shaIndex
                    if h <> h' then
                        arr.[h] <- Found item 
                        arr.[h'] <- Found item'
                        Tree arr
                    else 
                        arr.[h'] <- hAdd (shaIndex+1) (Found item')
                        Tree arr
                        
            | Tree arr ->
                let h = hash shaIndex
                let hm' = hAdd (shaIndex+1) arr.[h]
                arr.[h] <- hm'
                Tree arr
        hAdd 0 hm  

    let hMapTryFind (getFastEq: 'T -> string) (getSHA: 'T -> int -> int) (item: 'T) (hm: HMap<'T>) =
        let rec lookup shaIndex (hm:HMap<'T>) =
            match hm with
            | Found item' when getFastEq item' = getFastEq item ->
                Some item'
            | Tree arr ->
                let hit = arr.[getSHA item shaIndex]
                lookup (shaIndex+1) hit
            | _ -> None
        lookup 0 hm

    let rec hMapFilter (pred: 'T -> bool) (hm: HMap<'T>) =
        match hm with
        | Found item' as x when pred item' -> x
        | Tree arr ->
            let arr' = Array.map (hMapFilter pred) arr
            if Array.exists (fun x -> x <> Empty) arr' then 
                Tree arr'
            else Empty                
        | _ -> Empty

    let rec hMapToArray (hm: HMap<'T>) =
        match hm with
        | Empty -> [||]
        | Found x -> [|x|]
        | Tree arr -> 
            arr
            |> Array.map hMapToArray
            |> Array.concat

    let rec arrayToHmap (getFastEq: 'T -> string) (getSHA: 'T -> int -> int) (arr: 'T array) =
        (Empty, arr)
        ||> Array.fold (fun hm item -> hMapAddMutate getFastEq getSHA item hm)


    let rec hMapCount (hm: HMap<'T>) =
        match hm with
        | Empty -> 0
        | Found _ -> 1
        | Tree arr -> 
            arr
            |> Array.map hMapCount
            |> Array.sum

                

        
  

    //==========================================//
    // Canvas state mapped to f# data structure //
    //==========================================//

    // Specify the position and type of a port in a JSComponent.
    type PortType = Input | Output
    /// A component I/O.
    /// Id (like any other Id) is a string generated with 32 random hex charactes,
    /// so it is (practically) globally unique. These Ids are used by the draw2d
    /// library to uniquely refer to ports and components. They are generated via:
    /// http://www.draw2d.org/draw2d_touch/jsdoc_6/#!/api/draw2d.util.UUID.
    /// PortNumber is used to identify which port on a component, contiguous from 0
    /// separately for inputs and outputs.
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
            
            
    /// Name identifies the LoadedComponent used.
    /// The labels define legends on symbol designating inputs or outputs: and are the names of the Input or Output components of the CC sheet.
    /// Label strings are unique per CustomComponent.
    /// Label position in list determines inputportnumber or outputportnumber of label.
    /// Multiple CustomComponent instances are differentiated by Component data.
    type CustomComponentType = {
        Name: string
        // Tuples with (label * connection width).
        InputLabels: (string * int) list
        OutputLabels: (string * int) list 
    }

    type Memory = {
        // How many bits the address should have.
        // The memory will have 2^AddressWidth memory locations.
        AddressWidth : int 
        // How wide each memory word should be, in bits.
        WordWidth : int
        // Data is a list of <2^AddressWidth> elements, where each element is a
        // 64 bit integer. This makes words longer than 64 bits not supported.
        // This can be changed by using strings instead of int64, but that is way
        // less memory efficient.
        Data : Map<int64,int64>
    }

    type InitMemData = 
        | FromData // old method (from data field)
        | FromFile of string // FromFile fName => read a file fName.ram for data
        | ToFile of string // ToFile fName => write data to a file fName.ram
        | ToFileBadName of string // as ToFile but the name does not validate
        | UnsignedMultiplier
        | SignedMultiplier


    type Memory1 = {
    // is the data initialised from a file name.ram in the project directory, or some other way?
    Init: InitMemData
    // How many bits the address should have.
    // The memory will have 2^AddressWidth memory locations.
    AddressWidth : int 
    // How wide each memory word should be, in bits.
    WordWidth : int
    // Data is a list of <2^AddressWidth> elements, where each element is a
    // 64 bit integer. This makes words longer than 64 bits not supported.
    // This can be changed by using strings instead of int64, but that is way
    // less memory efficient.
    Data : Map<int64,int64>  
    } 

    // Types instantiating objects in the Digital extension.
    type ComponentType =
        | Input of BusWidth: int | Output of BusWidth: int | Viewer of BusWidth: int | IOLabel 
        | BusCompare of BusWidth: int * CompareValue: uint32
        | BusSelection of OutputWidth: int * OutputLSBit: int
        | Constant of Width: int * ConstValue: int64 
        | Constant1 of Width: int * ConstValue: int64 * DialogTextValue: string
        | Not | And | Or | Xor | Nand | Nor | Xnor |Decode4
        | Mux2 | Demux2
        | NbitsAdder of BusWidth: int | NbitsXor of BusWidth:int
        | Custom of CustomComponentType // schematic sheet used as component
        | MergeWires | SplitWire of BusWidth: int // int is bus width
        // DFFE is a DFF with an enable signal.
        // No initial state for DFF or Register? Default 0.
        | DFF | DFFE | Register of BusWidth: int | RegisterE of BusWidth: int 
        | AsyncROM of Memory | ROM of Memory | RAM of Memory // legacy components - to be deleted
        | AsyncROM1 of Memory1 | ROM1 of Memory1 | RAM1 of Memory1 | AsyncRAM1 of Memory1

    let (|IsBinaryCompType|_|) cType =
        match cType with
         | _ -> None

    /// get memory component type constructor
    /// NB only works with new-style memory components
    let getMemType (cType: ComponentType) =
        match cType with
        | RAM1 _ -> RAM1
        | AsyncRAM1 _ -> AsyncRAM1
        | ROM1 _ -> ROM1
        | AsyncROM1 _ -> AsyncROM1
        | _ -> failwithf $"Can't get memory type from {cType}"


    /// JSComponent mapped to F# record.
    /// Id uniquely identifies the component within a sheet and is used by draw2d library.
    /// Label is optional descriptor displayed on schematic.
    type Component = {
        Id : string
        Type : ComponentType
        Label : string // All components have a label that may be empty.
        InputPorts : Port list
        OutputPorts : Port list
        X : int
        Y : int
        H : int
        W : int
    }

    /// JSConnection mapped to F# record.
    /// Id uniquely identifies connection globally and is used by library.
    type Connection = {
        Id : string
        Source : Port
        Target : Port
        Vertices : (float * float) list
    }

    /// F# data describing the contents of a single schematic sheet.
    type CanvasState = Component list * Connection list

    //=======//
    // Other //
    //=======//

    ///unconfigured replaces Some -1, Error replaces None, Configured of int replaces Some (positive int)
    type WireWidth = | Configured of int | Unconfigured | ErrorWidth

    type NumberBase = | Hex | Dec | Bin | SDec

    /// Colors to highlight components
    /// Case name is used as HTML color name.
    /// See JSHelpers.getColorString
    /// lots of colors can be added, see https://www.w3schools.com/colors/colors_names.asp
    /// The Text() method converts it to the correct HTML string
    /// Where speed matters the color must be added as a case in the match statement
    type HighLightColor = Red | Blue | Yellow | Green | Orange | Grey | White | Purple | DarkSlateGrey | Thistle | Brown
    with 
        member this.Text() = // the match statement is used for performance
            match this with
            | Red -> "Red"
            | Blue -> "Blue"
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
    type ComponentId      = | ComponentId of string

    /// SHA hash unique to a segment
    type SegmentId      = | SegmentId of string

    /// SHA hash unique to a connection - common between JS and F#

    /// SHA hash unique to a connection - common between JS and F#
    [<Erase>]
    type ConnectionId     = | ConnectionId of string

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

    
    type MoreWaveData =
        | RamWaveData of addr: uint32 * ramPath: ComponentId list * label:string
        | ExtraData of ramPath: ComponentId list * label:string
        
    // The "NetList" types contain all the circuit from Diagram in an abstracted form that
    // removed layout info and connections as separate entities. However, connection Ids are
    // available as fileds in components for interface to the Diagram conmponents

    /// The driven (output) side of a connection.
    /// This is stored with a NLComponent output port number.
    /// Note that one output port can drive multiple NLTargets.
    type NLTarget = {
        TargetCompId: ComponentId
        InputPort: InputPortNumber
        TargetConnId: ConnectionId
        }

    /// The driving (input) side of a connection.
    /// This is stored with a NLComponent input port number
    type NLSource = {
        SourceCompId: ComponentId
        OutputPort: OutputPortNumber
        SourceConnId: ConnectionId
        }

    /// Components with inputs and outputs directly referencing other components.
    /// Output ports can connect to multiple components, or none.
    /// Input ports connect to a single driver, or nothing.
    type NetListComponent = {
        Id : ComponentId
        Type : ComponentType
        Label : string
        // List of input port numbers, and single mapped driving output port
        // and component.
        Inputs : Map<InputPortNumber, NLSource option>
        // Mapping from each output port number to all of the input ports and
        // Components connected to that port.
        Outputs : Map<OutputPortNumber, NLTarget list>
     }

    /// Circuit topology with connections abstracted away.
    /// Good for Wavesim calculations.
    type NetList = Map<ComponentId,NetListComponent>


    (*-----------------------------------------------------------------------------*)
    // Types used within waveform Simulation code, and for saved wavesim configuartion

    /// Identifies a fully connected net
    /// This ties together labelled nets.
    /// should it include the display name(s)? this can be calculated
    type NetGroup = { 
        driverComp: NetListComponent
        driverPort: OutputPortNumber
        driverNet: NLTarget list
        connectedNets: NLTarget list array }

    /// Info saved by Wave Sim.
    /// This info is not necessarilu uptodate with deletions or additions in the Diagram.
    /// The wavesim code processing this will not fail if non-existent nets are referenced.
    type SavedWaveInfo = {
        ClkWidth: float
        Cursor: uint32 
        Radix: NumberBase
        LastClk: uint32
        DisplayedPortIds: string array
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
    }

    /// Type for an open project which represents a complete design.
    /// ProjectPath is directory containing project files.
    /// OpenFileName is name of file from which current schematic sheet is loaded/saved, without extension or path
    /// LoadedComponents contains the list of schematic sheets, each as a component, one per sheet.
    type Project = {
        /// directory which contains the project files
        ProjectPath : string
        /// name of open sheet (without extension)
        OpenFileName : string
        /// componnets have one-one correspondence with files
        LoadedComponents : LoadedComponent list
        }

    (*-----------------------------------------------------------------------------*)
    // Types used for naming of waveforms in the Waveform Simulator

    /// Identifies the name of a single driving component of a waveform
    type LabelSegment = { 
        LabName : string
        BitLimits : int*int 
    }

    /// Identifies the names of the driving components and the named labels of a waveform
    type WaveLabel = {
        /// Identifies the names of Output and IOLabel components connected to the waveform's net
        OutputsAndIOLabels : string list
        /// Identifies the driving components' names
        ComposingLabels : LabelSegment list 
    }


    /// Value set to None if the connection width could not be inferred.
    type ConnectionsWidth = Map<ConnectionId, int option>

    /// Documents user circuit error found during connection width inference
    type WidthInferError = {
        Msg : string
        ConnectionsAffected : ConnectionId list // A list of connection Ids.
    }


    /// Messages that will be sent from JS code.
    /// This is a define here as a hack to deal with the F# requirement of no forward references.
    /// This type must be defined before Draw2dwrapper, however Draw2dWrapper must be defined before ModelType
    /// Therefore we must define this here rather than where it should be, which is in ModelType
    /// The type parameters allow us to keep JSCanvas and JSComponent in JSTypes where they should be.
    /// whenever JSDiagramMsg is actually used: TCanvas = JSCanvas, TComponent = JSComponent
    type JSDiagramMsg<'TCanvas,'TComponent> =
        | InitCanvas of 'TCanvas // Has to be dispatched only once.
        | SelectComponent of 'TComponent
        | UnselectComponent of unit
        | InferWidths of unit
        | SetHasUnsavedChanges of bool