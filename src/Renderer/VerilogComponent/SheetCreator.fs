module SheetCreator

open VerilogTypes
open CommonTypes
open DrawHelpers
open Helpers
open NumberHelpers

/////// TYPES ////////

type Circuit = {
    Comps: Component list;
    Conns: Connection list;
    Out: Port;
    OutWidth: int
}

type Slice = {
    MSB: int;
    LSB: int;
}

type LHSType =
    |OutputPort
    |Wire


/////// HELPERS ////////

/// Helper function to find a port's width from the range definition of IODecl
let getWidthFromRange (range:RangeT option) = 
    match range with
    |None -> 1
    |Some r ->
        let start = r.Start |> int
        start+1

/// Create a component (type: Component) based on the parameters given
let createComponent' id compType (name:string) inputPorts outputPorts =
    {
        Id = id
        Type = compType
        Label = name.ToUpper()
        InputPorts = inputPorts 
        OutputPorts = outputPorts
        X = 0.
        Y = 0.
        H = 30.
        W = 30.
        SymbolInfo = None
    }

/// Create a port (type: Port) based on the parameters given
let createPort hostId portType portNumber =
    {
        Id = DrawHelpers.uuid()
        PortNumber = portNumber
        PortType = portType
        HostId = hostId
    }

/// Connect source with target returning the connection (type: Connection)
let createConnection (source:Port) (target:Port) = 
    let source' = {source with PortNumber=None}
    let target' = {target with PortNumber=None}
    {
        Id = DrawHelpers.uuid()
        Source = source'
        Target = target'
        Vertices = []
    }

let createPortList (ofType:PortType) (number:int) (hostId:string) =
    [0..(number-1)] |> List.collect (fun i -> [createPort hostId ofType (Some i)] )


/// Main component creation function
/// Find all the parameters required for component creation
/// based on the component Type and the name(label) given
/// Returns the created component
let createComponent (compType:ComponentType) (name:string) : Component =
    let inputPortNo,outputPortNo =
        match compType with
        |BusSelection (_,_) |NbitSpreader _ |BusCompare (_,_)
        |Not |NbitsNot _ |IOLabel
            -> 1,1
        |Output _ |Viewer _ 
            -> 1,0 
        |NbitsAnd _ |NbitsOr _ |NbitsXor _
        |And |Or |MergeWires
            -> 2,1
        |Mux2 
            -> 3,1
        |NbitsAdder _
            -> 3,2
        |Input _ |Input1 (_,_)| Constant1 (_,_,_)
            -> 0,1
        |_ -> failwithf "Undefined component properties"
    
    let id = DrawHelpers.uuid()
    let inputPorts = createPortList PortType.Input inputPortNo id
    let outputPorts = createPortList PortType.Output outputPortNo id
    
    createComponent' id compType name inputPorts outputPorts

/// Helper to extract width from component type
let extractWidth compType =
        match compType with
        |Output width -> width
        |Input1 (width,_) -> width
        |IOLabel -> 1
        |_ -> failwithf "Can't happen"

let extractCircuit (input:(Circuit*string*Slice*LHSType)) = 
    match input with
    |(c,_,_,_) -> c

/// Join input ports of topCircuit with inCircuits
let joinCircuits (inCircuits:Circuit list) (inPorts: Port list) (topCircuit: Circuit) : Circuit = 
    let conns = 
        inPorts
        |> List.mapi (fun index inputPortId ->
            createConnection inCircuits[index].Out inputPortId
        )
        |> List.append topCircuit.Conns
    
    let allConns = 
        inCircuits
        |> List.collect (fun c -> c.Conns) 
        |> List.append conns
        
    let comps = 
        inCircuits
        |> List.collect (fun circ ->
            circ.Comps    
        )
        |> List.append topCircuit.Comps
    {Comps=comps;Conns=allConns;Out=topCircuit.Out;OutWidth=topCircuit.OutWidth}


/// Join a list of circuits with MergeWires components 
let rec joinWithMerge (lst:(Circuit*string*Slice*LHSType) list) = 
    
    let merge2Circuits (c1:Circuit,name:string,slice:Slice,lhsType:LHSType) (c2:Circuit,name2:string,slice2:Slice,lhsType2:LHSType) = 
        let comp = createComponent MergeWires "" 
        let topCircuit = {Comps=[comp];Conns=[];Out=comp.OutputPorts[0];OutWidth=0}
        joinCircuits [c1;c2] [comp.InputPorts[0];comp.InputPorts[1]] topCircuit, name, slice,lhsType

    match List.length lst with 
    |1 -> lst[0]
    |2 -> merge2Circuits lst[0] lst[1]
    |_ ->
        let _,back = lst |> List.splitAt 2
        let m1 = merge2Circuits lst[0] lst[1]
        joinWithMerge (List.append [m1] back)

/// Extract MSB,LSB from assignment and return as a Slice
/// type Slice = {MSB:int, LSB:int}
let sliceFromBits (lhs:AssignmentLHST) (ioAndWireToCompMap: Map<string,Component>) = 
    match (Option.isSome lhs.BitsStart) with
    |true -> 
        let bStart = (Option.get lhs.BitsStart)
        let bEnd = (Option.get lhs.BitsEnd)
        {MSB = (int bStart); LSB =(int bEnd) }
    |false ->
        let comp = Map.find lhs.Primary.Name ioAndWireToCompMap
        let width = extractWidth comp.Type
        {MSB = (width-1); LSB=0}

/// Attach the merged circuits to the correct output port 
let attachToOutput (ioAndWireToCompMap: Map<string,Component>) (circuit:Circuit,portName:string,slice:Slice,lhsType:LHSType) : CanvasState =
    let outputOrWire = Map.find portName ioAndWireToCompMap
    let conn = createConnection circuit.Out outputOrWire.InputPorts[0]
    
    let allComps = 
        match lhsType with
        |OutputPort -> circuit.Comps@[outputOrWire]
        |Wire -> circuit.Comps
    let allConns = circuit.Conns@[conn]
    (allComps,allConns)


let concatenateCanvasStates (mainCS: CanvasState) (newCS:CanvasState) : CanvasState =
    ((fst mainCS)@(fst newCS),(snd mainCS)@(snd newCS))

/// Helper function to resolve conflicts in labels (must be distinct) 
/// and component locations on canvas (must not overlap_)
let fixCanvasState (oldCanvasState:CanvasState) =
    let fixedComps =
        oldCanvasState
        |> fst
        |> List.mapi (fun i comp ->
            let newLabel = 
                match comp.Type with
                |IOLabel |Input1 (_,_)| Output _ -> comp.Label
                |_ ->
                    match comp.Label with 
                    |"" -> "" 
                    |_ -> comp.Label+(string i)
            let x,y = (float (i+1)*120.),(float (i+1)*120.)
            {comp with Label=newLabel;X=x;Y=y}
        )
    (fixedComps,snd oldCanvasState)
    
    

/////// STATIC MAP CREATION ////////

let createIOComponent (item:ItemT) ioType (oldMap)  =  
    
    let width = getWidthFromRange (Option.get item.IODecl).Range
    let compType = 
        match ioType with
        |"input_decl" -> Input1 (width,Some 0)
        |_ -> Output width

    let names =
        (Option.get item.IODecl).Variables 
        |> Array.map (fun identifier ->
            identifier.Name    
        )
        |> Array.toList
    
    (oldMap,names)||>List.fold (fun map name ->
        map@[(name,(createComponent compType name))]
    )

/// Return a Map<string,Component> for input and output ports
/// where string -> port name.
/// It is necessary in order to find components when building circuits for assignments
let getIOtoComponentMap (ioDecls:ItemT list) = 
    ([],ioDecls)
    ||> List.fold (fun map item ->
        createIOComponent item item.ItemType map
    )
    |> Map.ofList

/// Return a Map<string,Component> for wires
/// where string -> wire name.
/// It is necessary in order to find wire components when building circuits for assignments
let getWireToCompMap (lhs:AssignmentLHST) ioAndWireToCompMap =
    let name = lhs.Primary.Name
    
    let wireComp = createComponent IOLabel name
    Map.add name wireComp ioAndWireToCompMap


let collectWiresLHS (assignments:ItemT list) =
    let wires = assignments |> List.filter (fun item -> (Option.get item.Statement).StatementType = "wire")
    wires
    |> List.map (fun item -> (Option.get item.Statement).Assignment.LHS)

let collectInputAndWireComps (ioAndWireToCompMap:Map<string,Component>) =
    ioAndWireToCompMap
    |> Map.toList
    |> List.map snd
    |> List.filter (fun comp ->
        match comp.Type with
        |Input1 (_,_) |IOLabel -> true
        |_ -> false
    )

/////// COMPONENT CREATION ////////

/// Extract component type and name from expression (type ExpressionT)
/// Create the component using the createComponent function
let buildExpressionComponent (rhs:ExpressionT) width =
    
    let compType =
        match rhs.Type with
        | "negation" -> (NbitsNot width)  
        | "bitwise_OR" -> (NbitsOr width)
        | "bitwise_XOR" -> (NbitsXor width)
        | "bitwise_AND" -> (NbitsAnd width)
        | "additive" -> (NbitsAdder width)
        | "conditional_cond" -> (Mux2)
        | "logical_AND" -> (And) 
        | "logical_OR" -> (Or)
        |_ -> failwithf "Missing component(?) in buildExpressionComponent" 
    
    let baseName = 
        match rhs.Type with
        |"bitwise_OR" -> "OR"
        |"bitwise_XOR" -> "NXOR"
        | "additive" -> "ADD"
        | "bitwise_AND" -> "AND"
        | "negation" -> "NOT"
        | "conditional_cond" -> "MUX"
        | "logical_AND" -> "G"
        | "logical_OR" -> "G"
        |_ -> failwithf "Missing component(?) in buildExpressionComponent" 

        
    createComponent compType baseName


/////// CIRCUIT CREATION ////////

/// Finds the correct component based on the name of input/wire
/// creates a circuit with that component (and if required a busSel component
/// connected to it to return the correct slice) and returns that circuit
let createPrimaryCircuit (primary:PrimaryT) (ioAndWireToCompMap:Map<string,Component>) =
        let name = primary.Primary.Name
        let inputComp = Map.find name ioAndWireToCompMap
        match Option.isNone primary.BitsStart with
        |true -> 
            let width = extractWidth inputComp.Type
            {Comps=[];Conns=[];Out=inputComp.OutputPorts[0];OutWidth=width}
        |false ->
            let bStart,bEnd = (int (Option.get primary.BitsStart)),(int (Option.get primary.BitsEnd))
            
            let lsb,outWidth = bEnd,(bStart-bEnd+1)
            
            let busSelComp = createComponent (BusSelection (outWidth,lsb)) ""

            let conn = createConnection inputComp.OutputPorts[0] busSelComp.InputPorts[0]     
            {Comps=[busSelComp];Conns=[conn];Out=busSelComp.OutputPorts[0];OutWidth=outWidth}

/// Creates the correct component based on the number and returns a circuit with that component
let createNumberCircuit (number:NumberT) =
    let width = (Option.get number.Bits) |> int
    let _base = Option.get number.Base
    let no = (Option.get number.AllNumber)
    let text = 
        match _base with
        |"'b" -> "0b"+no
        |"'h" -> "0x"+no
        |_ -> no
    let constValue =
        match NumberHelpers.strToIntCheckWidth width text with
        |Ok n -> n
        |Error _ -> failwithf "Shouldn't happen!"
    
    let constComp = createComponent (Constant1 (width,constValue,text)) "C"
    {Comps=[constComp];Conns=[];Out=constComp.OutputPorts[0];OutWidth=width}

/// The main circuit creation function called with the RHS of an assignment as a parameter
/// Contains 6 recursive functions which eventually build the whole RHS expression
/// The starting point is the buildExpressionCircuit rec function
let mainExpressionCircuitBuilder (expr:ExpressionT) ioAndWireToCompMap =
    
    /// builds the appropriate circuit of an expression based on expr.Type
    let rec buildExpressionCircuit (expr:ExpressionT) = 
        match expr.Type with
        | "unary" -> buildUnaryCircuit (Option.get expr.Unary)
        | "negation" ->
            let (c1:Circuit) = buildUnaryCircuit (Option.get expr.Unary)
            let topComp = buildExpressionComponent expr c1.OutWidth
            let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
            joinCircuits [c1] [topComp.InputPorts[0]] topCircuit
        | "conditional_cond" -> 
            let (c3:Circuit) = buildExpressionCircuit (Option.get expr.Head) 
            // c1 is the (case=TRUE) circuit which goes to 1 of MUX, c2 goes to 0
            //that's why they are given in reverse order in the joinCircuits function 
            let c1,c2 = buildConditionalCircuit (Option.get expr.Tail)
            let topComp = buildExpressionComponent expr c1.OutWidth
            let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
            joinCircuits [c2;c1;c3] [topComp.InputPorts[0];topComp.InputPorts[1];topComp.InputPorts[2]] topCircuit
        | "SHIFT" ->
            buildShiftCircuit expr
        | "reduction" ->
            buildReductionAndLogicalCircuit expr
        | "logical_AND" | "logical_OR" ->
            let (c1:Circuit) = buildReductionAndLogicalCircuit (Option.get expr.Head) 
            let (c2:Circuit) = buildReductionAndLogicalCircuit (Option.get expr.Tail) 
            let topComp = buildExpressionComponent expr c1.OutWidth
            let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
            joinCircuits [c1;c2] [topComp.InputPorts[0];topComp.InputPorts[1]] topCircuit

        | _ ->  //everything else: bitwise gates and additive     
            let (c1:Circuit) = buildExpressionCircuit (Option.get expr.Head) 
            let (c2:Circuit) = buildExpressionCircuit (Option.get expr.Tail) 
            let topComp = buildExpressionComponent expr c1.OutWidth
            match expr.Type with
            |"additive" ->
                let inputB,cin =
                    match expr.Operator with
                    |Some "+" ->
                        let (tempNumber:NumberT) = {Type="";NumberType="";Bits=(Some "1");Base=(Some "'b");AllNumber=(Some "0");UnsignedNumber=None;Location=100} //location is Don't Care
                        c2,(createNumberCircuit tempNumber)

                    |Some "-" ->
                        let (tempNumber:NumberT) = {Type="";NumberType="";Bits=(Some "1");Base=(Some "'b");AllNumber=(Some "1");UnsignedNumber=None;Location=100} //location is Don't Care
                        let cinCircuit = createNumberCircuit tempNumber

                        let nBitsNotComp = createComponent (NbitsNot c2.OutWidth) "NOT"
                        let nBitsNotCircuit = {Comps=[nBitsNotComp];Conns=[];Out=nBitsNotComp.OutputPorts[0];OutWidth=c2.OutWidth}
                        let invertedCircuit = joinCircuits [c2] [nBitsNotComp.InputPorts[0]] nBitsNotCircuit
                        (invertedCircuit,cinCircuit)
                    |_ -> failwithf "Can't happen"

                let ioLabelComp = createComponent (Viewer 1) "Adder_Cout" 
                let conn = createConnection topComp.OutputPorts[1] ioLabelComp.InputPorts[0] 
                let topCircuit = {Comps=[topComp;ioLabelComp];Conns=[conn];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
                joinCircuits [cin;c1;inputB] [topComp.InputPorts[0];topComp.InputPorts[1];topComp.InputPorts[2]] topCircuit

            |_-> //bitwise gates               
                let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
                joinCircuits [c1;c2] [topComp.InputPorts[0];topComp.InputPorts[1]] topCircuit


    and buildUnaryCircuit (unary:UnaryT) =
        match unary.Type with
        |"primary" ->
            createPrimaryCircuit (Option.get unary.Primary) ioAndWireToCompMap
        |"number" ->
            createNumberCircuit (Option.get unary.Number)
        |"parenthesis" ->
            buildExpressionCircuit (Option.get unary.Expression)
        |"concat" ->
            buildUnaryListCircuit (Option.get unary.Expression)
        |_ -> failwithf "Can't happen"

    /// creates a list of unaries and merges them together using MergeWires
    /// used for concatenations
    and buildUnaryListCircuit (unaryList:ExpressionT) = 
        let head = buildExpressionCircuit (Option.get unaryList.Head)
        let list = 
            match Option.isSome unaryList.Tail with
            |true -> 
                let tail = buildUnaryListCircuit (Option.get unaryList.Tail)
                [head]@[tail]
            |false -> 
                [head]
    
        let length = List.length list
        list
        |> List.mapi (fun index circ -> 
            (circ,"",{MSB=(length-index);LSB=0;},OutputPort) //length-index to get them in correct order for joinWithMerge function
        )                                         //LSB and lhsType are don't cares in this case
        |> List.sortBy (fun (_,_,slice,_)->slice.MSB)
        |> joinWithMerge
        |> extractCircuit


    and buildConditionalCircuit (tail:ExpressionT) =
        let c1 = buildExpressionCircuit (Option.get tail.Head)
        let c2 = buildExpressionCircuit (Option.get tail.Tail)
        (c1,c2)


    and buildShiftCircuit (expr:ExpressionT) = 
        let operator = (Option.get expr.Operator)
        let tail = Option.get expr.Tail
        let unary = Option.get tail.Unary
        let number = Option.get unary.Number
        let shift = number.UnsignedNumber
        let shiftNo = int <| Option.get (shift)
        let (c1:Circuit) = buildExpressionCircuit (Option.get expr.Head)
    
        // check that shiftNo is smaller than the width of the unary being shifted
        // otherwise can't select the bits with BusSelect
        if shiftNo < c1.OutWidth then  
            let busSelComp = //keep the bits which will remain in the circuit after the shift
                match operator with
                |"<<" -> createComponent (BusSelection ((c1.OutWidth-shiftNo), 0)) ""
                |_ -> createComponent (BusSelection ((c1.OutWidth-shiftNo), shiftNo)) ""
    
            let busSelCircuit = {Comps=[busSelComp];Conns=[];Out=busSelComp.OutputPorts[0];OutWidth=(c1.OutWidth-shiftNo)}
            let selectedCircuit = joinCircuits [c1] [busSelComp.InputPorts[0]] busSelCircuit

            let constantCircuit =
                match operator with
                |">>" | "<<" -> //if logical shift, connect a constant of width=shift to the other side of MergeWires
                    let (tempNumber:NumberT) = {Type="";NumberType="";Bits=shift;Base=(Some "'b");AllNumber=(Some "0");UnsignedNumber=None;Location=100} //location is Don't Care
                    createNumberCircuit tempNumber
                |_ -> //CASE: ">>>" if arithmetic shift, use a bit-spreader with input the MSB and output width = shiftNo and connect that to MergeWires
                    let msbSelComp = createComponent (BusSelection (1,(c1.OutWidth-1))) "" 
                    let msbSelCircuit = {Comps=[msbSelComp];Conns=[];Out=msbSelComp.OutputPorts[0];OutWidth=1}
                    let msbCircuit = joinCircuits [c1] [msbSelComp.InputPorts[0]] msbSelCircuit
            
                    let spreaderComp = createComponent (NbitSpreader shiftNo) "SPREAD"
                    let spreaderCircuit = {Comps=[spreaderComp];Conns=[];Out=spreaderComp.OutputPorts[0];OutWidth=(shiftNo)}

                    joinCircuits [msbCircuit] [spreaderComp.InputPorts[0]] spreaderCircuit

            let inOrder =
                match operator with
                |"<<" -> [constantCircuit;selectedCircuit]
                |_ -> [selectedCircuit;constantCircuit]

            inOrder
            |> List.mapi(fun index circ ->
                (circ,"",{MSB=(index);LSB=0;},OutputPort)
            )
            |> joinWithMerge
            |> extractCircuit
        
        else // if shiftNo >= c1.OutWidth return a c1.OutWidth-width constant with value 0
            let (tempNumber:NumberT) = {Type="";NumberType="";Bits=(Some (string c1.OutWidth));Base=(Some "'b");AllNumber=(Some "0");UnsignedNumber=None;Location=100} //location is Don't Care
            createNumberCircuit tempNumber


    and buildReductionAndLogicalCircuit (expr:ExpressionT) =
        let (c1:Circuit) = buildUnaryCircuit (Option.get expr.Unary)
    
        // reductions are implemented with compares
        // (&) -> check that value is equal to (2^width - 1)
        // (|) -> check that value is NOT equal to 0
        // (!) -> check if equal to 0 (returns true if input is 0{false}, thus negates it)
        // Same with a not gate at the end for (~&,~|)
        let busCompareComp = 
            match expr.Operator with
                |Some "&" |Some "~&" -> createComponent (BusCompare (c1.OutWidth, (((2. ** c1.OutWidth)-1.) |> uint32))) "COMP"      
                |_ -> //Some "|" or Some "!" or Some "~|"
                    createComponent (BusCompare (c1.OutWidth,0u)) "COMP"

        let busCompareCircuit = {Comps=[busCompareComp];Conns=[];Out=busCompareComp.OutputPorts[0];OutWidth=1}

        match expr.Operator with
        |Some "&" | Some "~|" | Some "!" ->
            joinCircuits [c1] [busCompareComp.InputPorts[0]] busCompareCircuit
        |_ ->
            let comparedCircuit = joinCircuits [c1] [busCompareComp.InputPorts[0]] busCompareCircuit

            let notGateComp = createComponent Not "G"
            let notGateCircuit = {Comps=[notGateComp];Conns=[];Out=notGateComp.OutputPorts[0];OutWidth=1}
            joinCircuits [comparedCircuit] [notGateComp.InputPorts[0]] notGateCircuit
    

    buildExpressionCircuit expr



/////////   MAIN FUNCTION   //////////

let createSheet input = 
    let items = input.Module.ModuleItems.ItemList |> Array.toList
    let ioDecls = items |> List.filter (fun item -> Option.isSome item.IODecl)
    let assignments = items |> List.filter (fun item -> Option.isSome item.Statement) 
    let wiresLHS = collectWiresLHS assignments

    let ioToCompMap = getIOtoComponentMap ioDecls    

    // static map to search for input,wire components
    let ioAndWireToCompMap = 
        (ioToCompMap,wiresLHS) 
        ||> List.fold(fun map wire ->
            getWireToCompMap wire map
        )

    // list of circuits, one per assignment
    let perItemCircuits = 
        assignments
        |> List.map (fun item ->
            let assignment = (Option.get item.Statement).Assignment
            let circuit = mainExpressionCircuitBuilder assignment.RHS ioAndWireToCompMap
            let outPort = assignment.LHS.Primary.Name
            let bits = sliceFromBits assignment.LHS ioAndWireToCompMap
            let lhstype = 
                match (Option.get item.Statement).Assignment.Type with
                |"wire" -> Wire
                |"assign" -> OutputPort
                |_ -> failwithf "Can't happen" 
            (circuit,outPort,bits,lhstype)
        )

    // list of canvas states, one per output
    // here all slices are merged together with mergeWires to create one CanvasState per output
    // ex. assign out[5:4],assign out[3:1], assign out[0] -> CanvasState for output port: {out}
    let csList = 
        perItemCircuits
        |> List.groupBy (fun (_,portName,_,_) -> portName)
        |> List.map (fun (portName,circuits) ->
            let sorted = List.sortBy (fun (_,_,slice,_)->slice.MSB) circuits
            sorted
            |> joinWithMerge
            |> attachToOutput ioAndWireToCompMap
        )
    
    // concatenate canvasStates from csList
    // add input and wire components in CanvasState
    // (these are added now because they must appear only once in final Canvas State)
    // fix it by changing the label and position of components so that these are unique per component
    let finalCanvasState =
        match List.isEmpty csList with
        | true ->
            (collectInputAndWireComps ioAndWireToCompMap,[])
            |> fixCanvasState
        |false -> 
            csList
            |> List.reduce (fun cs1 cs2 -> concatenateCanvasStates cs1 cs2)
            |> concatenateCanvasStates (collectInputAndWireComps ioAndWireToCompMap,[])
            |> fixCanvasState

    finalCanvasState