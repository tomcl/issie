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

let findEmptyXY (oldMap: (string*Component) list) : XYPos = 
    let size  = List.length oldMap
    match size with
    |0 -> {X=100.;Y=100.}
    |_ ->
        let lastComp = snd oldMap[size-1]
        let prevX,prevY = lastComp.X,lastComp.Y
        {X=prevX+130.;Y=prevY+130.}


let getWidthFromRange (range:RangeT option) = 
    match range with
    |None -> 1
    |Some r ->
        let start = r.Start |> int
        start+1

let createComponent id compType (name:string) inputPorts outputPorts =
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

let createPort hostId portType portNumber =
    {
        Id = DrawHelpers.uuid()
        PortNumber = portNumber
        PortType = portType
        HostId = hostId
    }

let createConnection (source:Port) (target:Port) = 
    {
        Id = DrawHelpers.uuid()
        Source = source
        Target = target
        Vertices = []
    }


let extractWidth compType =
        match compType with
        |Output width -> width
        |Input1 (width,_) -> width
        |IOLabel -> 1
        |_ -> failwithf "Can't happen"

let extractCircuit (input:(Circuit*string*Slice*LHSType)) = 
    match input with
    |(c,_,_,_) -> c

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


let rec joinWithMerge (lst:(Circuit*string*Slice*LHSType) list) = 
    
    let merge2Circuits (c1:Circuit,name:string,slice:Slice,lhsType:LHSType) (c2:Circuit,name2:string,slice2:Slice,lhsType2:LHSType) = 
        let mergeWiresId = DrawHelpers.uuid();
        let inputPorts = 
            [
                createPort mergeWiresId PortType.Input (Some 0);
                createPort mergeWiresId PortType.Input (Some 1)
            ]
        let outputPorts =
            [
                createPort mergeWiresId PortType.Output (Some 0)
            ]

        let comp = createComponent mergeWiresId MergeWires "" inputPorts outputPorts
        let topCircuit = {Comps=[comp];Conns=[];Out=outputPorts[0];OutWidth=0}
        joinCircuits [c1;c2] [inputPorts[0];inputPorts[1]] topCircuit, name, slice,lhsType

    match List.length lst with 
    |1 -> lst[0]
    |2 -> merge2Circuits lst[0] lst[1]
    |_ ->
        let _,back = lst |> List.splitAt 2
        let m1 = merge2Circuits lst[0] lst[1]
        joinWithMerge (List.append [m1] back)


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
        let id = DrawHelpers.uuid()

        let inputPorts,outputPorts =
            match ioType with
            |"input_decl" -> [],[createPort id PortType.Output (Some 0)]
            |_ -> [createPort id PortType.Input (Some 0)],[]

        map@[(name,(createComponent id compType name inputPorts outputPorts))]
    )

  
let getIOtoComponentMap (ioDecls:ItemT list) = 
    ([],ioDecls)
    ||> List.fold (fun map item ->
        createIOComponent item item.ItemType map
    )
    |> Map.ofList


let getWireToCompMap (lhs:AssignmentLHST) ioAndWireToCompMap =
    let wireLabelId = DrawHelpers.uuid()
    let name = lhs.Primary.Name
    let inputPorts = [createPort wireLabelId PortType.Input (Some 0)]
    let outputPorts = [createPort wireLabelId PortType.Output (Some 0)]
    
    let wireComp = createComponent wireLabelId IOLabel name inputPorts outputPorts
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

let buildBusSelComponent outWidth outLSB = 
    let busSelCompId = DrawHelpers.uuid()
    let inputPorts = [createPort busSelCompId PortType.Input (Some 0)]
    let outputPorts = [createPort busSelCompId PortType.Output (Some 0)]
    createComponent busSelCompId (BusSelection (outWidth,outLSB)) "" inputPorts outputPorts

let buildBitSprederComponent width = 
    let bitSpreaderId = DrawHelpers.uuid()
    let inputPorts = [createPort bitSpreaderId PortType.Input (Some 0)]
    let outputPorts = [createPort bitSpreaderId PortType.Output (Some 0)]
    createComponent bitSpreaderId (NbitSpreader width) "SPREAD" inputPorts outputPorts

let buildBusCompareComponent busWidth compareValue = 
    let id = DrawHelpers.uuid();
    let inputPorts = [createPort id PortType.Input (Some 0)]
    let outputPorts = [createPort id PortType.Output (Some 0)]
    createComponent id (BusCompare (busWidth,compareValue)) "COMP" inputPorts outputPorts

let buildNotGate () = 
    let id = DrawHelpers.uuid();
    let inputPorts = [createPort id PortType.Input (Some 0)]
    let outputPorts = [createPort id PortType.Output (Some 0)]
    createComponent id Not "G" inputPorts outputPorts

let buildViewer name width =
    let id = DrawHelpers.uuid()
    let inputPorts = [createPort id PortType.Input (Some 0)]
    let outputPorts = []
    createComponent id (Viewer width) name inputPorts outputPorts

let buildNbitsNot width =
    let id = DrawHelpers.uuid()
    let inputPorts = [createPort id PortType.Input (Some 0)]
    let outputPorts = [createPort id PortType.Output (Some 0)]
    createComponent id (NbitsNot width) "NOT" inputPorts outputPorts

let buildExpressionComponent (rhs:ExpressionT) width =
    let compId = DrawHelpers.uuid()
    let inputPorts = 
        match rhs.Type with
        | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" ->
            [
                createPort compId PortType.Input (Some 0); 
                createPort compId PortType.Input (Some 1)
            ]
        | "negation" ->
            [createPort compId PortType.Input (Some 0)]
        | "conditional_cond" | "additive" ->
            [
                createPort compId PortType.Input (Some 0); 
                createPort compId PortType.Input (Some 1);
                createPort compId PortType.Input (Some 2)
            ]
        |_ -> [] //TODO
    let outputPorts =
        match rhs.Type with
        | "additive" ->
            [
                createPort compId PortType.Output (Some 0)
                createPort compId PortType.Output (Some 1)
            ]
        |_ -> [createPort compId PortType.Output (Some 0)] //TODO

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
    
    let baseName =  //TODO: GET from getPrefix
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

        
    let comp = createComponent compId compType baseName inputPorts outputPorts
    comp


/////// CIRCUIT CREATION ////////

let createPrimaryCircuit (primary:PrimaryT) (ioAndWireToCompMap:Map<string,Component>) =
        let name = primary.Primary.Name
        let inputComp = Map.find name ioAndWireToCompMap
        match Option.isNone primary.BitsStart with
        |true -> 
            let width = extractWidth inputComp.Type //TODO
            {Comps=[];Conns=[];Out=inputComp.OutputPorts[0];OutWidth=width}
        |false ->
            let bStart,bEnd = (int (Option.get primary.BitsStart)),(int (Option.get primary.BitsEnd))
            
            let lsb,outWidth = bEnd,(bStart-bEnd+1)
            let busSelCompId = DrawHelpers.uuid()
            let inputPorts = [createPort busSelCompId PortType.Input (Some 0)]
            let outputPorts = [createPort busSelCompId PortType.Output (Some 0)]

            let busSelComp = createComponent busSelCompId (BusSelection (outWidth,lsb)) "" inputPorts outputPorts

            let conn = createConnection inputComp.OutputPorts[0] busSelComp.InputPorts[0]     
            {Comps=[busSelComp];Conns=[conn];Out=busSelComp.OutputPorts[0];OutWidth=outWidth}


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
    
    let constId = DrawHelpers.uuid()
    let outputPorts = [createPort constId PortType.Output (Some 0)]
    let constComp = createComponent constId (Constant1 (width,constValue,text)) "C" [] outputPorts 
    {Comps=[constComp];Conns=[];Out=constComp.OutputPorts[0];OutWidth=width}


let rec buildExpressionCircuit (expr:ExpressionT) ioAndWireToCompMap = 
    match expr.Type with
    |"unary" -> buildUnaryCircuit (Option.get expr.Unary) ioAndWireToCompMap
    | "negation" ->
        let (c1:Circuit) = buildUnaryCircuit (Option.get expr.Unary) ioAndWireToCompMap
        let topComp = buildExpressionComponent expr c1.OutWidth
        let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
        joinCircuits [c1] [topComp.InputPorts[0]] topCircuit
    | "conditional_cond" -> 
        let (c3:Circuit) = buildExpressionCircuit (Option.get expr.Head) ioAndWireToCompMap 
        // c1 is the (case=TRUE) circuit which goes to 1 of MUX, c2 goes to 0
        //that's why they are given in reverse order in the joinCircuits function 
        let c1,c2 = buildConditionalCircuit (Option.get expr.Tail) ioAndWireToCompMap
        let topComp = buildExpressionComponent expr c1.OutWidth
        let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
        joinCircuits [c2;c1;c3] [topComp.InputPorts[0];topComp.InputPorts[1];topComp.InputPorts[2]] topCircuit
    | "SHIFT" ->
        buildShiftCircuit expr ioAndWireToCompMap
    | "reduction" ->
        buildReductionAndLogicalCircuit expr ioAndWireToCompMap
    | "logical_AND" | "logical_OR" ->
        let (c1:Circuit) = buildReductionAndLogicalCircuit (Option.get expr.Head) ioAndWireToCompMap 
        let (c2:Circuit) = buildReductionAndLogicalCircuit (Option.get expr.Tail) ioAndWireToCompMap 
        let topComp = buildExpressionComponent expr c1.OutWidth
        let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
        joinCircuits [c1;c2] [topComp.InputPorts[0];topComp.InputPorts[1]] topCircuit

    | _ ->  //everything else: bitwise gates      
        let (c1:Circuit) = buildExpressionCircuit (Option.get expr.Head) ioAndWireToCompMap 
        let (c2:Circuit) = buildExpressionCircuit (Option.get expr.Tail) ioAndWireToCompMap 
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

                    let nBitsNotComp = buildNbitsNot c2.OutWidth
                    let nBitsNotCircuit = {Comps=[nBitsNotComp];Conns=[];Out=nBitsNotComp.OutputPorts[0];OutWidth=c2.OutWidth}
                    let invertedCircuit = joinCircuits [c2] [nBitsNotComp.InputPorts[0]] nBitsNotCircuit
                    (invertedCircuit,cinCircuit)
                |_ -> failwithf "Can't happen"

            let ioLabelComp = buildViewer "AdderCout" 1
            let conn = createConnection topComp.OutputPorts[1] ioLabelComp.InputPorts[0] 
            let topCircuit = {Comps=[topComp;ioLabelComp];Conns=[conn];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
            joinCircuits [cin;c1;inputB] [topComp.InputPorts[0];topComp.InputPorts[1];topComp.InputPorts[2]] topCircuit

        |_->                
            let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
            joinCircuits [c1;c2] [topComp.InputPorts[0];topComp.InputPorts[1]] topCircuit

and buildUnaryCircuit (unary:UnaryT) ioAndWireToCompMap =
    match unary.Type with
    |"primary" ->
        createPrimaryCircuit (Option.get unary.Primary) ioAndWireToCompMap
    |"number" ->
        createNumberCircuit (Option.get unary.Number)
    |"parenthesis" ->
        buildExpressionCircuit (Option.get unary.Expression) ioAndWireToCompMap
    |"concat" ->
        buildUnaryListCircuit (Option.get unary.Expression) ioAndWireToCompMap
    |_ -> failwithf "Can't happen"

and buildUnaryListCircuit (unaryList:ExpressionT) ioAndWireToCompMap = 
    let head = buildExpressionCircuit (Option.get unaryList.Head) ioAndWireToCompMap
    let list = 
        match Option.isSome unaryList.Tail with
        |true -> 
            let tail = buildUnaryListCircuit (Option.get unaryList.Tail) ioAndWireToCompMap
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

and buildConditionalCircuit (tail:ExpressionT) ioAndWireToCompMap =
    let c1 = buildExpressionCircuit (Option.get tail.Head) ioAndWireToCompMap
    let c2 = buildExpressionCircuit (Option.get tail.Tail) ioAndWireToCompMap
    (c1,c2)

and buildShiftCircuit (expr:ExpressionT) ioAndWireToCompMap = 
    let operator = (Option.get expr.Operator)
    let tail = Option.get expr.Tail
    let unary = Option.get tail.Unary
    let number = Option.get unary.Number
    let shift = number.UnsignedNumber
    let shiftNo = int <| Option.get (shift)
    let (c1:Circuit) = buildExpressionCircuit (Option.get expr.Head) ioAndWireToCompMap
    
    match operator with
    |">>" ->
        let busSelComp = buildBusSelComponent (c1.OutWidth-shiftNo) shiftNo
        let busSelCircuit = {Comps=[busSelComp];Conns=[];Out=busSelComp.OutputPorts[0];OutWidth=(c1.OutWidth-shiftNo)}
        let SelectedCircuit = joinCircuits [c1] [busSelComp.InputPorts[0]] busSelCircuit
        let (tempNumber:NumberT) = {Type="";NumberType="";Bits=shift;Base=(Some "'b");AllNumber=(Some "0");UnsignedNumber=None;Location=100} //location is Don't Care
        let constantCircuit = createNumberCircuit tempNumber

        [SelectedCircuit;constantCircuit]
        |> List.mapi(fun index circ ->
            (circ,"",{MSB=(index);LSB=0;},OutputPort)
        )
        // |> List.sortBy (fun (_,_,slice,_)->slice.MSB)
        |> joinWithMerge
        |> extractCircuit
    |"<<" ->
        let busSelComp = buildBusSelComponent (c1.OutWidth-shiftNo) 0
        let busSelCircuit = {Comps=[busSelComp];Conns=[];Out=busSelComp.OutputPorts[0];OutWidth=(c1.OutWidth-shiftNo)}
        let SelectedCircuit = joinCircuits [c1] [busSelComp.InputPorts[0]] busSelCircuit
        let (tempNumber:NumberT) = {Type="";NumberType="";Bits=shift;Base=(Some "'b");AllNumber=(Some "0");UnsignedNumber=None;Location=100} //location is Don't Care
        let constantCircuit = createNumberCircuit tempNumber

        [constantCircuit;SelectedCircuit]
        |> List.mapi(fun index circ ->
            (circ,"",{MSB=(index);LSB=0;},OutputPort)
        )
        // |> List.sortBy (fun (_,_,slice,_)->slice.MSB)
        |> joinWithMerge
        |> extractCircuit
    
    | _ -> 
        let busSelComp = buildBusSelComponent (c1.OutWidth-shiftNo) shiftNo
        let busSelCircuit = {Comps=[busSelComp];Conns=[];Out=busSelComp.OutputPorts[0];OutWidth=(c1.OutWidth-shiftNo)}
        let SelectedCircuit = joinCircuits [c1] [busSelComp.InputPorts[0]] busSelCircuit
        
        let msbSelComp = buildBusSelComponent (1) (c1.OutWidth-1)
        let msbSelCircuit = {Comps=[msbSelComp];Conns=[];Out=msbSelComp.OutputPorts[0];OutWidth=1}
        let msbCircuit = joinCircuits [c1] [msbSelComp.InputPorts[0]] msbSelCircuit
        
        let spreaderComp = buildBitSprederComponent shiftNo
        let spreaderCircuit = {Comps=[spreaderComp];Conns=[];Out=spreaderComp.OutputPorts[0];OutWidth=(shiftNo)}

        let constantCircuit = joinCircuits [msbCircuit] [spreaderComp.InputPorts[0]] spreaderCircuit

        [SelectedCircuit;constantCircuit]
        |> List.mapi(fun index circ ->
            (circ,"",{MSB=(index);LSB=0;},OutputPort)
        )
        // |> List.sortBy (fun (_,_,slice,_)->slice.MSB)
        |> joinWithMerge
        |> extractCircuit

and buildReductionAndLogicalCircuit (expr:ExpressionT) ioAndWireToCompMap =
    let (c1:Circuit) = buildUnaryCircuit (Option.get expr.Unary) ioAndWireToCompMap
    
    let busCompareComp = 
        match expr.Operator with
            |Some "&" |Some "~&" ->       
                buildBusCompareComponent c1.OutWidth (((2. ** c1.OutWidth)-1.) |> uint32)
            |_ -> //Some "|" or Some "!" or Some "~|"
                buildBusCompareComponent c1.OutWidth 0u

    let busCompareCircuit = {Comps=[busCompareComp];Conns=[];Out=busCompareComp.OutputPorts[0];OutWidth=1}

    match expr.Operator with
    |Some "&" | Some "~|" | Some "!" ->
        joinCircuits [c1] [busCompareComp.InputPorts[0]] busCompareCircuit
    |_ ->
        let comparedCircuit = joinCircuits [c1] [busCompareComp.InputPorts[0]] busCompareCircuit

        let notGateComp = buildNotGate ()
        let notGateCircuit = {Comps=[notGateComp];Conns=[];Out=notGateComp.OutputPorts[0];OutWidth=1}
        joinCircuits [comparedCircuit] [notGateComp.InputPorts[0]] notGateCircuit



/////////   MAIN FUNCTION   //////////



let createSheet input = 
    let items = input.Module.ModuleItems.ItemList |> Array.toList
    let ioDecls = items |> List.filter (fun item -> Option.isSome item.IODecl)
    let assignments = items |> List.filter (fun item -> Option.isSome item.Statement) 
    let wiresLHS = collectWiresLHS assignments

    let ioToCompMap = getIOtoComponentMap ioDecls    

    let ioAndWireToCompMap =
        (ioToCompMap,wiresLHS) 
        ||> List.fold(fun map wire ->
            getWireToCompMap wire map
        )

    // let ioAndWireComps = ioAndWireToCompMap |> Map.toList |> List.map snd

    let perItemCircuits = 
        assignments
        |> List.map (fun item ->
            let assignment = (Option.get item.Statement).Assignment
            let circuit = buildExpressionCircuit assignment.RHS ioAndWireToCompMap
            let outPort = assignment.LHS.Primary.Name
            let bits = sliceFromBits assignment.LHS ioAndWireToCompMap
            let lhstype = 
                match (Option.get item.Statement).Assignment.Type with
                |"wire" -> Wire
                |"assign" -> OutputPort
                |_ -> failwithf "Can't happen" 
            (circuit,outPort,bits,lhstype)
        )

    let finalCanvasState = 
        perItemCircuits
        |> List.groupBy (fun (_,portName,_,_) -> portName)
        |> List.map (fun (portName,circuits) ->
            let sorted = List.sortBy (fun (_,_,slice,_)->slice.MSB) circuits
            sorted
            |> joinWithMerge
            |> attachToOutput ioAndWireToCompMap
        )
        |> List.reduce (fun cs1 cs2 -> concatenateCanvasStates cs1 cs2)
        |> concatenateCanvasStates (collectInputAndWireComps ioAndWireToCompMap,[])
        |> fixCanvasState

    Helpers.JsonHelpers.stateToJsonString (finalCanvasState, None)
