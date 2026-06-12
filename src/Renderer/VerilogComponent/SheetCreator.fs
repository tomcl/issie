module SheetCreator
open EEExtensions
open VerilogTypes
open CommonTypes
open DrawHelpers
open Helpers
open NumberHelpers
open VerilogAST
open FilesIO
open NearleyBindings
open Fable.SimpleJson
open Fable.Core.JsInterop
open MenuHelpers

NearleyBindings.importGrammar
NearleyBindings.importFix
NearleyBindings.importParser


open ErrorCheck
open ErrorCheckHelpers
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


type BitMapping = {
    Slice: Slice;
    Circuit: Circuit;
    LHSType: LHSType
}
/////// HELPERS ////////

/// Helper function to find a port's width from the range definition of IODecl
let getWidthFromRange (range:Range option) (paramMap: Map<string, int>) = 
    match range with
    |None -> 1
    |Some r ->
        let start = evalExprWithParams r.Start paramMap
        start+1

/// Create a component (type: Component) based on the parameters given
let createComponent' id compType (name:string) inputPorts outputPorts =
    {
        Id = id
        Type = compType
        Label = name.ToUpper()
        InputPorts = inputPorts 
        OutputPorts = outputPorts
        SlotInfo = None
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
        |NbitsAnd _ |NbitsOr _ |NbitsXor _ |Shift _ | MergeWires
            -> 2,1
        |Mux2 
            -> 3,1
        |NbitsAdder _
            -> 3,2
        |GateN (_, n) -> n, 1
        |Input _ |Input1 (_,_)| Constant1 (_,_,_)
            -> 0,1
        | Register _ -> 1,1
        | CounterNoEnableLoad _ -> 0,1
        | AsyncROM1 _ -> 1,1
        | RAM1 _ -> 3,1
        | Custom custom -> List.length custom.InputLabels, List.length custom.OutputLabels
        |_ -> failwithf $"Undefined component properties {compType}"
    
    let id = DrawHelpers.uuid()
    let inputPorts = createPortList PortType.Input inputPortNo id
    let outputPorts = createPortList PortType.Output outputPortNo id
    
    createComponent' id compType name inputPorts outputPorts



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

let merge2Circuits c1 c2 =
    let comp = createComponent MergeWires "" 
    let topCircuit = {Comps=[comp];Conns=[];Out=comp.OutputPorts[0];OutWidth=c1.OutWidth+c2.OutWidth}
    joinCircuits [c1;c2] [comp.InputPorts[0];comp.InputPorts[1]] topCircuit

let joinWithMerge' circuits =
    (List.head circuits, List.tail circuits)
    ||> List.fold merge2Circuits
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
let sliceFromBits (lhs:AssignmentLHS) (ioAndWireToCompMap: Map<string,Component>) varSizeMap paramMap = 
    // match Map.tryFind outPort arraySizeMap with 
    // | None ->
    // Assumption: bit-select ranges are constants when evaluated here.
    match getPrimaryRange lhs.PrimaryType paramMap with
    | Some (bStart, bEnd) -> {MSB = bStart; LSB = bEnd}
    | None ->
        let width = Map.find (getPrimaryName lhs.PrimaryType) varSizeMap // TO DO: make it TryFind
        //let width = extractWidth comp.Type
        {MSB = (width-1); LSB=0}
    // | Some (size, dims) -> 
    //     match getPrimaryRange lhs.PrimaryType with
    //     | Some (bStart, bEnd) -> {MSB = bStart; LSB = bEnd}
    //     | None ->
    //         let width = Map.find (getPrimaryName lhs.PrimaryType) varSizeMap // TO DO: make it TryFind
    //         //let width = extractWidth comp.Type
    //         {MSB = (width-1); LSB=0}

let rec evalExprWithVarsAndParams (expr: ExpressionDU) (paramMap: Map<string, int>) (varSizeMap: Map<string, int>): int =
    let rec strip (expr: ExpressionDU) =
        match expr with
        | ExpressionDU.Unary (Parenthesis e) -> strip e
        | _ -> expr
    let expr = strip expr

    match expr with
    | ExpressionDU.UnaryUnsigned n ->  tryEvalConst n
    | ExpressionDU.Negation (UnaryDU.Number n) -> -tryEvalConst n
    | ExpressionDU.Unary (UnaryDU.Number n) -> tryEvalConst n

    | ExpressionDU.Unary (UnaryDU.Primary (Identifier id)) ->
        match Map.tryFind id.Name paramMap with
        | Some v -> v
        | None -> 
            match Map.tryFind id.Name varSizeMap with
            | Some a -> a
            | None -> failwithf "Undefined identifier"
    
    | Additive (op, lhs, rhs) ->
        let l = evalExprWithVarsAndParams lhs paramMap varSizeMap
        let r = evalExprWithVarsAndParams rhs paramMap varSizeMap
        match op with
        | Plus -> l + r
        | Minus -> l - r
        | _ -> failwith "Unsupported additive operator for parameter evaluation"
    | Multiplicative (op, lhs, rhs) ->
        let l = evalExprWithVarsAndParams lhs paramMap varSizeMap
        let r = evalExprWithVarsAndParams rhs paramMap varSizeMap
        match op with
        | Mult -> l * r
        | _ -> failwith "Unsupported multiplicative operator for parameter evaluation"
    | ShiftExpr (op, lhs, rhs) ->
        let l = evalExprWithVarsAndParams lhs paramMap varSizeMap
        let r = evalExprWithVarsAndParams rhs paramMap varSizeMap
        match op with
        | Sll -> l <<< r
        | Srl -> l >>> r
        | Sra -> l >>> r 
        | _ -> failwith "Unsupported shift operator for parameter evaluation"
    | BitwiseAnd (lhs, rhs) ->
        let l = evalExprWithVarsAndParams lhs paramMap varSizeMap
        let r = evalExprWithVarsAndParams rhs paramMap varSizeMap
        l &&& r
    | BitwiseOr (lhs, rhs) ->
        let l = evalExprWithVarsAndParams lhs paramMap varSizeMap
        let r = evalExprWithVarsAndParams rhs paramMap varSizeMap
        l ||| r
    | BitwiseXor (lhs, rhs) ->
        let l = evalExprWithVarsAndParams lhs paramMap varSizeMap
        let r = evalExprWithVarsAndParams rhs paramMap varSizeMap
        l ^^^ r
    | _ -> failwith "Expression does not evaluate to a constant integer or parameter reference"

let sliceFromWordArray (lhs:AssignmentLHS) (varSizeMap: Map<string, int>) (arraySizeMap: Map<string, int * int array>) = 
    match lhs.PrimaryType with
    | Identifier _ -> failwithf "Not allowed to assign to whole array"
    | IdentifierBits (_, _, _) -> failwithf "Not allowed to assign to multiple arrays at once"
    | IdentifierBitsSelect (_, _, _, _) -> failwithf "Not allowed for arrays"
    | IdentifierBit (id, arrayElem) ->
        arrayElem
    | VariableBitSelect (id, expr) ->
        let arrayElem = evalExpr expr
        arrayElem
    | IdentifierArray (id, arrayDims, bitStart, bitEnd) ->
        let arrayElem = arrayDims[0]
        match arrayElem with
        | ConstArraySelect idx -> idx
        | VarArraySelect idx -> evalExpr idx
    | VariableArrayBitSel (id, arrayDims, index) ->
        let arrayElem = arrayDims[0]
        match arrayElem with
        | ConstArraySelect idx -> idx
        | VarArraySelect idx -> evalExpr idx
            // let width = evalExprWithVarsAndParams idx Map.empty varSizeMap //TODO: pass param map properly? check and see if its fully evaled before this pt
            // {MSB = (width-1); LSB=0}

let sliceFromBitsArray (lhs:AssignmentLHS) (arraySizeMap: Map<string, int * int array>) = 
    match lhs.PrimaryType with
    | Identifier _ -> failwithf "Not allowed to assign to whole array"
    | IdentifierBits (_, _, _) -> failwithf "Not allowed to assign to multiple arrays at once"
    | IdentifierBitsSelect (_, _, _, _) -> failwithf "Not allowed for arrays"
    | IdentifierBit (id, index) ->
        let (arrayWidth, _) = Map.find id.Name arraySizeMap
        {MSB = arrayWidth-1; LSB = 0}
    | VariableBitSelect (id, expr) ->
        let (arrayWidth, _) = (Map.find id.Name arraySizeMap)
        {MSB = arrayWidth-1; LSB = 0}
    | IdentifierArray (id, arrayDims, bitStart, bitEnd) ->
        // let arrayElem = evalExpr arrayDims[0] // TODO: expand beyond 2d arrays
        {MSB = bitStart; LSB = bitEnd}
    | VariableArrayBitSel (id, arrayDims, index) -> failwithf "Shouldnt be used here"
    
let sliceFromBitsPrimary (primary: PrimaryDU) (ioAndWireToCompMap: Map<string,Component>) varSizeMap paramMap = 
    // Assumption: bit-select ranges are constants when evaluated here.
    match getPrimaryRange primary paramMap with
    | Some (bStart, bEnd) -> {MSB = bStart; LSB = bEnd}
    | None ->
        let width = Map.find (getPrimaryName primary) varSizeMap // TO DO: make it TryFind
        //let width = extractWidth comp.Type
        {MSB = (width-1); LSB=0}

/// Attach the merged circuits to the correct output port 
let attachToOutput' (ioAndWireToCompMap: Map<string,Component>) (ioToCompMap: Map<string, Component>) (circuit:Circuit,portName:string,slice:Slice,lhsType:LHSType) : CanvasState =
    let outputOrWire = Map.find portName ioAndWireToCompMap // always a wirelabel
    let conn = createConnection circuit.Out outputOrWire.InputPorts[0]
    

    let allComps, allConns = 
        match lhsType with
        |OutputPort ->
            let outputPort =  Map.find portName ioToCompMap
            let conn' = createConnection outputOrWire.OutputPorts[0] outputPort.InputPorts[0]
            circuit.Comps@[outputPort], circuit.Conns@[conn; conn']
        |Wire -> circuit.Comps, circuit.Conns@[conn] // why don't we need to add outputOrWire to components here?
    (allComps,allConns)

/// Attach the merged circuits to the correct output port 
let attachToOutput (ioAndWireToCompMap: Map<string,Component>) (ioToCompMap: Map<string, Component>) (circuit:Circuit) (portName:string) : CanvasState =
    // let outputOrWire = Map.find portName ioAndWireToCompMap // always a wirelabel
    let outputOrWire = 
        match Map.tryFind portName ioAndWireToCompMap with
        | Some comp -> comp
        | None -> createComponent IOLabel portName // probably loop variable
        // | None -> []
    let conn = createConnection circuit.Out outputOrWire.InputPorts[0]
    

    let allComps, allConns = 
        match Map.tryFind portName ioToCompMap with
        | Some outputPort ->
            let conn' = createConnection outputOrWire.OutputPorts[0] outputPort.InputPorts[0]
            circuit.Comps@[outputOrWire;outputPort], circuit.Conns@[conn; conn']
        | _ -> circuit.Comps@[outputOrWire], circuit.Conns@[conn] // double check this
    (allComps,allConns)

let attachArrayToOutput (arrayToCompMap: Map<string, Component list>) (circuitList: Circuit list) (portName: string) : CanvasState =
    let arrayComps = Map.find portName arrayToCompMap
    let comps =
        circuitList
        |> List.collect (fun circuit -> circuit.Comps)
        |> List.append arrayComps
    let conns =
        ([], List.zip circuitList arrayComps)
        ||> List.fold (fun acc (circuit, arrayComp) ->
            acc @ (circuit.Conns @ [createConnection circuit.Out arrayComp.InputPorts[0]])
        )
    
    comps, conns

let concatenateCanvasStates (mainCS: CanvasState) (newCS:CanvasState) : CanvasState =
    ((fst mainCS)@(fst newCS) |> List.distinct,(snd mainCS)@(snd newCS) |> List.distinct)
    

let dfsTraversal (graph: Map<string, List<string>>) (componentMap: Map<string, Component>) (connections: List<Connection>) (parents: Set<string> )=
    let rec dfsHelper name (visited, (compMap: Map<string, Component>), conns) currentNode =
        if Set.contains currentNode visited then
            visited, compMap, conns // Node has already been visited, skip it
        else
            // Mark the current node as visited
            let newVisited = Set.add currentNode visited
            // Recursively traverse unvisited neighbors
            let neighbors = graph.TryFind currentNode |> Option.defaultValue []
            let unvisitedNeighbors =
                neighbors
                |> List.filter (fun neighbor -> not (Set.contains neighbor visited))
            // for each neighbour, remove connection between them
            let conns' =
                (conns, neighbors)
                ||> List.fold (fun c neighbor ->
                    c |> List.filter (fun conn -> not (conn.Source.HostId=currentNode && conn.Target.HostId=neighbor)) 
                )
            // rename current node to name
            let currComp = {Map.find currentNode compMap with Label=name} 
            let compMap' = Map.add currentNode currComp compMap
            List.fold (dfsHelper name) (newVisited, compMap', conns') unvisitedNeighbors  
    let _, componentMap', connections' =
        ((Set.empty, componentMap, connections), parents)
        ||> Set.fold (fun (v, compmap, conns) startNode -> 
            let name = (Map.find startNode componentMap).Label
            dfsHelper name (v, compmap, conns) startNode)
    componentMap', connections'

let fixConsecutiveWires (oldCanvasState: CanvasState) =
    let componentMap = // create component id to component map
        fst oldCanvasState
        //|> List.filter(fun comp -> comp.Type = IOLabel)
        |> List.map(fun comp -> comp.Id, comp)
        |> Map.ofList
    
    let componentIds =
        componentMap |> Map.keys |> Set.ofSeq

    snd oldCanvasState
    |> List.iter (fun conn ->
        if not (Set.contains conn.Source.HostId componentIds) then
            printfn "Missing source component %s in connection %A"
                conn.Source.HostId conn

        if not (Set.contains conn.Target.HostId componentIds) then
            printfn "Missing target component %s in connection %A"
                conn.Target.HostId conn
    )

    // get connections between wires
    let wireConns = 
        snd oldCanvasState
        |> List.filter(fun conn -> 
            // printfn "component map %A" componentMap
            // printfn "source: %A, target: %A" conn.Source.HostId conn.Target.HostId
            let src = Map.tryFind conn.Source.HostId componentMap
            let dst = Map.find conn.Target.HostId componentMap
            match src with
            | None -> 
                printfn "component map %A" componentMap
                // printfn "source %A" conn.Source.
                failwithf "source: %A, target: %A" conn.Source.HostId conn.Target.HostId
            | Some a -> a.Type = IOLabel && dst.Type = IOLabel
            // let src, dst = (Map.find conn.Source.HostId componentMap), (Map.find conn.Target.HostId componentMap)
            // src.Type = IOLabel && dst.Type = IOLabel
        )
    let wires =
        componentMap
        |> Map.filter(fun k v -> v.Type =IOLabel)
        |> Map.keys
        |> Set.ofSeq
    // build dependency graph + find root nodes
    let graph, parents =
        ((Map.empty, wires), wireConns)
        ||> List.fold (fun ((graph:Map<string,List<string>>), parents) conn ->
            let currDeps = (Option.defaultValue [] (Map.tryFind conn.Source.HostId graph))
            let graph' = Map.add conn.Source.HostId (currDeps@[conn.Target.HostId]) graph
            let parents' = Set.remove conn.Target.HostId parents
            graph', parents'
            )
    let componentMap', connections = dfsTraversal graph componentMap (snd oldCanvasState) parents
    componentMap'.Values|>List.ofSeq, connections

/// Helper function to resolve conflicts in labels (must be distinct) 
/// and component locations on canvas (must not overlap_)
let fixCanvasState (oldCanvasState:CanvasState) =
    let fixedComps =
        oldCanvasState
        |> fst
        |> List.mapi (fun i comp ->
            let newLabel = 
                match comp.Type with
                |Input1 _| Output _ ->
                    comp.Label
                |_ ->
                    match comp.Label with 
                    |"" -> "" 
                    |_ -> "_" + comp.Label+(string i)
            let x,y = (float (i+1)*120.),(float (i+1)*120.)
            {comp with Label=newLabel;X=x;Y=y}
        )
    (fixedComps,snd oldCanvasState)
    |> fixConsecutiveWires
/////// STATIC MAP CREATION ////////

let createIOComponent (item:ItemDU) ioType (oldMap) (paramMap: Map<string, int>)  =  
    let ioItem =
        match item with 
        | ItemDU.IOItem ioItem -> ioItem
        | _ -> failwithf "Should not happen! Expected IOItemDU"
    let width = getWidthFromRange ioItem.Range paramMap
    let compType = 
        match ioType with
        | InputDecl -> Input1 (width,Some 0I)
        |_ -> Output width

    let names =
        ioItem.Variables 
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
let getIOtoComponentMap (ioDecls:ItemDU list) (paramMap: Map<string, int>) = 
    ([],ioDecls)
    ||> List.fold (fun map item ->
        match item with
        | ItemDU.IOItem it -> createIOComponent item it.DeclarationType map paramMap
        | _ -> map
    )
    |> Map.ofList

let isOutputComponent (comp: Component) =
    match comp.Type with
    | Output _ -> true
    | _ -> false

/// Return a Map<string,Component> for wires
/// where string -> wire name.
/// It is necessary in order to find wire components when building circuits for assignments
let getWireToCompMap (lhs:AssignmentLHS) ioAndWireToCompMap =
    let name = lhs.PrimaryType |> getPrimaryName
    
    let wireComp = createComponent IOLabel name
    Map.add name wireComp ioAndWireToCompMap


let collectWiresLHS (assignments:ItemDU list) =
    let wires = assignments |> List.filter (function ItemDU.ContStatement {StatementType=ContStatementDU.Wire} -> true | _ -> false)
    wires
    |> List.map (function (ItemDU.ContStatement {Assignment={LHS=lhs}}) -> lhs | _ -> failwithf "Expected ContStatement with LHS")

let collectInputAndWireComps (ioAndWireToCompMap:Map<string,Component>) =
    ioAndWireToCompMap
    |> Map.toList
    |> List.map snd
    |> List.filter (fun comp ->
        match comp.Type with
        |Input1 (_,_) |IOLabel -> true
        |_ -> false
    )

// let collectArrayComps (arrayToCompMap: Map<string, Component list>) = 
//     arrayToCompMap
//     |> Map.toList
//     |> List.collect snd
//     |
    

/////// COMPONENT CREATION ////////
/// 
/// stores the expression along with the self determined width (MinWidth) and the contect determined Width
type ExprKind =
    | EUnary
    | ENegation
    | EConditionalCond
    | EConditionalResult
    | EShift
    | EReduction
    | ELogicalAnd
    | ELogicalOr
    | EEquality
    | EComparison
    | EUnaryUnsigned
    | EBitwiseAnd
    | EBitwiseOr
    | EBitwiseXor
    | EBitwiseXnor
    | EAdditive
    | EMultiplicative

type UnaryKind =
    | UPrimary
    | UNumber
    | UParenthesis
    | UConcat

type ExpressionCompilable = {Type: ExprKind; Operator: OperatorDU option; Head: ExpressionCompilable option; Tail: ExpressionCompilable option; Unary: UnaryCompilable option; Width: int}
    and UnaryCompilable = {Type: UnaryKind; Primary: PrimaryDU option; Number: Number option; Expression: ExpressionCompilable option; Width: int }

/// Extract component type and name from expression (type ExpressionT)
/// Create the component using the createComponent function
let buildExpressionComponent (rhs:ExpressionCompilable) width =
    
    let compType =
        match rhs.Type with
        | ENegation -> (NbitsNot width)  
        | EBitwiseOr -> (NbitsOr width)
        | EBitwiseXor -> (NbitsXor (width, None))
        | EBitwiseXnor -> (NbitsXor (width, None))
        | EBitwiseAnd -> (NbitsAnd width)
        | EAdditive -> (NbitsAdder width)
        | EConditionalCond -> (Mux2)
        | ELogicalAnd -> (GateN (And, 2)) 
        | ELogicalOr -> (GateN (Or, 2))
        | EMultiplicative -> (NbitsXor (width, Some Multiply))
        |_ -> failwithf "Missing component(?) in buildExpressionComponent" 
    
    let baseName = 
        match rhs.Type with
        | EBitwiseOr -> "OR"
        | EBitwiseXor -> "NXOR"
        | EBitwiseXnor -> "NXOR"
        | EAdditive -> "ADD"
        | EBitwiseAnd -> "AND"
        | ENegation -> "NOT"
        | EConditionalCond -> "MUX"
        | ELogicalAnd -> "G"
        | ELogicalOr -> "G"
        | EMultiplicative -> "MULT"
        |_ -> failwithf "Missing component(?) in buildExpressionComponent" 

        
    createComponent compType baseName


/////// CIRCUIT CREATION ////////
/// 
/// Finds the correct component based on the name of input/wire
/// creates a circuit with that component (and if required a busSel component
/// connected to it to return the correct slice) and returns that circuit
let createPrimaryCircuit (primary:PrimaryDU) (ioAndWireToCompMap:Map<string,Component>) varSizeMap (paramMap:Map<string, int>)=
        let name = getPrimaryName primary
        // match Map.tryFind name paramMap with
        // | Some paramValue -> // act as if it is a constant number
        //     let constComp = createComponent (Constant1 (1, paramValue |> bigint, string paramValue)) name
        //     {Comps=[constComp];Conns=[];Out=constComp.OutputPorts[0];OutWidth=32}
        // | None ->
        let inputComp = Map.find name ioAndWireToCompMap
        // match primary with
        // | VariableBitSelect (_, expr) ->
        //     let index = evalExprWithParams expr paramMap
        //     let busSelComp = createComponent (BusSelection (1, index)) ""
        //     let conn = createConnection inputComp.OutputPorts[0] busSelComp.InputPorts[0]
        //     {Comps=[busSelComp];Conns=[conn];Out=busSelComp.OutputPorts[0];OutWidth=1}
        // | _ ->
        match getPrimaryRange primary paramMap with
        | None -> 
            //let width = extractWidth inputComp.Type
            let width = Map.find name varSizeMap
            {Comps=[];Conns=[];Out=inputComp.OutputPorts[0];OutWidth=width}
        | Some (bStart, bEnd) ->
            let lsb,outWidth = bEnd,(bStart-bEnd+1)
            
            let busSelComp = createComponent (BusSelection (outWidth,lsb)) ""

            let conn = createConnection inputComp.OutputPorts[0] busSelComp.InputPorts[0]     
            {Comps=[busSelComp];Conns=[conn];Out=busSelComp.OutputPorts[0];OutWidth=outWidth}

/// Creates the correct component based on the number and returns a circuit with that component
let createNumberCircuit (number:Number) =
    let width, text =
        match number with
        | Unsigned (n, _) -> 32, string n
        | All (bits, numBase, allNumber, _) ->
            let digits = string allNumber
            let text = 
                match numBase with
                | Binary -> "0b"+digits
                | Hex -> "0x"+digits
                | Decimal -> digits
            bits, text
    // printf "widht: %i, text: %s" width text
    let constValue =
        match NumberHelpers.strToIntCheckWidth width text with
        |Ok n -> n
        |Error _ -> failwithf "Shouldn't happen!"
    
    let constComp = createComponent (Constant1 (width, constValue,text)) "C"
    {Comps=[constComp];Conns=[];Out=constComp.OutputPorts[0];OutWidth=width}
// handling size extension / 0 padding
// new expression type that stores width at each node (later signedness) "self determined"
// pass through this and update all context determined expression widths (passing in a parameter context)

/// Creates component for array access
let createArrayCircuit (primary:PrimaryDU) (ioAndWireToCompMap:Map<string,Component>) (arraySizeMap: Map<string, (int * int array)>) (arrayCompList: Component list) =
    let name = getPrimaryName primary
    // let inputComp = Map.find name arraySizeMap
    let arrayElem, bitsStart, bitsEnd, vbs = 
        match primary with
        | Identifier id -> failwithf "Currently entire array assignment not supported"
        | IdentifierBits (_, _, _) -> failwithf "Arrays cannot be assigned with bit slices"
        | IdentifierBitsSelect (_, _, _, _) -> failwithf "Arrays cannot be assigned with bit slices"
        | IdentifierBit (id, index) -> 
            let arrayElem = index
            // let arrayWidth, arrayDims = Map.find id.Name arraySizeMap
            arrayElem, None, None, None
        | VariableBitSelect (id, expr) ->
            // let arrayElem = Map.find name varSizeMap
            let arrayElem = evalExpr expr
            // let arrayWidth, arrayDims = Map.find id.Name arraySizeMap
            arrayElem, None, None, None
        | IdentifierArray (id, arrayDims, bitsStart, bitsEnd) ->
            let arrayElem = 
                match arrayDims[0] with 
                | ConstArraySelect a -> a
                | VarArraySelect v -> evalExpr v
            // TODO: expand beyond 2d arrays
            // let arrayWidth, arrayDims = Map.find id.Name arraySizeMap
            arrayElem, Some bitsStart, Some bitsEnd, None
        | VariableArrayBitSel (id, arrayDims, expr) ->
            let arrayElem = 
                match arrayDims[0] with 
                | ConstArraySelect a -> a
                | VarArraySelect v -> evalExpr v
            arrayElem, None, None, Some expr

    let arrayReg = arrayCompList[arrayElem]
    let arrayWidth, arrayDims = Map.find name arraySizeMap

    match bitsStart, bitsEnd, vbs with
    | None, None, None -> 
        {Comps=[];Conns=[];Out=arrayReg.OutputPorts[0];OutWidth=arrayWidth}
    | Some bStart, Some bEnd, None -> 
        let lsb, outWidth = bEnd, (bStart-bEnd+1)
        let busSelComp = createComponent (BusSelection (outWidth,lsb)) ""
        let conn = createConnection arrayReg.OutputPorts[0] busSelComp.InputPorts[0]
        {Comps=[busSelComp];Conns=[conn];Out=busSelComp.OutputPorts[0];OutWidth=outWidth}
    | None, None, Some expr ->
        let lsb, outWidth = evalExpr expr, 1
        let busSelComp = createComponent (BusSelection (outWidth,lsb)) ""
        let conn = createConnection arrayReg.OutputPorts[0] busSelComp.InputPorts[0]
        {Comps=[busSelComp];Conns=[conn];Out=busSelComp.OutputPorts[0];OutWidth=outWidth}
    | _ -> failwithf "Should not happen (array access)"

    // let addrWidth = int (ceil (System.Math.Log (float arrayWidth, 2.0)))
    // let addrCircuit = createNumberCircuit (All (addrWidth, Binary, arrayElem, 0))
    // let wenCircuit = createNumberCircuit (All (1, Binary, 0, 0))
    // let dinCircuit = createNumberCircuit (All (arrayWidth, Binary, 0, 0))

    // let topCircuit = {Comps = [inputComp]; Conns = []; Out = inputComp.OutputPorts[0]; OutWidth = arrayWidth}
    // let readCircuit = joinCircuits [addrCircuit; dinCircuit; wenCircuit] [addrCircuit.Out; wenCircuit.Out; dinCircuit.Out] topCircuit
    // readCircuit



        // let busSelComp = createComponent (BusSelection (arrayWidth, 0)) ""

        // let conn = createConnection inputComp.OutputPorts[0] busSelComp.InputPorts[0]     
        // {Comps=[busSelComp];Conns=[conn];Out=busSelComp.OutputPorts[0];OutWidth=arrayWidth}

let getExprWidths (varSizeMap: Map<string, int>)(paramMap: Map<string, int>)(expr': ExpressionDU) : (ExpressionCompilable)=
    let numberWidth number =
        match number with
        | Unsigned _ -> 32
        | All (bits, _, _, _) -> bits

    let rec getMinWidthsExpr (expr:ExpressionDU) =
        // first check for self determined 
        match expr with
        | ExpressionDU.Unary u ->    
            let unary = getMinWidthsUnary u
            {Type=EUnary; Operator=None; Head=None; Tail=None; Unary=(Some unary); Width= unary.Width}// what is the width?
        | Negation u ->
            let unary = getMinWidthsUnary u
            {Type=ENegation; Operator=None; Head=None; Tail=None; Unary=(Some unary); Width= unary.Width} // what is the width?
        | ConditionalOp (condExpr, ifTrue, ifFalse) -> // context determined
            let cond = getMinWidthsExpr condExpr
            let lhs = getMinWidthsExpr ifTrue
            let rhs = getMinWidthsExpr ifFalse
            let res = {Type=EConditionalResult; Operator=None; Head=Some lhs; Tail=Some rhs; Unary=None; Width= (max lhs.Width rhs.Width)}
            {Type=EConditionalCond; Operator=None; Head=Some cond; Tail=Some res; Unary=None; Width= res.Width}
        | ShiftExpr (op, lhsExpr, rhsExpr) ->
            let lhs = getMinWidthsExpr lhsExpr
            let rhs = getMinWidthsExpr rhsExpr
            {Type=EShift; Operator=Some op; Head=Some lhs; Tail=Some rhs; Unary=None; Width=lhs.Width}
        | Reduction (op, e) ->
            let elem = getMinWidthsExpr e
            let unary = {Type=UParenthesis; Primary=None; Number=None; Expression=Some elem; Width=elem.Width}
            {Type=EReduction; Operator=Some op; Head=None; Tail=None; Unary=Some unary; Width=1}
        | LogicalAnd (a, b) ->
            let lhs = getMinWidthsExpr a
            let rhs = getMinWidthsExpr b
            {Type=ELogicalAnd; Operator=None; Head=Some lhs; Tail=Some rhs; Unary=None; Width=1}
        | LogicalOr (a, b) ->
            let lhs = getMinWidthsExpr a
            let rhs = getMinWidthsExpr b
            {Type=ELogicalOr; Operator=None; Head=Some lhs; Tail=Some rhs; Unary=None; Width=1}
        | Equality (op, a, b) ->
            let lhs = getMinWidthsExpr a
            let rhs = getMinWidthsExpr b
            {Type=EEquality; Operator=Some op; Head=Some lhs; Tail=Some rhs; Unary=None; Width=1}
        | Comparison (op, a, b) ->
            let lhs = getMinWidthsExpr a
            let rhs = getMinWidthsExpr b
            {Type=EComparison; Operator=Some op; Head=Some lhs; Tail=Some rhs; Unary=None; Width=1}
        | UnaryUnsigned n ->
            let unary = {Type=UNumber; Primary=None; Number=Some n; Expression=None; Width=numberWidth n}
            {Type=EUnaryUnsigned; Operator=None; Head=None; Tail=None; Unary=Some unary; Width=unary.Width}
        | BitwiseAnd (a, b) ->
            let lhs = getMinWidthsExpr a
            let rhs = getMinWidthsExpr b
            {Type=EBitwiseAnd; Operator=None; Head=Some lhs; Tail=Some rhs; Unary=None; Width=max lhs.Width rhs.Width}
        | BitwiseOr (a, b) ->
            let lhs = getMinWidthsExpr a
            let rhs = getMinWidthsExpr b
            {Type=EBitwiseOr; Operator=None; Head=Some lhs; Tail=Some rhs; Unary=None; Width=max lhs.Width rhs.Width}
        | BitwiseXor (a, b) ->
            let lhs = getMinWidthsExpr a
            let rhs = getMinWidthsExpr b
            {Type=EBitwiseXor; Operator=None; Head=Some lhs; Tail=Some rhs; Unary=None; Width=max lhs.Width rhs.Width}
        | BitwiseXnor (a, b) ->
            let lhs = getMinWidthsExpr a
            let rhs = getMinWidthsExpr b
            {Type=EBitwiseXnor; Operator=None; Head=Some lhs; Tail=Some rhs; Unary=None; Width=max lhs.Width rhs.Width}
        | Additive (op, a, b) ->
            let lhs = getMinWidthsExpr a
            let rhs = getMinWidthsExpr b
            {Type=EAdditive; Operator=Some op; Head=Some lhs; Tail=Some rhs; Unary=None; Width=max lhs.Width rhs.Width}
        | Multiplicative (op, a, b) ->
            let lhs = getMinWidthsExpr a
            let rhs = getMinWidthsExpr b
            {Type=EMultiplicative; Operator=Some op; Head=Some lhs; Tail=Some rhs; Unary=None; Width=max lhs.Width rhs.Width}

    and getMinWidthsUnary (unary: UnaryDU): (UnaryCompilable) =
        match unary with
            | UnaryDU.Primary primary ->
                let width, expr, paramBool = 
                    match primary with
                    | Identifier id ->
                        match Map.tryFind id.Name varSizeMap, None with
                        | Some width, _ -> width, None, false
                        | None, _ -> 
                            printf "primary %s" id.Name
                            // printf "CHECK PARAM IS RIGHT?? paramname = %s, value = %i" id.Name (Map.find id.Name paramMap)
                            1, None, true
                    | IdentifierArray (id, _, bStart, bEnd) ->
                        (bStart - bEnd + 1), None, false
                        // match Map.tryFind id.Name varSizeMap, None with
                        // | Some width, _ -> width, None, false
                        // | None, _ -> 
                        //     printf "primary %s" id.Name
                        //     // printf "CHECK PARAM IS RIGHT?? paramname = %s, value = %i" id.Name (Map.find id.Name paramMap)
                        //     1, None, true
                        // // TODO: fail gracefully if not found in either map
                    | IdentifierBit _ ->
                        1, None, false
                    | IdentifierBits (_, bStart, bEnd) ->
                        // let bStart = evalExpr start
                        // let bEnd = evalExpr end_
                        (bStart - bEnd + 1), None, false // Assumption: bit-select range is constant here.
                    | IdentifierBitsSelect (_, start, width, _) ->
                        width, Some (getMinWidthsExpr start), false
                    | VariableBitSelect (_, expr) ->
                        1, Some (getMinWidthsExpr expr), false
                    | VariableArrayBitSel (id, indices, idx) ->
                        1, Some (getMinWidthsExpr idx), false
                        // printf "primary %s" id.Name
                        // printfn "varsize map %A" varSizeMap
                        // match Map.tryFind id.Name varSizeMap, None with
                        // | Some width, _ -> width, None, false
                        // | None, _ -> 1, None, true
                if paramBool then
                    let paramName = getPrimaryName primary
                    // let numberExpr = getMinWidthsExpr (ExpressionDU.Unary (UnaryDU.Number (Unsigned (Map.find paramName paramMap, getPrimaryLocation primary))))
                    let number = Unsigned (Map.find paramName paramMap, getPrimaryLocation primary)
                    // {Type=UNumber; Primary= None; Number=None; Expression=(Some numberExpr); Width=32}
                    {Type=UNumber; Primary= None; Number=Some number; Expression=None; Width=32}
                else
                    {Type=UPrimary; Primary= Some primary; Number=None; Expression=expr; Width=width}
            | UnaryDU.Number number ->
                let width = numberWidth number
                {Type=UNumber; Primary=None; Number=Some number; Expression=None; Width=width}
            | UnaryDU.Parenthesis expr ->
                let expr = getMinWidthsExpr expr
                {Type=UParenthesis; Primary=None; Number=None; Expression=Some expr; Width=expr.Width}
            | UnaryDU.Concat elems ->
                let lst = getWidthsUnaryList elems
                {Type=UConcat; Primary=None; Number=None; Expression=Some lst; Width=lst.Width}
            | UnaryDU.ParamNumber (paramName, bits) ->
                let name = getPrimaryName paramName
                let value = string (Map.find name paramMap)
                let number = All ((int bits), Decimal, value, getPrimaryLocation paramName)
                {Type=UNumber; Primary=None; Number=Some number; Expression=None; Width=(int bits)}
            |_ -> failwithf "Can't happen"
    and getWidthsUnaryList (lst: ExpressionDU array) : (ExpressionCompilable) =
        match lst with
        | [||] ->
            {Type=EUnary; Operator=None; Head=None; Tail=None; Unary=None; Width=0}
        | _ ->
            let headExpr = getMinWidthsExpr lst[0]
            match lst |> Array.tryItem 1 with
            | Some _ -> 
                let tail'= getWidthsUnaryList (lst |> Array.tail)
                {Type=EUnary; Operator=None; Head=Some headExpr; Tail=Some tail'; Unary=None; Width=headExpr.Width+tail'.Width}  
            | None ->
                {Type=EUnary; Operator=None; Head=Some headExpr; Tail=None; Unary=None; Width=headExpr.Width}  

    getMinWidthsExpr expr'


let extendCircuit (target:int) (circuit: Circuit)  =
    let widthDiff = target-circuit.OutWidth
    // printf "ERROR CHECK WIDTH: target = %i, circuit width = %i" target circuit.OutWidth
    // printf "circuit wrong: %A" circuit
    if widthDiff<0 then failwithf "Target width is smaller than circuit width!"
    elif widthDiff=0 then circuit
    else 
        let zero = createComponent (Constant1 (widthDiff,0I,"")) "const0"
        let zeroCircuit = {Comps=[zero]; Conns=[]; Out=zero.OutputPorts[0]; OutWidth=widthDiff}
        let c = joinWithMerge' [circuit; zeroCircuit] // check if the order is correct
        c

let sliceCircuit (circuit:Circuit) width lsb =
    let busSelectComp = createComponent (BusSelection (width, lsb)) "busSelect"
    let topCircuit = {Comps=[busSelectComp];Conns=[];Out=busSelectComp.OutputPorts[0];OutWidth=width}
    let newCircuit = joinCircuits [circuit] [busSelectComp.InputPorts[0]] topCircuit
    newCircuit
let nextPowerOfTwo value =
    let rec loop current =
        if current >= value then current
        else loop (current * 2)
    loop 1

let rec buildMuxTreeCircuit (inputs: Circuit list) (sel: Circuit) outputWidth : Circuit =
    match inputs with
    | [] -> failwithf "Mux tree requires at least one input"
    | [input] -> input
    | _ ->
        let currSel = sliceCircuit sel 1 0
        let sel' =
            match sel.OutWidth with
            | width when width > 1 -> sliceCircuit sel (width - 1) 1
            | _ -> currSel
        let nextInputs =
            inputs
            |> List.chunkBySize 2
            |> List.map (function
                | [first; second] ->
                    let mux = createComponent Mux2 "mux2"
                    let topCircuit = {Comps=[mux];Conns=[];Out=mux.OutputPorts[0];OutWidth=outputWidth}
                    joinCircuits [second; first; currSel] [mux.InputPorts[1]; mux.InputPorts[0]; mux.InputPorts[2]] topCircuit
                | [first] -> first
                | _ -> failwithf "Invalid mux tree input list"
            )
        buildMuxTreeCircuit nextInputs sel' outputWidth

let buildFixedShiftCircuit (inputCircuit: Circuit) shiftNo shiftType =
    if shiftNo = 0 then inputCircuit
    elif shiftNo < inputCircuit.OutWidth then
        let busSelComp =
            match shiftType with
            | LSL -> createComponent (BusSelection (inputCircuit.OutWidth - shiftNo, 0)) ""
            | _ -> createComponent (BusSelection (inputCircuit.OutWidth - shiftNo, shiftNo)) ""
        let busSelCircuit = {Comps=[busSelComp];Conns=[];Out=busSelComp.OutputPorts[0];OutWidth=inputCircuit.OutWidth - shiftNo}
        let selectedCircuit = joinCircuits [inputCircuit] [busSelComp.InputPorts[0]] busSelCircuit
        let paddingCircuit =
            match shiftType with
            | LSR
            | LSL ->
                createNumberCircuit (All (shiftNo, Binary, "0", 100))
            | ASR ->
                let msbCircuit = sliceCircuit inputCircuit 1 (inputCircuit.OutWidth - 1)
                let spreaderComp = createComponent (NbitSpreader shiftNo) "SPREAD"
                let spreaderCircuit = {Comps=[spreaderComp];Conns=[];Out=spreaderComp.OutputPorts[0];OutWidth=shiftNo}
                joinCircuits [msbCircuit] [spreaderComp.InputPorts[0]] spreaderCircuit
        match shiftType with
        | LSL -> joinWithMerge' [paddingCircuit; selectedCircuit]
        | _ -> joinWithMerge' [selectedCircuit; paddingCircuit]
    else
        match shiftType with
        | ASR ->
            let msbCircuit = sliceCircuit inputCircuit 1 (inputCircuit.OutWidth - 1)
            let spreaderComp = createComponent (NbitSpreader inputCircuit.OutWidth) "SPREAD"
            let spreaderCircuit = {Comps=[spreaderComp];Conns=[];Out=spreaderComp.OutputPorts[0];OutWidth=inputCircuit.OutWidth}
            joinCircuits [msbCircuit] [spreaderComp.InputPorts[0]] spreaderCircuit
        | _ ->
            createNumberCircuit (All (inputCircuit.OutWidth, Binary, "0", 100))

// let checkForParamsInExpr (expr: ExpressionCompilable) paramMap
//     match expr.Type with
//     | EUnary -> checkForParamsInUnary (Option.get expr.Unary) paramMap
//     | 

// and checkForParamsInUnary (unary: UnaryCompilable) paramMap =
//     match unary.Type with
//     | UPrimary ->
//         match unary.Primary with
//         | Some (Identifier id)
//         | Some (IdentifierArray (id, _, _, _)) ->
//             if Map.containsKey id.Name paramMap then 
//                     // let index = buildExpressionCircuit expr expr.Width

/// The main circuit creation function called with the RHS of an assignment as a parameter
/// Contains 6 recursive functions which eventually build the whole RHS expression
/// The starting point is the buildExpressionCircuit rec function
/// target is 0 if there is no lhs
let mainExpressionCircuitBuilder (expr:ExpressionDU) ioAndWireToCompMap varSizeMap paramMap target (arraySizeMap: Map<string, int * int array>) arrayToCompMap=
    
    /// builds the appropriate circuit of an expression based on expr.Type
    let rec buildExpressionCircuit (expr:ExpressionCompilable) (targetWidth: int)= 
        match expr.Type with
        | EUnary -> 
            buildUnaryCircuit (Option.get expr.Unary) targetWidth
            |> extendCircuit targetWidth
            // printf "ERROR CHECK: unarywidth = %i" (Option.get expr.Unary).Width
        | ENegation ->
            let (c1:Circuit) = buildUnaryCircuit (Option.get expr.Unary) targetWidth |> extendCircuit targetWidth
            let topComp = buildExpressionComponent expr c1.OutWidth
            let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
            joinCircuits [c1] [topComp.InputPorts[0]] topCircuit
        | EConditionalCond -> 
            let (c3:Circuit) = buildExpressionCircuit (Option.get expr.Head)  (Option.get expr.Head).Width
            // c1 is the (case=TRUE) circuit which goes to 1 of MUX, c2 goes to 0
            //that's why they are given in reverse order in the joinCircuits function 
            let c1,c2 = buildConditionalCircuit (Option.get expr.Tail) targetWidth
            let topComp = buildExpressionComponent expr c1.OutWidth
            let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
            joinCircuits [c2;c1;c3] [topComp.InputPorts[0];topComp.InputPorts[1];topComp.InputPorts[2]] topCircuit
        | EShift when (Option.get expr.Tail).Type = EUnaryUnsigned ->
            buildShiftCircuit expr targetWidth
        | EShift ->
            buildVariableShiftCircuit expr targetWidth
        | EReduction ->
            buildReductionAndLogicalCircuit expr "reduction" targetWidth
        | ELogicalAnd | ELogicalOr ->
            let (c1:Circuit) = buildReductionAndLogicalCircuit (Option.get expr.Head) "logical" (max (Option.get expr.Head).Width (Option.get expr.Tail).Width) //max
            let (c2:Circuit) = buildReductionAndLogicalCircuit (Option.get expr.Tail) "logical" (max (Option.get expr.Head).Width (Option.get expr.Tail).Width) //max 
            let topComp = buildExpressionComponent expr c1.OutWidth // this should always be 1
            let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
            joinCircuits [c1;c2] [topComp.InputPorts[0];topComp.InputPorts[1]] topCircuit
        | EEquality ->
            buildEqualityCircuit expr
        | EComparison -> 
            buildComparisonCircuit expr
        | _ ->  //everything else: bitwise gates and additive     
            let (c1:Circuit) = buildExpressionCircuit (Option.get expr.Head) targetWidth |> extendCircuit targetWidth
            let (c2:Circuit) = buildExpressionCircuit (Option.get expr.Tail) targetWidth |> extendCircuit targetWidth
            let topComp = buildExpressionComponent expr c1.OutWidth
            match expr.Type with
            | EAdditive ->
                let inputB,cin =
                    match expr.Operator with
                    |Some Plus ->
                        let tempNumber = All (1, Binary, "0", 100) //location is Don't Care
                        c2,(createNumberCircuit tempNumber)

                    |Some Minus ->
                        let tempNumber = All (1, Binary, "1", 100) //location is Don't Care
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

            |_ -> //bitwise gates and multiplication      
                let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
                joinCircuits [c1;c2] [topComp.InputPorts[0];topComp.InputPorts[1]] topCircuit


    and buildUnaryCircuit (unary:UnaryCompilable) (targetWidth:int)=
        match unary.Type with
        | UPrimary ->
            // handle arrays:
            let primaryName = getPrimaryName (Option.get unary.Primary) 
            let primaryLocation = getPrimaryLocation (Option.get unary.Primary)
            // if Map.tryFind primaryName paramMap |> Option.isSome then
                
                // createNumberCircuit (All (1, Binary, string (Map.find primaryName paramMap), primaryLocation))
                // match unary.Primary with
                // | Some (Identifier id)
                // buildUnaryCircuit {unary with Type=UNumber; Primary=None; Number=Some (Unsigned (Map.find primaryName paramMap, primaryLocation)); Expression=None} targetWidth
            // else
            match Map.tryFind primaryName arrayToCompMap with
            | Some compList -> 
                createArrayCircuit (Option.get unary.Primary) ioAndWireToCompMap arraySizeMap compList
            | None ->
                //handle variable bitselect:
                // printf("checking for vbs, primary: %A, expr: %A") unary.Primary unary.Expression
                match unary.Primary, unary.Expression with
                | Some (VariableBitSelect (id, _)), Some expr -> 
                    // printfn("vbs hit, expr: %A") expr
                    // let index = checkForParamsInExpr expr paramMap
                    let index = buildExpressionCircuit expr expr.Width
                    let (primaryComp: Component) = Map.find id.Name ioAndWireToCompMap
                    let primaryWidth = Map.find id.Name varSizeMap
                    let primaryCircuit = {Comps=[];Conns=[];Out=primaryComp.OutputPorts[0];OutWidth=primaryWidth}
                    let bitCircuits =
                        [0 .. primaryWidth - 1]
                        |> List.map (fun bit ->
                            let busSelComp = createComponent (BusSelection (1, bit)) ""
                            let topCircuit = {Comps=[busSelComp]; Conns=[]; Out=busSelComp.OutputPorts[0]; OutWidth=1}
                            joinCircuits [primaryCircuit] [busSelComp.InputPorts[0]] topCircuit
                        )
                    let paddedBitCircuits =
                        let zeroCircuits =
                            List.init (nextPowerOfTwo bitCircuits.Length - bitCircuits.Length) (fun _ ->
                                createNumberCircuit (All (1, Binary, "0", 100))
                            )
                        bitCircuits @ zeroCircuits
                    buildMuxTreeCircuit paddedBitCircuits index 1
                    // match unary.Expression
                | _ -> createPrimaryCircuit (Option.get unary.Primary) ioAndWireToCompMap varSizeMap paramMap
        | UNumber ->
            createNumberCircuit (Option.get unary.Number)
        | UParenthesis ->
            buildExpressionCircuit (Option.get unary.Expression) targetWidth |> extendCircuit targetWidth
        | UConcat ->
            buildUnaryListCircuit (Option.get unary.Expression)
        // | ParamNumber (paramName, bits) ->
        //     let paramval = Map.find (getPrimaryName paramName) paramMap
        //     createNumberCircuit paramval
        |_ -> failwithf "Can't happen"

    /// creates a list of unaries and merges them together using MergeWires
    /// used for concatenations
    and buildUnaryListCircuit (unaryList:ExpressionCompilable) = 
        let head = buildExpressionCircuit (Option.get unaryList.Head) (Option.get unaryList.Head).Width
        let list = 
            match Option.isSome unaryList.Tail with
            |true -> 
                let tail = buildUnaryListCircuit (Option.get unaryList.Tail)
                [head]@[tail]
            |false -> 
                [head]
    
        joinWithMerge' (List.rev list)


    and buildConditionalCircuit (tail:ExpressionCompilable) targetWidth=
        let c1 = buildExpressionCircuit (Option.get tail.Head) targetWidth |> extendCircuit targetWidth
        let c2 = buildExpressionCircuit (Option.get tail.Tail) targetWidth |> extendCircuit targetWidth
        (c1,c2)
    and buildVariableShiftCircuit (expr:ExpressionCompilable) targetWidth =
        let (c1:Circuit) = buildExpressionCircuit (Option.get expr.Head) targetWidth |> extendCircuit targetWidth
        let (c2:Circuit) = buildExpressionCircuit (Option.get expr.Tail) (Option.get expr.Tail).Width
        let c1Source = {c1 with Comps=[]; Conns=[]}
        let c2Source = {c2 with Comps=[]; Conns=[]}
        let shiftType = 
            match (Option.get expr.Operator) with
            | Sll -> LSL
            | Sra -> ASR
            | _ -> LSR
        let shiftedCircuits =
            [0 .. nextPowerOfTwo c1.OutWidth - 1]
            |> List.map (fun shiftNo -> buildFixedShiftCircuit c1Source shiftNo shiftType)
        let shiftedCircuit = buildMuxTreeCircuit shiftedCircuits c2Source c1.OutWidth
        {shiftedCircuit with Comps = c1.Comps @ c2.Comps @ shiftedCircuit.Comps; Conns = c1.Conns @ c2.Conns @ shiftedCircuit.Conns}

    // and buildVariableShiftCircuit (expr:ExpressionCompilable) targetWidth =
    //     let (c1:Circuit) = buildExpressionCircuit (Option.get expr.Head) targetWidth |> extendCircuit targetWidth
    //     let (c2:Circuit) = buildExpressionCircuit (Option.get expr.Tail) (Option.get expr.Tail).Width
        
    //     let shiftType = 
    //         match (Option.get expr.Operator) with
    //         | Sll -> LSL
    //         | Sra -> ASR
    //         | _ -> LSR

    //     let topComp = createComponent (Shift (c1.OutWidth,c2.OutWidth,shiftType)) "SHIFT"
    //     let topCircuit = {Comps=[topComp];Conns=[];Out=topComp.OutputPorts[0];OutWidth=c1.OutWidth}
    //     joinCircuits [c1;c2] [topComp.InputPorts[0];topComp.InputPorts[1]] topCircuit
    
    and buildShiftCircuit (expr:ExpressionCompilable) targetWidth = 
        let operator = (Option.get expr.Operator)
        let tail = Option.get expr.Tail
        let unary = Option.get tail.Unary
        let number = Option.get unary.Number
        let shiftNo =
            match number with
            | Unsigned (n, _) -> n
            | All (_, _, n, _) -> int n 
        let (c1:Circuit) = buildExpressionCircuit (Option.get expr.Head) targetWidth |> extendCircuit targetWidth
    
        // check that shiftNo is smaller than the width of the unary being shifted
        // otherwise can't select the bits with BusSelect
        if shiftNo < c1.OutWidth then  
            let busSelComp = //keep the bits which will remain in the circuit after the shift
                match operator with
                | Sll -> createComponent (BusSelection ((c1.OutWidth-shiftNo), 0)) ""
                |_ -> createComponent (BusSelection ((c1.OutWidth-shiftNo), shiftNo)) ""
    
            let busSelCircuit = {Comps=[busSelComp];Conns=[];Out=busSelComp.OutputPorts[0];OutWidth=(c1.OutWidth-shiftNo)}
            let selectedCircuit = joinCircuits [c1] [busSelComp.InputPorts[0]] busSelCircuit

            let constantCircuit =
                match operator with
                | Srl | Sll -> //if logical shift, connect a constant of width=shift to the other side of MergeWires
                    let tempNumber = All (shiftNo, Binary, "0", 100) //location is Don't Care
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
                | Sll -> [constantCircuit;selectedCircuit]
                |_ -> [selectedCircuit;constantCircuit]

            inOrder
            |> joinWithMerge'
            //|> extractCircuit
        
        else // if shiftNo >= c1.OutWidth return a c1.OutWidth-width constant with value 0
            let tempNumber = All (c1.OutWidth, Binary, "0", 100) //location is Don't Care
            createNumberCircuit tempNumber


    and buildReductionAndLogicalCircuit (expr:ExpressionCompilable) circType targetWidth=
        let (c1:Circuit) = 
            match circType with
            |"reduction" -> buildUnaryCircuit (Option.get expr.Unary) (Option.get expr.Unary).Width
            |"logical" -> buildExpressionCircuit expr targetWidth |> extendCircuit targetWidth
            |_ -> failwithf "Calling buildReductionAndLogicalCircuit with undefined circType"
        // reductions are implemented with compares
        // (&) -> check that value is equal to (2^width - 1)
        // (|) -> check that value is NOT equal to 0
        // (!) -> check if equal to 0 (returns true if input is 0{false}, thus negates it)
        // Same with a not gate at the end for (~&,~|)
        let busCompareComp = 
            match circType,expr.Operator with
                |"reduction",Some AndOp 
                |"reduction",Some Nand 
                    -> createComponent (BusCompare (c1.OutWidth, (((1I <<< c1.OutWidth) - 1I)))) "COMP"      
                |_,_ -> //Some "|" or Some "!" or Some "~|"
                    createComponent (BusCompare (c1.OutWidth,0I)) "COMP"

        let busCompareCircuit = {Comps=[busCompareComp];Conns=[];Out=busCompareComp.OutputPorts[0];OutWidth=1}

        match circType,expr.Operator with
        |"reduction",Some AndOp 
        |"reduction",Some NotOp 
            -> joinCircuits [c1] [busCompareComp.InputPorts[0]] busCompareCircuit
        |_,_ ->
            let comparedCircuit = joinCircuits [c1] [busCompareComp.InputPorts[0]] busCompareCircuit

            let notGateComp = createComponent Not "G"
            let notGateCircuit = {Comps=[notGateComp];Conns=[];Out=notGateComp.OutputPorts[0];OutWidth=1}
            joinCircuits [comparedCircuit] [notGateComp.InputPorts[0]] notGateCircuit
    
    and buildEqualityCircuit (expr: ExpressionCompilable) =
        let targetWidth = max (Option.get expr.Head).Width (Option.get expr.Tail).Width
        let (c1:Circuit) = buildExpressionCircuit (Option.get expr.Head) targetWidth |> extendCircuit targetWidth
        let (c2:Circuit) = buildExpressionCircuit (Option.get expr.Tail) targetWidth |> extendCircuit targetWidth
        let xorComp = createComponent (NbitsXor (c1.OutWidth, None)) "xor" 
        let xorCircuit ={Comps=[xorComp]; Conns=[]; Out=xorComp.OutputPorts[0]; OutWidth=c1.OutWidth}
        let xorCircuit' = joinCircuits [c1;c2] [xorComp.InputPorts[0]; xorComp.InputPorts[1]] xorCircuit 
        let busCompare = createComponent (BusCompare (c1.OutWidth,0I)) "COMP"
        let compCircuit = {Comps=[busCompare]; Conns=[]; Out=busCompare.OutputPorts[0]; OutWidth=1}
        let comparedCircuit = joinCircuits [xorCircuit'] [busCompare.InputPorts[0]] compCircuit
        match expr.Operator with
        | Some Neq ->
            let notGateComp = createComponent Not "NOT"
            let notGateCircuit = {Comps=[notGateComp];Conns=[];Out=notGateComp.OutputPorts[0];OutWidth=1}
            joinCircuits [comparedCircuit] [notGateComp.InputPorts[0]] notGateCircuit
        | Some Eq -> comparedCircuit
        | _ -> failwithf "Invalid operator in equality expression"

    and buildComparisonCircuit (expr: ExpressionCompilable) =
        let targetWidth = max (Option.get expr.Head).Width (Option.get expr.Tail).Width
        let (c1:Circuit) = buildExpressionCircuit (Option.get expr.Head) targetWidth |> extendCircuit (targetWidth+1)
        let (c2:Circuit) = buildExpressionCircuit (Option.get expr.Tail) targetWidth |> extendCircuit (targetWidth+1)
        let addComp = createComponent (NbitsAdder (targetWidth+1)) "Add"
        let subCircuit =
            let inputB,cin =
                let tempNumber = All (1, Binary, "1", 100) //location is Don't Care
                let cinCircuit = createNumberCircuit tempNumber

                let nBitsNotComp = createComponent (NbitsNot c2.OutWidth) "NOT"
                let nBitsNotCircuit = {Comps=[nBitsNotComp];Conns=[];Out=nBitsNotComp.OutputPorts[0];OutWidth=c2.OutWidth}
                let invertedCircuit = joinCircuits [c2] [nBitsNotComp.InputPorts[0]] nBitsNotCircuit
                (invertedCircuit,cinCircuit)

            let ioLabelComp = createComponent (Viewer 1) "Adder_Cout" 
            let conn = createConnection addComp.OutputPorts[1] ioLabelComp.InputPorts[0] 
            let topCircuit = {Comps=[addComp;ioLabelComp];Conns=[conn];Out=addComp.OutputPorts[0];OutWidth=c1.OutWidth}
            joinCircuits [cin;c1;inputB] [addComp.InputPorts[0];addComp.InputPorts[1];addComp.InputPorts[2]] topCircuit
        let MSB = sliceCircuit subCircuit 1 targetWidth
        // need to check top bit to see which one is bigger
        let busCompare =
            match expr.Operator with 
            | Some Lte | Some Gt -> // compare msb to 1
                createComponent (BusCompare (1, 0I)) "COMP"
            | Some Gte | Some Lt -> // compare msb to 0
                createComponent (BusCompare (1, 1I)) "COMP"
            | _ -> failwithf "Invalid comparison operator!"
        let busCompareCircuit = {Comps=[busCompare]; Conns=[]; Out=busCompare.OutputPorts[0]; OutWidth=1}
        let compareCircuit = joinCircuits [MSB] [busCompare.InputPorts[0]] busCompareCircuit
        match expr.Operator with
        | Some Lte | Some Gte ->
            let notGateComp = createComponent Not "NOT"
            let notGateCircuit = {Comps=[notGateComp];Conns=[];Out=notGateComp.OutputPorts[0];OutWidth=1}
            joinCircuits [compareCircuit] [notGateComp.InputPorts[0]] notGateCircuit
        | _ -> compareCircuit
        
    let exprWidths = getExprWidths varSizeMap paramMap expr //pass in varsizemap
    buildExpressionCircuit exprWidths (max target exprWidths.Width) // get lhssize




let getCombinationalVars (ast:VerilogInput) (project: Project) =
    let contAssignVars = 
        ([], VerilogInput ast) ||> foldAST getContAssignments
        |> List.map (fun assign -> getPrimaryName assign.LHS.PrimaryType)
    let alwaysCombVars = 
        ([], VerilogInput ast) ||> foldAST getBlockingAssignments
        |> List.map (fun assign -> getPrimaryName assign.LHS.PrimaryType)
    let modInst = 
        foldAST getModuleInstantiationStatements [] (VerilogInput ast)
        |> List.collect (fun modInst -> getModuleInstantiationOutputPrimaries modInst project)
        |> List.map getPrimaryName
    contAssignVars @ alwaysCombVars @ modInst
    |> List.distinct

let getClockedVars (ast:VerilogInput) =
    let alwaysCombVars = 
        ([], VerilogInput ast) ||> foldAST getNonBlockingAssignmentsWithLocation
        |> List.map (fun (assign, _) -> getPrimaryName assign.LHS.PrimaryType)
        |> List.distinct
    alwaysCombVars

let overlaps (slice1:Slice) (slice2:Slice) =
    max slice1.LSB slice2.LSB <= min slice1.MSB slice2.MSB

/// debug:
let isCircuitValid (circuit:Circuit) (varToCompMap:Map<string,Component>)=
    let inputPorts = 
        circuit.Comps @ (Map.valuesL varToCompMap)
        |> List.collect (fun comp -> comp.InputPorts)
        |> List.map (fun port -> port.Id)
    let outputPorts =
        circuit.Comps @ (Map.valuesL varToCompMap)
        |> List.collect (fun comp -> comp.OutputPorts)
        |> List.map (fun port -> port.Id)
    let wrongConns =
        circuit.Conns
        |> List.filter (fun conn ->
            not (List.contains conn.Source.Id outputPorts && List.contains conn.Target.Id inputPorts)
        )
    match wrongConns with
    | [] -> true
    | _ -> false


/// Helper function to see if an array is written to multiple times - determines whether it synthesises to a memory or registers
// let rec checkMultipleArrayWrites arrayName node =
//     // map of arrayelem number to number of times its written to
//     let arrayElem = -1
//     match node with
//     | Assignment assign ->
//         let lhsVar = getPrimaryName assign.LHS.PrimaryType
//         if lhsVar = arrayName then 
//             let arrayElemNew = 
//                 match assign.LHS.PrimaryType with
//                 | Identifier id -> -1 // whole array
//                 | IdentifierBit (id, idx) -> idx
//                 | IdentifierBits (id, _, _)
//                 | IdentifierBitsSelect (id, _, _, _) -> failwith "Not allowed for arrays"
//                 | VariableBitSelect (id, expr) -> evalExpr expr
//                 | IdentifierArray (id, idx, bStart, bEnd) -> evalExpr idx[0]
//             if arrayElemNew <> arrayElem && arrayElem <> -1 then
//                 -1
//             else arrayElemNew
//         else 0
//     | Statement stmt ->
//         match stmt with
//         | NonBlockingAssign assign
//         | BlockingAssign assign -> checkMultipleArrayWrites arrayName (Assignment assign)
//         | Conditional (ifStmt, elseStmt) -> // if its in both if and else statements its fine to overlap
//             let ifCount = checkMultipleArrayWrites arrayName (Statement ifStmt)
//             let elseCount = checkMultipleArrayWrites arrayName (Statement elseStmt)
//             Map.fold (fun acc key value -> 
//                 let elseValue = Map.tryFind key elseCount |> Option.defaultValue 0
//                 Map.add key (value + elseValue) acc
//             ) arrayElemCount ifCount
        
// let chooseArrayType var (arraySizeMap: Map<string, int * int[]>) combVars clockedVars ast project : Component list=
//     let contAssign =
//         ([], VerilogInput ast) ||> foldAST getContAssignments
//     let alwaysCombAssign =
//         ([], VerilogInput ast) ||> foldAST getBlockingAssignments
//     let clockedAssign =
//         ([], VerilogInput ast) ||> foldAST getNonBlockingAssignments

    
//     let varAssignmentsOnRHS = 
//         contAssign @ alwaysCombAssign @ clockedAssign 
//         |> List.collect (fun assign ->
//             primariesUsedInAssignment [] assign.RHS
//             |> List.map getPrimaryName)
//         // filter for the specific variable
//         |> List.filter (fun name -> name = var)

//     let varAssignmentsOnLHS = 
//         contAssign @ alwaysCombAssign @ clockedAssign 
//         |> List.filter (fun assign -> getPrimaryName assign.LHS.PrimaryType = var)

//     // check for multiple reads - must be reg array NOT rom/ram
//     let arrayType =
//         match varAssignmentsOnRHS with
//         | [] -> "memory"
//         | [_] -> "memory"
//         | _ -> "registers"

    // TODO: might need to chabnge this so it only creates the registers that are actually used?
    // let createRegisterArray var (arraySizeMap: Map<string, int * int[]>) =
    //     let arrayWidth, arrayDims = Map.find var arraySizeMap
    //     let regArrayComps = [for i in 0 .. (arrayDims[0]-1) -> createComponent (Register arrayWidth) (sprintf "%s_reg_%d" var i)]
    //     // let regArrayCircuits = [for comp in regArrayComps -> {Comps=[comp]; Conns=[]; Out=comp.OutputPorts[0]; OutWidth=arrayWidth}]
    //     regArrayComps

    // let convertRHStoRAM (rhs: ExpressionDU) =
    //     match rhs with
    //     | ExpressionDU.Unary u -> 
    //         match u with 
    //         | UnaryDU.Concat 
    // match varAssignmentsOnLHS with
    // | [] -> failwithf "Array %s is never assigned to!" var
    // | [assign] -> 
    //     let arrayWidth, arrayDims = Map.find var arraySizeMap
    //     // eventually differentiate between memory and registers, but for now
    //     // match arrayType with
    //     // | "memory" -> 
    //     //     let addressWidth = int (ceil (System.Math.Log (float arrayDims[0], 2.0)))
    //     //     let arrayData = 
    //     //         match assign.LHS.PrimaryType with
    //     //         // | Identifier id -> convertRHStoRAM assign.RHS
    //     //         | Identifier id -> failwithf "Currently does not support SV style of aggregate array assignment"
    //     //         | IdentifierBit (id, index) 
    //     // | "registers" -> createRegisterArrayComponent var arrayWidth arrayDims
    //     // | _ -> failwithf "Invalid array type!"
    //     createRegisterArray var arrayWidth arrayDims
    //     // Map.add var regArrayComps Map.empty
    // | _ -> 
    //     let arrayWidth, arrayDims = Map.find var arraySizeMap
    //     createRegisterArray var arrayWidth arrayDims
    //     // Map.add var regArrayComps Map.empty
    
    // createRegisterArray var arrayWidth arrayDims

let createRegisterArray var (arraySizeMap: Map<string, int * int[]>) =
    let arrayWidth, arrayDims = Map.find var arraySizeMap
    let regArrayComps = [for i in 0 .. (arrayDims[0]-1) -> createComponent (Register arrayWidth) (sprintf "%s_reg_%d" var i)]
    // let regArrayCircuits = [for comp in regArrayComps -> {Comps=[comp]; Conns=[]; Out=comp.OutputPorts[0]; OutWidth=arrayWidth}]
    regArrayComps
    
let rec mergeIfElse (lst1: List<BitMapping>) (lst2:List<BitMapping>) varToCompMap: List<BitMapping Option*BitMapping Option> =
    match lst1, lst2 with
    | h1 ::t1, h2 ::t2 -> 
        if h1.Slice = h2.Slice then
            [Some h1, Some h2] @ mergeIfElse t1 t2 varToCompMap
        elif overlaps h1.Slice h2.Slice then
            let start1, end1 = h1.Slice.LSB, h1.Slice.MSB
            let start2, end2 = h2.Slice.LSB, h2.Slice.MSB
            let minEnd, maxStart = min end1 end2, max start1 start2
            let first =
                if start1 < maxStart then 
                    let width = maxStart - start1
                    let newCircuit = sliceCircuit h1.Circuit width 0
                    let newMapping = {Slice={LSB=start1; MSB=maxStart-1}; Circuit=newCircuit; LHSType=h1.LHSType}
                    (Some newMapping, None) 
                elif start2 < maxStart then
                    let width = maxStart - start2
                    let newCircuit = sliceCircuit h2.Circuit width 0
                    let newMapping = {Slice={LSB=start2; MSB=maxStart-1}; Circuit=newCircuit; LHSType=h2.LHSType}
                    (None, Some newMapping)
                else (None, None)
            let second =
                let width = minEnd - maxStart + 1
                let newCircuit1 = sliceCircuit h1.Circuit width (maxStart-start1)
                let newMapping1 = {Slice={LSB=maxStart; MSB=minEnd}; Circuit=newCircuit1; LHSType=h1.LHSType}

                let newCircuit2 = sliceCircuit h2.Circuit width (maxStart-start2)
                let newMapping2 = {Slice={LSB=maxStart; MSB=minEnd}; Circuit=newCircuit2; LHSType=h2.LHSType}
                (Some newMapping1, Some newMapping2)
            let third =
                if end1 > minEnd then 
                    let width = end1 - minEnd
                    let newCircuit = sliceCircuit h1.Circuit width (minEnd-start1+1)
                    let newMapping = {Slice={LSB=minEnd+1; MSB=end1}; Circuit=newCircuit; LHSType=h1.LHSType}
                    (Some newMapping, None) 
                elif end2 > minEnd then 
                    let width = end2 - minEnd
                    let newCircuit = sliceCircuit h2.Circuit width (minEnd-start2+1)
                    let newMapping = {Slice={LSB=minEnd+1; MSB=end2}; Circuit=newCircuit; LHSType=h2.LHSType}
                    (None, Some newMapping) 
                else (None, None)
            [first; second; third]
                
        elif h1.Slice < h2.Slice then
            [Some h1, None] @ mergeIfElse t1 lst2 varToCompMap
        else
            [None, Some h2] @ mergeIfElse lst1 t2 varToCompMap
    | h1::t1, _ ->
        [Some h1, None] @ mergeIfElse t1 [] varToCompMap
    | _, h2::t2 ->
        [Some h2, None] @ mergeIfElse t2 [] varToCompMap
    | _ -> []


let addAssignment (assignment: BitMapping) (bits: List<BitMapping>) varToCompMap =
    // assuming bits is sorted by slices
    let overlapping =
        bits
        |> List.filter (fun bitmapping -> overlaps  assignment.Slice bitmapping.Slice)
    match overlapping with
    | [] -> bits @ [assignment] |> List.sortBy (fun mapping -> mapping.Slice)
    | _ ->
        let first = List.head overlapping
        let last = List.last overlapping
        match assignment.Slice.LSB <= first.Slice.LSB, assignment.Slice.MSB >= last.Slice.MSB with
        | true, true ->
            bits
            |> List.filter (fun mapping -> not (overlaps assignment.Slice mapping.Slice)) 
            |> List.append [assignment;]
            |> List.sortBy (fun mapping -> mapping.Slice)  // check if this is correct
        | false, true -> // need to cut first into two pieces, remove first and add new pieces back
            let width = assignment.Slice.LSB - first.Slice.LSB
            let newFirstMSB = first.Slice.LSB + width - 1
            // split circuit of first
            let busSelectComp = createComponent (BusSelection (width, 0)) "busSelect"
            let topCircuit = {Comps=[busSelectComp];Conns=[];Out=busSelectComp.OutputPorts[0];OutWidth=width}
            let newCircuit = joinCircuits [first.Circuit] [busSelectComp.InputPorts[0]] topCircuit
            let newFirst = {Slice={LSB=first.Slice.LSB; MSB=newFirstMSB}; Circuit=newCircuit; LHSType=first.LHSType}
            bits
            |> List.filter (fun mapping -> not (overlaps assignment.Slice mapping.Slice)) 
            |> List.append [newFirst; assignment]
            |> List.sortBy (fun mapping -> mapping.Slice)
        | true, false ->
            let width = last.Slice.MSB - assignment.Slice.MSB
            let newLastLSB = last.Slice.MSB - width + 1
            // split circuit of first
            let busSelectComp = createComponent (BusSelection (width, assignment.Slice.MSB-last.Slice.LSB+1)) "busSelect"
            let topCircuit = {Comps=[busSelectComp];Conns=[];Out=busSelectComp.OutputPorts[0];OutWidth=width}
            let newCircuit = joinCircuits [last.Circuit] [busSelectComp.InputPorts[0]] topCircuit
            let newLast = {Slice={LSB=newLastLSB; MSB=last.Slice.MSB}; Circuit=newCircuit; LHSType=last.LHSType}
            bits
            |> List.filter (fun mapping -> not (overlaps assignment.Slice mapping.Slice)) 
            |> List.append [newLast; assignment]
            |> List.sortBy (fun mapping -> mapping.Slice)
        | false, false ->
            // dealing with first
            let width = assignment.Slice.LSB - first.Slice.LSB
            let newFirstMSB = first.Slice.LSB + width - 1
            // split circuit of first
            let busSelectComp = createComponent (BusSelection (width, 0)) "busSelect"
            let topCircuit = {Comps=[busSelectComp];Conns=[];Out=busSelectComp.OutputPorts[0];OutWidth=width}
            let newCircuit = joinCircuits [first.Circuit] [busSelectComp.InputPorts[0]] topCircuit
            let newFirst = {Slice={LSB=first.Slice.LSB; MSB=newFirstMSB}; Circuit=newCircuit; LHSType=first.LHSType}
            //dealing with last
            let width = last.Slice.MSB - assignment.Slice.MSB
            let newLastLSB = last.Slice.MSB - width + 1
            // split circuit of first
            let busSelectComp = createComponent (BusSelection (width, assignment.Slice.MSB-last.Slice.LSB+1)) "busSelect"
            let topCircuit = {Comps=[busSelectComp];Conns=[];Out=busSelectComp.OutputPorts[0];OutWidth=width}
            let newCircuit = joinCircuits [last.Circuit] [busSelectComp.InputPorts[0]] topCircuit
            let newLast = {Slice={LSB=newLastLSB; MSB=last.Slice.MSB}; Circuit=newCircuit; LHSType=last.LHSType}
            bits
            |> List.filter (fun mapping -> not (overlaps assignment.Slice mapping.Slice)) 
            |> List.append [newFirst; assignment; newLast]
            |> List.sortBy (fun mapping -> mapping.Slice)


/// returns a mapping from lhs variable name -> bits -> rhs final circuit
/// maybe store the bits in a sorted array instead of a map
let compileModule' node varToCompMap ioToCompMap varSizeMap paramMap arraySizeMap arrayToCompMap=
    let rec compileModule (node: ASTNode) varToCompMap (currCircuits: Map<string, List<BitMapping>>) =
        // let node = foldParams paramMap node
        match node with
        | VerilogInput input ->
            compileModule (Module (convertModule input.Module)) varToCompMap currCircuits
        | Module m ->
            compileModule (ModuleItems m.ModuleItems) varToCompMap currCircuits
        | ModuleItems items ->
            (currCircuits, items.ItemList)
            ||> Array.fold (fun circuits item -> compileModule (Item item) varToCompMap circuits)
        | Item item ->
            compileModule (getItem item) varToCompMap currCircuits
        | ContStatement contAssign ->
            compileModule (Assignment contAssign.Assignment) varToCompMap currCircuits
        | Assignment assign -> 
            let outPort = getPrimaryName assign.LHS.PrimaryType
            let bits = sliceFromBits assign.LHS varToCompMap varSizeMap paramMap
            let circuit = mainExpressionCircuitBuilder assign.RHS varToCompMap varSizeMap paramMap (bits.MSB-bits.LSB+1) arraySizeMap arrayToCompMap
            let lhstype = 
                match Map.tryFind outPort ioToCompMap with
                | None -> Wire
                | _ -> OutputPort // TO DO: !! need to fix this as we can have: logic l; assign l=1'b1; !!
            let newAssignment = {Slice=bits; Circuit=circuit; LHSType=lhstype}
            let currVarAssignments = 
                match Map.tryFind outPort currCircuits with // need to fix this later: handle overlapping intervals properly
                | Some bitToCircuitMap -> 
                    bitToCircuitMap
                | _ -> 
                    [] // don't need lhstype in the future
            let updatedAssignments = addAssignment newAssignment currVarAssignments varToCompMap
            Map.add outPort updatedAssignments currCircuits
        | AlwaysConstruct always ->
            compileModule (Statement always.Statement) varToCompMap currCircuits
        | Statement statement ->
            match statement with
            | StatementDU.NonBlockingAssign (assign, _) ->
                compileModule (Assignment assign) varToCompMap currCircuits
            | StatementDU.BlockingAssign (assign, _) ->
                compileModule (Assignment assign) varToCompMap currCircuits // TO DO: get += etc. operators working too! currently this is just =
            | StatementDU.SeqBlock (seq, _) ->
                (currCircuits, seq)
                ||> Array.fold (fun circuits stmt ->
                    compileModule (Statement stmt) varToCompMap circuits) 
            | StatementDU.Conditional (ifStmt, elseStmt, _) ->
                let ifCircuits = compileModule (Statement ifStmt.Statement) varToCompMap Map.empty

                let elseCircuits =
                    match elseStmt with
                    | Some stmt -> compileModule (Statement stmt) varToCompMap Map.empty
                    | _ -> Map.empty
                let condCircuit = mainExpressionCircuitBuilder ifStmt.Condition varToCompMap varSizeMap paramMap 0 arraySizeMap arrayToCompMap
                let res =
                    (currCircuits, Set.union (ifCircuits.Keys |> Set.ofSeq) (elseCircuits.Keys |> Set.ofSeq))
                    ||> Set.fold (fun circuits var ->
                        let c1,c2 = Option.defaultValue [] (Map.tryFind var ifCircuits), Option.defaultValue [] (Map.tryFind var elseCircuits)
                        let merged = mergeIfElse c1 c2 varToCompMap
                        (circuits, merged)
                        ||> List.fold (fun c intervals ->
                            match intervals with
                            | Some ifMapping, Some elseMapping -> 
                                // make mux connect to stuff add to map 
                                let mux = createComponent Mux2 var
                                let topCircuit = {Comps=[mux];Conns=[];Out=mux.OutputPorts[0];OutWidth=ifMapping.Circuit.OutWidth}
                                let newCircuit = joinCircuits [elseMapping.Circuit;ifMapping.Circuit;condCircuit] [mux.InputPorts[0];mux.InputPorts[1];mux.InputPorts[2]] topCircuit
                                let newMapping = {Slice=ifMapping.Slice; Circuit=newCircuit; LHSType=ifMapping.LHSType}
                                let currSlices = Option.defaultValue [] (Map.tryFind var c)
                                Map.add var (addAssignment newMapping currSlices varToCompMap) c
                            | Some ifMapping, None ->
                                // find overlapping slices
                                let currSlices = Option.defaultValue [] (Map.tryFind var c)
                                let overlappingSlices =
                                    mergeIfElse [ifMapping] currSlices varToCompMap
                                    |> List.filter (fun pair -> Option.isSome (snd pair) && Option.isSome (fst pair))
                                (c, overlappingSlices)
                                ||> List.fold (fun c' ifElse->
                                    let ifM, elseM = Option.get (fst ifElse), Option.get (snd ifElse)
                                    let mux = createComponent Mux2 var
                                    let topCircuit = {Comps=[mux];Conns=[];Out=mux.OutputPorts[0];OutWidth=ifM.Circuit.OutWidth}
                                    let newCircuit = joinCircuits [elseM.Circuit;ifM.Circuit;condCircuit] [mux.InputPorts[0];mux.InputPorts[1];mux.InputPorts[2]] topCircuit
                                    let newMapping = {Slice=ifM.Slice; Circuit=newCircuit; LHSType=ifM.LHSType}
                                    let currSlices = Option.defaultValue [] (Map.tryFind var c')
                                    Map.add var (addAssignment newMapping currSlices varToCompMap) c'
                                )
                            | None, Some elseMapping ->
                                // find overlapping slices
                                let currSlices = Option.defaultValue [] (Map.tryFind var c)
                                let overlappingSlices =
                                    mergeIfElse currSlices [elseMapping] varToCompMap
                                    |> List.filter (fun pair -> Option.isSome (snd pair) && Option.isSome (fst pair))
                                
                                (c, overlappingSlices)
                                ||> List.fold (fun c' ifElse->
                                    let ifM, elseM = Option.get (fst ifElse), Option.get (snd ifElse)
                                    let mux = createComponent Mux2 var
                                    let topCircuit = {Comps=[mux];Conns=[];Out=mux.OutputPorts[0];OutWidth=ifM.Circuit.OutWidth}
                                    let newCircuit = joinCircuits [elseM.Circuit;ifM.Circuit;condCircuit] [mux.InputPorts[0];mux.InputPorts[1];mux.InputPorts[2]] topCircuit
                                    let newMapping = {Slice=ifM.Slice; Circuit=newCircuit; LHSType=ifM.LHSType}
                                    let currSlices = Option.defaultValue [] (Map.tryFind var c')
                                    Map.add var (addAssignment newMapping currSlices varToCompMap) c'
                                )

                            | _ -> c
                        )    
                    )
                res
                // if the if and else circuits were stored in a sorted array based on starting index, i can go through them in parallel
            | StatementDU.ForStatement (forStmt, _) ->
                let forStmts = unrollForLoops forStmt
                compileModule (Statement forStmts) varToCompMap currCircuits
                // failwithf "Reaching compile module, Forstatements: %A" forStmts
            | StatementDU.Case (case, _) ->
                compileModule (Case case) varToCompMap currCircuits
        | _ -> currCircuits
    let res = compileModule node varToCompMap Map.empty
    res
/// debug:
let isCircuitValid' comps conns=
    let inputPorts = 
        comps
        |> List.collect (fun comp -> comp.InputPorts)
        |> List.map (fun port -> port.Id)
    let outputPorts =
        comps
        |> List.collect (fun comp -> comp.OutputPorts)
        |> List.map (fun port -> port.Id)
    let wrongConns =
        conns
        |> List.filter (fun conn ->
            not (List.contains conn.Source.Id outputPorts && List.contains conn.Target.Id inputPorts)
        )
    match wrongConns with
    | [] -> true
    | _ -> false

/// takes in n - number of inputs, must be a power of 2
/// circuit being returned has N (bus) data inputs and 1 select 
let rec multiplexerNto1Circuit (inputs: List<Circuit>) (sel: Circuit) : Circuit =
    let n = inputs.Length
    match n with
    | 1 | 0 -> failwithf "Reached 1 or 0np in muxN creation, should not happen!"
    | 2 ->
        let mux = createComponent Mux2 "mux2"
        let topCircuit = {Comps=[mux];Conns=[];Out=mux.OutputPorts[0];OutWidth=1}
        let circuit = joinCircuits [inputs[1]; inputs[0]; sel] [mux.InputPorts[1]; mux.InputPorts[0]; mux.InputPorts[2]] topCircuit
        circuit
    | _ ->
        let inputPairs =
            List.chunkBySize 2 inputs
            |> List.map (fun chunk -> 
                match chunk with 
                | [first; second] -> (first, second)
                | _ -> failwithf "Invalid number of inputs for Nx1 multiplexer" )
        let currSel = sliceCircuit sel 1 0
        let sel' = sliceCircuit sel (sel.OutWidth-1) 1
        let inputs' =
            inputPairs
            |> List.map ( fun (first, second) -> 
                multiplexerNto1Circuit [first; second] currSel
            )
        multiplexerNto1Circuit inputs' sel'

// default case is last?
let multiplexerCircuit (inputs: List<bigint*Circuit>) (condition: Circuit) (defaultInput: Circuit): Circuit =
    (defaultInput, inputs)
    ||> List.fold (fun prevCircuit (caseItem, inputCircuit) ->
        let busComparator = createComponent (BusCompare (condition.OutWidth, caseItem)) "CMP"
        let topCircuit = {Comps=[busComparator];Conns=[];Out=busComparator.OutputPorts[0];OutWidth=1}
        let condCircuit = joinCircuits [condition] [busComparator.InputPorts[0]] topCircuit
        let mux2 = createComponent Mux2 "mux2"
        let muxCircuit = {Comps=[mux2];Conns=[];Out=mux2.OutputPorts[0];OutWidth=prevCircuit.OutWidth}
        joinCircuits [prevCircuit; inputCircuit; condCircuit] [mux2.InputPorts[0]; mux2.InputPorts[1]; mux2.InputPorts[2]] muxCircuit 
    )

// Helper: parse a parameterized name into (baseName, paramList)
let parseParamName (name: string) : string * string list =
    match name.Split([|"_P_"|], System.StringSplitOptions.None) with
    | [|baseName|] -> baseName, []
    | [|baseName; paramString|] ->
        let parts = paramString.Split('_')
        let paramNames =
            parts
            |> Array.chunkBySize 2
            |> Array.choose (function
                | [|p; _|] -> Some p
                | _ -> None)
            |> Array.toList
        baseName, paramNames
    | _ -> name, []

// Returns true if two parameterized names have the same base module
let isSameModuleIgnoringParams (name1: string) (name2: string) =
    let base1, params1 = parseParamName name1
    let base2, params2 = parseParamName name2
    (base1 = base2 && params1 = params2) || base1 = name2 || base2 = name1

let getInitialMapAndCircuits (veriloginput: VerilogInput) (project:Project) paramMap =
    let input = convertModule veriloginput.Module
    let items = input.ModuleItems.ItemList |> Array.toList
    let ioDecls = items |> List.filter (function ItemDU.IOItem _ -> true | _ -> false)
    
    let assignments = items |> List.filter (function ItemDU.ContStatement _ -> true | _ -> false)
    let wiresLHS = collectWiresLHS assignments // get declarations too
    let ioToCompMap = 
        getIOtoComponentMap ioDecls paramMap
        |> Map.filter (fun var _ -> var <> "clk")   // for output ports make a wire label like for wires / we only need it for vars driven by continuous assigns though
    let inputs = 
        ioDecls
        |> List.filter (function ItemDU.IOItem {DeclarationType=InputDecl} -> true | _ -> false)
        |> List.fold (fun lst item -> 
            match item with 
            | ItemDU.IOItem ioItem -> Array.append lst ioItem.Variables
            | _ -> failwithf "Should not happen! Expecting only IOItems"
        ) [||]
        |> Array.map (fun id -> id.Name)
        |> Set.ofArray

    // static map to search for input,wire components
    let ioAndWireToCompMap = 
        (ioToCompMap,wiresLHS) 
        ||> List.fold(fun map wire ->
            getWireToCompMap wire map
        )

    let portSizeMap,_ = getPortSizeAndLocationMap items paramMap
    let wireSizeMap = getWireSizeMap items paramMap
    let declarations = foldAST getDeclarations [] (VerilogInput veriloginput)

    let arraySizeMap = getArraySizeMap items paramMap
    let wireSizeMap =
        (wireSizeMap, declarations)
        ||> List.fold (fun map decl ->
            (map, decl.Variables)
            ||> Array.fold (fun map' variable -> 
                // check if variable is in array map - handle separately
                if Map.containsKey variable.Name arraySizeMap then map'
                else if Option.isNone decl.Range then Map.add variable.Name 1 map'
                else
                    let bStart = evalExprWithParams (Option.get decl.Range).Start paramMap
                    let bEnd = evalExprWithParams (Option.get decl.Range).End paramMap
                    Map.add variable.Name (bStart - bEnd + 1) map'
            )
        )
    // let wireSizeMap = Map.fold (fun acc key value -> Map.add key value acc) wireSizeMap arraySizeMap
    let varSizeMap = Map.fold (fun acc key value -> Map.add key value acc) wireSizeMap portSizeMap
    let combVars = getCombinationalVars veriloginput project
    let clockedVars = getClockedVars veriloginput
    // printf "Combinational vars: %A\n" combVars
    // printf "Clocked vars: %A\n" clockedVars
    let varToCompMap = 
        (ioToCompMap, combVars)
        ||> List.fold ( fun map var ->
            match Map.tryFind var arraySizeMap with
            | Some _ -> map // handle arrays separately
            | None ->
                let wireComp = createComponent IOLabel var
                Map.add var wireComp map
            )
    // printfn "varToCompMap %A" varToCompMap
    let combArrays = 
        combVars
        |> List.filter (fun var -> Map.containsKey var arraySizeMap) 
    let arrayToCompMap : Map<string, Component list> = 
        (Map.empty, combArrays)
        ||> List.fold (fun map var ->
            // iterate through
            let arrayWidth, arrayDims = Map.find var arraySizeMap
            let arrayComps = 
                [for i in 0 .. (arrayDims[0]-1) -> createComponent IOLabel (sprintf "%s_%d" var i) ]
                // TODO: figure out why naming is duplicating?
            Map.add var arrayComps map
        )
    let varToCompMap, arrayToCompMap =
        ((varToCompMap, arrayToCompMap), clockedVars)
        ||> List.fold (fun (varMap, arrayMap) var ->
            match Map.tryFind var varSizeMap with
            | Some s -> 
                let regComp = createComponent (Register s) var
                Map.add var regComp varMap, arrayMap
            | _ -> match Map.tryFind var arraySizeMap with
                    | Some s ->
                        printfn "Adding array %s to component map with size %A" var s
                        let vectorWidth = fst s
                        let arrayDims = snd s
                        let addressWidth = int (ceil (System.Math.Log (float arrayDims[0], 2.0)))

                        let arrayComps = 
                            createRegisterArray var arraySizeMap
                        varMap, Map.add var arrayComps arrayMap

                        // // Data : Map<bigint,bigint>
                        // let data = [for i in 0 .. (arrayDims[0]-1) -> bigint i, 0I] |> Map.ofList
                        // // check here if initial declaration did any assignment? or handle in compilecircuit
                        // // parser needs to be extended fo rthis - TODO
                        // let memory1 = {Init = FromData; AddressWidth = addressWidth; WordWidth = vectorWidth; Data = data}
                        // createComponent (RAM1 memory1) var
                    | _ -> failwith "What? variable doesn't have a size?"
            // let regComp = createComponent (Register size) var
            // Map.add var regComp map
        ) 
    let clockedVarsSet = clockedVars |> Set.ofList
    let initialCircuits = 
        (Map.empty, varSizeMap)
        ||> Map.fold (fun map var width->
            match Set.contains var inputs, Set.contains  var clockedVarsSet with
            | true, _ -> map
            | false, false ->
                let zero = All (width, Binary, "0", 100) //location is Don't Care
                Map.add var (createNumberCircuit zero) map
            | false,true -> 
                let reg = 
                    match Map.tryFind var varToCompMap with
                    | Some comp -> comp
                    | _ -> failwithf "Clocked variable doesn't have a component"
                let circuit = {Comps=[reg]; Conns=[]; Out=reg.OutputPorts[0]; OutWidth=width}
                Map.add var circuit map
        )
        |> Map.filter (fun var _ -> var <> "clk")
    
    // printfn "initialCircuits %A" initialCircuits

    // add in array circuits
    let initialArrayCircuits =
        (Map.empty, arraySizeMap)
        ||> Map.fold (fun map var dims ->
            // dims = (vector width, array dims)
            // printf "Adding initial circuit for array %s with dims %A\n" var dims
            match Set.contains var inputs, Set.contains  var clockedVarsSet with
            | true, _ -> failwithf "Arrays should not be accepted as IO"
            | false, false ->
                let arrayWidth, arrayDims = dims
                let zero = All (arrayWidth, Binary, "0", 100) //location is Don't Care
                // let regArrayComps = [for i in 0 .. (arrayDims[0]-1) -> createComponent (Register arrayWidth) (sprintf "%s_reg_%d" var i)]
                let regArrayZeros = [for i in 0 .. (arrayDims[0]-1) -> createNumberCircuit zero]
                Map.add var regArrayZeros map
            | false,true ->
                let arrayWidth, arrayDims = dims
                let regs = 
                    match Map.tryFind var arrayToCompMap with
                    | Some comp -> comp
                    | _ -> failwithf "Clocked variable doesn't have a component"
                let circuits =
                    regs
                    |> List.map (fun comp ->
                        {Comps=[comp]; Conns=[]; Out=comp.OutputPorts[0]; OutWidth=arrayWidth}
                    )
                Map.add var circuits map
            |> Map.filter (fun var _ -> var <> "clk")
        )

    // printf "Initial array circuits: %A\n" initialArrayCircuits


    let ioVars = 
        ioDecls
        |> List.collect (function ItemDU.IOItem ioItem -> ioItem.Variables |> Array.toList | _ -> failwithf "Should not happen! Expecting only IOItems")
        |> List.map (fun id -> (id.Name).ToUpper())
    
    varToCompMap, ioToCompMap, varSizeMap, initialCircuits, initialArrayCircuits, ioVars, arraySizeMap, arrayToCompMap
let rec compileModule (node: ASTNode) (varToCompMap: Map<string,Component>) (ioToCompMap: Map<string,Component>) (varSizeMap: Map<string,int>) (paramMap: Map<string,int>) (arraySizeMap: Map<string, int * int array>) (arrayToCompMap: Map<string, Component list>) initialCircuits initialArrayCircuits (project:Project) compName model dispatch =
    let rec compileModuleRec (node: ASTNode) varToCompMap (currCircuits: Map<string, Circuit>) (currArrayCircuits: Map<string, list<Circuit>>) =
        // let node = foldParams paramMap node
        match node with
        | VerilogInput input ->
            compileModuleRec (Module (convertModule input.Module)) varToCompMap currCircuits currArrayCircuits
        | Module m ->
            compileModuleRec (ModuleItems m.ModuleItems) varToCompMap currCircuits currArrayCircuits
        | ModuleItems items ->
            ((currCircuits, currArrayCircuits), items.ItemList)
            ||> Array.fold (fun (circuits, arrayCircuits) item ->
                compileModuleRec (Item item) varToCompMap circuits arrayCircuits)
        | Item item ->
            compileModuleRec (getItem item) varToCompMap currCircuits currArrayCircuits
        | ContStatement contAssign ->
            compileModuleRec (Assignment contAssign.Assignment) varToCompMap currCircuits currArrayCircuits
        | Assignment assign -> 
            // printfn "Compiling assignment: %A" assign
            let outPort = assign.LHS.PrimaryType |> getPrimaryName

            let arrayBool =
                match Map.tryFind outPort arraySizeMap with
                | Some _ -> true
                | _ -> false
        
            // var[0] and var[1:0]
            let compileVarCircuits currCircuits =
                let bits = sliceFromBits assign.LHS varToCompMap varSizeMap paramMap // need different logic for variable indexed bit select
                let circuit = mainExpressionCircuitBuilder assign.RHS varToCompMap varSizeMap paramMap (bits.MSB-bits.LSB+1) arraySizeMap arrayToCompMap
                let currCircuit = 
                    match Map.tryFind outPort currCircuits with
                    | Some c -> c
                    | _ -> failwithf "This should not happen, variable doesn't have a circuit"
                let MSBs = 
                    if (currCircuit.OutWidth-bits.MSB-1) > 0 then
                        [sliceCircuit  currCircuit (currCircuit.OutWidth-bits.MSB-1) (bits.MSB+1)] // add logic to make sure this is not splitting off width 0!
                    else []
                let LSBs = 
                    if bits.LSB > 0 then
                        [sliceCircuit currCircuit (bits.LSB) 0]
                    else [] // add logic to make sure this is not splitting off width 0!
                let newCircuit = joinWithMerge' (LSBs @ [circuit] @ MSBs)
                printfn "newCircuit %A" newCircuit
                Map.add outPort newCircuit currCircuits, currArrayCircuits

            // array constant word and constant bits select - returns the array element
            let compileArrayCircuits currArrayCircuits =
                let arrayElem = sliceFromWordArray assign.LHS varSizeMap arraySizeMap
                // let circuit = mainExpressionCircuitBuilder assign.RHS varToCompMap varSizeMap paramMap (bits.MSB-bits.LSB+1) arraySizeMap arrayToCompMap
                let currArrayCircuit = 
                    match Map.tryFind outPort currArrayCircuits with
                    | Some c -> c
                    | _ -> failwithf "This should not happen, variable doesn't have a circuit"
                arrayElem, currArrayCircuit
                
            // works for both array[0] and array[0][3:0] and array[0][0]
            let compileArrayCircuit1 (currArrayCircuits: Map<string,list<Circuit>>) =
                let bits = sliceFromBitsArray assign.LHS arraySizeMap
                let arrayElem, currArrayCircuit = compileArrayCircuits currArrayCircuits
                let circuit = mainExpressionCircuitBuilder assign.RHS varToCompMap varSizeMap paramMap (bits.MSB-bits.LSB+1) arraySizeMap arrayToCompMap
                
                let MSBs = 
                    if (currArrayCircuit[arrayElem].OutWidth-bits.MSB-1) > 0 then
                        [sliceCircuit  currArrayCircuit[arrayElem] (currArrayCircuit[arrayElem].OutWidth-bits.MSB-1) (bits.MSB+1)] // add logic to make sure this is not splitting off width 0!
                    else []
                let LSBs = 
                    if bits.LSB > 0 then
                        [sliceCircuit currArrayCircuit[arrayElem] (bits.LSB) 0]
                    else [] // add logic to make sure this is not splitting off width 0!

                let newCircuit = joinWithMerge' (LSBs @ [circuit] @ MSBs)
                let newArrayCircuits = List.mapi (fun i c -> if i = arrayElem then newCircuit else c) currArrayCircuit
                newArrayCircuits
                // currCircuits, Map.add outPort newArrayCircuits currArrayCircuits
         
            // makes mux for variable bit select
            let compileVarSelCircuits outWidth rhsSource currSource =
                [0 .. nextPowerOfTwo outWidth - 1]
                |> List.map (fun bit ->
                    if bit < outWidth then
                        let lsbs =
                            if bit > 0 then [sliceCircuit currSource bit 0]
                            else []
                        let msbs =
                            if outWidth - bit - 1 > 0 then [sliceCircuit currSource (outWidth - bit - 1) (bit + 1)]
                            else []
                        joinWithMerge' (lsbs @ [rhsSource] @ msbs)
                    else currSource
                )

            let buildIndexSelector (indexSource:Circuit) (indexValue:int) : Circuit =
                let cmp = createComponent (BusCompare (indexSource.OutWidth, bigint (int64 indexValue))) "CMP"
                let topCircuit = {Comps=[cmp]; Conns=[]; Out=cmp.OutputPorts[0]; OutWidth=1}
                joinCircuits [indexSource] [cmp.InputPorts[0]] topCircuit

            let buildArrayElementMux (currSource:Circuit) (updatedSource:Circuit) (selector:Circuit) : Circuit =
                let mux = createComponent Mux2 "Mux2"
                let topCircuit = {Comps=[mux]; Conns=[]; Out=mux.OutputPorts[0]; OutWidth=currSource.OutWidth}
                joinCircuits [currSource; updatedSource; selector] [mux.InputPorts[0]; mux.InputPorts[1]; mux.InputPorts[2]] topCircuit

            let compileVarSelCircuits1 expr =
                let outWidth =
                    match Map.tryFind outPort varSizeMap with
                    | Some size -> size
                    | None -> failwithf "Variable doesn't have a size?"
                let currCircuit =
                    match Map.tryFind outPort currCircuits with
                    | Some c -> c
                    | _ -> failwithf "This should not happen, variable doesn't have a circuit"
                let rhsCircuit = mainExpressionCircuitBuilder assign.RHS varToCompMap varSizeMap paramMap outWidth arraySizeMap arrayToCompMap
                let indexCircuit = mainExpressionCircuitBuilder expr varToCompMap varSizeMap paramMap 0 arraySizeMap arrayToCompMap
                let currSource = {currCircuit with Comps=[]; Conns=[]}
                let rhsSource = {rhsCircuit with Comps=[]; Conns=[]}
                let indexSource = {indexCircuit with Comps=[]; Conns=[]}
                let updatedBitCircuits = compileVarSelCircuits outWidth rhsSource currSource
                let updatedCircuit = buildMuxTreeCircuit updatedBitCircuits indexSource outWidth
                let updatedCircuit =
                    {updatedCircuit with
                        Comps = currCircuit.Comps @ rhsCircuit.Comps @ indexCircuit.Comps @ updatedCircuit.Comps
                        Conns = currCircuit.Conns @ rhsCircuit.Conns @ indexCircuit.Conns @ updatedCircuit.Conns}
                Map.add outPort updatedCircuit currCircuits, currArrayCircuits

            let compileVarSelArray (outDim:int) (outWidth:int) (currSources:list<Circuit>) (indexSource:Circuit) (makeUpdated:Circuit -> Circuit) : list<Circuit> =
                currSources
                |> List.mapi (fun i currSource ->
                    let updatedSource = makeUpdated currSource
                    let selector = buildIndexSelector indexSource i
                    buildArrayElementMux currSource updatedSource selector
                )

            let compileVarSelArray1 (arraySelects:ArraySelect array) =
                let outWidth, outDim =
                    match Map.tryFind outPort arraySizeMap with
                    | Some (arrayWidth, arrayDims) -> arrayWidth, arrayDims[0]
                    | None -> failwithf "Variable doesn't have a size?"
                let bits = sliceFromBitsArray assign.LHS arraySizeMap
                let rhsCircuit = mainExpressionCircuitBuilder assign.RHS varToCompMap varSizeMap paramMap (bits.MSB - bits.LSB + 1) arraySizeMap arrayToCompMap
                let currArrayCircuit =
                    match Map.tryFind outPort currArrayCircuits with
                    | Some c -> c
                    | _ -> failwithf "This should not happen, variable doesn't have a circuit"
                let indexExpr =
                    match arraySelects[0] with
                    | ConstArraySelect idx -> ExpressionDU.Unary (UnaryDU.Number (Unsigned (idx, 0)))
                    | VarArraySelect expr -> expr
                let indexCircuit = mainExpressionCircuitBuilder indexExpr varToCompMap varSizeMap paramMap 0 arraySizeMap arrayToCompMap
                let currSources = currArrayCircuit |> List.map (fun c -> {c with Comps=[]; Conns=[]})
                let rhsSource = {rhsCircuit with Comps=[]; Conns=[]}
                let indexSource = {indexCircuit with Comps=[]; Conns=[]}
                let updatedArrayCircuits =
                    compileVarSelArray outDim outWidth currSources indexSource (fun currSource ->
                        let MSBs =
                            if (currSource.OutWidth - bits.MSB - 1) > 0 then
                                [sliceCircuit currSource (currSource.OutWidth - bits.MSB - 1) (bits.MSB + 1)]
                            else []
                        let LSBs =
                            if bits.LSB > 0 then
                                [sliceCircuit currSource bits.LSB 0]
                            else []
                        joinWithMerge' (LSBs @ [rhsSource] @ MSBs)
                    )
                updatedArrayCircuits

            let compileVarSelArrayWordVarIndex (indexExpr:ExpressionDU) =
                let outWidth, outDim =
                    match Map.tryFind outPort arraySizeMap with
                    | Some (arrayWidth, arrayDims) -> arrayWidth, arrayDims[0]
                    | None -> failwithf "Variable doesn't have a size?"
                let rhsCircuit = mainExpressionCircuitBuilder assign.RHS varToCompMap varSizeMap paramMap outWidth arraySizeMap arrayToCompMap
                let currArrayCircuit =
                    match Map.tryFind outPort currArrayCircuits with
                    | Some c -> c
                    | _ -> failwithf "This should not happen, variable doesn't have a circuit"
                let indexCircuit = mainExpressionCircuitBuilder indexExpr varToCompMap varSizeMap paramMap 0 arraySizeMap arrayToCompMap
                let currSources = currArrayCircuit |> List.map (fun c -> {c with Comps=[]; Conns=[]})
                let rhsSource = {rhsCircuit with Comps=[]; Conns=[]}
                let indexSource = {indexCircuit with Comps=[]; Conns=[]}
                compileVarSelArray outDim outWidth currSources indexSource (fun _ -> rhsSource)

            let compileVarSelArrayBitSel (arraySelects:ArraySelect array) (bitExpr:ExpressionDU) =
                let outWidth, outDim =
                    match Map.tryFind outPort arraySizeMap with
                    | Some (arrayWidth, arrayDims) -> arrayWidth, arrayDims[0]
                    | None -> failwithf "Variable doesn't have a size?"
                let rhsCircuit = mainExpressionCircuitBuilder assign.RHS varToCompMap varSizeMap paramMap 1 arraySizeMap arrayToCompMap
                let currArrayCircuit =
                    match Map.tryFind outPort currArrayCircuits with
                    | Some c -> c
                    | _ -> failwithf "This should not happen, variable doesn't have a circuit"
                let arrayIndexExpr =
                    match arraySelects[0] with
                    | ConstArraySelect idx -> ExpressionDU.Unary (UnaryDU.Number (Unsigned (idx, 0)))
                    | VarArraySelect expr -> expr
                let arrayIndexCircuit = mainExpressionCircuitBuilder arrayIndexExpr varToCompMap varSizeMap paramMap 0 arraySizeMap arrayToCompMap
                let bitIndexCircuit = mainExpressionCircuitBuilder bitExpr varToCompMap varSizeMap paramMap 0 arraySizeMap arrayToCompMap
                let currSources = currArrayCircuit |> List.map (fun c -> {c with Comps=[]; Conns=[]})
                let rhsSource = {rhsCircuit with Comps=[]; Conns=[]}
                let arrayIndexSource = {arrayIndexCircuit with Comps=[]; Conns=[]}
                let bitIndexSource = {bitIndexCircuit with Comps=[]; Conns=[]}
                let updatedArrayCircuits =
                    compileVarSelArray outDim outWidth currSources arrayIndexSource (fun currSource ->
                        let updatedBitCircuits = compileVarSelCircuits outWidth rhsSource currSource
                        buildMuxTreeCircuit updatedBitCircuits bitIndexSource outWidth
                    )
                updatedArrayCircuits

            match assign.LHS.PrimaryType with
            | Identifier _
            | IdentifierBit _
            | IdentifierBits _
            | IdentifierBitsSelect _ ->
                let priName = getPrimaryName assign.LHS.PrimaryType
                if Map.containsKey priName arraySizeMap then
                    let updatedArrayCircuits = compileArrayCircuit1 currArrayCircuits
                    currCircuits, Map.add outPort updatedArrayCircuits currArrayCircuits
                else
                    compileVarCircuits currCircuits
            | IdentifierArray (_, indices, _, _) ->
                let updatedArrayCircuits = compileVarSelArray1 indices
                currCircuits, Map.add outPort updatedArrayCircuits currArrayCircuits
            | VariableBitSelect (_, expr) ->
                let priName = getPrimaryName assign.LHS.PrimaryType
                if Map.containsKey priName arraySizeMap then
                    let updatedArrayCircuits = compileVarSelArrayWordVarIndex expr
                    currCircuits, Map.add outPort updatedArrayCircuits currArrayCircuits
                else
                    compileVarSelCircuits1 expr
            | VariableArrayBitSel (_, indices, bitExpr) ->
                let updatedArrayCircuits = compileVarSelArrayBitSel indices bitExpr
                currCircuits, Map.add outPort updatedArrayCircuits currArrayCircuits

        | AlwaysConstruct always ->
            compileModuleRec (Statement always.Statement) varToCompMap currCircuits currArrayCircuits
        | Statement statement ->
            match statement with
            | StatementDU.NonBlockingAssign (assign, _) ->
                compileModuleRec (Assignment assign) varToCompMap currCircuits currArrayCircuits
            | StatementDU.BlockingAssign (assign, _) ->
                compileModuleRec (Assignment assign) varToCompMap currCircuits currArrayCircuits // TO DO: get += etc. operators working too! currently this is just =
            | StatementDU.SeqBlock (seq, _) ->
                ((currCircuits, currArrayCircuits), seq)
                ||> Array.fold (fun (circuits, arrayCircuits) stmt ->
                    compileModuleRec (Statement stmt) varToCompMap circuits arrayCircuits) 
            | StatementDU.Conditional (ifStmt, elseStmt, _) ->
                let ifCircuits, ifArrayCircuits = compileModuleRec (Statement ifStmt.Statement) varToCompMap currCircuits currArrayCircuits
                let elseCircuits, elseArrayCircuits =
                    match elseStmt with
                    | Some stmt -> compileModuleRec (Statement stmt) varToCompMap currCircuits currArrayCircuits
                    | _ -> currCircuits, currArrayCircuits
                let condCircuit = mainExpressionCircuitBuilder ifStmt.Condition varToCompMap varSizeMap paramMap 0 arraySizeMap arrayToCompMap // need to reduce it to 1 bit
                let comp = createComponent (BusCompare (condCircuit.OutWidth, 0I)) "CMP"
                let topCircuit = {Comps=[comp];Conns=[];Out=comp.OutputPorts[0];OutWidth=1}
                let condCircuitN = joinCircuits [condCircuit] [comp.InputPorts[0]] topCircuit
                let newCircuits =
                    (currCircuits, ifCircuits)
                    ||> Map.fold (fun circuits var ifCircuit ->
                        let elseCircuit = 
                            match Map.tryFind var elseCircuits with
                            | Some c -> c
                            | _ -> failwithf "This should not happen variable doesn't have a circuit in else branch!"
                        if ifCircuit = elseCircuit then circuits
                        else
                            let mux = createComponent Mux2 "Mux2"
                            let topCircuit = {Comps=[mux];Conns=[];Out=mux.OutputPorts[0];OutWidth=ifCircuit.OutWidth}
                            let newCircuit = joinCircuits [ifCircuit;elseCircuit;condCircuitN] [mux.InputPorts[0];mux.InputPorts[1];mux.InputPorts[2]] topCircuit
                            Map.add var newCircuit circuits
                    )
                let newArrayCircuits =
                    if ifArrayCircuits = elseArrayCircuits then ifArrayCircuits
                    else currArrayCircuits
                newCircuits, newArrayCircuits
            | StatementDU.ForStatement (forStmt, _) ->
                let forStmts = unrollForLoops forStmt
                let loopVar = forStmt.Initialisation.LHS.PrimaryType |> getPrimaryName
                // loop variable should not generate hardware, remove
                let currCircuitsUpdated = currCircuits.Remove loopVar
                compileModuleRec (Statement forStmts) varToCompMap currCircuitsUpdated currArrayCircuits
                // failwithf "Reaching compile module, Forstatements seq block: %A, number of statements: %d" forStmts forStmts.Statements.Length
            | StatementDU.Case (case, _) ->
                compileModuleRec (Case case) varToCompMap currCircuits currArrayCircuits
        | Case case ->
            let caseItemMap: Map<bigint, StatementDU> =
                (Map.empty, case.CaseItems)
                ||> Array.fold (fun map caseItem -> 
                    (map, caseItem.Expressions)
                    ||> Array.fold (fun m num -> 
                        let dec =
                            match num with
                            | Unsigned (n, _) -> bigint n
                            | All (bits, numBase, allNumber, _) ->
                                let numBaseStr =
                                    match numBase with
                                    | Binary -> "'b"
                                    | Hex -> "'h"
                                    | Decimal -> "'d"
                                toDecimal (string allNumber) numBaseStr (string bits)
                        Map.add dec caseItem.Statement m
                    ) 
                )
            let muxInputs: Map<string, List<bigint*Circuit>> =
                (Map.empty, caseItemMap)
                ||> Map.fold (fun inputs num stmt->
                    let circuits, _ = compileModuleRec (Statement stmt) varToCompMap currCircuits currArrayCircuits
                    let newInputs =
                        (inputs, circuits)
                        ||> Map.fold (fun currMap var circuit ->
                            let newList =
                                match Map.tryFind var currMap with
                                | Some lst -> 
                                    lst @ [num, circuit]
                                | _ -> [num, circuit]
                            Map.add var newList currMap
                        )
                    newInputs
                )
            let defaultCircuits, _ = 
                match case.Default with
                | Some stmt -> compileModuleRec (Statement stmt) varToCompMap currCircuits currArrayCircuits
                | None -> currCircuits, currArrayCircuits
            let sel = mainExpressionCircuitBuilder case.Expression varToCompMap varSizeMap paramMap 0 arraySizeMap arrayToCompMap
            let newCircuits =
                (currCircuits, muxInputs)
                ||> Map.fold (fun circuits var inputs ->
                    let defaultCircuit =
                        match Map.tryFind var defaultCircuits with
                        | Some c -> c
                        | _ -> failwithf "What? Variable doesn't have a circuit in the default case"
                    let muxN = multiplexerCircuit inputs sel defaultCircuit
                    Map.add var muxN circuits
                )
            newCircuits, currArrayCircuits
        | ModuleInstantiation modInst ->
            let loadedComp = 
                match List.tryFind (fun comp -> comp.Name = modInst.Module.Name) project.LoadedComponents with
                    | Some comp -> comp
                    | _ -> failwithf "No such loaded component found, this should never happen %s" modInst.Module.Name
            let loadedComp, name =
                match modInst.Parameters with
                | Some parameters -> 
                    let overrideMap =
                        (Map.empty, parameters)
                        ||> Array.fold (fun map param ->
                        // TODO: add check that param matches module def
                            Map.add param.Identifier.Name (evalExpr param.Value) map
                        )

                    let modInstVerilog = 
                        match loadedComp.Form with
                        | Some (Verilog name) -> 
                            let folderPath = project.ProjectPath
                            let path = pathJoin [| folderPath; name + ".v" |]
                            let code = 
                                match tryReadFileSync path with
                                |Ok text -> text
                                |Error _ -> sprintf "Error: file {%s.v} has been deleted from the project directory" name
                            let parsedCodeNearley = parseFromFile code
                            let output = Json.parseAs<ParserOutput> parsedCodeNearley
                            let result = Option.get output.Result
                            let fixedAST = fix result
                            let parsedAST = fixedAST |> Json.parseAs<VerilogTypes.VerilogInput>
                            // failwithf "failing at %s" fixedAST 
                            parsedAST
                        | _ -> failwithf "Loaded component doesn't have a form - Only verilog modules supported currently"

                    
                    let paramsOverriden = 
                        overrideMap 
                        |> Map.toList
                        |> List.map (fun (param, value) -> sprintf "%s_%A" param value ) 
                    
                    let original_name = modInst.Module.Name
                    let name = modInst.Module.Name + "_P_" + String.concat "_" paramsOverriden
                    // failwithf "name: %s" name

                    let folderPath = project.ProjectPath
                    let oldPath = pathJoin [| folderPath; original_name + ".v" |]
                    let code = 
                        match tryReadFileSync oldPath with
                        |Ok text -> text
                        |Error _ -> sprintf "Error: file {%s.v} has been deleted from the project directory" original_name
                    
                    let path = pathJoin [| folderPath; name + ".v" |]
                    let path2 = pathJoin [| folderPath; name + ".dgm" |]
                    
                    match writeFile path code with
                    | Ok _ -> ()
                    | Error _ -> failwithf "Writing verilog file FAILED"

                    // replicating createSheet
                    let input = convertModule modInstVerilog.Module
                    
                    let items = input.ModuleItems.ItemList |> Array.toList
                    
                    let varToCompMap, ioToCompMap, varSizeMap, initialCircuits, initialArrayCircuits, ioVars, arraySizeMap, arrayToCompMap = getInitialMapAndCircuits modInstVerilog project overrideMap
                    // failwithf "param override: %A" overrideMap

                    let perItemCircuits, _ =
                        compileModule (VerilogInput modInstVerilog) varToCompMap ioToCompMap varSizeMap overrideMap arraySizeMap arrayToCompMap initialCircuits initialArrayCircuits project compName model dispatch
                    let perItemCircuits =
                        perItemCircuits
                        |> Map.toList
                        |> List.sortBy (fun (s,c) -> Option.defaultValue -1 (List.tryFindIndex (fun var -> var=s) ioVars)) 
                    // failwithf "per item circuits: %A" perItemCircuits

                    let csList = 
                        perItemCircuits
                        |> List.map (fun (portName,circuit) ->
                            
                            attachToOutput varToCompMap ioToCompMap circuit portName
                        )
                    let v =
                        List.map (fun cs -> 
                            cs
                        ) csList

                    let finalCanvasState =
                        match List.isEmpty csList with
                        | true ->
                            (collectInputAndWireComps varToCompMap,[])
                        |false -> 
                            csList
                            |> List.reduce (fun cs1 cs2 -> concatenateCanvasStates cs1 cs2)
                            |> concatenateCanvasStates (collectInputAndWireComps varToCompMap,[])

                    let components = 
                        fst finalCanvasState
                        |> List.sortBy (fun (c) -> Option.defaultValue -1 (List.tryFindIndex (fun var -> var=c.Label) ioVars))
                    let finalCanvasState = 
                        (components, snd finalCanvasState)
                        |> fixCanvasState
                    // failwithf "Final Canvas State: %A" finalCanvasState


                    let toSaveCanvasState = Helpers.JsonHelpers.stateToJsonString (finalCanvasState, None, Some {
                                    Form = Some (Verilog name);
                                    Description=None;
                                    ParameterDefinitions = None})
                    // failwithf "cs: %A" toSaveCanvasState
                    // failwithf "path2 = %s" path2

                    match writeFile path2 toSaveCanvasState with
                    | Ok _ ->
                        // failwithf "path2 = %s" path2
                        let nestedComponent = 
                            match tryLoadComponentFromPath path2 with
                            |Ok comp -> comp
                            |Error _ -> failwithf "failed to load the created Verilog file"
                        // printf "initial project: %A" project

                        let updateParentCanvasStates (project: Project) (parentName: string) (oldName: string) (newName: string) =
                            let updateComp (comp: Component) =
                                let isOldInstance (ct: CustomComponentType) =
                                    isSameModuleIgnoringParams ct.Name oldName
                                match comp.Type with
                                | Custom ct when isOldInstance ct || ct.Name = oldName ->
                                    { comp with Type = Custom { ct with Name = newName } }
                                | _ -> comp
                            let updateCanvasState (cs: CanvasState) =
                                (fst cs |> List.map updateComp, snd cs)
                            { project with
                                LoadedComponents =
                                    project.LoadedComponents
                                    |> List.map (fun ldc ->
                                        if ldc.Name = parentName then
                                            { ldc with CanvasState = updateCanvasState ldc.CanvasState }
                                        else ldc)
                            }

                        let updatedProject =
                            {project with LoadedComponents = nestedComponent :: project.LoadedComponents}
                        let updatedProject =
                            updateParentCanvasStates updatedProject compName modInst.Module.Name name
                            
                        // printf "updated project: %A" updatedProject.LoadedComponents
                        openFileInProject project.OpenFileName updatedProject model dispatch
                        // failwithf "nested comp %A" nestedComponent
                        // failwithf "project: %A" project
                        // failwithf "Loaded component: %A, name: %s" nestedComponent name
                        nestedComponent, name
                    | Error _ -> failwithf "Writing .dgm file FAILED"

                | None -> loadedComp, modInst.Module.Name

            // failwithf "Loaded component: %A" loadedComp
            let (customCompType: CustomComponentType) =
                {
                    Name=name;
                    InputLabels=loadedComp.InputLabels;
                    OutputLabels=loadedComp.OutputLabels;
                    Form=None;
                    Description=None;
                    ParameterBindings = None
                }
            // failwithf "comp type: %A" customCompType

            let comp = createComponent (Custom customCompType) modInst.Identifier.Name
            // failwithf "Loaded component: %A" loadedComp
            let portLabels = loadedComp.InputLabels@loadedComp.OutputLabels
            let connections =
                modInst.Connections
                |> Array.sortBy (fun conn ->
                    match List.tryFindIndex (fun (id,w) -> id=conn.PortId.Name.ToUpper()) portLabels with
                    | Some idx -> idx
                    | _ -> failwithf "portId doesn't exist, should never happen"
                )
                |> Array.map (fun conn -> conn.Primary)
                |> Array.toList
            let inputPrimaries, outputPrimaries = List.splitAt loadedComp.InputLabels.Length connections
            let inputCircuits = 
                inputPrimaries
                |> List.map (fun primary -> createPrimaryCircuit primary varToCompMap varSizeMap paramMap)

            let topCircuit = {Conns=[]; Comps= [comp]; Out=comp.OutputPorts[0]; OutWidth=0}
            let inputCircuit = joinCircuits inputCircuits comp.InputPorts topCircuit
            
            let newCircuits =
                (currCircuits, List.zip outputPrimaries comp.OutputPorts) 
                ||> List.fold (fun circuits (primary, port) ->
                    let outPort = getPrimaryName primary
                    let bits = sliceFromBitsPrimary primary varToCompMap varSizeMap paramMap// need different logic for variable indexed bit select
                    // printf "outPort: %s, inputCircuit: %A, bits: %A" outPort inputCircuit bits
                    let circuit = {inputCircuit with Out=port; OutWidth=(bits.MSB-bits.LSB+1)}
                    let currCircuit = 
                        match Map.tryFind outPort circuits with
                        | Some c -> c
                        | _ -> failwithf "This should not happen, variable doesn't have a circuit"
                    let MSBs = 
                        if (currCircuit.OutWidth-bits.MSB-1) > 0 then
                            [sliceCircuit  currCircuit (currCircuit.OutWidth-bits.MSB-1) (bits.MSB+1)] // add logic to make sure this is not splitting off width 0!
                        else []
                    let LSBs = 
                        if bits.LSB > 0 then
                            [sliceCircuit currCircuit (bits.LSB) 0]
                        else [] // add logic to make sure this is not splitting off width 0!
                    let newCircuit = joinWithMerge' (LSBs @ [circuit] @ MSBs)
                    Map.add outPort newCircuit circuits

                )
            newCircuits, currArrayCircuits

            
            
        | _ -> currCircuits, currArrayCircuits
    let res = compileModuleRec node varToCompMap initialCircuits initialArrayCircuits // pass in everything set to 0 or flip flop output
    res
/////////   MAIN FUNCTION   //////////

let createSheet (veriloginput:VerilogInput) (project:Project) model dispatch= 
    let input = convertModule veriloginput.Module
    let items = input.ModuleItems.ItemList |> Array.toList
    
    let paramMap, _ = getParamMap veriloginput [] items
    
    let varToCompMap, ioToCompMap, varSizeMap, initialCircuits, initialArrayCircuits, ioVars, arraySizeMap, arrayToCompMap = getInitialMapAndCircuits veriloginput project paramMap
    let compName = input.ModuleName.Name
    let perItemCircuits, perArrayCircuits =
        compileModule (VerilogInput veriloginput) varToCompMap ioToCompMap varSizeMap paramMap arraySizeMap arrayToCompMap initialCircuits initialArrayCircuits project compName model dispatch
    let perItemCircuits =
        perItemCircuits
        |> Map.toList
        |> List.sortBy (fun (s,c) -> Option.defaultValue -1 (List.tryFindIndex (fun var -> var=s) ioVars)) 
    let perArrayCircuits =
        perArrayCircuits
        |> Map.toList
        |> List.sortBy (fun (s,c) -> Option.defaultValue -1 (List.tryFindIndex (fun var -> var=s) ioVars))
       
    // printfn "peritemcircuits %A" perItemCircuits

    // list of canvas states, one per output
    // here all slices are merged together with mergeWires to create one CanvasState per output
    // ex. assign out[5:4],assign out[3:1], assign out[0] -> CanvasState for output port: {out}
    let csList = 
        perItemCircuits
        |> List.map (fun (portName,circuit) ->
            // if (portName = "i") then printfn "circuit %A" circuit
            
            attachToOutput varToCompMap ioToCompMap circuit portName
        )
    let arrayCsList =
        perArrayCircuits
        |> List.map (fun (portName, circuitList) ->
            attachArrayToOutput arrayToCompMap circuitList portName
        )

    let csList =
        csList @ arrayCsList

    let v =
        List.map (fun cs -> 
            cs
        ) csList
    // concatenate canvasStates from csList
    // add input and wire components in CanvasState
    // (these are added now because they must appear only once in final Canvas State)
    // fix it by changing the label and position of components so that these are unique per component
    let finalCanvasState =
        match List.isEmpty csList with
        | true ->
            (collectInputAndWireComps varToCompMap,[])
            //|> fixCanvasState
        |false -> 
            csList
            |> List.reduce (fun cs1 cs2 -> concatenateCanvasStates cs1 cs2)
            |> concatenateCanvasStates (collectInputAndWireComps varToCompMap,[])
            //|> fixCanvasState

    let components = 
        fst finalCanvasState
        |> List.sortBy (fun (c) -> Option.defaultValue -1 (List.tryFindIndex (fun var -> var=c.Label) ioVars))
   
    // components
    // |> List.iter (fun comp ->
    //     // printfn "component label = %s" comp.Label
    // )

    let finalCanvasState = 
        (components, snd finalCanvasState)
        |> fixCanvasState
    // failwithf "Final Canvas State: %A" finalCanvasState
    finalCanvasState


// 1. create wire label for every variable and port maybe bit by bit?? shouldn't be bit by bit because performance
// 2. create circuit for rhs of cont assign using wire labels: rhs variables use output of wire label, lhs is input of wire label
// maybe transform the ast into a variable name -> "smth easily translated to issie components"
// go through continuous assignments lhs -> rhs circuit


