module SheetCreator

open VerilogTypes
open CommonTypes
open DrawHelpers
open Helpers

//// HELPERS  ////

let findEmptyXY (oldMap: (string*Component) list) : XYPos = 
    let size  = List.length oldMap
    match size with
    |0 -> {X=100.;Y=100.}
    |_ ->
        let lastComp = snd oldMap[size-1]
        let prevX,prevY = lastComp.X,lastComp.Y
        {X=prevX+60.;Y=prevY+60.}

let findEmptyXYFromCanvasState (cs: CanvasState) : XYPos = 
    let lastComp = List.tryLast (fst cs)
    match lastComp with
    |None -> {X=100.;Y=100.}
    |Some c ->
        let prevX,prevY = c.X,c.Y
        {X=prevX+60.;Y=prevY+60.}

let getWidthFromRange (range:RangeT option) = 
    match range with
    |None -> 1
    |Some r ->
        let start = r.Start |> int
        start+1

let createComponent id compType name inputPorts outputPorts x y =
    {
        Id = id
        Type = compType
        Label = name
        InputPorts = inputPorts 
        OutputPorts = outputPorts
        X = x
        Y = y
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

let updateCanvasState (newComps:Component list) (newConns:Connection list) (oldCanvasState:CanvasState) : CanvasState =
    match oldCanvasState with
    |(comps,conns) -> 
        ( comps@newComps , conns@newConns)    


//////////////////////



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
        let newPos = findEmptyXY map

        let inputPorts,outputPorts =
            match ioType with
            |"input_decl" -> [],[createPort id PortType.Output (Some 0)]
            |_ -> [createPort id PortType.Input (Some 0)],[]

        map@[(name,(createComponent id compType name inputPorts outputPorts newPos.X newPos.Y))]
    )

  


let getIOtoComponentMap (ioDecls:ItemT list) = 
    ([],ioDecls)
    ||> List.fold (fun map item ->
        createIOComponent item item.ItemType map
    )
    |> Map.ofList


// let buildCanvasStateForExpression (expression: ExpressionT) ioToCompMap = 

let buildInputConnections (rhs:ExpressionT) (mainComp:Component) (ioToCompMap:Map<string,Component>) =
    let target1 = mainComp.InputPorts[0]
    let target2 = mainComp.InputPorts[1]
    let source1 =
        Map.find (Option.get (Option.get (Option.get rhs.Head).Unary).Primary).Primary.Name ioToCompMap
    let source2 = 
        Map.find (Option.get (Option.get (Option.get rhs.Tail).Unary).Primary).Primary.Name ioToCompMap
    createConnection source1.OutputPorts[0] target1


let buildLhsRhsConnection (mainComp: Component) (outputComp: Component) (oldCanvasState:CanvasState)= 
    let source = mainComp.OutputPorts[0]
    let target =  outputComp.InputPorts[0]
    let newCon = createConnection source target
    updateCanvasState [] [newCon] oldCanvasState


let createConnectionWithUnaryPort (unary:UnaryT) prevComp ioToCompMap oldCanvasState = 
    
    let createConnectionWithPrimary (primary:PrimaryT) =
        let name = primary.Primary.Name
        //TODO: size as well
        let input = Map.find name ioToCompMap
        let conn = createConnection input.OutputPorts[0] prevComp.InputPorts[0]
        updateCanvasState [] [conn] oldCanvasState

    match unary.Type with
    |"primary" -> 
        createConnectionWithPrimary (Option.get unary.Primary)
    |_ -> oldCanvasState //TODO



let buildExpressionComponent (rhs:ExpressionT) widthList oldCanvasState =
    let compId = DrawHelpers.uuid()
    let inputPorts = 
        match rhs.Type with
        | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" 
        | "additive" | "logical_AND" | "logical_OR" ->
            [createPort compId PortType.Input (Some 0); createPort compId PortType.Input (Some 1)]
        |_ -> [] //TODO
    let outputPorts =
        match rhs.Type with
        | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" 
        | "additive" | "logical_AND" | "logical_OR" ->
            [createPort compId PortType.Output (Some 0)]
        |_ -> [] //TODO

    let width =
            match List.length widthList with
            |1 -> widthList[0]
            |x -> widthList[x-1]-widthList[0]

    let compType =
        match rhs.Type with
        // | "bitwise_OR" ->  
        | "bitwise_XOR" -> (NbitsXor width)
        | "bitwise_AND" -> (NbitsAnd width)
        | "additive" -> (NbitsAdder width)
        // | "logical_AND" -> 
        // | "logical_OR" ->
        |_ -> (NbitsAdder width) //TODO
    
    let name =
        match rhs.Type with
        |"bitwise_XOR" -> "NXOR"
        |_ -> "TODO"

    let emptyPos = findEmptyXYFromCanvasState oldCanvasState
    
    let comp = createComponent compId compType name inputPorts outputPorts emptyPos.X emptyPos.Y 
    comp
    // updateCanvasState [comp] [] oldCanvasState





let rec expressionUpdateCanvasState (expr:ExpressionT) prevComp widthList ioToCompMap (oldCanvasState:CanvasState) : CanvasState = 
    
    let comp = buildExpressionComponent expr widthList oldCanvasState      
    let conn = createConnection comp.OutputPorts[0] prevComp.InputPorts[0]
    let newCS = updateCanvasState [comp] [conn] oldCanvasState 
    
    
    match expr.Type with
    | "unary" -> createConnectionWithUnaryPort (Option.get expr.Unary) comp ioToCompMap newCS
    |_ ->
        expressionUpdateCanvasState (Option.get expr.Head) comp widthList ioToCompMap newCS
        |> expressionUpdateCanvasState (Option.get expr.Head) comp widthList ioToCompMap



let buildCanvasStateForAssignment (statement:StatementItemT) (ioToCompMap:Map<string,Component>) (prevCS:CanvasState) =
    match statement.StatementType with
    |"assign" ->
        let outputComp = Map.find statement.Assignment.LHS.Primary.Name ioToCompMap
        let outId = outputComp.Id
        let bits = 
            match (statement.Assignment.LHS.BitsStart,statement.Assignment.LHS.BitsEnd) with
            |(Some s,Some e) -> [(int e)..(int s)]
            |_ ->
                match outputComp.Type with
                |Output width -> [0..(width-1)]  
                |_ -> failwithf "Can't happen!"
        prevCS
        |> expressionUpdateCanvasState statement.Assignment.RHS outputComp bits ioToCompMap 
        // |> buildLhsRhsConnection () outputComp
    
    |_ -> prevCS







//// MAIN FUNCTION ////

let createSheet input = 
    let items = input.Module.ModuleItems.ItemList |> Array.toList
    let ioDecls = items |> List.filter (fun item -> Option.isSome item.IODecl)
    let assignments = items |> List.filter (fun item -> Option.isSome item.Statement) 
    
    let ioToCompMap = getIOtoComponentMap ioDecls
    printfn "test %A" ioToCompMap
    let ioComps = ioToCompMap |> Map.toList |> List.map snd
    
    let finalCanvasState = 
        ((ioComps,[]),assignments)
        ||> List.fold (fun cState assignment ->
            buildCanvasStateForAssignment (Option.get assignment.Statement) ioToCompMap cState    
        )
    
    Helpers.JsonHelpers.stateToJsonString (finalCanvasState, None)
