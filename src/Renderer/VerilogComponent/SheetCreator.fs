module SheetCreator

open VerilogTypes
open CommonTypes
open DrawHelpers
open Helpers

//// HELPERS  ////

type SheetCreationInfo = {
    Components: Component list
    Connections: Connection list
    UsedNames: Map<string,int>
}

let findEmptyXY (oldMap: (string*Component) list) : XYPos = 
    let size  = List.length oldMap
    match size with
    |0 -> {X=100.;Y=100.}
    |_ ->
        let lastComp = snd oldMap[size-1]
        let prevX,prevY = lastComp.X,lastComp.Y
        {X=prevX+130.;Y=prevY+130.}

let findEmptyXYFromSheetCreationInfo (sheetInfo: SheetCreationInfo) : XYPos = 
    let comps = sheetInfo.Components
    let lastComp = List.tryLast comps
    match lastComp with
    |None -> {X=100.;Y=100.}
    |Some c ->
        let prevX,prevY = c.X,c.Y
        {X=prevX+130.;Y=prevY+130.}

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

let updateSheetInfo (newComps:Component list) (newConns:Connection list) (newMap:Map<string,int>) (oldSheetInfo:SheetCreationInfo) : SheetCreationInfo =   
    {
        Components = oldSheetInfo.Components@newComps;
        Connections = oldSheetInfo.Connections@newConns;
        UsedNames = newMap
    }

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






let buildExpressionComponent (rhs:ExpressionT) widthList oldSheetInfo =
    let compId = DrawHelpers.uuid()
    let inputPorts = 
        match rhs.Type with
        | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" 
        | "additive" | "logical_AND" | "logical_OR" ->
            [createPort compId PortType.Input (Some 0); createPort compId PortType.Input (Some 1)]
        |_ -> [] //TODO
    let outputPorts =
        match rhs.Type with
        | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" ->
            [createPort compId PortType.Output (Some 0)]
        |_ -> [] //TODO

    let width =
            match List.length widthList with
            |1 -> widthList[0]+1
            |x -> widthList[x-1]-widthList[0]+1

    let compType =
        match rhs.Type with
        // | "bitwise_OR" ->  
        | "bitwise_XOR" -> (NbitsXor width)
        | "bitwise_AND" -> (NbitsAnd width)
        | "additive" -> (NbitsAdder width)
        // | "logical_AND" -> 
        // | "logical_OR" ->
        |_ -> (NbitsAdder width) //TODO
    
    let baseName =
        match rhs.Type with
        |"bitwise_XOR" -> "NXOR"
        | "additive" -> "ADD"
        | "bitwise_AND" -> "AND"
        | "negation" -> "NOT"
        |_ -> "TODO"

    let name =
        match Map.tryFind baseName oldSheetInfo.UsedNames with
        |Some x -> baseName + (string (x+1))
        |None -> baseName + "1" 
    

    let updatedNamesUsed =
        match Map.tryFind baseName oldSheetInfo.UsedNames with
        |Some x -> Map.add baseName (x+1) oldSheetInfo.UsedNames
        |None -> Map.add baseName 1 oldSheetInfo.UsedNames

    let emptyPos = findEmptyXYFromSheetCreationInfo oldSheetInfo
    
    let comp = createComponent compId compType name inputPorts outputPorts emptyPos.X emptyPos.Y 
    comp,updatedNamesUsed



let createConnectionWithUnaryPort (unary:UnaryT) prevComp portNo ioToCompMap oldSheetInfo = 
    
    let createConnectionWithPrimary (primary:PrimaryT) =
        let name = primary.Primary.Name
        let inputComp = Map.find name ioToCompMap
        match Option.isNone primary.BitsStart with
        |true -> 
            let conn = createConnection inputComp.OutputPorts[0] prevComp.InputPorts[portNo]
            updateSheetInfo [] [conn] oldSheetInfo.UsedNames oldSheetInfo
        |false ->
            let bStart,bEnd = (int (Option.get primary.BitsStart)),(int (Option.get primary.BitsEnd))
            
            // TODO : integrate this in the buildExpressionComponent function
            let lsb,outWidth = bEnd,(bStart-bEnd+1)
            let busSelCompId = DrawHelpers.uuid()
            let inputPorts = [createPort busSelCompId PortType.Input (Some 0)]
            let outputPorts = [createPort busSelCompId PortType.Output (Some 0)]
            let emptyPos = findEmptyXYFromSheetCreationInfo oldSheetInfo

            let busSelComp = createComponent busSelCompId (BusSelection (outWidth,lsb)) "" inputPorts outputPorts emptyPos.X emptyPos.Y 

            let conn1 = createConnection busSelComp.OutputPorts[0] prevComp.InputPorts[portNo]
            let conn2 = createConnection inputComp.OutputPorts[0] busSelComp.InputPorts[0]     

            updateSheetInfo [busSelComp] [conn1;conn2] oldSheetInfo.UsedNames oldSheetInfo

    match unary.Type with
    |"primary" -> 
        createConnectionWithPrimary (Option.get unary.Primary)
    |_ -> oldSheetInfo //TODO: numbers, etc.



let rec expressionUpdateCanvasState (expr:ExpressionT) prevComp portNo widthList ioToCompMap (prevSI:SheetCreationInfo) : SheetCreationInfo = 
    
    let comp,newMap = buildExpressionComponent expr widthList prevSI      
    let conn = createConnection comp.OutputPorts[0] prevComp.InputPorts[portNo]
    let newCS = updateSheetInfo [comp] [conn] newMap prevSI 
    
    rhsUpdateCanvasState (Option.get expr.Head) comp 0 widthList ioToCompMap newCS
    |> rhsUpdateCanvasState (Option.get expr.Tail) comp 1 widthList ioToCompMap

and rhsUpdateCanvasState (expr:ExpressionT) prevComp portNo widthList ioToCompMap (prevSI:SheetCreationInfo) : SheetCreationInfo = 
    match expr.Type with
    |"unary" -> createConnectionWithUnaryPort (Option.get expr.Unary) prevComp portNo ioToCompMap prevSI
    |_ -> expressionUpdateCanvasState expr prevComp portNo widthList ioToCompMap prevSI
        


let buildCanvasStateForAssignment (statement:StatementItemT) (ioToCompMap:Map<string,Component>) (prevSI:SheetCreationInfo) =
    match statement.StatementType with
    |"assign" ->
        let outputComp = Map.find statement.Assignment.LHS.Primary.Name ioToCompMap
        let bits = 
            match (statement.Assignment.LHS.BitsStart,statement.Assignment.LHS.BitsEnd) with
            |(Some s,Some e) -> [(int e)..(int s)]
            |_ ->
                match outputComp.Type with
                |Output width -> [0..(width-1)]  
                |_ -> failwithf "Can't happen!"
        prevSI
        |> rhsUpdateCanvasState statement.Assignment.RHS outputComp 0 bits ioToCompMap 
    
    |_ -> prevSI //TODO: wires




//// MAIN FUNCTION ////

let createSheet input = 
    let items = input.Module.ModuleItems.ItemList |> Array.toList
    let ioDecls = items |> List.filter (fun item -> Option.isSome item.IODecl)
    let assignments = items |> List.filter (fun item -> Option.isSome item.Statement) 
    
    let ioToCompMap = getIOtoComponentMap ioDecls
    // printfn "test %A" ioToCompMap
    let ioComps = ioToCompMap |> Map.toList |> List.map snd
    
    let initialSheetInfo = 
        {
            Components = ioComps
            Connections = []
            UsedNames = Map.empty<string,int> 
        }

    let finalSheetInfo: SheetCreationInfo = 
        (initialSheetInfo,assignments)
        ||> List.fold (fun sheetCreationInfo assignment ->
            buildCanvasStateForAssignment (Option.get assignment.Statement) ioToCompMap sheetCreationInfo    
        )
    
    let finalCanvasState = finalSheetInfo.Components,finalSheetInfo.Connections
    Helpers.JsonHelpers.stateToJsonString (finalCanvasState, None)
