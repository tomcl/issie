module ErrorCheck

open VerilogTypes
open Fable.Core.JsInterop
open CommonTypes 

let private getFileInProject name project = project.LoadedComponents |> List.tryFind (fun comp -> comp.Name = name)

let private isFileInProject name project =
    getFileInProject name project
    |> function
    | None -> false
    | Some _ -> true

/// Helper function to create an ErrorInfo-type Error Message 
/// given the location, the variable name, and the message
let createErrorMessage 
    (newLinesLocations: int list)
    (currLocation: int)
    (message: string)
    (extraMessages: ExtraErrorInfo array)
    (name: string)
        : ErrorInfo list = 
      
    let isSmallerThan x y = y <= x
    
    let prevIndex = List.findIndexBack (fun x -> isSmallerThan currLocation x) newLinesLocations
    let line = prevIndex+1
    let prevLineLocation = newLinesLocations[prevIndex]
    let length = String.length name
    
    [{Line = line; Col=currLocation-prevLineLocation+1;Length=length;Message = message;ExtraErrors=Some extraMessages}]


/// Recursive function to get all the primaries used in the RHS of an assignment
/// Used by checkNamesOnRHSOfAssignment and checkSizesOnRHSOfAssignment
let rec primariesUsedInAssignment inLst (tree: ExpressionT) = 
    match tree.Type with
    | "unary" when (Option.get tree.Unary).Type = "primary" 
        -> List.append inLst [(Option.get (Option.get tree.Unary).Primary)]
    | "unary" when (Option.get tree.Unary).Type = "parenthesis" 
        -> primariesUsedInAssignment inLst   (Option.get (Option.get tree.Unary).Expression)
    | "unary" when (Option.get tree.Unary).Type = "concat" 
        -> primariesUsedInAssignment inLst  (Option.get (Option.get tree.Unary).Expression)
    | "negation" | "reduction" when (Option.get tree.Unary).Type = "primary" 
        -> List.append inLst [(Option.get (Option.get tree.Unary).Primary)] 
    | "negation" | "reduction" when (Option.get tree.Unary).Type = "parenthesis" 
        -> primariesUsedInAssignment inLst (Option.get (Option.get tree.Unary).Expression)    
    | "unary" when (Option.get tree.Unary).Type = "number" -> 
        match (Option.get (Option.get tree.Unary).Number).NumberType with
        | "all" -> 
            let afterBitsSection = (string ((Option.get (Option.get (Option.get tree.Unary).Number).Base)[1])) + (Option.get (Option.get (Option.get tree.Unary).Number).AllNumber)
            List.append inLst 
                    [(
                            {
                            Type= "primary"; 
                            PrimaryType= afterBitsSection; 
                            BitsStart= Some "-3";
                            BitsEnd= Some (Option.get (Option.get (Option.get tree.Unary).Number).Bits); 
                            Primary= {
                                Name="delete123";
                                Location=(Option.get (Option.get tree.Unary).Number).Location
                                }
                            }
                        )]
        | _ -> inLst

    | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" 
    | "additive" | "SHIFT" | "logical_AND" 
    | "logical_OR" | "unary_list" 
    | "conditional_cond" | "conditional_result"
        -> List.append 
            (primariesUsedInAssignment inLst (Option.get tree.Head))
            (if isNullOrUndefined tree.Tail 
                        then inLst 
                    else primariesUsedInAssignment inLst (Option.get tree.Tail))
    | _ -> inLst

/// Checks whether all ports given in the beginning of the module are defined as input/output
/// Also if all ports have distinct names
let portCheck ast linesLocations errorList  = 
    let portList = ast.Module.PortList |> Array.toList
    let distinctPortList = portList |> Seq.distinct |> List.ofSeq

    let locationList = ast.Module.Locations |> Array.toList
    let locationMap =
        (portList, locationList) ||> List.map2 (fun p i -> (p,int i)) |> Map.ofList
    match ast.Module.Type with
    |"module_new" -> errorList //if new-style there is no port list
    |_ ->
        match List.length portList = List.length distinctPortList with
        | false ->  //CASE 1: ports with same name
            portList
            |> List.map (fun name -> name.ToUpper())
            |> Seq.countBy id
            |> Map.ofSeq
            |> Map.filter (fun name count -> count > 1)
            |> Map.toList
            |> List.map fst
            |> List.collect (fun name ->
                let message = "Ports must have different names"     
                let extraMessages = [|
                    {Text=sprintf "Name '%s' has already been used for a port \n Please use a different name" name ;Copy=false;Replace=NoReplace}
                |]       
                createErrorMessage linesLocations locationMap[name] message extraMessages name
                )        
            |> List.append errorList 
    
        | true -> // Distinct names
            let items = ast.Module.ModuleItems.ItemList |> Array.toList
            let decls = 
                items |> List.collect (fun x -> 
                    match (x.IODecl |> isNullOrUndefined) with
                    | false -> 
                        match x.IODecl with
                        | Some d -> 
                            d.Variables 
                            |> Array.toList 
                            |> List.collect (fun x -> [x.Name]) 
                        | None -> []
                    | true -> []
                )
            let diff = List.except decls portList
            match Seq.isEmpty diff with
            | false ->  //CASE 2: ports not declared as input/output
                diff
                |> List.collect (fun name ->
                    let message = sprintf "Port '%s' is not declared either as input or output" name
                    let extraMessages = 
                        [|
                            {Text=sprintf "Port '%s' must be declared as input or output" name;Copy=false;Replace=NoReplace}
                            {Text=sprintf "input %s;|output %s;" name name;Copy=true;Replace=IODeclaration}
                        |]
                    createErrorMessage linesLocations locationMap[name] message extraMessages name
                )
                |> List.append errorList
            | true -> //CASE 3: no errors 
                errorList

/// Checks whether all ports defined as input/output are declared as ports in the module header
/// Also checks for double definitions and for input ports not used in the assignments
let checkIODeclarations 
    (ast: VerilogInput)
    (portWidthDeclarationMap: Map<string,int*int>) 
    (portLocationMap: Map<string,int>) 
    (linesLocations: int list) 
    (nonUniquePortDeclarations: string list)
    (portMap: Map<string,string>)
    (items: ItemT list)
    (errorList: ErrorInfo list)
        : ErrorInfo list = 
    
    let portList = ast.Module.PortList |> Array.toList
    let assignments = List.filter (fun item -> (Option.isSome item.Statement)) items

    let allPrimariesUsed =
        assignments
        |> List.map (fun x -> 
            primariesUsedInAssignment [] (Option.get x.Statement).Assignment.RHS
        )
        |> List.concat
        |> List.map (fun primary -> primary.Primary.Name)


    portWidthDeclarationMap
    |> Map.toList
    |> List.map fst
    |> List.collect (fun port -> 
        match ((List.tryFind (fun p -> p=port) allPrimariesUsed),(Map.tryFind port portMap)) with
        |None, Some "input" -> // CASE 1: port is not used in the assignments
            let currLocation = Map.find port portLocationMap
            let message = sprintf "Variable '%s' is defined as an input port but is not used" port
            let extraMessages =
                [|
                    {Text=sprintf "Variable '%s' is defined as an input port but is not used \n Please delete it if it is not needed" port;Copy=false;Replace=NoReplace}
                |]
            createErrorMessage linesLocations currLocation message extraMessages port
        | _, _ ->
            match (List.tryFind (fun p -> p=port) portList) with
            | None -> // CASE 2: Doesn't exist in the module header
                let currLocation = Map.find port portLocationMap
                let message = sprintf "Variable '%s' is not defined as a port in the module declaration" port
                let extraMessages =
                    [|
                        {Text=sprintf "Variable '%s' is not defined as a port \n Please define it in the module declaration" port;Copy=false;Replace=NoReplace}
                    |]
                createErrorMessage linesLocations currLocation message extraMessages port
            | Some _ -> // Exists in module header
                match List.tryFind (fun p -> p=port) nonUniquePortDeclarations with
                |Some found -> // CASE 3: Double definition
                    let currLocation = Map.find port portLocationMap
                    let message = sprintf "Port '%s' is already defined" port
                    let extraMessages =
                        [|
                            {Text=sprintf "Port '%s' is already defined" port ;Copy=false;Replace=NoReplace}
                        |]
                    createErrorMessage linesLocations currLocation message extraMessages port
                |None -> [] //CASE 4: No errors
    )
    |> List.append errorList   

/// Checks whether the IO declarations have correct width format (i.e. Little-endian)
let checkIOWidthDeclarations (ast: VerilogInput) linesLocations errorList  =
    ast.Module.ModuleItems.ItemList
    |> Array.filter (fun item -> 
        item.ItemType = "output_decl" || item.ItemType = "input_decl"  
    )
    |> Array.toList
    |> List.map (fun item -> Option.get item.IODecl)
    |> List.collect (fun ioDecl ->
        match isNullOrUndefined ioDecl.Range with
        | true -> [] //No range given (i.e. one bit)
        | false -> 
            let range = Option.get ioDecl.Range
            // CASE 1: Wrong width format
            if (range.End <> "0" || (int range.Start) <= (int range.End)) then
                let message = "Wrong width declaration"
                let temp = if (int range.Start) <= (int range.End) then "\nBig-Endian format is not allowed yet by ISSIE" else ""
                let extraMessages = 
                    [|
                        {Text=(sprintf "A port's width can't be '[%s:%s]'\nCorrect form: [X:0]" range.Start range.End)+temp;Copy=false;Replace=NoReplace}
                    |]
                createErrorMessage linesLocations range.Location message extraMessages (range.Start+"[:0]")
            else [] //CASE 2: No Errors
    )
    |> List.append errorList


/// Checks if the name of the module is valid (i.e. this sheet doesn't exist)
let nameCheck ast linesLocations (origin:CodeEditorOpen) (project:Project)  errorList = 
    let moduleName =  ast.Module.ModuleName.Name
    // printfn "working %s" (Option.defaultValue "" project.WorkingFileName) 
    let exists = 
        match origin with
        |NewVerilogFile -> isFileInProject moduleName project 
        |UpdateVerilogFile initialName -> moduleName <> initialName

    let localError = 
        match (exists,origin) with
        |true,NewVerilogFile -> 
            let message = "A sheet with that name already exists"
            let extraMessages = 
                [|
                    {Text="Module Name must be different from existing Sheets/Components";Copy=false;Replace=NoReplace}
                |]
            createErrorMessage linesLocations ast.Module.ModuleName.Location message extraMessages moduleName
        |true,UpdateVerilogFile _ ->
            let message = "Verilog component's name cannot be changed "
            let extraMessages = 
                [|
                    {Text="Module Name of Verilog component cannot be changed";Copy=false;Replace=NoReplace}
                |]
            createErrorMessage linesLocations ast.Module.ModuleName.Location message extraMessages moduleName
        |false,_ ->
            []
    
    List.append localError errorList


/// Checks if all declared output ports have a value assigned to them
/// The check is done bit-by-bit
let checkAllOutputsAssigned
    (ast:VerilogInput) 
    (portMap: Map<string,string>)
    (portSizeMap: Map<string,int>)  
    (linesLocations: int list)
    (errorList: ErrorInfo list)
        : ErrorInfo list =
    

    // List of declared ports, bit by bit
    // e.g. output [2:0] b -> b0,b1,b2
    let outputPortListMap = 
        portMap 
        |> Map.filter (fun n s -> s = "output") 
        |> Map.toList 
        |> List.map fst
        |> List.collect (fun x -> 
            let size = Map.find x portSizeMap
            let names = [0..size-1] |> List.map (fun y -> (x+(string y),x))
            names 
        )

    let outputPortList = List.map fst outputPortListMap

    // List of assignments in the form of (port name, BitsStart, BitsEnd)
    let assignments = 
        ast.Module.ModuleItems.ItemList
        |> Array.toList 
        |> List.collect (fun x -> 
            match (x.Statement |> isNullOrUndefined) with
            | false -> 
                match x.Statement with
                | Some statement when statement.StatementType = "assign" -> [statement.Assignment.LHS]
                | _ -> []
            | true -> []
        )
        |> List.map (fun assignment ->
            match assignment with
            | a when isNullOrUndefined assignment.BitsStart -> (a.Primary.Name,-1,-1)
            | a -> (a.Primary.Name,(int (Option.get a.BitsStart)),(int (Option.get a.BitsEnd)))
        )
    
    
    // List of assigned ports, bit by bit
    let assignmentPortListMap =
        assignments
        |> List.collect ( fun x ->
            match x with
            |(name,-1,-1)->
                match Map.tryFind name portSizeMap with
                | Some size -> 
                    let names = [0..size-1] |> List.map (fun y -> (name+(string y),name))
                    names
                | None -> []
            |(name,x,y) when x=y ->
                [(name+(string x),name)]
            |(name,bStart,bEnd)->
                let names = [bEnd..bStart] |> List.map (fun y -> (name+(string y),name))
                names
        )
    let assignmentPortList = List.map fst assignmentPortListMap
   
    let genErrorMessage portList mapping errorType mess  = 
        match List.isEmpty portList with
        |true -> []
        |false ->
            // transform names from "b2" to "b[2]" 
            let fullNames = 
                portList 
                |> List.collect(fun x ->
                    match Map.tryFind x (Map.ofList mapping) with
                    | Some name -> 
                        let length = (Seq.except name x) |> Seq.map string |> String.concat ""
                        [name+"["+length+"]"]
                    | None -> []
                )
            let currLocation = linesLocations[((List.length linesLocations)-2)]
            let message = mess
            let extraMessages = 
                match errorType with
                |Unassigned ->
                    [|
                        {Text=sprintf "The following ports are declared but not assigned: %A" fullNames;Copy=false;Replace=NoReplace};
                        {Text=sprintf "assign %s = 1'b0;" fullNames[0];Copy=true;Replace=Assignment}
                    |]
                |DoubleAssignment ->
                    [|
                    {Text=sprintf "The following ports are assigned more than once: %A" fullNames;Copy=false;Replace=NoReplace};
                    |]
            createErrorMessage linesLocations currLocation message extraMessages "endmodule"
    
    let countAssignments = assignmentPortList |> List.countBy id
    let notUnique = 
        countAssignments
        |> List.filter (fun (x,y)->y>1)
        |> List.map fst

    let unassignedPorts = List.except (List.toSeq assignmentPortList) (outputPortList)

    let localErrors =
        match unassignedPorts with
        |[] -> genErrorMessage notUnique outputPortListMap DoubleAssignment "Some output ports have been assigned more than once"
        |_ -> genErrorMessage unassignedPorts outputPortListMap Unassigned "All output ports must be assigned"

    
    List.append errorList localErrors


/// Helper function used by checkWidthOfAssignment
/// with 3 recursive subfunctions
/// Returns the RHS Unary Size tree of type OneUnary
/// where OneUnary = {Name:string;ResultWidth:int;Head:OneUnary option;Tail:OneUnary option;Elements:OneUnary list}
let RHSUnaryAnalysis
    (assignmentRHS:ExpressionT)
    (inputWireSizeMap: Map<string,int>)
        : OneUnary =

    let rec findSizeOfExpression (tree:ExpressionT) : OneUnary = 
        match tree.Type with
        | "unary" |"negation" when (Option.get tree.Unary).Type = "primary" ->
            let primary = Option.get (Option.get tree.Unary).Primary
            match isNullOrUndefined primary.BitsStart with
                    | true -> 
                        match Map.tryFind primary.Primary.Name inputWireSizeMap with
                        | Some num -> {Name=primary.Primary.Name;ResultWidth=num;Head=None;Tail=None;Elements=[]}
                        | None -> {Name="undefined";ResultWidth=(0);Head=None;Tail=None;Elements=[]} // if name doesn't exist skip it, error found by assignmentRHSNameCheck
                    | false -> 
                        {Name=primary.Primary.Name;ResultWidth=((Option.get primary.BitsStart) |> int) - ((Option.get primary.BitsEnd) |> int) + 1;Head=None;Tail=None;Elements=[]}
                        
        | "unary" when (Option.get tree.Unary).Type = "number" ->
                {Name="[number]";ResultWidth=int <| (Option.get (Option.get (Option.get tree.Unary).Number).Bits) ;Head=None;Tail=None;Elements=[]}
        
        | "unary" when (Option.get tree.Unary).Type = "concat" -> 
            let unariesList = (findSizeOfConcat (Option.get (Option.get tree.Unary).Expression) [])
            let length= (0,unariesList) ||> List.fold(fun s unary-> s+unary.ResultWidth)
            {Name="{...}";ResultWidth=length;Head=None;Tail=None;Elements=unariesList}
       
        | "unary" |"negation" when (Option.get tree.Unary).Type = "parenthesis" -> 
            let elements = (findSizeOfExpression (Option.get (Option.get tree.Unary).Expression))
            {Name="(...)";ResultWidth=elements.ResultWidth;Head=None;Tail=None;Elements=[elements]}

        | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" 
        | "additive" 
            -> 
            let u1 = findSizeOfExpression (Option.get tree.Head)
            let u2 = findSizeOfExpression (Option.get tree.Tail)
            {Name="[bitwise_op]";ResultWidth=u1.ResultWidth;Head=Some u1;Tail=Some u2;Elements=[]}
            

        | "conditional_cond" -> 
            let result = (findSizeOfExpression (Option.get tree.Head))
            let u1 = (findSizeOfExpression (Option.get (Option.get tree.Tail).Head))
            let u2 = (findSizeOfExpression (Option.get (Option.get tree.Tail).Tail))
            {Name="[conditional]";ResultWidth=u1.ResultWidth;Head=Some u1;Tail=Some u2;Elements=[result]}
        
        | "SHIFT" ->
            let u1 = (findSizeOfExpression (Option.get tree.Head))
            {Name="[shift]";ResultWidth=u1.ResultWidth;Head=Some u1;Tail=None;Elements=[]}

        | "reduction" when (Option.get tree.Unary).Type = "parenthesis" ->
            let result = findSizeOfExpression (Option.get (Option.get tree.Unary).Expression)
            {Name="[reduction]";ResultWidth=1;Head=None;Tail=None;Elements=[result]} 

        | "reduction" ->
            {Name="[reduction]";ResultWidth=1;Head=None;Tail=None;Elements=[]} 

        | "logical_OR" | "logical_AND" ->
            let u1 = findSizeOfExpression (Option.get tree.Head)
            let u2 = findSizeOfExpression (Option.get tree.Tail)
            {Name="[logical_op]";ResultWidth=1;Head=Some u1;Tail=Some u2;Elements=[]}
        | _ -> failwithf "Case not covered!"

    and findSizeOfConcat (tree:ExpressionT) concatList : OneUnary list=
        
        match isNullOrUndefined tree.Tail with
        |true -> concatList@[(findSizeOfExpression (Option.get tree.Head))]
        |false ->
            let updated = concatList@[(findSizeOfExpression (Option.get tree.Head))]
            findSizeOfConcat (Option.get tree.Tail) updated
    

    findSizeOfExpression assignmentRHS


/// Helper recursive function to transform the produced OneUnary-type tree
/// by RHSUnaryAnalysis to a string which can be used for ErrorInfo
let rec unaryTreeToString treeDepth targetLength (unary:OneUnary)  =
    
    let targetLength' = targetLength //if targetLength=(-2) then 1 else targetLength
    let depthToSpaces = ("",[0..treeDepth])||>List.fold (fun s v -> s+"   ") 
    let sizeString =
        match targetLength' with
        |(-1) -> (string unary.ResultWidth)
        |(-2) when unary.ResultWidth<>1  -> (string unary.ResultWidth)+" -> ERROR! (Exp: 1, condition must be a single bit!)"
        |(-2) -> (string unary.ResultWidth)
        |x when x=(unary.ResultWidth)-> (string unary.ResultWidth)
        |_ -> (string unary.ResultWidth)+" -> ERROR! (Exp: "+(string targetLength')+")"
    
    let propagatedLength =
            match unary.Name with
            |"{...}" -> (-1)
            |"[condition]" -> targetLength
            |"[reduction]" -> (-1)
            |"[logical_op]" -> (-1)
            | _ -> unary.ResultWidth

    let elem =
        match unary.Name with
        |"[bitwise_op]" |"[logical_op]" ->
            let s1 =  (unaryTreeToString (treeDepth+2) propagatedLength (Option.get unary.Head))
            let s2 = (unaryTreeToString (treeDepth+2) propagatedLength (Option.get unary.Tail))
            s1+s2
        |"[conditional]" ->
            let cond = unaryTreeToString (treeDepth+2) (-2) unary.Elements[0]
            let s1 =  (unaryTreeToString (treeDepth+2) propagatedLength (Option.get unary.Head))
            let s2 = (unaryTreeToString (treeDepth+2) propagatedLength (Option.get unary.Tail))
            cond+s1+s2
        |"[reduction]" when unary.Elements = [] -> ""
        |"[reduction]" |"(...)" ->
            unaryTreeToString (treeDepth+2) propagatedLength unary.Elements[0]
        |"[shift]" -> ""    
        |"{...}" ->
            ("",[0..((List.length unary.Elements)-1)])||>List.fold (fun s v -> s+(unaryTreeToString (treeDepth+2) propagatedLength unary.Elements[v]))
        |_ -> ""

    match elem with
    |"" ->
        depthToSpaces+
        "-'"+
        unary.Name+
        "' with Width: "+
        sizeString+
        "\n"
    |_ ->
        depthToSpaces+
        "-'"+
        unary.Name+
        "' with Width: "+
        sizeString+
        "\n"+
        depthToSpaces+
        "   "+
        "Elements: \n"+
        elem
    


/// Checks one-by-one all wire and output port assignments for:
/// 1) LHS Name and Width
/// 2) RHS Names
/// 3) RHS Width of inputs/wires
/// 4) Width LHS = Width RHS 
let checkWiresAndAssignments 
    (ast:VerilogInput) 
    (portMap: Map<string,string>)
    (portSizeMap:Map<string,int>)
    (portWidthDeclarationMap: Map<string,(int*int)>)
    (inputSizeMap:Map<string,int>) 
    (inputNameList: string list) 
    (linesLocations: int list) 
    (wireNameList: string list) 
    (wireSizeMap: Map<string,int>) 
    (wireLocationMap: Map<string,int>) 
    (errorList: ErrorInfo list) 
        : ErrorInfo list =

    let portAndWireNames =
        portMap
        |> Map.toList
        |> List.map fst
        |> List.append wireNameList
    
    /// Helper function to find the closest port or wire name
    /// Used by checkNamesOnRHSOfAssignment
    /// Gives an appropriate suggestion if the wrong name is close to a name in the list
    let findCloseVariable variable portAndWireNames =
        portAndWireNames
        |> List.collect (fun name ->
            let one = Seq.except name variable     
            let two = Seq.except variable name
            if ((Seq.length one = 0) && (Seq.length two <= 2)) then
                [name]
            elif ((Seq.length two = 0) && (Seq.length one <= 2)) then
                [name]
            else []
        )

    
    /// Checks the name and width of a wire assignment
    /// Name : if the variable is free
    /// Width : correct definition of width (i.e. Little-endian)
    let checkWireNameAndWidth wire notUniqueNames (localErrors:ErrorInfo list) =     
        let lhs = wire.LHS
        match Map.tryFind lhs.Primary.Name portMap with
        | Some portType  ->  //CASE 1: Invalid Name (already used variable by port)
            let message = sprintf "Variable '%s' is already used by a port" lhs.Primary.Name
            let extraMessages = 
                [|
                    {Text=(sprintf "Variable '%s' is declared as an %s port\nPlease use a different name for this wire" lhs.Primary.Name portType);Copy=false;Replace=NoReplace}
                |]
            createErrorMessage linesLocations lhs.Primary.Location message extraMessages lhs.Primary.Name
        | _ -> 
            match List.tryFind (fun x -> x=lhs.Primary.Name) notUniqueNames with
            | Some found  -> //CASE 2: Invalid Name (already used variable by another wire)
                let message = sprintf "Variable '%s' is already used by another wire" lhs.Primary.Name
                let extraMessages = 
                    [|
                        {Text=(sprintf "Variable '%s' is already used by another wire\nPlease use a different name for this wire" lhs.Primary.Name);Copy=false;Replace=NoReplace}
                    |]
                createErrorMessage linesLocations lhs.Primary.Location message extraMessages lhs.Primary.Name
            | _ ->
                match isNullOrUndefined lhs.BitsStart with
                |true -> localErrors // No errors
                |false -> 
                    let bStart = int <| Option.get lhs.BitsStart
                    let bEnd = int <| Option.get lhs.BitsEnd
                    // CASE 3: Wrong Width declaration
                    if (bEnd <> 0 || bStart <= bEnd) then
                        let message = "Wrong width declaration"
                        let extraMessages = 
                            [|
                                {Text=(sprintf "A port's width can't be '[%i:%i]'\nCorrect form: [X:0]" bStart bEnd);Copy=false;Replace=NoReplace}
                            |]
                        createErrorMessage linesLocations lhs.Primary.Location message extraMessages lhs.Primary.Name
                    else localErrors // No errors


    /// Checks the name and width of an output port assignment
    /// Name : if the variable is indeed an output port
    /// Width : width is within the declared width range
    let checkAssignmentNameAndWidth assignment localErrors = 
        let lhs = assignment.LHS
        match Map.tryFind lhs.Primary.Name portMap with
        | Some found when found = "output"  -> 
            match Map.tryFind lhs.Primary.Name portWidthDeclarationMap with
            | Some (bStart,bEnd) -> 
                match isNullOrUndefined lhs.BitsStart with
                | false ->
                    if (bStart >= (int (Option.get lhs.BitsStart))) && (bEnd <= (int (Option.get lhs.BitsEnd))) then
                        localErrors
                    else 
                        let name = lhs.Primary.Name
                        let definition =
                            match bStart=bEnd with
                            |true -> " a single bit "
                            |false -> sprintf " %s[%i:0] " name (bStart)
                        let usedWidth =
                            match lhs.BitsStart=lhs.BitsEnd with
                            |true -> sprintf " %s[%s] " name (Option.get lhs.BitsStart)
                            |false -> sprintf " %s[%s:%s] " name (Option.get lhs.BitsStart) (Option.get lhs.BitsEnd)
                        let message = sprintf "Wrong width of variable: '%s'" name
                        let extraMessages = 
                            [|
                                {Text=(sprintf "Variable: '%s' is defined as" name)+definition+"\nTherefore,"+usedWidth+"is invalid" ; Copy=false;Replace=NoReplace}
                                {Text=sprintf "assign %s = 0;"name; Copy=true;Replace=Assignment}
                            |]
                        List.append 
                            localErrors 
                            (createErrorMessage linesLocations lhs.Primary.Location message extraMessages lhs.Primary.Name)
                | true -> localErrors
            | None -> failwithf "Can't happen! PortMap and PortSizeMap should have the same keys"
        | _ -> 
            let message = sprintf "Variable '%s' is not declared as an output port" lhs.Primary.Name
            let extraMessages = 
                [|
                    {Text=(sprintf "Variable '%s' is not declared as an output port" lhs.Primary.Name);Copy=false;Replace=NoReplace}
                    {Text=(sprintf "output %s;" lhs.Primary.Name);Copy=true;Replace=IODeclaration}
                |]
            List.append 
                localErrors 
                (createErrorMessage linesLocations lhs.Primary.Location message extraMessages lhs.Primary.Name)

    /// Checks if the variables used in the RHS of on assignment
    /// (either output port or wire) have been declared as input or wire
    let checkNamesOnRHSOfAssignment (assignment: AssignmentT) currentInputWireList localErrors = 
        let PrimariesRHS = primariesUsedInAssignment [] assignment.RHS
        
        let namesWithLocRHS = PrimariesRHS |> List.map (fun x -> (x.Primary.Name, x.Primary.Location))
        let namesRHS = namesWithLocRHS |> List.map fst
        let namesToLocMap = namesWithLocRHS |> Map.ofList

        let diff = List.except (List.toSeq (List.append currentInputWireList ["delete123"])) namesRHS
        match List.isEmpty diff with
        | true -> localErrors
        | false -> 
            diff
            |> List.collect (fun name ->
                let currLocation = Map.find name namesToLocMap
                match List.exists (fun x->x=name) wireNameList with
                |true ->
                    let message = sprintf "Wire '%s' is defined after this assignment" name
                    let extraMessages = 
                        [|
                            {Text=(sprintf "Wire '%s' is defined after this assignment" name);Copy=false;Replace=NoReplace}
                            {Text=(sprintf "Move the definition of wire '%s' above this line" name);Copy=false;Replace=NoReplace}
                        |]
                    createErrorMessage linesLocations currLocation message extraMessages name
                |false ->
                    let closeVariables = findCloseVariable name portAndWireNames 
                    match List.isEmpty closeVariables with
                    |true ->
                        let message = sprintf "Variable '%s' is not declared as input or wire" name
                        let extraMessages = 
                            [|
                                {Text=(sprintf "Variable '%s' is not declared as input or wire" name);Copy=false;Replace=NoReplace}
                                {Text=(sprintf "input %s;|wire %s = 1'b0;" name name);Copy=true;Replace=IODeclaration}
                            |]
                        createErrorMessage linesLocations currLocation message extraMessages name
                    |false ->
                        let message = sprintf "Variable '%s' is not declared as input or wire" name
                        let extraMessages = 
                            [|
                                {Text=(sprintf "Variable '%s' is not declared as input or wire" name);Copy=false;Replace=NoReplace}
                                {Text=(sprintf "%s" closeVariables[0]);Copy=true;Replace=Variable name}
                            |]
                        createErrorMessage linesLocations currLocation message extraMessages name
            )
            
    /// Check if the width of each wire/input used
    /// is within the correct range (defined range)
    let checkSizesOnRHSOfAssignment (assignment: AssignmentT) currentInputWireSizeMap localErrors =
        let primariesRHS = primariesUsedInAssignment [] assignment.RHS
        primariesRHS
        |> List.collect (fun x -> 
            match isNullOrUndefined x.BitsStart with
            | false ->
                let name = x.Primary.Name
                let bStart = int <| Option.get x.BitsStart 
                let bEnd = int <| Option.get x.BitsEnd
                match bStart with   
                |(-3) ->   // hack to identify numbers
                    if bEnd = 0 then
                        let message = "Number can't be 0 bits wide"
                        let extraMessages = 
                            [|
                                {Text="Number can't be 0 bits wide"; Copy=false;Replace=NoReplace}
                                {Text=("The integer before 'h/'b represents the width of the number\n e.g. 12'hc7 -> 000011000111");Copy=false;Replace=NoReplace}
                            |]
                        List.append 
                            localErrors 
                            (createErrorMessage linesLocations x.Primary.Location message extraMessages "0'b")
                    else 
                        let no = 
                            match x.PrimaryType[0] with
                            |'b' -> "0"+x.PrimaryType
                            |'h' ->
                                let withoutH = 
                                    String.mapi (fun index char -> 
                                    match index with
                                    |0 -> '0'
                                    |_ -> char
                                    ) x.PrimaryType
                                "0x"+withoutH
                            |_ -> 
                                String.mapi (fun index char -> 
                                    match index with
                                    |0 -> '0'
                                    |_ -> char
                                ) x.PrimaryType
                        match NumberHelpers.strToIntCheckWidth bEnd no with
                        |Ok n -> localErrors
                        |Error _ -> 
                            let message = sprintf "Number can't fit in %i bits" bEnd
                            let extraMessages = 
                                [|
                                    {Text=sprintf "Number can't fit in %i bits" bEnd; Copy=false;Replace=NoReplace}
                                    {Text=("The integer before 'h/'b represents the width of the number\n e.g. 12'hc7 -> 000011000111");Copy=false;Replace=NoReplace}
                                |]
                            List.append 
                                localErrors 
                                (createErrorMessage linesLocations x.Primary.Location message extraMessages "0'b")
                        
                | _ -> 
                    match Map.tryFind name currentInputWireSizeMap with
                    | Some size -> 
                        if (bStart<size) && (bEnd>=0) && (bStart>=bEnd) then
                            localErrors //ok
                        else 
                            let definition =
                                match size with
                                |1 -> " a single bit "
                                |_ -> sprintf " %s[%i:0] " name (size-1)
                            let usedWidth =
                                match bStart=bEnd with
                                |true -> sprintf " %s[%i] " name bStart
                                |false -> sprintf " %s[%i:%i] " name bStart bEnd
                            let message = sprintf "Wrong width of variable: '%s'" name
                            let extraMessages = 
                                [|
                                    {Text=(sprintf "Variable: '%s' is defined as" name)+definition+"\nTherefore,"+usedWidth+"is invalid" ; Copy=false;Replace=NoReplace}
                                |]
                            List.append 
                                localErrors 
                                (createErrorMessage linesLocations x.Primary.Location message extraMessages name)        
                    | None -> localErrors //invalid name, error found by AssignmentRHSNameCheck 
            | true -> localErrors
        )

    /// Checks whether the width on the LHS of assignment
    /// matches the width on the RHS
    let checkWidthOfAssignment (assignment: AssignmentT) currentInputWireSizeMap location localErrors =
        let lengthLHS = 
            match isNullOrUndefined assignment.LHS.BitsStart with
            | true -> 
                match Map.tryFind assignment.LHS.Primary.Name portSizeMap with
                | Some num -> num
                | None -> 
                    match assignment.Type with
                    |"wire" -> 1
                    |_ -> (-1)
            | false -> ((Option.get assignment.LHS.BitsStart) |> int) - ((Option.get assignment.LHS.BitsEnd) |> int)+1

        let unariesList = RHSUnaryAnalysis assignment.RHS currentInputWireSizeMap
        let sizesString = unaryTreeToString 1 lengthLHS unariesList
        
        let intToBool x =
            match x with
            |0 -> false
            |_ -> true
        
        let hasError =
            sizesString
            |> String.filter (fun ch -> ch='!')
            |> String.length
            // if it contains '!' -> it has an error  because string will contain "ERROR!"              
            |> intToBool
        
        match hasError with
        |false -> localErrors
        |true ->
            let text = 
                "Bit width on LHS and RHS must be equal!\n"+
                "  *LHS: '"+
                assignment.LHS.Primary.Name+
                "' with Width: "+
                (string lengthLHS)+
                "\n  *RHS:\n"+
                sizesString

            let extraMessages =
                [|
                    {Text=text;Copy=false;Replace=NoReplace}
                |]
            List.append 
                localErrors 
                (createErrorMessage linesLocations location "Different width on RHS<->LHS" extraMessages assignment.Type)        

    /// Helper function to extract all inputs + wires declared 
    /// prior to the assignment being checked
    let getCurrentInputWireList location = 
        wireNameList
        |> List.filter (fun x -> 
            match (Map.tryFind x wireLocationMap) with
            |Some wireLoc -> location>wireLoc  
            |None -> false
        )
        |> List.append inputNameList
    
    /// Helper function to extract all inputs + wires declared 
    /// prior to the assignment being checked
    let getCurrentInputWireSizeMap location = 
        wireSizeMap
        |> Map.filter (fun wire size ->
            match (Map.tryFind wire wireLocationMap) with
            |Some wireLoc -> location>wireLoc  
            |None -> false
        )
        |> Map.toList
        |> List.append (Map.toList inputSizeMap)
        |> Map.ofList
    

    let notUniqeWireNames = 
                wireNameList 
                |> List.countBy id
                |> List.filter (fun (name,count) -> count>1)
                |> List.map fst
    

    let assignmentsWithLocation = 
        ast.Module.ModuleItems.ItemList 
            |> Array.toList 
            |> List.filter (fun item -> item.ItemType = "statement")
            |> List.map (fun item -> (Option.get item.Statement),item.Location)
            |> List.map (fun (statement,loc) -> statement.Assignment,loc)

    
    let localErrors =
        assignmentsWithLocation
        |> List.collect (fun (assignment, location)->
            let currentInputWireList = getCurrentInputWireList location
            let currentInputWireSizeMap = getCurrentInputWireSizeMap location

            match assignment.Type with
                |"wire" -> checkWireNameAndWidth assignment notUniqeWireNames []
                |_ -> checkAssignmentNameAndWidth assignment []
            |> checkNamesOnRHSOfAssignment assignment currentInputWireList
            |> checkSizesOnRHSOfAssignment assignment currentInputWireSizeMap
            |> checkWidthOfAssignment assignment currentInputWireSizeMap location 
        )


    List.append errorList localErrors



/////////////////////////////


let getNotUniquePortDeclarations items =
    items
    |> List.collect (fun x -> 
        match (x.IODecl |> isNullOrUndefined) with
        | false -> 
            match x.IODecl with
            | Some decl -> 
                decl.Variables 
                |> Array.toList 
                |> List.collect (fun x -> [x.Name]) 
            | None -> []
        | true -> []
    )
    |> List.countBy id
    |> List.filter (fun (name, size) -> size>1)
    |> List.map fst

/// Returns the port-size map (e.g. (port "a" => 4 bits wide))
let getPortSizeAndLocationMap items = 
    let portSizeLocation = 
        items |> List.collect (fun x -> 
            match (x.IODecl |> isNullOrUndefined) with
            | false -> 
                match x.IODecl with
                | Some d -> 
                    let size = 
                        match isNullOrUndefined d.Range with
                        | true -> 1
                        | false -> ((Option.get d.Range).Start |> int) - ((Option.get d.Range).End |> int) + 1
                    let location = x.Location
                    d.Variables 
                    |> Array.toList 
                    |> List.collect (fun identifier -> [(identifier.Name,size,identifier.Location)]) 
                | None -> []
            | true -> []
        )
    let ps = List.map (fun x -> match x with | p,s,l -> (p,s)) portSizeLocation
    let pl = List.map (fun x -> match x with | p,s,l -> (p,l)) portSizeLocation
    (Map.ofList ps, Map.ofList pl)




/// Returns the port-width declaration map (e.g. (  port "a" => (4,0)  ))
let getPortWidthDeclarationMap items = 
    items 
    |> List.collect (fun x -> 
        match (x.IODecl |> isNullOrUndefined) with
        | false -> 
            match x.IODecl with
            | Some d -> 
                let size = 
                    match isNullOrUndefined d.Range with
                    | true -> (0,0)
                    | false -> ((Option.get d.Range).Start |> int),((Option.get d.Range).End |> int)
                d.Variables 
                |> Array.toList 
                |> List.collect (fun x -> [(x.Name,size)]) 
            | None -> []
        | true -> []) 
    |> Map.ofList

/// Returns the port-type map (e.g. (port "a" => INPUT))
let getPortMap items = 
    items |> List.collect (fun x -> 
            match (x.IODecl |> isNullOrUndefined) with
            | false -> 
                match x.IODecl with
                | Some d -> 
                    d.Variables 
                    |> Array.toList 
                    |> List.collect (fun x -> [(x.Name,d.DeclarationType)]) 
                | None -> []
            | true -> []
    ) |> Map.ofList
    

let getInputSizeMap inputNameList portSizeMap =
    portSizeMap
    |> Map.filter (fun n s -> (List.exists (fun x -> x = n) inputNameList))

/// Returns the names of the ports declared as INPUT
let getInputNames portMap = 
    portMap 
    |> Map.filter (fun n s -> s = "input") 
    |> Map.toList 
    |> List.map fst


/// Returns the names of the declared WIRES
let getWireSizeMap items = 
    items 
    |> List.collect (fun x -> 
        match (x.Statement |> isNullOrUndefined) with
        | false -> 
            match x.Statement with
            | Some statement when statement.StatementType = "wire" ->
                let lhs = statement.Assignment.LHS 
                match isNullOrUndefined lhs.BitsStart with
                |true  -> [lhs.Primary.Name,1]
                |false -> 
                    let size = ((Option.get lhs.BitsStart) |> int) - ((Option.get lhs.BitsEnd) |> int) + 1
                    [lhs.Primary.Name,size]
            | _ -> []
        | true -> [])
    |> Map.ofList


let getWireNames items =
    items 
    |> List.collect (fun x -> 
        match (x.Statement |> isNullOrUndefined) with
        | false -> 
            match x.Statement with
            | Some statement when statement.StatementType = "wire" ->
                let lhs = statement.Assignment.LHS 
                [lhs.Primary.Name]
            | _ -> []
        | true -> [])

let getWireLocationMap items = 
    items 
    |> List.collect (fun x -> 
        match (x.Statement |> isNullOrUndefined) with
        | false -> 
            match x.Statement with
            | Some statement when statement.StatementType = "wire" ->
                let lhs = statement.Assignment.LHS 
                let loc = x.Location
                [lhs.Primary.Name,loc]
            | _ -> []
        | true -> [])
    |> Map.ofList


/// Main error-finder function
/// Returns a list of errors (type ErrorInfo)
let getSemanticErrors ast linesLocations (origin:CodeEditorOpen) (project:Project) =
    
    let (items: ItemT list) = ast.Module.ModuleItems.ItemList |> Array.toList
    ///////// MAPS, LISTS NEEDED  ////////////////
    let portMap  = getPortMap items
    let portSizeMap,portLocationMap = getPortSizeAndLocationMap items
    let portWidthDeclarationMap = getPortWidthDeclarationMap items
    
    let notUniquePortDeclarations = getNotUniquePortDeclarations items
    
    let inputNameList = getInputNames portMap
    let inputSizeMap = getInputSizeMap inputNameList portSizeMap
    let wireSizeMap = getWireSizeMap items
    let wireNameList = getWireNames items
    let wireLocationMap = getWireLocationMap items
    //////////////////////////////////////////////
    
    []  //begin with empty list and add errors to it
    |> nameCheck ast linesLocations origin project //name is valid (not used by another sheet/component)
    |> portCheck ast linesLocations //all ports are declared as input/output
    |> checkIODeclarations ast portWidthDeclarationMap portLocationMap linesLocations notUniquePortDeclarations portMap items  //all ports declared as IO are defined in the module header
    |> checkIOWidthDeclarations ast linesLocations //correct port width declaration (e.g. [1:4] -> invalid)
    |> checkWiresAndAssignments ast portMap portSizeMap portWidthDeclarationMap inputSizeMap inputNameList linesLocations wireNameList wireSizeMap wireLocationMap //checks 1-by-1 all assignments (wires & output ports)
    |> checkAllOutputsAssigned ast portMap portSizeMap linesLocations //checks whether all output ports have been assined a value
    |> List.distinct // filter out possible double Errors