module ErrorCheck

open VerilogTypes
open Fable.Core.JsInterop
open CommonTypes
open VerilogAST
open ErrorCheckProcedural
open ErrorCheckHelpers

let private getFileInProject (name:string) project = project.LoadedComponents |> List.tryFind (fun comp -> comp.Name.ToUpper() = name.ToUpper())

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
    
    let prevIndex = List.findIndexBack (fun x -> x <= currLocation) newLinesLocations
    let line = prevIndex+1
    let prevLineLocation = newLinesLocations[prevIndex]
    let length = String.length name
    
    [{Line = line; Col=currLocation-prevLineLocation+1;Length=length;Message = message;ExtraErrors=Some extraMessages}]


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
                            {Text=sprintf "input bit %s;|output bit %s;" name name;Copy=true;Replace=IODeclaration}
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
    (project: Project)
    (errorList: ErrorInfo list)
        : ErrorInfo list = 
    
    let portList = ast.Module.PortList |> Array.toList

    let moduleInstantiationsPrimaries = 
        ([], (VerilogInput ast)) ||> foldAST getModuleInstantiationStatements
        |> List.collect (fun modInst -> getModuleInstantiationInputPrimaries modInst project)
        |> List.map (fun primary -> primary.Primary.Name)
    // get variables from other expressions too
    let PrimariesUsedExpr =
        foldAST getAllExpressions' [] (VerilogInput(ast))
        |> List.map (fun expr -> primariesUsedInAssignment [] expr)
        |> List.concat
        |> List.map (fun primary -> primary.Primary.Name)
        |> List.append moduleInstantiationsPrimaries
    portWidthDeclarationMap
    |> Map.toList
    |> List.map fst
    |> List.collect (fun port -> 
        match ((List.contains port PrimariesUsedExpr),(Map.tryFind port portMap)) with
        | false, Some "input" -> // CASE 1: port is not used in the assignments
            // if port is clk we check if there are clocked always blocks
            let alwaysFFs = foldAST getAlwaysBlocks [] (VerilogInput(ast)) |> List.filter (fun always -> always.AlwaysType="always_ff")
            if port = "clk" && alwaysFFs <> [] then errorList
            else
                let currLocation = Map.find port portLocationMap
                let message = sprintf "Variable '%s' is defined as an input port but is not used" port
                let extraMessages =
                    [|
                        {Text=sprintf "Variable '%s' is defined as an input port but is not used \n Please delete it if it is not needed" port;Copy=false;Replace=NoReplace}
                    |]
                createErrorMessage linesLocations currLocation message extraMessages port
        | _, _ ->
            match (List.contains port portList) with
            | false -> // CASE 2: Doesn't exist in the module header (declaration present but not in module header)
                let currLocation = Map.find port portLocationMap
                let message = sprintf "Port '%s' is not defined as a port in the module declaration" port
                let extraMessages =
                    [|
                        {Text=sprintf "Port '%s' is not defined as a port \n Please define it in the module declaration" port;Copy=false;Replace=NoReplace}
                    |]
                createErrorMessage linesLocations currLocation message extraMessages port
            | true -> // Exists in module header
                match List.contains port nonUniquePortDeclarations with
                | true -> // CASE 3: Double definition
                    let currLocation = Map.find port portLocationMap
                    let message = sprintf "Port '%s' is already defined" port
                    let extraMessages =
                        [|
                            {Text=sprintf "Port '%s' is already defined" port ;Copy=false;Replace=NoReplace}
                        |]
                    createErrorMessage linesLocations currLocation message extraMessages port
                | false -> [] //CASE 4: No errors
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
    let exists, initialFileName = 
        match origin with
        |NewVerilogFile -> isFileInProject moduleName project , ""
        |UpdateVerilogFile initialName -> moduleName <> initialName, initialName

    let localError = 
        match (exists,origin) with
        |true,NewVerilogFile -> 
            let message = "A sheet/component with that name already exists"
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
                    {Text= sprintf "%s" initialFileName ;Copy=true;Replace=Variable moduleName}
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
        |> Map.filter (fun _ s -> s = "output") 
        |> Map.toList 
        |> List.map fst
        |> List.collect (fun x -> 
            let size = Map.find x portSizeMap
            let names = [0..size-1] |> List.map (fun y -> (x+(string y),x))
            names 
        )
    let outputPortList = List.map fst outputPortListMap


    let getVariablesAssigned vars node =
        match node with
        | ContinuousAssign contAssign when isNullOrUndefined contAssign.Assignment.LHS.BitsStart ->
            vars@[(contAssign.Assignment.LHS.Primary.Name,-1,-1)]
        | ContinuousAssign contAssign -> 
            [(contAssign.Assignment.LHS.Primary.Name,
            (int (Option.get contAssign.Assignment.LHS.BitsStart)),
            (int (Option.get contAssign.Assignment.LHS.BitsEnd)))]
            |> List.append vars
        | BlockingAssign blocking -> vars@[blocking.Assignment.LHS.Primary.Name,-1,-1]
        | NonBlockingAssign nonblocking -> vars@[nonblocking.Assignment.LHS.Primary.Name,-1,-1]
        | ModuleInstantiation modInst -> 
            modInst.Connections
            |> Array.toList
            |> List.map (fun connection ->
                match connection.Primary with
                | a when isNullOrUndefined a.BitsStart -> (a.Primary.Name,-1,-1)
                | a -> (a.Primary.Name,(int (Option.get a.BitsStart)),(int (Option.get a.BitsEnd)))
                )
            |> List.append vars
        | _ -> vars
    let variablesAssigned = foldAST getVariablesAssigned [] (VerilogInput ast)
    // List of assigned ports, bit by bit
    let assignmentPortList =
        variablesAssigned
        |> List.collect ( fun x ->
            match x with
            |(name,-1,-1)->
                match Map.tryFind name portSizeMap with
                | Some size -> 
                    let names = [0..size-1] |> List.map (fun y -> name+(string y))
                    names
                | None -> []
            |(name,x,y) when x=y ->
                [name+(string x)]
            |(name,bStart,bEnd)->
                let names = [bEnd..bStart] |> List.map (fun y -> name+ (string y))
                names
        )

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
            let currLocation = ast.Module.EndLocation
            let message = mess
            let extraMessages = 
                match errorType with
                |Unassigned ->
                    [|
                        {Text=sprintf "The following ports are declared but not assigned: %A" fullNames;Copy=false;Replace=NoReplace};
                        {Text=sprintf "assign %s = 1'b0;" fullNames[0];Copy=true;Replace=ReplaceType.Assignment}
                    |]
                |DoubleAssignment ->
                    // handled in errorcheckprocedural
                    [||]
            match errorType with
            | Unassigned ->
                createErrorMessage linesLocations currLocation message extraMessages "endmodule"
            | _ -> []
    
    let countAssignments = assignmentPortList |> List.countBy id
    let notUnique = 
        countAssignments
        |> List.filter (fun (_,y)->y>1)
        |> List.map fst

    let unassignedPorts = List.except (List.toSeq assignmentPortList) (outputPortList)

    let localErrors =
        match unassignedPorts with
        |[] -> genErrorMessage notUnique outputPortListMap DoubleAssignment "Some output ports have been assigned more than once"
        |_ -> genErrorMessage unassignedPorts outputPortListMap Unassigned "All output ports must be assigned"

    List.append errorList localErrors


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
    (inputNameList: string list) 
    (linesLocations: int list) 
    (wireNameList: string list) 
    (wireSizeMap: Map<string,int>) 
    (wireLocationMap: Map<string,int>) 
    (errorList: ErrorInfo list) 
        : ErrorInfo list =

    let declarations = foldAST getDeclarations [] (VerilogInput(ast))
    let logicNameList = 
        declarations
        |> List.collect (fun decl -> List.ofArray decl.Variables)
        |> List.map (fun id -> id.Name)
    let wireNameList' = wireNameList @ logicNameList
    
    let portAndWireNames =
        portMap
        |> Map.toList
        |> List.map fst
        |> List.append wireNameList'

    let outputNameList = portMap |> Map.toList |> List.map fst 
    /// Helper function to extract all inputs + wires declared + outputs
    /// prior to the assignment being checked
    let getCurrentInputWireList location = 
        wireNameList'
        |> List.filter (fun x -> 
            match (Map.tryFind x wireLocationMap) with
            |Some wireLoc -> location>wireLoc  
            |None -> false
        )
        |> List.append inputNameList
        |> List.append outputNameList

    
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
                let message = sprintf "Identifier '%s' is already used by another variable" lhs.Primary.Name
                let extraMessages = 
                    [|
                        {Text=(sprintf "Identifier '%s' is already used by another variable\nPlease use a different name for this wire" lhs.Primary.Name);Copy=false;Replace=NoReplace}
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

    let checkLogicName (decl: DeclarationT) notUniqueNames (localErrors:ErrorInfo list) =
        let variables = decl.Variables
        (localErrors, variables)
        ||> Array.fold (fun errorList lhs ->
            match Map.tryFind lhs.Name portMap with
            | Some portType  ->  //CASE 1: Invalid Name (already used variable by port)
                let message = sprintf "Variable '%s' is already used by a port or variable" lhs.Name
                let extraMessages = 
                    [|
                        {Text=(sprintf "Variable '%s' is declared as an %s port\nPlease use a different name for this variable" lhs.Name portType);Copy=false;Replace=NoReplace}
                    |]
                errorList @ createErrorMessage linesLocations lhs.Location message extraMessages lhs.Name
            | _ -> 
                match List.tryFind (fun x -> x=lhs.Name) notUniqueNames with
                | Some found  -> //CASE 2: Invalid Name (already used variable by another wire)
                    let message = sprintf "Variable '%s' is already used by another wire" lhs.Name
                    let extraMessages = 
                        [|
                            {Text=(sprintf "Variable '%s' is already used by another wire\nPlease use a different name for this wire" lhs.Name);Copy=false;Replace=NoReplace}
                        |]
                    createErrorMessage linesLocations lhs.Location message extraMessages lhs.Name
                | _ ->
                    match isNullOrUndefined decl.Range with
                    |true -> localErrors // No errors
                    |false -> 
                        let bStart = int <| (Option.get decl.Range).Start
                        let bEnd = int <| (Option.get decl.Range).End
                        // CASE 3: Wrong Width declaration
                        if (bEnd <> 0 || bStart <= bEnd) then
                            let message = "Wrong width declaration"
                            let extraMessages = 
                                [|
                                    {Text=(sprintf "A port's width can't be '[%i:%i]'\nCorrect form: [X:0]" bStart bEnd);Copy=false;Replace=NoReplace}
                                |]
                            createErrorMessage linesLocations lhs.Location message extraMessages lhs.Name
                        else localErrors // No 
        )

    /// Checks the name and width of an output port assignment
    /// Name : if the variable is indeed an output port
    /// Width : width is within the declared width range
    let checkAssignmentNameAndWidth assignment localErrors = 
        let lhs = assignment.LHS
        match Map.tryFind lhs.Primary.Name portMap with
        | Some found when found = "output" -> 
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
                        let usedWidth, message =
                            match lhs.BitsStart=lhs.BitsEnd with
                            |true -> 
                                sprintf " %s[%s] " name (Option.get lhs.BitsStart), sprintf "Out of bounds index for variable '%s'" name
                            |false -> 
                                sprintf " %s[%s:%s] " name (Option.get lhs.BitsStart) (Option.get lhs.BitsEnd), sprintf "Out of bounds range for variable '%s'" name
                        //let message = sprintf "Wrong width of variable: '%s'" name
                        let extraMessages = 
                            [|
                                {Text=(sprintf "Variable: '%s' is defined as" name)+definition+"\nTherefore,"+usedWidth+"is invalid" ; Copy=false;Replace=NoReplace}
                                {Text=sprintf "assign %s = 0;"name; Copy=true;Replace=ReplaceType.Assignment}
                            |]
                        List.append 
                            localErrors 
                            (createErrorMessage linesLocations lhs.Primary.Location message extraMessages lhs.Primary.Name)
                | true -> localErrors
            | None -> failwithf "Can't happen! PortMap and PortSizeMap should have the same keys"
        | _ -> 
            // check if a logic with this name has been declared
            let wiresDeclared = getCurrentInputWireList lhs.Primary.Location
            match List.tryFind (fun wire -> wire = lhs.Primary.Name) wiresDeclared  with
            | Some _ -> errorList
            | _ ->
                let message = sprintf "Variable '%s' is not declared as an output port" lhs.Primary.Name
                let extraMessagesMain = 
                    [|
                        {Text=(sprintf "Variable '%s' is not declared as an output port" lhs.Primary.Name);Copy=false;Replace=NoReplace}
                    |]

                let possibleAddition =
                    match ast.Module.Type with
                    |"module_new" -> [||]
                    |_ -> [|{Text=(sprintf "output bit %s;" lhs.Primary.Name);Copy=true;Replace=IODeclaration}|]

                let extraMessages = Array.append extraMessagesMain possibleAddition

                List.append 
                    localErrors 
                    (createErrorMessage linesLocations lhs.Primary.Location message extraMessages lhs.Primary.Name)

    /// Checks if the variables used in the RHS of on assignment
    /// (either output port or wire) have been declared as input or wire
    let checkNamesInPrimaries (primariesRHS: PrimaryT list) currentInputWireList localErrors = 
        //let PrimariesRHS = primariesUsedInAssignment [] expression
        
        let namesWithLocRHS = primariesRHS |> List.map (fun x -> (x.Primary.Name, x.Primary.Location))
        let namesRHS = namesWithLocRHS |> List.map fst
        let namesToLocMap = namesWithLocRHS |> Map.ofList

        let diff = List.except (List.toSeq (List.append currentInputWireList ["delete123"])) namesRHS
        match List.isEmpty diff with
        | true -> localErrors
        | false -> 
            diff
            |> List.collect (fun name ->
                let currLocation = Map.find name namesToLocMap
                match List.exists (fun x->x=name) wireNameList' with
                |true ->
                    let message = sprintf "Variable '%s' is defined after this assignment" name
                    let extraMessages = 
                        [|
                            {Text=(sprintf "Variable '%s' is defined after this assignment" name);Copy=false;Replace=NoReplace}
                            {Text=(sprintf "Move the definition of variable '%s' above this line" name);Copy=false;Replace=NoReplace}
                        |]
                    createErrorMessage linesLocations currLocation message extraMessages name
                |false ->
                    let closeVariables = findCloseVariable name portAndWireNames 
                    match List.isEmpty closeVariables with
                    |true ->
                        let message = sprintf "Variable '%s' is not declared as input or variable" name
                        let extraMessagesMain = 
                            [|
                                {Text=(sprintf "Variable '%s' is not declared as input or variable" name);Copy=false;Replace=NoReplace}
                            |]

                        let possibleAddition =
                            match ast.Module.Type with
                            |"module_new" -> [||]
                            |_ -> [|{Text=(sprintf "input bit %s;|bit %s = 1'b0;" name name);Copy=true;Replace=IODeclaration}|]

                        let extraMessages = Array.append extraMessagesMain possibleAddition
                        
                        createErrorMessage linesLocations currLocation message extraMessages name
                    |false ->
                        let message = sprintf "Variable '%s' is not declared as input or variable" name
                        let extraMessages = 
                            [|
                                {Text=(sprintf "Variable '%s' is not declared as input or variable" name);Copy=false;Replace=NoReplace}
                                {Text=(sprintf "%s" closeVariables[0]);Copy=true;Replace=Variable name}
                            |]
                        createErrorMessage linesLocations currLocation message extraMessages name
            )
    let checkNamesOnRHSOfAssignment (expression: ExpressionT) currentInputWireList localErrors =
        let primariesRHS = primariesUsedInAssignment [] expression
        checkNamesInPrimaries primariesRHS currentInputWireList localErrors
            
    /// Check if the width of each wire/input used
    /// is within the correct range (defined range)
    let checkSizesOnRHSOfAssignment (assignment: AssignmentT) currentInputWireSizeMap localErrors =
        checkExpr linesLocations currentInputWireSizeMap localErrors assignment.RHS
    
    /// Helper function to extract all inputs + wires declared 
    /// prior to the assignment being checked
    let getCurrentInputWireSizeMap location = 
        wireSizeMap
        |> Map.filter (fun wire _ ->
            match (Map.tryFind wire wireLocationMap) with
            |Some wireLoc -> location>wireLoc  
            |None -> false
        )
        |> Map.toList
        |> List.append (Map.toList portSizeMap)
        |> Map.ofList
    
    let declarationsNames = 
        foldAST getDeclarations [] (VerilogInput(ast)) 
        |> List.collect (fun decl -> List.ofArray decl.Variables)
        |> List.map (fun var -> var.Name)

    let notUniqeWireNames = 
                wireNameList @ declarationsNames 
                |> List.countBy id
                |> List.filter (fun (name,count) -> count>1)
                |> List.map fst
    
    let assignmentsWithLocation =
        foldAST getAssignmentsWithLocations [] (VerilogInput(ast))
    let moduleInstantiationPrimaries =
        foldAST getModuleInstantiationStatements [] (VerilogInput ast)
        |> List.collect (fun modInst -> modInst.Connections |> Array.toList)
        |> List.map (fun conn -> conn.Primary)
    let moduleInstantiationErrors =
        moduleInstantiationPrimaries
        |> List.collect (fun primary ->
            let currentInputWireList = getCurrentInputWireList primary.Primary.Location
            let currentInputWireSizeMap = getCurrentInputWireSizeMap primary.Primary.Location
            checkNamesInPrimaries [primary] currentInputWireList []
            |> List.append (checkPrimariesWidths linesLocations currentInputWireSizeMap [] [primary])
        )
        
    let localErrors =
        assignmentsWithLocation
        |> List.collect (fun (assignment, location)->
            let currentInputWireList = getCurrentInputWireList location
            let currentInputWireSizeMap = getCurrentInputWireSizeMap location

            match assignment.Type with
                |"bit" -> checkWireNameAndWidth assignment notUniqeWireNames []
                |_ -> checkAssignmentNameAndWidth assignment []
            |> checkNamesOnRHSOfAssignment assignment.RHS currentInputWireList
            |> (fun errlst -> 
                match assignment.LHS.VariableBitSelect with
                | Some expr -> checkNamesOnRHSOfAssignment expr currentInputWireList errlst
                | _ -> errlst)
            |> checkSizesOnRHSOfAssignment assignment currentInputWireSizeMap
            |> (fun errlst -> 
                match assignment.LHS.VariableBitSelect with
                | Some expr -> checkExpr linesLocations currentInputWireSizeMap errlst expr
                | _ -> errlst)
            //|> checkWidthOfAssignment assignment currentInputWireSizeMap location 
        )
        |> List.append moduleInstantiationErrors
    // checking other expressions (conditional, case expression)
    let expressions = foldAST getCondAndCaseExpressions [] (VerilogInput ast)
    let exprErrors = 
        expressions
        |> List.collect (fun (expr, location) -> checkNamesOnRHSOfAssignment expr (getCurrentInputWireList location) [])
    let declarations = foldAST getDeclarations [] (VerilogInput(ast))
    let localErrorsDecl =
        declarations
        |> List.collect (fun decl -> checkLogicName decl notUniqeWireNames  [])

    errorList @ localErrors @ localErrorsDecl @ exprErrors


let checkUnsupportedKeywords 
    (ast:VerilogInput) 
    (linesLocations: int list) 
    (errorList: ErrorInfo list) 
        : ErrorInfo list =

    let localErrors =
        ast.Module.ModuleItems.ItemList
        |> Array.toList
        |> List.filter( fun item -> item.Type = "NO-COMB" || item.Type = "NO-CASE" || item.Type = "WIRE-DECL" )
        |> List.map (fun item -> item.Type,item.ItemType, item.Location)
        |> List.collect (fun (tp,keyW,loc) ->
            let message = 
                match tp with
                |"NO-COMB" -> "Non-Combinational logic is not supported"
                |"NO-CASE" -> "Case statement is not supported"
                |"WIRE-DECL" -> "Assign directly a value to the wire \n 'wire {name} = {value};'"
                |_ -> "Non-Combinational logic is not supported"
            let extraMessages = 
                [|{Text=message; Copy=false;Replace=NoReplace}|]
            createErrorMessage linesLocations loc message extraMessages keyW
        
        )

    List.append errorList localErrors

/// Checks if the RHS expression is wider than the LHS of an assignment.
/// Checks every assignment: continuous and combinational
let checkAssignmentWidths
    (ast:VerilogInput) 
    (linesLocations: int list)
    (portSizeMap: Map<string,int>)
    (wireSizeMap: Map<string,int>)
    (errorList: ErrorInfo list) =

    let wireAndPortSizeMap = Map.fold (fun acc key value -> Map.add key value acc) wireSizeMap portSizeMap
    let assignments = foldAST getAssignmentsWithLocations [] (VerilogInput ast)

    let localErrors = 
        assignments
        |> List.collect (fun (assign, loc) ->
            let rhsW = getWidthOfExpr assign.RHS wireAndPortSizeMap
            let lhsW = getLHSWidth assign wireAndPortSizeMap 
            if rhsW > lhsW then
                let message = sprintf "The RHS expression (%A bits wide) doesn't fit in the variable on the LHS (%A bits wide)" rhsW lhsW
                let extraMessages = 
                    [|{Text=message; Copy=false;Replace=NoReplace}|]
                createErrorMessage linesLocations loc message extraMessages assign.Type
            else []
        )
    errorList @ localErrors

let checkInputsAssigned ast linesLocations portMap errorInfoList =
    let assignments = foldAST getAssignments' [] (VerilogInput ast)
    assignments
    |> List.collect (fun assign ->
        match Map.tryFind assign.LHS.Primary.Name portMap with
        | Some "input" -> 
            let message = sprintf "Cannot assign to input port '%s'" assign.LHS.Primary.Name
            let extraMessages = 
                [|{Text=message; Copy=false;Replace=NoReplace}|]
            createErrorMessage linesLocations assign.LHS.Primary.Location message extraMessages assign.LHS.Primary.Name
        | _ -> []
    )
    |> List.append errorInfoList
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
    ///////// STATIC MAPS, LISTS NEEDED  ////////////////
    let portMap  = getPortMap items
    let portSizeMap,portLocationMap = getPortSizeAndLocationMap items
    let portWidthDeclarationMap = getPortWidthDeclarationMap items
    
    let notUniquePortDeclarations = getNotUniquePortDeclarations items
    
    let inputNameList = getInputNames portMap

    let wireSizeMap = getWireSizeMap items
    let declarations = foldAST getDeclarations [] (VerilogInput(ast))
    let wireSizeMap =
        (wireSizeMap, declarations)
        ||> List.fold (fun map decl ->
            (map, decl.Variables)
            ||> Array.fold (fun map' variable -> 
                if isNullOrUndefined decl.Range then Map.add variable.Name 1 map'
                else Map.add variable.Name ((Option.get(decl.Range).Start |> int)-(Option.get(decl.Range).End |> int)+1) map'
            )
        )
    let wireNameList = getWireNames items
    let wireLocationMap = getWireLocationMap items //need to add declarations
    let wireLocationMap = 
        (wireLocationMap, declarations)
        ||> List.fold (fun (wireLocMap: Map<string, int>) (decl: DeclarationT) -> 
                (wireLocMap, decl.Variables)
                ||> Array.fold (fun map var -> Map.add var.Name var.Location map))
    //////////////////////////////////////////////
    
    let errors =
        []  //begin with empty list and add errors to it
        |> nameCheck ast linesLocations origin project //name is valid (not used by another sheet/component)
        |> portCheck ast linesLocations //all ports are declared as input/output
        |> checkIODeclarations ast portWidthDeclarationMap portLocationMap linesLocations notUniquePortDeclarations portMap project //all ports declared as IO are defined in the module header
        |> checkIOWidthDeclarations ast linesLocations //correct port width declaration (e.g. [1:4] -> invalid)
        |> checkWiresAndAssignments ast portMap portSizeMap portWidthDeclarationMap inputNameList linesLocations wireNameList wireSizeMap wireLocationMap //checks 1-by-1 all assignments (wires & output ports)
        |> checkAllOutputsAssigned ast portMap portSizeMap linesLocations //checks whether all output ports have been assined a value
        |> checkUnsupportedKeywords ast linesLocations
        |> checkProceduralAssignments ast linesLocations
        |> checkVariablesDrivenSimultaneously ast linesLocations
        |> checkVariablesAlwaysAssigned ast linesLocations portSizeMap wireSizeMap
        |> checkCasesStatements ast linesLocations portSizeMap wireSizeMap
        |> checkExpressions ast linesLocations wireSizeMap
        |> checkClk ast linesLocations portMap
        |> checkClkNames ast linesLocations portMap portLocationMap portSizeMap
        |> cycleCheck ast linesLocations portSizeMap wireSizeMap
        |> checkVariablesUsed ast linesLocations portSizeMap wireSizeMap
        |> checkAlwaysCombRHS ast linesLocations portSizeMap wireSizeMap
        |> checkAssignmentWidths ast linesLocations portSizeMap wireSizeMap
        |> checkModuleInstantiations ast linesLocations portSizeMap wireSizeMap project portMap
        |> checkInputsAssigned ast linesLocations portMap
        |> List.distinct // filter out possible double Errors
    errors