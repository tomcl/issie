module ErrorCheck

open VerilogTypes
open Fable.Core.JsInterop

/// Checks whether all ports given in the beginning of the module are defined as input/output
let portCheck ast portMap errorList = 
    let portList = ast.Module.PortList |> Array.toList
    printfn "Ports: %A" portList
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let decls = 
        items |> List.collect (fun x -> 
            match (x.IODecl |> isNullOrUndefined) with
            | false -> 
                match x.IODecl with
                | Some d -> 
                    d.Variables 
                    |> Array.toList 
                    |> List.collect (fun x -> [x]) 
                | None -> []
            | true -> []
        )
    let diff = Seq.except (decls |> List.toSeq) (portList |> List.toSeq)

    match Seq.isEmpty diff with
    | false -> List.append errorList [sprintf "The following ports are not declared either as input or output: %A" diff]
    | true -> errorList

/// Checks if the name of the module is valid (i.e. starts with a character)
let nameCheck ast errorList = 
    let name =  ast.Module.ModuleName
    printfn "Name of module: %s" ast.Module.ModuleName
    let notGoodName =
        name
        |> Seq.toList
        |> List.tryHead
        |> function | Some ch when  System.Char.IsLetter ch -> false | _ -> true
    match notGoodName with
    | true -> List.append errorList [sprintf "Module Name must start with a character to be valid"]
    | false -> errorList

/// Checks the names used for parameters
/// Throws an error if the name is used for a port 
let parameterNameCheck ast parameterNames portMap errorList = 
    let localErrors = 
        List.collect (fun name -> 
            match Map.tryFind name portMap with
            | Some found -> [sprintf "Variable %s cannot be used as a parameter name because it is declared as a port" name]
            | None -> []
        ) parameterNames
    List.append errorList localErrors

/// Checks the names used for wires
/// Throws an error if the name is used for a port 
let wireNameCheck ast portMap parameterSizeMap wireNameList errorList =   
    let localErrors = 
        List.collect (fun name -> 
            match Map.tryFind name portMap with
            | Some found -> [sprintf "Variable %s cannot be used as a wire name because it is declared as a port" name]
            | None -> 
                match Map.tryFind name parameterSizeMap with
                | Some p -> [sprintf "Variable %s cannot be used as a wire name because it is declared as a parameter" name]
                | None -> []
        ) wireNameList
    List.append errorList localErrors

/// Checks whether the assignment variables are declared as output ports 
let assignmentNameCheck ast portMap errorList = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let names = 
        items |> List.collect (fun x -> 
            match (x.Statement |> isNullOrUndefined) with
            | false -> 
                match x.Statement with
                | Some statement when statement.StatementType = "assign" -> [statement.Assignment.LHS.Primary]
                | _ -> []
            | true -> []
        )
    let localErrors = 
        List.collect (fun name -> 
            match Map.tryFind name portMap with
            | Some found when found = "output"  -> []
            | _ -> [sprintf "Variable %s is not declared as an output port" name]
        ) names
    List.append errorList localErrors

/// Checks if all declared ports have a value assigned to them
/// The check is done bit-by-bit
let checkAllOutputsAssigned ast portMap portSizeMap errorList =
    
    // List of declared ports, bit by bit
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
            | a when isNullOrUndefined assignment.BitsStart -> (a.Primary,-1,-1)
            | a -> (a.Primary,(int (Option.get a.BitsStart)),(int (Option.get a.BitsEnd)))
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
    
    /// Returns the unassigned port names given the output port list and the assigned port list 
    let diffChecker exclude lst mapping message errorList =
        let diff = List.except (List.toSeq exclude) (lst)
        match List.isEmpty diff with
        |true -> errorList
        |false -> 
            // transform names from "b2" to "b[2]" 
            let unassignedPorts = 
                diff 
                |> List.collect(fun x ->
                    match Map.tryFind x (Map.ofList mapping) with
                    | Some name -> 
                        let length = (Seq.except name x) |> Seq.map string |> String.concat ""
                        [name+"["+length+"]"]
                    | None -> []
                )
            List.append errorList [sprintf "%s: %A" message unassignedPorts]

    let localErrors =
        []
        |> diffChecker assignmentPortList outputPortList outputPortListMap "The following ports are unassigned"
        // |> diffChecker outputPortList assignmentPortList assignmentPortListMap "The following ports are assigned, but not declared as output ports" 
    List.append errorList localErrors


/// Recursive function to get all the primaries used in the RHS of an expression
let rec usedInAssignment inLst (tree: ExpressionT) = 
    match tree.Type with
    | "unary" when (Option.get tree.Unary).Type = "primary" -> List.append inLst [Option.get (Option.get tree.Unary).Primary] 
    | "reduction_negation" when (Option.get tree.Unary).Type = "primary" -> List.append inLst [Option.get (Option.get tree.Unary).Primary] 
    | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" 
    | "additive"  -> List.append (usedInAssignment inLst (Option.get tree.Head)) (usedInAssignment inLst (Option.get tree.Tail))
    | _ -> inLst

/// Checks whether the length of bits on LHS of assignment matches the length of bits on the RHS
/// TODO: needs improvement -> which item (currently returns item number)
///                         -> which variable has the wrong width
let checkBitsOfAssignemnt (assignment: AssignmentT) itemNo portSizeMap errorList=
    let lengthLHS = 
        match isNullOrUndefined assignment.LHS.BitsStart with
        | true -> 
            match Map.tryFind assignment.LHS.Primary portSizeMap with
            | Some num -> [num]
            | None -> [-2]
        | false -> [((Option.get assignment.LHS.BitsStart) |> int) - ((Option.get assignment.LHS.BitsEnd) |> int)+1]

    let PrimariesRHS = usedInAssignment [] assignment.RHS
    let lengthRHS =
        PrimariesRHS
        |> List.map (fun x ->
            match isNullOrUndefined x.BitsStart with
            | true -> 
                match Map.tryFind x.Primary portSizeMap with
                | Some num -> num
                | None -> lengthLHS[0]
            | false -> ((Option.get x.BitsStart) |> int) - ((Option.get x.BitsEnd) |> int) + 1
        )
    match lengthLHS with
    | [-2] -> errorList
    | _ ->
        let diff = List.except (List.toSeq lengthLHS) lengthRHS
        match List.isEmpty diff with
        | true -> 
            errorList
        | false -> 
            List.append errorList [sprintf "Different bit size on right <-> left on item number: %i" itemNo]

/// Checks one-by-one all the assignments for width errors using 'checkBitsOfAssignemnt'
let checkBitLengthOfAssignments ast portSizeMap errorList =
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let localErrors = 
        items
        |> List.indexed
        |> List.collect (fun (index, item) ->
        match (index, item.ItemType) with
        | i, "statement" -> 
            checkBitsOfAssignemnt (Option.get item.Statement).Assignment i portSizeMap []
        | _ -> []
        )
    List.append errorList localErrors


/// Checks whether tha variables used in the RHS of expressions have been defined as input/parameter/wire
let checkNamesOfAssignments ast inputParameterWireList errorList =
    
    let checkNamesOfAssignment (assignment: AssignmentT) itemNo inputParameterWireList errorList=
        let PrimariesRHS = usedInAssignment [] assignment.RHS
        
        let namesRHS = PrimariesRHS |> List.map (fun x -> x.Primary)

        let diff = List.except (List.toSeq inputParameterWireList) namesRHS
        match List.isEmpty diff with
        | true -> 
            errorList
        | false -> 
            List.append errorList [sprintf "The following ports are not defined as input/parameter/wire: %A" diff + sprintf " in item number: %i" itemNo ]

    
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let localErrors = 
        items
        |> List.indexed
        |> List.collect (fun (index, item) ->
        match (index, item.ItemType) with
        | i, "statement" -> 
            checkNamesOfAssignment (Option.get item.Statement).Assignment i inputParameterWireList []
        | _ -> []
        )
    List.append errorList localErrors



/// Returns the port-size map (e.g. (port "a" => 4 bits wide))
let getPortSizeMap ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    items |> List.collect (fun x -> 
            match (x.IODecl |> isNullOrUndefined) with
            | false -> 
                match x.IODecl with
                | Some d -> 
                    let size = 
                        match isNullOrUndefined d.Range with
                        | true -> 1
                        | false -> ((Option.get d.Range).Start |> int) - ((Option.get d.Range).End |> int) + 1
                    d.Variables 
                    |> Array.toList 
                    |> List.collect (fun x -> [(x,size)]) 
                | None -> []
            | true -> []
    ) |> Map.ofList

/// Returns the port-type map (e.g. (port "a" => INPUT))
let getPortMap ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    items |> List.collect (fun x -> 
            match (x.IODecl |> isNullOrUndefined) with
            | false -> 
                match x.IODecl with
                | Some d -> 
                    d.Variables 
                    |> Array.toList 
                    |> List.collect (fun x -> [(x,d.DeclarationType)]) 
                | None -> []
            | true -> []
    ) |> Map.ofList


////// NOT FINISHED : TODO ADD RANGE
let getParameterSizeMap ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    items |> List.collect (fun x -> 
            match (x.ParamDecl |> isNullOrUndefined) with
            | false -> 
                match x.ParamDecl with
                | Some p -> 
                    let size = 1 ////// FOR NOW 
                        // match isNullOrUndefined d.Range with
                        // | true -> 1
                        // | false -> ((Option.get d.Range).Start |> int) - ((Option.get d.Range).End |> int) + 1
                    [(p.Parameter.Name,size)] 
                    // |> Array.toList 
                    // |> List.collect (fun x -> [(x,size)]) 
                | None -> []
            | true -> []
    ) |> Map.ofList

/// Returns the names of the ports declared as INPUT
let getInputNames portMap = 
    portMap 
    |> Map.filter (fun n s -> s = "input") 
    |> Map.toList 
    |> List.map fst


/// Returns the names of the declared WIRES
let getWireNames ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    items |> List.collect (fun x -> 
        match (x.Statement |> isNullOrUndefined) with
        | false -> 
            match x.Statement with
            | Some statement when statement.StatementType = "wire" -> [statement.Assignment.LHS.Primary]
            | _ -> []
        | true -> []
    )

/// Main error-finder function
/// Returns a list of errors (strings)
let getErrors ast model=
    
    ///////// MAPS, LISTS NEEDED  ////////////////
    printfn "Parsed input: %A" ast
    let portMap  = getPortMap ast
    let portSizeMap = getPortSizeMap ast
    printfn "Port size map: %A" portSizeMap
    let parameterSizeMap = getParameterSizeMap ast
    let parameterNameList = parameterSizeMap |> Map.toList |> List.map fst
    let inputNameList = getInputNames portMap
    let wireNameList = getWireNames ast
    let inputParameterWireList =
        parameterNameList
        |> List.append inputNameList
        |> List.append wireNameList
    //////////////////////////////////////////////

    
    []  //begin with empty list and add errors to it
    |> nameCheck ast
    |> portCheck ast portMap
    |> parameterNameCheck ast parameterNameList portMap
    |> wireNameCheck ast portMap parameterSizeMap wireNameList
    |> assignmentNameCheck ast portMap 
    |> checkAllOutputsAssigned ast portMap portSizeMap
    |> checkNamesOfAssignments ast inputParameterWireList 
    |> checkBitLengthOfAssignments ast portSizeMap 