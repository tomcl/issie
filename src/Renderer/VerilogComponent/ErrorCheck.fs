module ErrorCheck

open VerilogTypes
open Fable.Core.JsInterop


/// Helper function to find the line of an item in the original string
let findLineOfItem (newLinesLocations:int list) (currLocation:int) : int*int*int =
    let isSmallerThan x y = y <= x
    let isGreaterThan x y = y > x
    
    let prevIndex = List.findIndexBack (fun x -> isSmallerThan currLocation x) newLinesLocations
    let nextIndex = List.findIndex (fun x -> isGreaterThan currLocation x) newLinesLocations
    (prevIndex+1,newLinesLocations[prevIndex],newLinesLocations[nextIndex])

/// Checks whether all ports given in the beginning of the module are defined as input/output
/// Also if all ports have distinct names
let portCheck ast linesLocations errorList  = 
    let portList = ast.Module.PortList |> Array.toList
    let distinctPortList = portList |> Seq.distinct |> List.ofSeq

    let indexList = ast.Module.Locations |> Array.toList
    let indexMap =
        (portList, indexList) ||> List.map2 (fun p i -> (p,int i)) |> Map.ofList
    
    let nameCount = Seq.countBy (fun x -> x) portList |> Map.ofSeq
    match List.length portList = List.length distinctPortList with
    | false ->
        nameCount
        |> Map.filter (fun name count -> count > 1)
        |> Map.toList
        |> List.map fst
        |> List.collect (fun name ->
            let currLocation = indexMap[name]
            let line,prevLineLocation,_ = findLineOfItem linesLocations currLocation
            let mess = sprintf "Ports must have different names"
            let length = String.length name
            [{Line = line; Col=currLocation-prevLineLocation+1;Length=length;Message = mess}]
            )        
        |> List.append errorList 
    
    | true ->
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
        let diff = Seq.except (decls |> List.toSeq) (portList |> List.toSeq)
        match Seq.isEmpty diff with
        | false -> 
            diff
            |> List.ofSeq
            |> List.collect (fun name ->
                let currLocation = indexMap[name]
                let line,prevLineLocation,_ = findLineOfItem linesLocations currLocation
                let mess = sprintf "Port '%s' is not declared either as input or output" name
                let length = String.length name
                [{Line = line; Col=currLocation-prevLineLocation+1;Length=length;Message = mess}]
            )
            |> List.append errorList
            
            
            // let mess = sprintf "Port(s): [%A] are not declared either as input or output" (diff |> Array.ofSeq)
            // let index = indexMap[portList[0]]
            // let length = indexMap[(List.last portList)] + String.length (List.last portList) - index            
            
            // List.append errorList [{Line = 1; Col=index+1;Length=length;Message = mess}]

        | true -> errorList

/// Checks whether the portSizeMap contains valid size information
let checkIODeclarations ast (portWidthDeclarationMap:Map<string,int*int>) portLocationMap linesLocations errorList = 
    let portList = ast.Module.PortList |> Array.toList
    
    portWidthDeclarationMap
    |> Map.toList
    |> List.map fst
    |> List.collect (fun port -> 
        let currLocation = Map.find port portLocationMap
        let line,prevLineLocation, _ = findLineOfItem linesLocations currLocation
        let length = String.length port
        match (List.tryFind (fun p -> p=port) portList) with
        | None -> [{Line = line; Col=currLocation-prevLineLocation+1 ; Length=length ; Message = sprintf "Variable '%s' is not defined as a port in the module declaration" port}]
        | Some _ -> []
    )
    |> List.append errorList   


let checkIOWidthDeclarations (ast: VerilogInput) linesLocations errorList  =
    ast.Module.ModuleItems.ItemList
    |> Array.filter (fun item -> 
        item.ItemType = "output_decl" || item.ItemType = "input_decl"  
    )
    |> Array.toList
    |> List.map (fun item -> Option.get item.IODecl)
    |> List.collect (fun ioDecl ->
        match isNullOrUndefined ioDecl.Range with
        | true -> []
        | false -> 
            let range = Option.get ioDecl.Range
            if (range.End <> "0" || (int range.Start) <= (int range.End)) then
                let currLocation = range.Location
                let line,prevLineLocation, _ = findLineOfItem linesLocations currLocation
                let length = 4+(String.length (string (range.Start)))
                [{Line = line; Col=currLocation-prevLineLocation+1 ; Length=length ; Message = sprintf "Wrong width declaration \n Correct form: [X:0]"}]
            else []
    )
    |> List.append errorList


/// Checks if the name of the module is valid (i.e. starts with a character)
let nameCheck ast errorList = 
    let name =  ast.Module.ModuleName.Name
    let notGoodName =
        name
        |> Seq.toList
        |> List.tryHead
        |> function | Some ch when  System.Char.IsLetter ch -> false | _ -> true
    match notGoodName with
    | true -> List.append errorList [{Line = 0; Col=0;Length=0;Message = sprintf "Module Name must start with a character to be valid"}]
    | false -> errorList

/// Checks the names used for parameters
/// Throws an error if the name is used for a port 
let parameterNameCheck ast parameterNames portMap parameterLocationMap linesLocations errorList = 
    let localErrors = 
        List.collect (fun name -> 
            match Map.tryFind name portMap with
            | Some found -> 
                let currLocation = Map.find name parameterLocationMap
                let line,prevLineLocation, nextLineLocation= findLineOfItem linesLocations currLocation
                [{Line = line; Col=currLocation-prevLineLocation+1;Length=nextLineLocation-currLocation-1;Message = sprintf "Variable %s cannot be used as a parameter name because it is declared as a port" name}]
            | None -> []
        ) parameterNames
    List.append errorList localErrors

/// Checks the names used for wires
/// Throws an error if the name is used for a port 
let wireNameCheck ast portMap parameterSizeMap wireNameList errorList =   
    let localErrors = 
        List.collect (fun name -> 
            match Map.tryFind name portMap with
            | Some found -> [{Line = 0; Col=0;Length=0;Message = sprintf "Variable %s cannot be used as a wire name because it is declared as a port" name}]
            | None -> 
                match Map.tryFind name parameterSizeMap with
                | Some p -> [{Line = 0; Col=0;Length=0;Message = sprintf "Variable %s cannot be used as a wire name because it is declared as a parameter" name}]
                | None -> []
        ) wireNameList
    List.append errorList localErrors


/// Checks if all declared ports have a value assigned to them
/// The check is done bit-by-bit
let checkAllOutputsAssigned ast portMap portSizeMap portLocationMap (linesLocations: int list) errorList =
    
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
    
    /// Returns the unassigned port names given the output port list and the assigned port list 
    let diffChecker exclude lst mapping message =
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
            let portLoc = linesLocations[((List.length linesLocations)-2)]  
            let line,prevLineLocation, nextLineLocation= findLineOfItem linesLocations portLoc     
            // [{Line = line; Col=currLocation-prevLineLocation+2 ; Length=nextLineLocation-currLocation-3  ;Message = sprintf "Wrong width on assignment of output port: %s" lhs.Primary}]   
            [{Line = line; Col=portLoc-prevLineLocation+1 ; Length=nextLineLocation+1-portLoc-1; Message = sprintf "%s: %A" message unassignedPorts}]

    diffChecker assignmentPortList outputPortList outputPortListMap "The following ports are declared but not assigned"
    |> List.append errorList


/// Recursive function to get all the primaries used in the RHS of an assignment
let rec primariesUsedInAssignment inLst (isConcat: bool) (tree: ExpressionT) = 
    match tree.Type with
    | "unary" when (Option.get tree.Unary).Type = "primary" 
        -> List.append inLst [(Option.get (Option.get tree.Unary).Primary, isConcat)]
    | "unary" when (Option.get tree.Unary).Type = "parenthesis" 
        -> primariesUsedInAssignment inLst isConcat  (Option.get (Option.get tree.Unary).Expression)
    | "unary" when (Option.get tree.Unary).Type = "concat" 
        -> primariesUsedInAssignment inLst true  (Option.get (Option.get tree.Unary).Expression)
    | "negation" when (Option.get tree.Unary).Type = "primary" 
        -> List.append inLst [(Option.get (Option.get tree.Unary).Primary, isConcat)] 
    | "negation" when (Option.get tree.Unary).Type = "parenthesis" 
        -> primariesUsedInAssignment inLst isConcat (Option.get (Option.get tree.Unary).Expression)    
    
    // hack for numbers to get their width in BitsEnd (correct?)
    | "unary" when (Option.get tree.Unary).Type = "number"  
        -> List.append inLst 
            [(
                        {
                        Type= "primary"; 
                        PrimaryType= "numeric"; 
                        BitsStart= Some "-3"; 
                        BitsEnd= Some (Option.get (Option.get (Option.get tree.Unary).Number).Bits); 
                        Primary= {
                            Name="delete123";
                            Location=(Option.get (Option.get tree.Unary).Number).Location
                            }
                        }, isConcat
                    )]     
    
    // hack for reduction to get that its width is 1 (correct?)
    | "reduction" 
        -> List.append inLst 
            [(
                        {
                        Type= "primary"; 
                        PrimaryType= "reduction"; 
                        BitsStart= Some "-1"; 
                        BitsEnd= Some "-1"; 
                        Primary= {Name="delete123";Location=0}
                        }, isConcat
                    )] 
    
    | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" 
    | "additive" | "logical_SHIFT" | "logical_AND" 
    | "logical_OR" | "unary_list" 
        -> List.append 
            (primariesUsedInAssignment inLst isConcat (Option.get tree.Head))
            (if isNullOrUndefined tree.Tail 
                        then inLst 
                    else primariesUsedInAssignment inLst isConcat (Option.get tree.Tail))
    | _ -> inLst


//////////////////////////////

let checkAssignments (ast:VerilogInput) portMap portSizeMap portWidthDeclarationMap inputParameterWireSizeMap inputParameterWireList linesLocations errorList =
    
    
    let checkLHSNameAndWidth2 assignment localErrors = 
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
                        let currLocation = lhs.Primary.Location
                        let line,prevLineLocation, nextLineLocation= findLineOfItem linesLocations currLocation
                        List.append localErrors [{Line = line; Col=currLocation-prevLineLocation+1 ; Length=(String.length lhs.Primary.Name)  ; Message = sprintf "Wrong width of output port: '%s'" lhs.Primary.Name}]
                | true -> localErrors
            | None -> failwithf "Can't happen! PortMap and PortSizeMap should have the same keys"
        | _ -> 
            let currLocation = lhs.Primary.Location
            let line,prevLineLocation, nextLineLocation= findLineOfItem linesLocations currLocation
            List.append localErrors [{Line = line; Col=currLocation-prevLineLocation+1 ; Length=(String.length lhs.Primary.Name) ; Message = sprintf "Variable '%s' is not declared as an output port" lhs.Primary.Name}]


    let checkNamesOfAssignment2 (assignment: AssignmentT) localErrors = 
        let PrimariesRHS = primariesUsedInAssignment [] false assignment.RHS |> List.map fst
        
        let namesWithLocRHS = PrimariesRHS |> List.map (fun x -> (x.Primary.Name, x.Primary.Location))
        let namesRHS = namesWithLocRHS |> List.map fst
        let namesToLocMap = namesWithLocRHS |> Map.ofList

        let diff = List.except (List.toSeq (List.append inputParameterWireList ["delete123"])) namesRHS
        match List.isEmpty diff with
        | true -> localErrors
        | false -> 
            diff
            |> List.collect (fun name ->
                let currLocation = Map.find name namesToLocMap
                let line,prevLineLocation, nextLineLocation= findLineOfItem linesLocations currLocation
                List.append localErrors [{Line = line; Col=currLocation-prevLineLocation+1 ; Length=(String.length name) ; Message = sprintf "Variable '%s' is not declared as input/parameter/wire" name}]
            )
            

    let checkSizesOfAssignment2 (assignment: AssignmentT) localErrors =
        let primariesRHS = primariesUsedInAssignment []false assignment.RHS |> List.map fst
        primariesRHS
        |> List.collect (fun x -> 
            match isNullOrUndefined x.BitsStart with
            | false ->
                let name = x.Primary.Name
                let bStart = int <| Option.get x.BitsStart 
                let bEnd = int <| Option.get x.BitsEnd
                match bStart with
                |(-3) ->   // number
                    if bEnd = 0 then
                        let currLocation = x.Primary.Location
                        let line,prevLineLocation, _ = findLineOfItem linesLocations currLocation
                        List.append localErrors [{Line = line; Col=currLocation-prevLineLocation+1 ; Length=3 ; Message = sprintf "Number can't be 0 bits wide"}]
                    else localErrors
                | _ -> 
                    match Map.tryFind name inputParameterWireSizeMap with
                    | Some size -> 
                        if (bStart<size) && (bEnd>=0) && (bStart>=bEnd) then
                            localErrors //ok
                        else 
                            let currLocation = x.Primary.Location
                            let line,prevLineLocation, nextLineLocation= findLineOfItem linesLocations currLocation
                            List.append localErrors [{Line = line; Col=currLocation-prevLineLocation+1 ; Length=(String.length name) ; Message = sprintf "Wrong width of variable: '%s'" name}]
                    | None -> localErrors //invalid name, error found by AssignmentRHSNameCheck 
            | true -> localErrors
        )
    
    

    /// Checks whether the length of bits on LHS of assignment matches the length of bits on the RHS
    /// TODO: needs improvement -> need to search RHS for wires and parameters as well
    ///                         -> which variable has the wrong width
    let checkBitsOfAssignment2 (assignment: AssignmentT) location localErrors =
        let lengthLHS = 
            match isNullOrUndefined assignment.LHS.BitsStart with
            | true -> 
                match Map.tryFind assignment.LHS.Primary.Name portSizeMap with
                | Some num -> [num]
                | None -> [-2]
            | false -> [((Option.get assignment.LHS.BitsStart) |> int) - ((Option.get assignment.LHS.BitsEnd) |> int)+1]

        let PrimariesRHS = primariesUsedInAssignment [] false assignment.RHS
        
        // printfn "primariesRHS: %A" PrimariesRHS 
        
        let lengthRHS =
            PrimariesRHS
            |> List.map (fun (x,y) ->
                match isNullOrUndefined x.BitsStart with
                | true -> 
                    match Map.tryFind x.Primary.Name inputParameterWireSizeMap with
                    | Some num -> (num,y)
                    | None -> (lengthLHS[0],false) // if name doesn't exist skip it, error found by assignmentRHSNameCheck
                | false -> 
                    match x.BitsStart with
                    |Some "-1" -> (1,y)
                    |Some "-3" -> ((Option.get x.BitsEnd) |> int, y) 
                    | _ -> (((Option.get x.BitsStart) |> int) - ((Option.get x.BitsEnd) |> int) + 1,y)
            )
        
        let concatenatedLengthRHS = 
            lengthRHS
            |> List.filter (fun (l,c) -> c)
            |> List.map fst
            |> List.fold (fun state length -> state+length ) 0
        
        let trueLengthRHS = 
            lengthRHS
            |> List.filter (fun (l,c) -> c=false)
            |> List.map fst
        let trueLengthRHS' = if concatenatedLengthRHS <> 0 then List.append trueLengthRHS [concatenatedLengthRHS] else trueLengthRHS

        
        match trueLengthRHS' with
        | [-2] -> localErrors
        | _ ->
            let diff = List.except (List.toSeq lengthLHS) trueLengthRHS'
            match List.isEmpty diff with
            | true -> 
                localErrors
            | false -> 
                let currLocation = location
                let line,prevLineLocation, nextLineLocation= findLineOfItem linesLocations currLocation
                List.append localErrors [{Line = line; Col=currLocation-prevLineLocation+1 ; Length=(String.length "assign") ; Message = sprintf "Different bit width on right <-> left"}]


    let assignmentsWithLocation = 
        ast.Module.ModuleItems.ItemList 
        |> Array.toList 
        |> List.filter (fun item -> item.ItemType = "statement")
        |> List.map (fun item -> (Option.get item.Statement),item.Location)
        |> List.filter (fun (statement,loc) -> statement.StatementType = "assign")
        |> List.map (fun (statement,loc) -> statement.Assignment,loc)
        // |> List.unzip

    let localErrors = 
        assignmentsWithLocation
        |> List.collect (fun (assignment,location) ->
            []
            |> checkLHSNameAndWidth2 assignment
            |> checkNamesOfAssignment2 assignment
            |> checkSizesOfAssignment2 assignment
            |> checkBitsOfAssignment2 assignment location
        )

    List.append errorList localErrors
/////////////////////////////

/// Returns the port-size map (e.g. (port "a" => 4 bits wide))
let getPortSizeAndLocationMap ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let portSizeLocation = items |> List.collect (fun x -> 
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
let getPortWidthDeclarationMap ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    items |> List.collect (fun x -> 
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
                    |> List.collect (fun x -> [(x.Name,d.DeclarationType)]) 
                | None -> []
            | true -> []
    ) |> Map.ofList
    
////// NOT FINISHED : TODO
let getParameterSizeAndLocationMap ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let parameterSizeLocation = items |> List.collect (fun x -> 
            match (x.ParamDecl |> isNullOrUndefined) with
            | false -> 
                match x.ParamDecl with
                | Some p -> 
                    let size = 1 ////// FOR NOW 
                        // match isNullOrUndefined d.Range with
                        // | true -> 1
                        // | false -> ((Option.get d.Range).Start |> int) - ((Option.get d.Range).End |> int) + 1
                    let location = x.Location
                    [(p.Parameter.Identifier.Name,size,location)] 
                    // |> Array.toList 
                    // |> List.collect (fun x -> [(x,size)]) 
                | None -> []
            | true -> []
    )
    let ps = List.map (fun x -> match x with | p,s,l -> (p,s)) parameterSizeLocation
    let pl = List.map (fun x -> match x with | p,s,l -> (p,l)) parameterSizeLocation
    (Map.ofList ps, Map.ofList pl)

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
let getWireNames ast = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    items |> List.collect (fun x -> 
        match (x.Statement |> isNullOrUndefined) with
        | false -> 
            match x.Statement with
            | Some statement when statement.StatementType = "wire" -> [statement.Assignment.LHS.Primary.Name]
            | _ -> []
        | true -> []
    )

/// Main error-finder function
/// Returns a list of errors (strings)
let getErrors ast model linesLocations =
    
    ///////// MAPS, LISTS NEEDED  ////////////////
    let portMap  = getPortMap ast
    let portSizeMap,portLocationMap = getPortSizeAndLocationMap ast
    let portWidthDeclarationMap = getPortWidthDeclarationMap ast
    let parameterSizeMap, parameterLocationMap = getParameterSizeAndLocationMap ast
    let parameterNameList = parameterSizeMap |> Map.toList |> List.map fst
    let inputNameList = getInputNames portMap
    let inputSizeMap = getInputSizeMap inputNameList portSizeMap
    let wireNameList = getWireNames ast
    let inputParameterWireList =
        parameterNameList
        |> List.append inputNameList
        |> List.append wireNameList
    
    let inputParameterWireSizeMap = 
        Map.toList inputSizeMap
        |> List.append (Map.toList parameterSizeMap)
        // |> List.append (Map.toList wireSizeList)   //TODO: WireSizeMap
        |> Map.ofList
    //////////////////////////////////////////////

    
    []  //begin with empty list and add errors to it
    |> portCheck ast linesLocations//all ports are declared as input/output
    |> checkIODeclarations ast portWidthDeclarationMap portLocationMap linesLocations//correct port width declaration (e.g. [1:4] -> invalid)
    |> checkIOWidthDeclarations ast linesLocations
    |> parameterNameCheck ast parameterNameList portMap parameterLocationMap linesLocations
    |> wireNameCheck ast portMap parameterSizeMap wireNameList
    |> checkAssignments ast portMap portSizeMap portWidthDeclarationMap inputParameterWireSizeMap inputParameterWireList linesLocations
    |> checkAllOutputsAssigned ast portMap portSizeMap portLocationMap linesLocations