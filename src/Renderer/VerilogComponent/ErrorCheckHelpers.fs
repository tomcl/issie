module ErrorCheckHelpers


open VerilogTypes
open Fable.Core.JsInterop
open CommonTypes
open VerilogAST
open NumberHelpers

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

/// return line number based on location
let getLineNumber
    (linesLocations: int list)
    (location: int) =
    List.findIndexBack (fun x -> x <= location) linesLocations + 1 

/// Recursive function to get all the primaries used in the RHS of an assignment
/// Used by checkNamesOnRHSOfAssignment and checkSizesOnRHSOfAssignment
let rec primariesUsedInAssignment inLst (tree: ExpressionT) = 
    match tree.Type with
    | "unary" when (Option.get tree.Unary).Type = "primary" ->
        match (Option.get tree.Unary).Expression with
        | Some expr -> 
            inLst @ [(Option.get (Option.get tree.Unary).Primary)]  @ primariesUsedInAssignment [] expr
        | _ -> 
            List.append inLst [(Option.get (Option.get tree.Unary).Primary)]
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
                            Width=None;
                            }
                            
                            
                        )]
        | _ -> inLst

    | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" 
    | "additive" | "SHIFT" | "logical_AND" 
    | "logical_OR" | "unary_list" | "equality" | "comparison" | "multiplicative"
    | "conditional_cond" | "conditional_result"
        -> List.append 
            (primariesUsedInAssignment inLst (Option.get tree.Head))
            (if isNullOrUndefined tree.Tail 
                        then inLst 
                    else primariesUsedInAssignment inLst (Option.get tree.Tail))
    | _ -> inLst

/// replace this later with getLHSBits'!
let getLHSBits portSizeMap (assignment: AssignmentT)  =
    let assignmentWithRange =
        match assignment.LHS with
        | a when isNullOrUndefined assignment.LHS.BitsStart -> (a.Primary.Name,-1,-1)
        | a -> (a.Primary.Name,(int (Option.get a.BitsStart)),(int (Option.get a.BitsEnd)))
    
    let portListMap =
        match assignmentWithRange with
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

    portListMap

/// returns all the bits of the lhs of an assignment
/// the strings returned are unique, index surrounded by "[]" is appended to the name of the variable
let getLHSBits' portSizeMap (assignment: AssignmentT)  =
    let assignmentWithRange =
        match assignment.LHS with
        | a when isNullOrUndefined assignment.LHS.BitsStart -> (a.Primary.Name,-1,-1)
        | a -> (a.Primary.Name,(int (Option.get a.BitsStart)),(int (Option.get a.BitsEnd)))
    
    let portListMap =
        match assignmentWithRange with
        |(name,-1,-1)->
            match Map.tryFind name portSizeMap with
            | Some size -> 
                let names = [0..size-1] |> List.map (fun y -> (name+"["+(string y)+"]"))
                names
            | None -> []
        |(name,x,y) when x=y ->
            [(name+"["+(string x)+"]")]
        |(name,bStart,bEnd)->
            let names = [bEnd..bStart] |> List.map (fun y -> (name+"["+(string y)+"]"))
            names

    portListMap

/// returns each bit of an assignment LHS. In the case of variable indexing, no bits are returned
let getLHSBitsAssignedCertainly portSizeMap (assignment: AssignmentT) =
    match assignment.LHS.BitsStart, assignment.LHS.BitsEnd, assignment.LHS.VariableBitSelect with
    | None, None, None ->
        match Map.tryFind assignment.LHS.Primary.Name portSizeMap with
        | Some size -> 
            let names = [0..size-1] |> List.map (fun y -> (assignment.LHS.Primary.Name+"["+(string y)+"]"))
            names
        | None -> []
    | Some s, Some e, None ->
        let names = [int e..int s] |> List.map (fun y -> (assignment.LHS.Primary.Name+"["+(string y)+"]"))
        names
    | None, None, Some _ -> []
    | _ -> failwithf "Wrong combination of bitstart, bitsend and variable bitselect"

let getPrimaryBits portSizeMap (primary: PrimaryT) =
    let primaryWithRange =
        match primary with
        | a when isNullOrUndefined primary.BitsStart -> (a.Primary.Name,-1,-1)
        | a -> (a.Primary.Name,(int (Option.get a.BitsStart)),(int (Option.get a.BitsEnd)))
    
    let portListMap =
        match primaryWithRange with
        |(name,-1,-1)->
            match Map.tryFind name portSizeMap with
            | Some size -> 
                let names = [0..size-1] |> List.map (fun y -> (name+"["+(string y)+"]"))
                names
            | None -> []
        |(name,x,y) when x=y ->
            [(name+"["+(string x)+"]")]
        |(name,bStart,bEnd)->
            let names = [bEnd..bStart] |> List.map (fun y -> (name+"["+(string y)+"]"))
            names

    portListMap


let getDeclarations declarations node =
    match node with
    | Declaration decl -> declarations @ [decl]
    | _ -> declarations

let getCaseStatements caseStatements node =
    match node with 
    | Case case -> caseStatements @ [case]
    | _ -> caseStatements

let getCaseStatementsWithLoc caseStatements node =
    match node with 
    | Statement statement -> 
        let stmt = getAlwaysStatement statement
        match stmt with
        | StatementDU.Case case -> caseStatements @ [case, statement.Location]
        | _ -> caseStatements
    | _ -> caseStatements

let getAlwaysBlocksWithLocations alwaysBlocks node =
    match node with 
    | Item item -> 
        match getItem item with
        | AlwaysConstruct always -> alwaysBlocks @ [always, item.Location]
        | _ -> alwaysBlocks
    | _ -> alwaysBlocks

let getCaseItemNums nums node =
    match node with
    | Number num -> nums @ [num]
    | _ -> nums


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
            match (Option.get tree.Tail).Type with
            |"unary_unsigned" ->
                let u1 = (findSizeOfExpression (Option.get tree.Head))
                {Name="[shift]";ResultWidth=u1.ResultWidth;Head=Some u1;Tail=None;Elements=[]}
            |_ ->
                let u1 = (findSizeOfExpression (Option.get tree.Head))
                let u2 = (findSizeOfExpression (Option.get tree.Tail))
                {Name="[shift]";ResultWidth=u1.ResultWidth;Head=Some u1;Tail=Some u2;Elements=[]}

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


let getWidthOfExpr
    (assignmentRHS:ExpressionT)
    (inputWireSizeMap: Map<string,int>)
        =

    let rec findSizeOfExpression (tree:ExpressionT) = 
        match tree.Type with
        | "unary" |"negation" when (Option.get tree.Unary).Type = "primary" ->
            let primary = Option.get (Option.get tree.Unary).Primary
            match (isNullOrUndefined primary.BitsStart, (Option.get tree.Unary).Expression) with
                    | true, None -> 
                        match Map.tryFind primary.Primary.Name inputWireSizeMap with
                        | Some num -> num
                        | None -> 0 // if name doesn't exist skip it, error found by assignmentRHSNameCheck
                    | false, _ -> 
                        ((Option.get primary.BitsStart) |> int) - ((Option.get primary.BitsEnd) |> int) + 1
                    | true , Some expr -> 1
                        
        | "unary" when (Option.get tree.Unary).Type = "number" ->
                int <| (Option.get (Option.get (Option.get tree.Unary).Number).Bits)
        
        | "unary" when (Option.get tree.Unary).Type = "concat" -> 
            let unariesList = (findSizeOfConcat (Option.get (Option.get tree.Unary).Expression) [])
            let length= unariesList |> List.sum
            length
       
        | "unary" |"negation" when (Option.get tree.Unary).Type = "parenthesis" -> 
            let elements = (findSizeOfExpression (Option.get (Option.get tree.Unary).Expression))
            elements

        | "bitwise_OR" | "bitwise_XOR" | "bitwise_AND" 
        | "additive" 
            -> 
            let u1 = findSizeOfExpression (Option.get tree.Head)
            let u2 = findSizeOfExpression (Option.get tree.Tail)
            max u1 u2
            
        | "conditional_cond" -> 
            let u1 = (findSizeOfExpression (Option.get (Option.get tree.Tail).Head))
            let u2 = (findSizeOfExpression (Option.get (Option.get tree.Tail).Tail))
            max u1 u2
        
        | "SHIFT" ->
            match (Option.get tree.Tail).Type with
            |"unary_unsigned" ->
                let u1 = (findSizeOfExpression (Option.get tree.Head))
                u1
            |_ ->
                let u1 = (findSizeOfExpression (Option.get tree.Head))
                let u2 = (findSizeOfExpression (Option.get tree.Tail))
                //max u1 u2
                u1

        | "reduction" when (Option.get tree.Unary).Type = "parenthesis" ->
            let result = findSizeOfExpression (Option.get (Option.get tree.Unary).Expression)
            1 // check what this is

        | "reduction" ->
            1 // check this

        | "logical_OR" | "logical_AND" ->
            let u1 = findSizeOfExpression (Option.get tree.Head)
            let u2 = findSizeOfExpression (Option.get tree.Tail)
            1
        | "equality" -> 1
        | "comparison" -> 1
        | "multiplicative" -> 
            let w1 = findSizeOfExpression (Option.get tree.Head)
            let w2 = findSizeOfExpression (Option.get tree.Tail)
            w1+w2
        | _ -> failwithf "Case not covered!"

    and findSizeOfConcat (tree:ExpressionT) (concatList: int List) : int List =
        
        match isNullOrUndefined tree.Tail with
        |true -> concatList@[(findSizeOfExpression (Option.get tree.Head))]
        |false ->
            let updated = concatList@[(findSizeOfExpression (Option.get tree.Head))]
            findSizeOfConcat (Option.get tree.Tail) updated
    

    findSizeOfExpression assignmentRHS


        /// Check if the width of each wire/input used
    /// is within the correct range (defined range)
let checkPrimariesWidths linesLocations currentInputWireSizeMap localErrors (primariesRHS: PrimaryT list) =
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
let checkExpr linesLocations currentInputWireSizeMap localErrors expr =
    let primariesRHS = primariesUsedInAssignment [] expr
    checkPrimariesWidths linesLocations currentInputWireSizeMap localErrors primariesRHS

let checkNumber linesLocations (num:NumberT) =
    let numBase, allNum, width = Option.get num.Base, Option.get num.AllNumber, Option.get num.Bits
    if int width = 0 then
        let message = "Number can't be 0 bits wide"
        let extraMessages = 
            [|
                {Text="Number can't be 0 bits wide"; Copy=false;Replace=NoReplace}
                {Text=("The integer before 'h/'b represents the width of the number\n e.g. 12'hc7 -> 000011000111");Copy=false;Replace=NoReplace}
            |]
        (createErrorMessage linesLocations num.Location message extraMessages "0'b")
    else 
        let no = 
            match numBase with
            |"'b" -> "0"+allNum
            |"'h" ->
                "0x"+allNum
            |_ -> 
                allNum
        
        let no = toDecimal allNum numBase "64"
        match NumberHelpers.strToIntCheckWidth (int width) (string no) with
        |Ok n -> []
        |Error _ -> 
            let message = sprintf "Number can't fit in %A bits" width
            let extraMessages = 
                [|
                    {Text=sprintf "Number can't fit in %A bits" width; Copy=false;Replace=NoReplace}
                    {Text=("The integer before 'h/'b represents the width of the number\n e.g. 12'hc7 -> 000011000111");Copy=false;Replace=NoReplace}
                |]
            createErrorMessage linesLocations num.Location message extraMessages "0'b"


// /////////// Helpers for Expressions ////////////////

type ExpressionNode =
    | Expression of ExpressionT
    | Unary of UnaryT
    | Number of NumberT
    | Primary of PrimaryT

/// make sure to include variables AND ports in portSizeMap
let getRHSBits portSizeMap expression=
    
    let rec getExprBits (expr:ExpressionT) =
        let leftBits =
            match expr.Head with
            | Some head -> getExprBits head
            | _ -> Set.empty
        let rightBits =
            match expr.Tail with
            | Some tail -> getExprBits tail
            | _ -> Set.empty
        let unaryBits =
            match expr.Unary with
            |Some unary -> 
                match unary.Expression, unary.Number, unary.Primary with
                | Some expr, None, None -> getExprBits expr
                | None, Some _, None -> Set.empty
                | None, None, Some primary -> 
                    let var = primary.Primary.Name
                    let bitsStart =
                        match primary.BitsStart with
                        |Some bitstart -> bitstart |> int
                        | _ -> ( Option.defaultValue 1 (Map.tryFind var portSizeMap))-1// get bit width from portmap
                    let bitsEnd =
                        match primary.BitsEnd with
                        |Some bitsend -> bitsend |> int
                        | _ ->  0
                    [bitsEnd .. bitsStart]
                    |> List.map (fun idx -> var+"["+(string idx)+"]")
                    |> Set.ofList
                | Some expr, None, Some primary ->
                    let bitSelectBits = getExprBits expr
                    let var = primary.Primary.Name
                    let bitsStart =
                        match primary.BitsStart with
                        |Some bitstart -> bitstart |> int
                        | _ -> ( Option.defaultValue 1 (Map.tryFind var portSizeMap))-1/// get bit width from portmap
                    let bitsEnd =
                        match primary.BitsEnd with
                        |Some bitsend -> bitsend |> int
                        | _ ->  0
                    [bitsEnd .. bitsStart]
                    |> List.map (fun idx -> var+"["+(string idx)+"]")
                    |> Set.ofList
                    |> Set.union bitSelectBits
                | _ -> failwithf "Invalid expression, should not happen!!!"
            | _ -> Set.empty
        (Set.empty, [leftBits; rightBits; unaryBits])
        ||> List.fold Set.union

    getExprBits expression

let getLHSWidth (assign:AssignmentT) (varSizeMap: Map<string, int>)  =
    match assign.LHS.BitsStart, assign.LHS.BitsEnd, assign.LHS.VariableBitSelect, assign.LHS.Width with
    | Some s, Some e, _, _ -> (int s)-(int e)+1
    | None, None, None, _ -> 
        match Map.tryFind assign.LHS.Primary.Name varSizeMap with
        | Some size -> size
        | _ -> 0 //failwithf "What? Variable doesn't have a size" // if the variable is not declared there should be different logic
    | None, None, Some _, Some w -> w
    | _ -> failwithf "Only one of bitsStart and bitsEnd present"


let getCondAndCaseExpressions (expressions: (ExpressionT*int) list) (node: ASTNode) = 
    match node with
    | Case case -> expressions @ [case.Expression, case.Location]
    | IfStatement cond -> expressions @ [cond.Condition, cond.Location]
    | _ -> expressions

let getModuleInstantiationStatements moduleInstantiations node =
    match node with
    | ModuleInstantiation modInst -> moduleInstantiations @ [modInst]
    | _ -> moduleInstantiations


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
    |> List.sortBy String.length

/// Output primaries
let getModuleInstantiationOutputPrimaries (modInst:ModuleInstantiationT) (project:Project) =
    match List.tryFind (fun c -> c.Name = modInst.Module.Name) project.LoadedComponents with
    | Some comp ->
        let outputs =
            comp.OutputLabels
            |> List.map fst
            |> Set.ofList
        let outputPrimaries =
            modInst.Connections
            |> Array.toList
            |> List.filter (fun conn -> Set.contains (conn.PortId.Name.ToUpper()) outputs)
            |> List.map (fun conn -> conn.Primary)
        outputPrimaries
    | None -> 
        modInst.Connections
        |> Array.toList
        |> List.map (fun conn->conn.Primary)


/// Input primaries
let getModuleInstantiationInputPrimaries (modInst:ModuleInstantiationT) (project:Project) =
    match List.tryFind (fun c -> c.Name = modInst.Module.Name) project.LoadedComponents with
    | Some comp ->
        let inputs =
            comp.InputLabels
            |> List.map fst
            |> Set.ofList
        let inputPrimaries =
            modInst.Connections
            |> Array.toList
            |> List.filter (fun conn -> Set.contains (conn.PortId.Name.ToUpper()) inputs)
            |> List.map (fun conn -> conn.Primary)
        inputPrimaries
    | None -> 
        modInst.Connections
        |> Array.toList
        |> List.map (fun conn->conn.Primary)