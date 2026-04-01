module ErrorCheckHelpers


open VerilogTypes
open Fable.Core.JsInterop
open CommonTypes
open VerilogAST
open NumberHelpers

/// Helper function to convert expressions to ints and back (for width checking)
/// TODO: THIS CAN BE SIMPLIFIED AFTER REFACTORING
let evalExpr (expr: ExpressionDU) =
        expr
        |> evalIntExpression

/// Helper function to evaluate numeric bit-select bounds
let evalNumber (num: string) =
    num
    |> int
// let evalExprDU (expr: ExpressionDU) =
//     expr |> evalIntExpression

/// Helper functions to extract details from the LHS of an assignment
let getPrimaryName (p: PrimaryDU) =
    match p with
    | Identifier id
    | IdentifierBit (id, _)
    | VariableBitSelect (id, _)
    | IdentifierBits (id, _, _)
    | IdentifierBitsSelect (id, _, _, _)
    | IdentifierArray (id, _) -> id.Name

let getPrimaryLocation (p: PrimaryDU) =
    match p with
    | Identifier id
    | IdentifierBit (id, _)
    | VariableBitSelect (id, _)
    | IdentifierBits (id, _, _)
    | IdentifierBitsSelect (id, _, _, _)
    | IdentifierArray (id, _) -> id.Location

let getPrimaryRange (p: PrimaryDU) =
    match p with
    | Identifier _ -> None
    | IdentifierArray _ -> None
    | IdentifierBit (_, idx) ->
        Some (idx, idx)
    | VariableBitSelect (_, _) -> None
    | IdentifierBits (_, start, end_) ->
        Some (start, end_)
    | IdentifierBitsSelect (_, start, width, sel) ->
        let bStart = evalIntExpression start
        let bEnd =
            match sel with
            | PlusWidth -> bStart + width - 1
            | MinusWidth -> bStart - width + 1
        Some (bStart, bEnd)

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
let rec primariesUsedInAssignment inLst (tree: ExpressionDU) =
    match tree with
    | ExpressionDU.Unary u ->
        match u with
        | UnaryDU.Primary p -> inLst @ [p]
        | Parenthesis e -> primariesUsedInAssignment inLst e
        | Concat elems -> elems |> Array.fold primariesUsedInAssignment inLst
        | UnaryDU.Number _ -> inLst
    | Negation u ->
        match u with
        | UnaryDU.Primary p -> inLst @ [p]
        | Parenthesis e -> primariesUsedInAssignment inLst e
        | Concat elems -> elems |> Array.fold primariesUsedInAssignment inLst
        | UnaryDU.Number _ -> inLst
    | Reduction (_, e) -> primariesUsedInAssignment inLst e
    | UnaryUnsigned _ -> inLst
    | LogicalOr (a, b)
    | LogicalAnd (a, b)
    | BitwiseOr (a, b)
    | BitwiseXor (a, b)
    | BitwiseXnor (a, b)
    | BitwiseAnd (a, b)
    | Equality (_, a, b)
    | Comparison (_, a, b)
    | ShiftExpr (_, a, b)
    | Additive (_, a, b)
    | Multiplicative (_, a, b) ->
        primariesUsedInAssignment inLst a
        |> fun acc -> primariesUsedInAssignment acc b
    | ConditionalOp (c, t, f) ->
        primariesUsedInAssignment inLst c
        |> fun acc -> primariesUsedInAssignment acc t
        |> fun acc -> primariesUsedInAssignment acc f

let rec numbersUsedInAssignment inLst (tree: ExpressionDU) =
    match tree with
    | ExpressionDU.Unary u ->
        match u with
        | UnaryDU.Number n -> inLst @ [n]
        | Parenthesis e -> numbersUsedInAssignment inLst e
        | Concat elems -> elems |> Array.fold numbersUsedInAssignment inLst
        | UnaryDU.Primary _ -> inLst
    | Negation u ->
        match u with
        | UnaryDU.Number n -> inLst @ [n]
        | Parenthesis e -> numbersUsedInAssignment inLst e
        | Concat elems -> elems |> Array.fold numbersUsedInAssignment inLst
        | UnaryDU.Primary _ -> inLst
    | Reduction (_, e) -> numbersUsedInAssignment inLst e
    | UnaryUnsigned n -> inLst @ [n]
    | LogicalOr (a, b)
    | LogicalAnd (a, b)
    | BitwiseOr (a, b)
    | BitwiseXor (a, b)
    | BitwiseXnor (a, b)
    | BitwiseAnd (a, b)
    | Equality (_, a, b)
    | Comparison (_, a, b)
    | ShiftExpr (_, a, b)
    | Additive (_, a, b)
    | Multiplicative (_, a, b) ->
        numbersUsedInAssignment inLst a
        |> fun acc -> numbersUsedInAssignment acc b
    | ConditionalOp (c, t, f) ->
        numbersUsedInAssignment inLst c
        |> fun acc -> numbersUsedInAssignment acc t
        |> fun acc -> numbersUsedInAssignment acc f

/// replace this later with getLHSBits'!
let getLHSBits portSizeMap (assignment: Assignment)  =
    let assignmentWithRange =
        match assignment.LHS.PrimaryType with
        | Identifier id
        | IdentifierArray (id, _) -> (id.Name, -1, -1)
        | IdentifierBit (id, idx) ->
            (id.Name, idx, idx)
        | VariableBitSelect (id, idx) ->
            (id.Name, -1, -1)
        | IdentifierBits (id, start, end_) ->
            // let bStart = evalExpr start
            // let bEnd = evalExpr end_
            (id.Name, start, end_)
        | IdentifierBitsSelect (id, start, width, sel) ->
            let bStart = evalIntExpression start
            let bEnd =
                match sel with
                | PlusWidth -> bStart + width - 1
                | MinusWidth -> bStart - width + 1
            (id.Name, bStart, bEnd)
    
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
let getLHSBits' portSizeMap (assignment: Assignment)  =
    let assignmentWithRange =
        match assignment.LHS.PrimaryType with
        | Identifier id
        | IdentifierArray (id, _) -> (id.Name, -1, -1)
        | IdentifierBit (id, idx) ->
            // let b = evalIntExpression idx
            (id.Name, idx, idx)
        | VariableBitSelect (id, idx) ->
            (id.Name, -1, -1)
        | IdentifierBits (id, start, end_) ->
            // let bStart = evalIntExpression start
            // let bEnd = evalExpr end_
            (id.Name, start, end_)
        | IdentifierBitsSelect (id, start, width, sel) ->
            let bStart = evalIntExpression start
            let bEnd =
                match sel with
                | PlusWidth -> bStart + width - 1
                | MinusWidth -> bStart - width + 1
            (id.Name, bStart, bEnd)

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
let getLHSBitsAssignedCertainly portSizeMap (assignment: Assignment) =
    match assignment.LHS.VariableBitSelect with
    | Some _ -> []
    | _ ->
        match assignment.LHS.PrimaryType with
        | Identifier id
        | IdentifierArray (id, _) ->
            match Map.tryFind id.Name portSizeMap with
            | Some size ->
                let names = [0..size-1] |> List.map (fun y -> id.Name + "[" + string y + "]")
                names
            | None -> []
        | VariableBitSelect (id, idx) ->
            []
        | IdentifierBits (id, bStart, bEnd) ->
            let names = [bEnd..bStart] |> List.map (fun y -> id.Name + "[" + string y + "]")
            names
        | IdentifierBit _
        | IdentifierBitsSelect _ ->
        []
    // | _ -> failwithf "Wrong combination of bitstart, bitsend and variable bitselect"

let getPrimaryBits portSizeMap (primary: PrimaryDU) =
    let primaryWithRange =
        match primary with
        | Identifier id
        | IdentifierArray (id, _) -> (id.Name, -1, -1)
        | VariableBitSelect (id, idx) ->
            (id.Name, -1, -1)
        | IdentifierBit (id, idx) ->
            (id.Name, idx, idx)
        | IdentifierBits (id, start, end_) ->
            // let bStart = evalIntExpression start
            // let bEnd = evalExpr end_
            (id.Name, start, end_)
        | IdentifierBitsSelect (id, start, width, sel) ->
            let bStart = evalIntExpression start
            let bEnd =
                match sel with
                | PlusWidth -> bStart + width - 1
                | MinusWidth -> bStart - width + 1
            (id.Name, bStart, bEnd)

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
        match statement with
        | StatementDU.Case case -> caseStatements @ [case, case.Location]
        | _ -> caseStatements
    | _ -> caseStatements

let getAlwaysBlocksWithLocations alwaysBlocks node =
    match node with
    | Item item ->
        match getItem item with
        | AlwaysConstruct always -> alwaysBlocks @ [always, always.Location]
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
/// Seems unused currently?
let RHSUnaryAnalysis
    (assignmentRHS:ExpressionDU)
    (inputWireSizeMap: Map<string,int>)
        : OneUnary =

    let rec findSizeOfExpression (tree: ExpressionDU) : OneUnary =
        match tree with
        | ExpressionDU.Unary u
        | Negation u ->
            match u with
            | UnaryDU.Primary primary ->
                match primary with
                | Identifier id
                | IdentifierArray (id, _) ->
                    match Map.tryFind id.Name inputWireSizeMap with
                    | Some num -> {Name=id.Name;ResultWidth=num;Head=None;Tail=None;Elements=[]}
                    | None -> {Name="undefined";ResultWidth=0;Head=None;Tail=None;Elements=[]} 
                | VariableBitSelect (id, idx) ->
                    match Map.tryFind id.Name inputWireSizeMap with
                    | Some num -> {Name=id.Name;ResultWidth=num;Head=None;Tail=None;Elements=[]}
                    | None -> {Name="undefined";ResultWidth=0;Head=None;Tail=None;Elements=[]} 
                | IdentifierBit (id, _) ->
                    {Name=id.Name;ResultWidth=1;Head=None;Tail=None;Elements=[]}
                | IdentifierBits (id, bStart, bEnd) ->
                    // let bStart = evalExpr start
                    // let bEnd = evalExpr end_
                    {Name=id.Name;ResultWidth=bStart - bEnd + 1;Head=None;Tail=None;Elements=[]}
                | IdentifierBitsSelect (id, _, width, _) ->
                    {Name=id.Name;ResultWidth=width;Head=None;Tail=None;Elements=[]}
            | UnaryDU.Number n ->
                let width =
                    match n with
                    | Unsigned _ -> 32
                    | All (bits, _, _, _) -> bits
                {Name="[number]";ResultWidth=width;Head=None;Tail=None;Elements=[]}
            | UnaryDU.Concat elems ->
                let unariesList = findSizeOfConcat elems []
                let length = (0, unariesList) ||> List.fold (fun s unary -> s + unary.ResultWidth)
                {Name="{...}";ResultWidth=length;Head=None;Tail=None;Elements=unariesList}
            | UnaryDU.Parenthesis e ->
                let elements = findSizeOfExpression e
                {Name="(...)";ResultWidth=elements.ResultWidth;Head=None;Tail=None;Elements=[elements]}

        | BitwiseOr (a, b)
        | BitwiseXor (a, b)
        | BitwiseAnd (a, b)
        | Additive (_, a, b) ->
            let u1 = findSizeOfExpression a
            let u2 = findSizeOfExpression b
            {Name="[bitwise_op]";ResultWidth=u1.ResultWidth;Head=Some u1;Tail=Some u2;Elements=[]}

        | ConditionalOp (c, t, f) ->
            let result = findSizeOfExpression c
            let u1 = findSizeOfExpression t
            let u2 = findSizeOfExpression f
            {Name="[conditional]";ResultWidth=u1.ResultWidth;Head=Some u1;Tail=Some u2;Elements=[result]}

        | ShiftExpr (_, a, b) ->
            let u1 = findSizeOfExpression a
            let u2 = findSizeOfExpression b
            match b with
            | UnaryUnsigned _ ->
                {Name="[shift]";ResultWidth=u1.ResultWidth;Head=Some u1;Tail=None;Elements=[]}
            | _ ->
                {Name="[shift]";ResultWidth=u1.ResultWidth;Head=Some u1;Tail=Some u2;Elements=[]}

        | Reduction (_, e) ->
            match e with
            | ExpressionDU.Unary (UnaryDU.Parenthesis p) ->
                let result = findSizeOfExpression p
                {Name="[reduction]";ResultWidth=1;Head=None;Tail=None;Elements=[result]}
            | _ ->
                {Name="[reduction]";ResultWidth=1;Head=None;Tail=None;Elements=[]}

        | LogicalOr (a, b)
        | LogicalAnd (a, b) ->
            let u1 = findSizeOfExpression a
            let u2 = findSizeOfExpression b
            {Name="[logical_op]";ResultWidth=1;Head=Some u1;Tail=Some u2;Elements=[]}

        | _ -> failwithf "Case not covered!"

    and findSizeOfConcat (elements: ExpressionDU array) (concatList: OneUnary list) : OneUnary list =
        elements
        |> Array.map findSizeOfExpression
        |> Array.fold (fun acc unary -> acc @ [unary]) concatList

    findSizeOfExpression assignmentRHS


let getWidthOfExpr
    (assignmentRHS:ExpressionDU)
    (inputWireSizeMap: Map<string,int>)
        =

    let rec findSizeOfExpression (tree:ExpressionDU) = 
        match tree with
        | ExpressionDU.Unary (UnaryDU.Primary primary)
        | Negation (UnaryDU.Primary primary) ->
            match primary with
            | Identifier id
            | IdentifierArray (id, _) ->
                match Map.tryFind id.Name inputWireSizeMap with
                | Some num -> num
                | None -> 0
            | VariableBitSelect (id, idx) -> 
                match Map.tryFind id.Name inputWireSizeMap with
                | Some num -> num
                | None -> 0
            | IdentifierBit _ -> 1
            | IdentifierBits (id, bStart, bEnd) ->
                // let bStart = evalExpr start
                // let bEnd = evalExpr end_
                bStart - bEnd + 1
            | IdentifierBitsSelect (_, _, width, _) -> width
                        
        | ExpressionDU.Unary (UnaryDU.Number n)
        | UnaryUnsigned n ->
            match n with
            | Unsigned _ -> 32
            | All (bits, _, _, _) -> bits
            
        | ExpressionDU.Unary (UnaryDU.Concat e) -> 
            let unariesList = (findSizeOfConcat e [])
            let length = unariesList |> List.sum
            length
       
        | ExpressionDU.Unary (UnaryDU.Parenthesis e)
        | Negation (UnaryDU.Parenthesis e) ->
            let elements = (findSizeOfExpression e)
            elements

        | BitwiseOr (head, tail)
        | BitwiseXor (head, tail)
        | BitwiseAnd (head, tail)
        | Additive (_, head, tail)
            -> 
            let u1 = findSizeOfExpression head
            let u2 = findSizeOfExpression tail
            max u1 u2
            
        | ConditionalOp (_, t, f) -> 
            let u1 = findSizeOfExpression t
            let u2 = findSizeOfExpression f
            max u1 u2
        
        | ShiftExpr (_, a, _) ->
            let u1 = findSizeOfExpression a
            u1

        | Reduction _ ->
            // let result = findSizeOfExpression (Option.get (Option.get tree.Unary).Expression)
            1 // check what this is


        | LogicalOr _
        | LogicalAnd _ -> 1

        | Equality _ -> 1
        | Comparison _ -> 1

        | Multiplicative (_, head, tail) -> 
            let w1 = findSizeOfExpression head
            let w2 = findSizeOfExpression tail
            w1+w2
        | _ -> failwithf "Case not covered!"

    and findSizeOfConcat (elements:ExpressionDU array) (concatList: int List) : int List =
        elements
        |> Array.map findSizeOfExpression
        |> Array.fold (fun acc w -> acc @ [w]) concatList
        // |true -> concatList@[(findSizeOfExpression (Option.get tree.Head))]
        // |false ->
        //     let updated = concatList@[(findSizeOfExpression (Option.get tree.Head))]
        //     findSizeOfConcat (Option.get tree.Tail) updated
    

    findSizeOfExpression assignmentRHS


        /// Check if the width of each wire/input used
    /// is within the correct range (defined range)
let checkPrimariesWidths linesLocations currentInputWireSizeMap localErrors (primariesRHS: PrimaryDU list) (numbersRHS: Number list) =
    let primaryErrors =
        primariesRHS
        |> List.collect (fun x ->
            match x with
            | Identifier id
            | IdentifierArray (id, _) ->
                localErrors
            | VariableBitSelect (id, idx) ->
                localErrors
            | IdentifierBit (id, bStart) ->
                let bEnd = bStart
                match Map.tryFind id.Name currentInputWireSizeMap with
                | Some size ->
                    if (bStart < size) && (bEnd >= 0) then
                        localErrors
                    else
                        let definition =
                            match size with
                            | 1 -> " a single bit "
                            | _ -> sprintf " %s[%i:0] " id.Name (size - 1)
                        let usedWidth = sprintf " %s[%i] " id.Name bStart
                        let message = sprintf "Wrong width of variable: '%s'" id.Name
                        let extraMessages =
                            [|
                                {Text=(sprintf "Variable: '%s' is defined as" id.Name)+definition+"\nTherefore,"+usedWidth+"is invalid"; Copy=false; Replace=NoReplace}
                            |]
                        List.append localErrors (createErrorMessage linesLocations id.Location message extraMessages id.Name)
                | None -> localErrors
            | IdentifierBits (id, bStart, bEnd) ->
                match Map.tryFind id.Name currentInputWireSizeMap with
                | Some size ->
                    if (bStart < size) && (bEnd >= 0) && (bStart >= bEnd) then
                        localErrors
                    else
                        let definition =
                            match size with
                            | 1 -> " a single bit "
                            | _ -> sprintf " %s[%i:0] " id.Name (size - 1)
                        let usedWidth =
                            match bStart = bEnd with
                            | true -> sprintf " %s[%i] " id.Name bStart
                            | false -> sprintf " %s[%i:%i] " id.Name bStart bEnd
                        let message = sprintf "Wrong width of variable: '%s'" id.Name
                        let extraMessages =
                            [|
                                {Text=(sprintf "Variable: '%s' is defined as" id.Name)+definition+"\nTherefore,"+usedWidth+"is invalid"; Copy=false; Replace=NoReplace}
                            |]
                        List.append localErrors (createErrorMessage linesLocations id.Location message extraMessages id.Name)
                | None -> localErrors
            | IdentifierBitsSelect (id, start, width, _) ->
                let bStart = evalIntExpression start
                let bEnd = bStart - width + 1
                match Map.tryFind id.Name currentInputWireSizeMap with
                | Some size ->
                    if (bStart < size) && (bEnd >= 0) then
                        localErrors
                    else
                        let definition =
                            match size with
                            | 1 -> " a single bit "
                            | _ -> sprintf " %s[%i:0] " id.Name (size - 1)
                        let usedWidth = sprintf " %s[%i:%i] " id.Name bStart bEnd
                        let message = sprintf "Wrong width of variable: '%s'" id.Name
                        let extraMessages =
                            [|
                                {Text=(sprintf "Variable: '%s' is defined as" id.Name)+definition+"\nTherefore,"+usedWidth+"is invalid"; Copy=false; Replace=NoReplace}
                            |]
                        List.append localErrors (createErrorMessage linesLocations id.Location message extraMessages id.Name)
                | None -> localErrors
        )

    let numberErrors =
        numbersRHS
        |> List.collect (fun n ->
            match n with
            | Unsigned _ -> localErrors
            | All (bits, numBase, allNumber, loc) ->
                if bits = 0 then
                    let message = "Number can't be 0 bits wide"
                    let extraMessages =
                        [|
                            {Text="Number can't be 0 bits wide"; Copy=false; Replace=NoReplace}
                            {Text=("The integer before 'h/'b represents the width of the number\n e.g. 12'hc7 -> 000011000111"); Copy=false; Replace=NoReplace}
                        |]
                    List.append localErrors (createErrorMessage linesLocations loc message extraMessages "0'b")
                else
                    let baseText =
                        match numBase with
                        | Binary -> "'b"
                        | Hex -> "'h"
                        | Decimal -> "'d"
                    let no = toDecimal (string allNumber) baseText "64"
                    match NumberHelpers.strToIntCheckWidth bits (string no) with
                    | Ok _ -> localErrors
                    | Error _ ->
                        let message = sprintf "Number can't fit in %i bits" bits
                        let extraMessages =
                            [|
                                {Text=sprintf "Number can't fit in %i bits" bits; Copy=false; Replace=NoReplace}
                                {Text=("The integer before 'h/'b represents the width of the number\n e.g. 12'hc7 -> 000011000111"); Copy=false; Replace=NoReplace}
                            |]
                        List.append localErrors (createErrorMessage linesLocations loc message extraMessages "0'b")
        )

    List.append primaryErrors numberErrors

let checkExpr linesLocations currentInputWireSizeMap localErrors expr =
    let primariesRHS = primariesUsedInAssignment [] expr
    let numbersRHS = numbersUsedInAssignment [] expr
    checkPrimariesWidths linesLocations currentInputWireSizeMap localErrors primariesRHS numbersRHS

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
let getRHSBits portSizeMap (expression: ExpressionDU) =
    
    let rec getExprBits (expr: ExpressionDU) =
        let leftBits =
            match expr with
            | LogicalOr (a, _)
            | LogicalAnd (a, _)
            | BitwiseOr (a, _)
            | BitwiseXor (a, _)
            | BitwiseXnor (a, _)
            | BitwiseAnd (a, _)
            | Equality (_, a, _)
            | Comparison (_, a, _)
            | ShiftExpr (_, a, _)
            | Additive (_, a, _)
            | Multiplicative (_, a, _) -> getExprBits a
            | ConditionalOp (c, _, _) -> getExprBits c
            | _ -> Set.empty
        let rightBits =
            match expr with
            | LogicalOr (_, b)
            | LogicalAnd (_, b)
            | BitwiseOr (_, b)
            | BitwiseXor (_, b)
            | BitwiseXnor (_, b)
            | BitwiseAnd (_, b)
            | Equality (_, _, b)
            | Comparison (_, _, b)
            | ShiftExpr (_, _, b)
            | Additive (_, _, b)
            | Multiplicative (_, _, b) -> getExprBits b
            | ConditionalOp (_, t, f) -> Set.union (getExprBits t) (getExprBits f)
            | _ -> Set.empty
        let unaryBits =
            match expr with
            | ExpressionDU.Unary u
            | Negation u ->
                match u with
                | UnaryDU.Parenthesis e -> getExprBits e
                | UnaryDU.Number _ -> Set.empty
                | UnaryDU.Concat elems ->
                    elems
                    |> Array.map getExprBits
                    |> Array.fold Set.union Set.empty
                | UnaryDU.Primary primary ->
                    let primaryBits = getPrimaryBits portSizeMap primary |> Set.ofList
                    let indexBits =
                        match primary with
                        | IdentifierBit (_, idx) -> Set.empty
                        | VariableBitSelect (_, idx) -> getExprBits idx
                        | IdentifierBits (_, start, end_) -> Set.empty
                            // let startBits = getExprBits start
                            // let endBits = getExprBits end_
                            // Set.union startBits endBits
                        | IdentifierBitsSelect (_, start, _, _) -> getExprBits start
                        | IdentifierArray (_, indices) ->
                            indices
                            |> Array.map getExprBits
                            |> Array.fold Set.union Set.empty
                        | Identifier _
                        | IdentifierArray _ -> Set.empty
                    Set.union primaryBits indexBits
            | Reduction (_, e) -> getExprBits e
            | UnaryUnsigned _ -> Set.empty
            | _ -> Set.empty
        (Set.empty, [leftBits; rightBits; unaryBits])
        ||> List.fold Set.union

    getExprBits expression

let getLHSWidth (assign:Assignment) (varSizeMap: Map<string, int>)  =
    match assign.LHS.VariableBitSelect with
    | Some _ -> 1
    | None ->
        match assign.LHS.PrimaryType with
        | Identifier id
        | IdentifierArray (id, _) ->
            Map.tryFind id.Name varSizeMap |> Option.defaultValue 0
        | IdentifierBit _ ->
            1
        | VariableBitSelect _ ->
            1
        | IdentifierBits (id, bStart, bEnd) ->
            // let bStart = evalExpr start
            // let bEnd = evalExpr end_
            bStart - bEnd + 1
        | IdentifierBitsSelect (_, _, width, _) ->
            width
    // match assign.LHS.BitsStart, assign.LHS.BitsEnd, assign.LHS.VariableBitSelect, assign.LHS.Width with
    // | Some s, Some e, _, _ -> (int s)-(int e)+1
    // | None, None, None, _ -> 
    //     match Map.tryFind assign.LHS.Primary.Name varSizeMap with
    //     | Some size -> size
    //     | _ -> 0 //failwithf "What? Variable doesn't have a size" // if the variable is not declared there should be different logic
    // | None, None, Some _, Some w -> w
    // | _ -> failwithf "Only one of bitsStart and bitsEnd present"


let getCondAndCaseExpressions (expressions: (ExpressionDU*int) list) (node: ASTNode) = 
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
let getModuleInstantiationOutputPrimaries (modInst:ModuleInstantiation) (project:Project) =
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
let getModuleInstantiationInputPrimaries (modInst:ModuleInstantiation) (project:Project) =
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

/// Helper functions to estimate size of memory to set limits
/// Counts assignment, expression nodes, and primaries, as well as indices and ranges
let rec estimateExprCost (expr: ExpressionDU) : int =
    match expr with
    | LogicalOr (a, b)
    | LogicalAnd (a, b)
    | BitwiseOr (a, b)
    | BitwiseXor (a, b)
    | BitwiseXnor (a, b)
    | BitwiseAnd (a, b)
    | Equality (_, a, b)
    | Comparison (_, a, b)
    | ShiftExpr (_, a, b)
    | Additive (_, a, b)
    | Multiplicative (_, a, b) ->
        1 + estimateExprCost a + estimateExprCost b
    | Reduction (_, e) ->
        1 + estimateExprCost e
    | Negation u ->
        1 + estimateUnaryCost u
    | ExpressionDU.Unary u ->
        1 + estimateUnaryCost u
    | UnaryUnsigned _ ->
        1
    | ConditionalOp (c, t, f) ->
        1 + estimateExprCost c + estimateExprCost t + estimateExprCost f
and estimateUnaryCost (u: UnaryDU) : int =
    match u with
    | UnaryDU.Primary p ->
        1 + estimatePrimaryCost p
    | UnaryDU.Number _ ->
        1
    | UnaryDU.Parenthesis e ->
        1 + estimateExprCost e
    | UnaryDU.Concat elems ->
        1 + (elems |> Array.toList |> List.map estimateExprCost |> List.sum)
and estimatePrimaryCost (p: PrimaryDU) : int =
    match p with
    | Identifier _ ->
        1
    | IdentifierBit (_, idx) ->
        2
    | VariableBitSelect (_, idx) ->
        1 + estimateExprCost idx
    | IdentifierBits (_, bStart, bEnd) ->
        2
    | IdentifierBitsSelect (_, start, _, _) ->
        2 + estimateExprCost start
    | IdentifierArray (_, indices) ->
        2 + (indices |> Array.toList |> List.map estimateExprCost |> List.sum)
let estimateAssignmentCost (a: Assignment) : int =
    1 + estimatePrimaryCost a.LHS.PrimaryType + estimateExprCost a.RHS
let rec estimateStatementCost (stmt: StatementDU) : int =
    match stmt with
    | BlockingAssign a ->
        estimateAssignmentCost a
    | NonBlockingAssign a ->
        estimateAssignmentCost a
    | SeqBlock (stmts, _) ->
        stmts |> Array.toList |> List.map estimateStatementCost |> List.sum
    | StatementDU.Case c ->
        let casesCost =
            c.CaseItems
            |> Array.toList
            |> List.map (fun item -> estimateStatementCost item.Statement)
            |> List.sum
        let defaultCost =
            c.Default
            |> Option.map estimateStatementCost
            |> Option.defaultValue 0
        1 + estimateExprCost c.Expression + casesCost + defaultCost
    | Conditional (ifStmt, elseStmt) ->
        let ifCost = estimateStatementCost ifStmt.Statement
        let elseCost = elseStmt |> Option.map estimateStatementCost |> Option.defaultValue 0
        1 + estimateExprCost ifStmt.Condition + ifCost + elseCost
    | StatementDU.ForStatement f ->
        estimateStatementCost f.Statement

