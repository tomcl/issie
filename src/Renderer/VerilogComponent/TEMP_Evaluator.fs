module Evaluator

open VerilogTypes
open ErrorCheck
open System
open Fable.Core.JsInterop

let private uint32ToBool num =
    if num = 0u then false
    else true

/// Convert an hex string into a binary string.
let private hexToBin (hStr : string) : string =
    let rec convert h =
        match h with
        | [] -> ""
        | c :: h' ->
            let digit =
                match c with
                | '0' -> "0000" | '1' -> "0001" | '2' -> "0010" | '3' -> "0011"
                | '4' -> "0100" | '5' -> "0101" | '6' -> "0110" | '7' -> "0111"
                | '8' -> "1000" | '9' -> "1001" | 'a' -> "1010" | 'b' -> "1011"
                | 'c' -> "1100" | 'd' -> "1101" | 'e' -> "1110" | 'f' -> "1111"
                | c -> failwithf "Invalid char %c while converting hex %s to binary" c hStr
            digit + (convert h')
    let chars = hStr.ToLower() |> Seq.toList
    match chars with
    | [] -> ""
    | c :: chars' ->
        let firstDigit = // Avoid leading zeros.
            match c with
            | '0' -> "0" | '1' -> "1" | '2' -> "10" | '3' -> "11"
            | '4' -> "100" | '5' -> "101" | '6' -> "110" | '7' -> "111"
            | '8' -> "1000" | '9' -> "1001" | 'a' -> "1010" | 'b' -> "1011"
            | 'c' -> "1100" | 'd' -> "1101" | 'e' -> "1110" | 'f' -> "1111"
            | c -> failwithf "Invalid char %c while converting hex %s to binary" c hStr
        firstDigit + (convert chars')



let getOutputPortMap portMap portSizeMap : Map<string,bool option> = 
    portMap 
    |> Map.filter (fun n s -> s = "output") 
    |> Map.toList 
    |> List.map fst
    |> List.collect (fun x -> 
        let size = Map.find x portSizeMap
        let names = [0..size-1] |> List.map (fun y -> (x+(string y),None))
        names 
    )
    |> Map.ofList


let convertToUInt32 (number:NumberT) = 
    
    
    match number.NumberType with
    |"all" ->
        let bits = int <| Option.get number.Bits
        let bin = 
            match number.Base with
            |Some "'h" -> (Option.get number.AllNumber) |> hexToBin
            |Some "'b" -> Option.get number.AllNumber
            |_ -> failwithf "Not Possible!"
        let length = String.length bin  
        let cutBin = 
            if bits > length
                then bin
            else ("",[length-bits..length-1]) ||> List.fold (fun s v -> s+string bin[v])
        printfn "cut bin %s" cutBin
        ((cutBin, 2) |> Convert.ToUInt32 )
        
    |_ -> (Option.get number.UnsignedNumber) |> Convert.ToUInt32 




let rec expressionEvaluator (tree:ExpressionT) inputAndWireValuesMap :uint32 =
    match tree.Type with
    |"unary" -> unaryEvaluator (Option.get tree.Unary) inputAndWireValuesMap
    |"negation" -> ~~~ (unaryEvaluator (Option.get tree.Unary) inputAndWireValuesMap) 
    |"reduction" -> 
        match Option.get tree.Operator with
        |"!" ->  Convert.ToUInt32 (not(uint32ToBool (unaryEvaluator (Option.get tree.Unary) inputAndWireValuesMap)))
        |_ -> 0u
    |_ ->
        let headResult = expressionEvaluator (Option.get tree.Head) inputAndWireValuesMap
        let tailResult = expressionEvaluator (Option.get tree.Tail) inputAndWireValuesMap
        
        match Option.get tree.Operator with
        |"+" 
            -> headResult + tailResult
        |"-" 
            -> headResult - tailResult
        |"<<" 
            -> (headResult <<< (Convert.ToInt32 tailResult))
        |">>"
            -> (headResult >>> (Convert.ToInt32 tailResult))
        |"&" 
            -> headResult &&& tailResult
        |"^" 
            -> headResult ^^^ tailResult
        |"^~" 
            -> ~~~ (headResult ^^^ tailResult)
        |"|" 
            -> headResult ||| tailResult
        |"&&" 
            -> (Convert.ToUInt32 (uint32ToBool headResult && uint32ToBool tailResult))
        |"||" 
            -> (Convert.ToUInt32 (uint32ToBool headResult || uint32ToBool tailResult))
        | _     
            -> failwithf "Not Possible!"

and unaryEvaluator (tree:UnaryT) inputAndWireValuesMap : uint32 = 
    match tree.Type with
    |"primary" -> 0u 
        // match isNullOrUndefined (Option.get tree.Primary).BitsStart with
        // |
    |"number" -> convertToUInt32 (Option.get tree.Number)
    |"parenthesis" -> expressionEvaluator (Option.get tree.Expression) inputAndWireValuesMap
    |"concat" -> concatExpressionEvaluator (Option.get tree.Expression) inputAndWireValuesMap
    |_ -> 0u
and concatExpressionEvaluator (tree:ExpressionT) inputAndWireValuesMap : uint32 =
    0u


let getWireAssignments ast =
    ast.Module.ModuleItems.ItemList 
    |> Array.toList 
    |> List.filter (fun item -> item.ItemType = "statement")
    |> List.map (fun item -> (Option.get item.Statement))
    |> List.filter (fun (statement) -> statement.StatementType = "wire")
    |> List.map (fun (statement) -> statement.Assignment)

let getOutputAssignments ast =
    ast.Module.ModuleItems.ItemList 
    |> Array.toList 
    |> List.filter (fun item -> item.ItemType = "statement")
    |> List.map (fun item -> (Option.get item.Statement))
    |> List.filter (fun (statement) -> statement.StatementType = "assign")
    |> List.map (fun (statement) -> statement.Assignment)

let updateMap (prevMap: Map<string,bool>) portSizeMap (wire:AssignmentT) result =
    let bStart,bEnd =
        match isNullOrUndefined wire.LHS.BitsStart with
        |true -> 
            match Map.tryFind wire.LHS.Primary.Name portSizeMap with
            |Some size -> (size-1,0)
            |None -> (0,0)
        |false -> (int (Option.get wire.LHS.BitsStart),int (Option.get wire.LHS.BitsEnd))
    let name = wire.LHS.Primary.Name
    (prevMap,[bEnd..bStart])||> List.fold (fun map bit ->
        let key = name+string bit
        let value = 
            match ((result>>>(bit-bEnd)) &&& 1u) with
            |0u -> false
            |1u -> true
            |_ -> failwithf "Not possible!"
        Map.add key value map
    )
    

let updateOutputMap (prevMap: Map<string,bool option>) portSizeMap (assignment:AssignmentT) result =
    let bStart,bEnd =
        match isNullOrUndefined assignment.LHS.BitsStart with
        |true -> 
            match Map.tryFind assignment.LHS.Primary.Name portSizeMap with
            |Some size -> (size-1,0)
            |None -> (0,0)
        |false -> (int (Option.get assignment.LHS.BitsStart),int (Option.get assignment.LHS.BitsEnd))
    let name = assignment.LHS.Primary.Name
    (prevMap,[bEnd..bStart])||> List.fold (fun map bit ->
        let key = name+string bit
        let value = 
            match ((result>>>(bit-bEnd)) &&& 1u) with
            |0u -> Some false
            |1u -> Some true
            |_ -> None
        Map.add key value map
    )

let evaluator ast =
    let portMap = getPortMap ast
    let (portSizeMap,_) = getPortSizeAndLocationMap ast

    let wires = getWireAssignments ast

    let inputValuesMap = Map.empty<string,bool>

    

    let inputAndWireValuesMap =
        (inputValuesMap,wires)
        ||> List.fold (fun map wire -> 
            let result = expressionEvaluator wire.RHS map
            updateMap map portSizeMap wire result
        )
        // Map.empty<string,bool option>
    
    let assignments = getOutputAssignments ast


    let outputEmptyMap = getOutputPortMap portMap portSizeMap
    
    let outputValuesMap =
        (outputEmptyMap,assignments)
        ||> List.fold (fun map assignment -> 
            let result = expressionEvaluator assignment.RHS inputAndWireValuesMap
            updateOutputMap map portSizeMap assignment result
        )

    
    outputValuesMap
    // Map.empty<string,bool option>