module ErrorCheck

open VerilogTypes
open Fable.Core.JsInterop




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
    printfn "Declared Ports: %A" decls
    let diff = Seq.except (decls |> List.toSeq) (portList |> List.toSeq)
    if Seq.isEmpty diff then 
        printfn "All ports are declared either as input or output"
    else 
        printfn"Undeclared ports: %A" diff
    
    
    match Seq.isEmpty diff with
    | false -> List.append errorList [sprintf "Undeclared ports: %A" diff]
    | true -> errorList

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


let parameterNameCheck ast portMap errorList = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let names = 
        items |> List.collect (fun x -> 
            match (x.ParamDecl |> isNullOrUndefined) with
            | false -> 
                match x.ParamDecl with
                | Some d -> 
                    [d.Parameter.Name]
                | None -> []
            | true -> []
        )
    let localErrors = 
        List.collect (fun name -> 
            match Map.tryFind name portMap with
            | Some found -> [sprintf "Variable %s cannot be used as a parameter name because it is declared as a port" name]
            | None -> []
        ) names
    List.append errorList localErrors
    
let wireNameCheck ast portMap errorList = 
    let items = ast.Module.ModuleItems.ItemList |> Array.toList
    let names = 
        items |> List.collect (fun x -> 
            match (x.Statement |> isNullOrUndefined) with
            | false -> 
                match x.Statement with
                | Some statement when statement.StatementType = "wire" -> [statement.Assignment.LHS.Primary]
                | _ -> []
            | true -> []
        )
    let localErrors = 
        List.collect (fun name -> 
            match Map.tryFind name portMap with
            | Some found -> [sprintf "Variable %s cannot be used as a wire name because it is declared as a port" name]
            | None -> []
        ) names
    List.append errorList localErrors

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


let checkAllOutputsAssigned ast portMap portSizeMap errorList =
    
    // List of declared ports, bit by bit
    let outputPortList = 
        portMap 
        |> Map.filter (fun n s -> s = "output") 
        |> Map.toList 
        |> List.map fst
        |> List.collect (fun x -> 
            let size = Map.find x portSizeMap
            let names = [0..size-1] |> List.map (fun y -> x+(string y))
            names 
        )


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
            | a -> (a.Primary,(int a.BitsStart),(int a.BitsEnd))
        )
    
    
    // List of assigned ports, bit by bit
    let assignmentPortList =
        assignments
        |> List.collect ( fun x ->
            match x with
            |(name,-1,-1)->
                let size = Map.find name portSizeMap
                let names = [0..size-1] |> List.map (fun y -> name+(string y))
                names
            |(name,x,y) when x=y ->
                [name+(string x)]
            |(name,bStart,bEnd)->
                let names = [bEnd..bStart] |> List.map (fun y -> name+(string y))
                names
        )


    let diff = List.except (List.toSeq assignmentPortList) outputPortList
    let localErrors =
        match List.isEmpty diff with
        |true -> []
        |false -> 
            // transform names from "b2" to "b[2]" 
            let unassignedPorts = diff |> List.map(fun x -> (string x[0])+"["+(string x[1])+"]")
            [sprintf "The following ports are unassigned: %A" unassignedPorts]
    List.append errorList localErrors

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





let getErrors ast model=
    printfn "Parsed input: %A" ast
    let portMap  = getPortMap ast
    let portSizeMap = getPortSizeMap ast
    printfn "Port size map: %A" portSizeMap
    []
    |> nameCheck ast
    |> portCheck ast portMap
    |> parameterNameCheck ast portMap
    |> wireNameCheck ast portMap
    |> assignmentNameCheck ast portMap
    |> checkAllOutputsAssigned ast portMap portSizeMap