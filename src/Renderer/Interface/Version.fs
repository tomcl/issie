module Version
let VERSION = [ "5" ; "2";  "1" ]

// The first 12  white-space separated words in this file must be in the above format - note that spaces are required.
// This works as valid F# data for displaying the code version and can also be read programmatically from the master branch github file

let VersionString = 
    if List.length VERSION < 3 then failwithf "Badly formatted version %A (VERSION must be list of 3 or more strings)" VERSION
    VERSION 
    |> List.map (fun (i:string)-> i.ToString()) |> String.concat "."
    |> fun s -> "v" + s

open CommonTypes

module TestDU =

    type ComponentType =
        | Input of BusWidth: int * DefaultValue: int option
        | Input1 of BusWidth: int
        | Output of BusWidth: int
        | Viewer of BusWidth: int
        | IOLabel
        | NotConnected

    type Component = {
            Id : string
            Type : ComponentType // This is JSONComponent.ComponentType!
        }

    let makeDummyComponents() =
        [1..2]
        |> List.map (fun n -> {Id= $"{n}"; Type = NotConnected})

    let testDUEquality() =
        let testCases = [NotConnected; Input(1,Some 1); Input(0,Some 0)]
        List.allPairs testCases testCases
        |> List.iter (fun (a,b) -> printfn $"a={a}, b={b}, a=b={a=b}")


        let comps = makeDummyComponents()
        printfn $"\nDummy components are {comps}\n"
        

        let checkCompsForId (id:string) =
            comps
            |> List.tryFind (fun comp ->
                comp.Id = id && comp.Type = NotConnected)
            |> function
                | Some _ -> true
                | None -> false
            |> fun res -> printfn $"checking: id={id} ->  Found = {res}"

        checkCompsForId "1"
        checkCompsForId "2"
        checkCompsForId "3"

