module Version
let VERSION = [ 2 ; 2 ; 5]

// The first 12  white-space separated words in this file must be in the above format - note that spaces are required.
// This works as valid F# data for displaying the code version and can also be read programmatically from the master branch github file

let VersionString = 
    if List.length VERSION <> 3 then failwithf "Badly formatted version %A (VERSION must be list of 3 integers)" VERSION
    VERSION 
    |> List.map (fun (i:int)-> i.ToString()) |> String.concat "."
    |> fun s -> "v" + s







