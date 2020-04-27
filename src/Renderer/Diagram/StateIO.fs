module StateIO

open DiagramTypes

open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Fable.SimpleJson
open Node.Exports

// TODO: Json.parseAs may fail, if the data is not structured properly.
//       Need to use tryParseAs.

let private stateToJsonString (state : CanvasState) : string =
    Json.stringify state

let private jsonStringToState (jsonString : string) : CanvasState =
    Json.parseAs<CanvasState> jsonString

let private fileFilterOpts =
    ResizeArray [
        createObj [
            "name" ==> "Diagram"
            "extensions" ==> ResizeArray [ "dgm" ]
        ]
    ] |> Some

let private loadStateFromPath filePath =
    let dataBuffer = fs.readFileSync (filePath, Option.None)
    dataBuffer.toString "utf8" |> jsonStringToState

let private writeStateToPath filePath state =
    let stateStr = stateToJsonString state
    fs.writeFile (filePath, stateStr, ignore) // Asynchronous.

/// Extract the labels of the inputs and outputs nodes.
/// TODO: should this also extract the port ids?
//let private parseDiagramSignature canvasState : string list * string list =
//    let rec extractIO components inputs outputs =
//        match components with
//        | [] -> inputs, outputs
//        | comp :: components' ->
//            match comp.Type with
//            | Input  -> extractIO components' (comp.Label :: inputs) outputs
//            | Output -> extractIO components' inputs (comp.Label :: outputs)
//            | _ -> extractIO components' inputs outputs
//    let components, _ = canvasState
//    extractIO components [] []

/// Save the state to a file and return the path where it was saved.
/// TODO: handle errors?
let saveStateToFile maybePath state =
    let path =
        match maybePath with
        | Some path -> path
        | Option.None -> // Open dialog window to obtain the path.
            let options = createEmpty<SaveDialogOptions>
            options.filters <- fileFilterOpts
            electron.remote.dialog.showSaveDialog(options)
    match isNull path with
    | true -> Option.None // User did not completed the save dialog interaction.
    | false -> writeStateToPath path state
               Some path

/// Load the state from a file and return it toghether with the path from where
/// it is loaded.
/// TODO: handle errors?
let loadStateFromFile () =
    let options = createEmpty<OpenDialogOptions>
    options.properties <- ResizeArray([ "openFile" ]) |> Some
    options.filters <- fileFilterOpts
    let paths = electron.remote.dialog.showOpenDialog(options)
    match isNull paths with
    | true -> Option.None // User did not completed the load dialog interaction.
    | false -> match Seq.toList paths with
               | [path] -> Some (path, loadStateFromPath path)
               | _ -> Option.None

//let parseAllDiagramsInFolder folderPath =
//    fs.
