(*
    FilesIO.fs

    Utility functions to interact with files.
*)

module FilesIO

open Helpers
open DiagramTypes

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Fable.SimpleJson
open Node.Exports

let private stateToJsonString (state : CanvasState) : string =
    Json.stringify state

let private jsonStringToState (jsonString : string) =
    Json.tryParseAs<CanvasState> jsonString

let private tryLoadStateFromPath filePath =
    let dataBuffer = fs.readFileSync (filePath, Option.None)
    dataBuffer.toString "utf8" |> jsonStringToState

/// Extract the labels of the inputs and outputs nodes.
let private parseDiagramSignature canvasState : string list * string list =
    let rec extractIO
            (components : Component list)
            (inputs : string list)
            (outputs : string list) =
        match components with
        | [] -> inputs, outputs
        | comp :: components' ->
            match comp.Type with
            | Input  -> extractIO components' (comp.Label :: inputs) outputs
            | Output -> extractIO components' inputs (comp.Label :: outputs)
            | _ -> extractIO components' inputs outputs
    let components, _ = canvasState
    extractIO components [] []

let private getBaseNameNoExtension filePath =
    let baseName = path.basename filePath
    match baseName.Split '.' |> Seq.toList with
    | [] -> failwithf "what? split at . in a filename should never return empty list"
    | [baseName] -> baseName // No dots found.
    | firstSplit :: splits ->
        // Quite ugly but works.
        let rest =
            ("", [0..splits.Length - 2]) ||> List.fold (fun baseName i ->
                baseName + "." + splits.[i]
            )
        firstSplit + rest

let private projectFilters =
    ResizeArray [
        createObj [
            "name" ==> "Diagrams project" // TODO rename this.
            "extensions" ==> ResizeArray [ "dprj" ]
        ]
    ] |> Some

/// Ask the user to choose a project path, with a dialog window.
/// Return None if the user exits withouth selecting a path.
let askForExistingProjectPath () : string option =
    let options = createEmpty<OpenDialogOptions>
    options.properties <- ResizeArray([ "openDirectory" ]) |> Some
    options.filters <- projectFilters
    let paths = electron.remote.dialog.showOpenDialog(options)
    match isNull paths with
    | true -> Option.None // User did not completed the load dialog interaction.
    | false -> match Seq.toList paths with
               | [] -> Option.None
               | path :: _ -> Some path

/// Ask the user a new project path, with a dialog window.
/// Return None if the user exits withouth selecting a path.
let askForNewProjectPath () : string option =
    let options = createEmpty<SaveDialogOptions>
    options.filters <- projectFilters
    let path = electron.remote.dialog.showSaveDialog(options)
    match isNull path with
    | true -> Option.None // User did not completed the load dialog interaction.
    | false -> Option.Some path

let tryCreateFolder (path : string) =
    try
        Result.Ok <| fs.mkdirSync path
    with
        | ex -> Result.Error <| sprintf "%A" ex

let pathJoin args = path.join args

/// Asyncronously remove file.
let removeFile folderPath baseName =
    let path = path.join [| folderPath; baseName + ".dgm" |]
    fs.unlink (U2.Case1 path, ignore) // Asynchronous.

/// Save state to file. Automatically add the .dgm suffix.
let saveStateToFile folderPath baseName state = // TODO: catch error?
    let path = path.join [| folderPath; baseName + ".dgm" |]
    let data = stateToJsonString state
    let options = createObj ["encoding" ==> "utf8"] |> Some
    fs.writeFileSync(path, data, options)

/// Create new empty diagram file. Automatically add the .dgm suffix.
let createEmptyDgmFile folderPath baseName =
    saveStateToFile folderPath baseName ([],[])

let private tryLoadComponentFromPath filePath =
    match tryLoadStateFromPath filePath with
    | Result.Error err -> Result.Error err
    | Result.Ok state ->
        let inputs, outputs = parseDiagramSignature state
        Result.Ok {
            Name = getBaseNameNoExtension filePath
            FilePath = filePath
            CanvasState = state
            InputLabels = inputs
            OutputLabels = outputs
        }

/// Try to load all diagram components from a file path.
/// Return a string with error if not possible.
let tryLoadComponentsFromPath folderPath : Result<LoadedComponent list, string> =
    fs.readdirSync (U2.Case1 folderPath)
    |> Seq.toList
    |> List.filter (path.extname >> ((=) ".dgm"))
    |> List.map (fun fileName ->
            path.join [| folderPath; fileName |] |> tryLoadComponentFromPath
        )
    |> tryFindError
