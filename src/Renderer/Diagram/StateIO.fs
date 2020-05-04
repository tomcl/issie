module StateIO

open Helpers
open DiagramTypes

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Electron
open Fable.SimpleJson
open Node.Exports

// TODO: Json.parseAs may fail, if the data is not structured properly.
//       Need to use tryParseAs.

let private stateToJsonString (state : CanvasState) : string =
    Json.stringify state

let private jsonStringToState (jsonString : string) =
    Json.tryParseAs<CanvasState> jsonString

let private fileFilterOpts =
    ResizeArray [
        createObj [
            "name" ==> "Diagram"
            "extensions" ==> ResizeArray [ "dgm" ]
        ]
    ] |> Some

let private tryLoadStateFromPath filePath =
    let dataBuffer = fs.readFileSync (filePath, Option.None)
    dataBuffer.toString "utf8" |> jsonStringToState

let private writeStateToPath filePath state =
    let stateStr = stateToJsonString state
    fs.writeFile (filePath, stateStr, ignore) // Asynchronous.

/// Extract the labels of the inputs and outputs nodes.
/// TODO: should this also extract the port ids?
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

(*
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

let private getFileExtension (filePath : string) : string =
    match filePath.Split '.' |> Seq.toList with
    | [] -> failwithf "what? split at . in a filename should never return empty list"
    | [_] -> "" // No dots found.
    | splits -> List.last splits

/// All that is before the extension.
let private getFileName (Path filePath) : string =
    match filePath.Split '/' |> Seq.toList with
    | [] -> failwithf "what? split at / in a filename should never return empty list"
    | splits ->
        let fileWithoutPath = List.last splits
        match fileWithoutPath.Split '.' |> Seq.toList with
        | [] -> failwithf "what? split at . in a filename should never return empty list"
        | [baseName] -> baseName // No dots found.
        | splits -> ("", [0..splits.Length - 2]) ||> List.fold (fun baseName i ->
            baseName + splits.[i]
        )

let parseAllDiagramsInFolder (folderPath : string) =
    let loadComponent fileName =
        let state = loadStateFromPath <| folderPath + fileName
        let inputs, outputs = parseDiagramSignature state
        {
            Name = getFileBaseName fileName
            InputLabels = inputs
            OutputLabels = outputs
            FilePath = "" // folderPath + fileName
            CanvasState = state
        }
    fs.readdirSync (U2.Case1 folderPath)
    |> Seq.toList
    |> List.filter (getFileExtension >> ((=) "dgm"))
    |> List.map loadComponent
*)

/////////////

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

/// Ask the user to choose a project path, with a dialog window.
/// Return None if the user exits withouth selecting a path.
let askForExistingProjectPath () : string option =
    let filters =
        ResizeArray [
            createObj [
                "name" ==> "Diagrams project" // TODO rename this.
                "extensions" ==> ResizeArray [ "dprj" ]
            ]
        ] |> Some
    let options = createEmpty<OpenDialogOptions>
    options.properties <- ResizeArray([ "openDirectory" ]) |> Some
    options.filters <- filters
    let paths = electron.remote.dialog.showOpenDialog(options)
    match isNull paths with
    | true -> Option.None // User did not completed the load dialog interaction.
    | false -> match Seq.toList paths with
               | [] -> Option.None
               | path :: _ -> Some path

let private tryLoadComponentFromPath filePath =
    JSHelpers.log <| getBaseNameNoExtension filePath
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
