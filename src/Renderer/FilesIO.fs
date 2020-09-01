(*
    FilesIO.fs

    Utility functions to interact with files.
*)

module FilesIO

open Helpers
open CommonTypes
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Electron
open Node

[<AutoOpen>]
module JsonHelpers =
    open Fable.SimpleJson


    type SavedInfo =
        | CanvasOnly of CanvasState
        | CanvasWithFileWaveInfo of CanvasState * SimulatorTypes.SavedWaveInfo option * System.DateTime

        member self.getCanvas = 
            match self with
            | CanvasOnly c -> c 
            | CanvasWithFileWaveInfo (c,_,_) -> c

        member self.getTimeStamp = 
            match self with
            | CanvasOnly _ -> System.DateTime.MinValue 
            | CanvasWithFileWaveInfo (_,_,ts) -> ts
        member self.getWaveInfo =
            match self with
            | CanvasOnly _ -> None 
            | CanvasWithFileWaveInfo (_,waveInfo,_) -> waveInfo


    let stateToJsonString (cState: CanvasState, waveInfo: SimulatorTypes.SavedWaveInfo option) : string =
        let time = System.DateTime.Now
        SimpleJson.stringify (CanvasWithFileWaveInfo (cState, waveInfo, time))

    let jsonStringToState (jsonString : string) =
         Json.tryParseAs<CanvasState> jsonString
         |> (function
                | Ok state -> Some (CanvasOnly state)
                | Error _ ->
                    match Json.tryParseAs<SavedInfo> jsonString with
                    | Ok state -> Some state
                    | Error _ -> None)


let private fileExistsWithExtn extn folderPath baseName =
    let path = path.join [| folderPath; baseName + extn |]
    fs.existsSync (U2.Case1 path)

let private tryLoadStateFromPath (filePath: string) =
    if not (fs.existsSync (U2.Case1 filePath)) then
        None
    else
        fs.readFileSync(filePath, "utf8")
        |> jsonStringToState


let pathJoin args = path.join args
let baseName filePath = path.basename filePath
let dirName filePath = path.dirname filePath



/// Extract the labels and bus widths of the inputs and outputs nodes.
let private parseDiagramSignature canvasState
        : (string * int) list * (string * int) list =
    let rec extractIO
            (components : Component list)
            (inputs : (string * int) list)
            (outputs : (string * int) list) =
        match components with
        | [] -> inputs, outputs
        | comp :: components' ->
            match comp.Type with
            | Input width  -> extractIO components' ((comp.Label, width) :: inputs) outputs
            | Output width -> extractIO components' inputs ((comp.Label, width) :: outputs)
            | _ -> extractIO components' inputs outputs
    let components, _ = canvasState
    let inputs, outputs = extractIO components [] []
    List.rev inputs, List.rev outputs

let private getBaseNameNoExtension filePath =
    let name = baseName filePath
    match name.Split '.' |> Seq.toList with
    | [] -> failwithf "what? split at . in a filename should never return empty list"
    | [name] -> name // No dots found.
    | firstSplit :: splits ->
        // Quite ugly but works.
        let rest =
            ("", [0..splits.Length - 2]) ||> List.fold (fun baseName i ->
                name + "." + splits.[i]
            )
        firstSplit + rest

let private projectFileFilters =
    createObj !![
        "name" ==> "ISSIE project file"
        "extensions" ==> ResizeArray [ "dprj" ]
    ] 
    |> unbox<FileFilter> 
    |> Array.singleton

let private projectFilters =
    createObj !![ 
        "name" ==> "ISSIE project"   
        "extensions" ==> ResizeArray [ "" ]
    ]
    |> unbox<FileFilter>
    |> Array.singleton

/// Ask the user to choose a project file, with a dialog window.
/// Return the folder containing the chosen project file.
/// Return None if the user exits withouth selecting a path.
let askForExistingProjectPath () : string option =
    let options = createEmpty<OpenDialogOptions>
    options.filters <- projectFileFilters

    electron.remote.dialog.showOpenDialogSync(options)
    |> Option.bind (
        Seq.toList
        >> function
        | [] -> Option.None
        | p :: _ -> Some <| path.dirname p
    )



/// Ask the user a new project path, with a dialog window.
/// Return None if the user exits withouth selecting a path.
let rec askForNewProjectPath () : string option =
    let options = createEmpty<SaveDialogOptions>
    options.filters <- projectFilters
    options.title <- "Enter new project directory and name"
    options.nameFieldLabel <- "New project name"
    options.buttonLabel <- "Create Project"
    options.properties <- [|
        SaveDialogFeature.CreateDirectory
        SaveDialogFeature.ShowOverwriteConfirmation
        |]
    electron.remote.dialog.showSaveDialogSync options
    |> Option.bind (fun dPath ->
        let dir = dirName dPath
        let files = fs.readdirSync <| U2.Case1 dir
        if Seq.exists (fun (fn:string) -> fn.EndsWith ".dprj") files
        then
            electron.remote.dialog.showErrorBox(
                "Invalid project directory",
                "You are trying to craete a new Issie project inside an existing project directory. \
                 This is not allowed, please choose a different directory")
            askForNewProjectPath()
            
        else
            Some dPath)


    
let tryCreateFolder (path : string) =
    if Seq.exists (fun (ch:char) -> (not (System.Char.IsLetterOrDigit ch))) (baseName path) then 
        Result.Error <| "'%s' file or project names nust contain only letters or digits"
    else
        try
            Result.Ok <| fs.mkdirSync path
        with
            | ex -> Result.Error <| sprintf "%A" ex


/// Asyncronously remove file.
let removeFileWithExtn extn folderPath baseName  =
    let path = path.join [| folderPath; baseName + extn |]
    fs.unlink (U2.Case1 path, ignore) // Asynchronous.

let removeFile (folderPath:string) (baseName:string) = removeFileWithExtn ".dgm" folderPath baseName

let removeAutoFile folderPath baseName =
    let path = path.join [| folderPath; baseName + ".dgmauto" |]
    fs.unlink (U2.Case1 path, ignore) // Asynchronous.



/// Write base64 encoded data to file.
/// Create file if it does not exist.
let writeFileBase64 path data =
    let options = createObj ["encoding" ==> "base64"] |> Some
    fs.writeFileSync(path, data, options)

/// Write utf8 encoded data to file.
/// Create file if it does not exist.
let writeFile path data =
    let options = createObj ["encoding" ==> "utf8"] |> Some
    fs.writeFileSync(path, data, options)

/// Save a PNG file (encoded base64, as from draw2d)
/// Overwrite existing file if needed
let savePngFile folderPath baseName png = // TODO: catch error?
    let path = pathJoin [| folderPath; baseName + ".png" |]
    writeFileBase64 path png

let formatSavedState (canvas,wave) =
    CanvasWithFileWaveInfo(canvas,wave,System.DateTime.Now)
/// Save state to file. Automatically add the .dgm suffix.
let saveAutoStateToFile folderPath baseName state = // TODO: catch error?
    let path = pathJoin [| folderPath; baseName + ".dgmauto" |]
    let data = stateToJsonString state
    writeFile path data

/// Save state to autosave file. Automatically add the .dgauto suffix.
let saveStateToFile folderPath baseName state = // TODO: catch error?
    let path = pathJoin [| folderPath; baseName + ".dgm" |]
    let data = stateToJsonString state
    writeFile path data

/// Create new empty diagram file. Automatically add the .dgm suffix.
let createEmptyDgmFile folderPath baseName =
    saveStateToFile folderPath baseName (([],[]), None)

let private tryLoadComponentFromPath filePath : Result<SimulatorTypes.LoadedComponent, string> =
    match tryLoadStateFromPath filePath with
    | None -> Result.Error <| sprintf "Can't load component from '%s'" filePath
    | Some state ->
        let inputs, outputs = parseDiagramSignature state.getCanvas
        Result.Ok {
            Name = getBaseNameNoExtension filePath
            TimeStamp = state.getTimeStamp
            WaveInfo = state.getWaveInfo
            FilePath = filePath
            CanvasState = state.getCanvas
            InputLabels = inputs
            OutputLabels = outputs
        }

type LoadStatus =
    | Resolve of SimulatorTypes.LoadedComponent * SimulatorTypes.LoadedComponent
    | OkComp of SimulatorTypes.LoadedComponent
    | OkAuto of SimulatorTypes.LoadedComponent

    
/// load all files in folderpath. Return Ok list of LoadStatus or a single Error.
let loadAllComponentFiles (folderPath:string) = 
    fs.readdirSync (U2.Case1 folderPath)
    |> Seq.toList
    |> List.filter (path.extname >> ((=) ".dgm"))
    |> List.map (fun fileName ->
            let filePath = path.join [| folderPath; fileName |]
            let ldComp =  filePath |> tryLoadComponentFromPath
            let autoComp = filePath + "auto" |> tryLoadComponentFromPath
            match (ldComp, autoComp) with
            | Ok ldComp, Ok autoComp when ldComp.TimeStamp < autoComp.TimeStamp ->
                Resolve(ldComp,autoComp) |> Ok
            | Ok ldComp, _ -> 
                OkComp ldComp |> Ok
            | Error _, Ok autoComp ->
                OkAuto autoComp |> Ok
            | Error msg, _ -> Error msg
        )
    |> tryFindError
