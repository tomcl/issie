(*
    FilesIO.fs

    Utility functions to interact with files.
*)

module FilesIO
open Fulma
open Fable.React.Props
open Helpers
open CommonTypes
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Electron
open Node
open EEExtensions





let pathJoin args = path.join args
let baseName filePath = path.basename filePath


let dirName filePath = path.dirname filePath
let ensureDirectory dPath =
    if (not <| fs.existsSync (U2.Case1 dPath)) then 
        fs.mkdirSync(dPath);
    


let pathWithoutExtension filePath =
    let ext = path.extname filePath
    filePath 
    |> Seq.rev
    |> Seq.skip ext.Length
    |> Seq.rev
    |> String.ofSeq

let baseNameWithoutExtension =
    pathWithoutExtension >> baseName

let fileNameIsBad name = 
    name 
    |> Seq.filter (fun ch -> not (ch = ' ' || System.Char.IsLetter ch || System.Char.IsDigit ch))
    |> Seq.isEmpty
    |> not

let filePathIsBad = 
    baseNameWithoutExtension >> fileNameIsBad

let fileExistsWithExtn extn folderPath baseName =
    let path = path.join [| folderPath; baseName + extn |]
    fs.existsSync (U2.Case1 path)

let readFilesFromDirectory (path:string) : string list =
    if fs.existsSync (U2.Case1 path) then
        fs.readdirSync(U2.Case1 path)
        |> Seq.toList
    else
        []

/// returns the sequence number and name of the most recent (highest sequence number) backup file
let latestBackupFileData (path:string) (baseName: string) =
    readFilesFromDirectory path
    |> List.filter (fun fn -> String.startsWith (baseName + "-") fn)
    |> List.map (fun fn -> 
            String.splitString [|"-"|] fn 
            |> Array.tryItem 1
            |> Option.bind (String.tryParseWith System.Int32.TryParse)
            |> fun n -> n,fn)
    |> List.sortDescending
    |> List.tryHead
    |> Option.bind (function 
        | None,_ -> None 
        | Some n, fn -> Some(n, fn))

/// read canvas state from file found on filePath (which includes .dgm suffix etc).
/// return Error if file does not exist or cannot be parsed.
let private tryLoadStateFromPath (filePath: string) =
    if not (fs.existsSync (U2.Case1 filePath)) then
        Error <| sprintf "Can't read file from %s because it does not seem to exist!" filePath      
    else
        fs.readFileSync(filePath, "utf8")
        |> jsonStringToState
        |> ( function
            | Error msg  -> Error <| sprintf "could not convert file '%s' to a valid issie design sheet. Details: %s" filePath msg
            | Ok res -> Ok res)




/// Extract the labels and bus widths of the inputs and outputs nodes.
let parseDiagramSignature canvasState
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

let getBaseNameNoExtension filePath =
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
    let w = renderer.remote.getCurrentWindow()
    electron.remote.dialog.showOpenDialogSync(w,options)
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
    options.title <- "Enter new ISSIE project directory and name"
    options.nameFieldLabel <- "New project name"
    options.buttonLabel <- "Create Project"
    options.properties <- [|
        SaveDialogFeature.CreateDirectory
        SaveDialogFeature.ShowOverwriteConfirmation
        |]
    match renderer.remote.getCurrentWindow() with
    | w ->
        electron.remote.dialog.showSaveDialogSync(options)
        |> Option.bind (fun dPath ->
            let dir = dirName dPath
            let files = fs.readdirSync <| U2.Case1 dir
            if Seq.exists (fun (fn:string) -> fn.EndsWith ".dprj") files
            then
                electron.remote.dialog.showErrorBox(
                    "Invalid project directory",
                    "You are trying to create a new Issie project inside an existing project directory. \
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
/// ignore if file does not exist
let removeFileWithExtn extn folderPath baseName  =
    let path = path.join [| folderPath; baseName + extn |]
    if fs.existsSync (U2.Case1 path) then
        fs.unlink (U2.Case1 path, ignore) // Asynchronous.
    else
        ()

let renameFile extn folderPath baseName newBaseName =
    let oldPath = path.join [| folderPath; baseName + extn |]
    let newPath = path.join [| folderPath; newBaseName + extn |]
    if fs.existsSync <| U2.Case1 oldPath then
        fs.renameSync (oldPath, newPath) // synchronous.

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

/// Save state to autosave file. Automatically add the .dgmauto suffix.
let saveAutoStateToFile folderPath baseName state = // TODO: catch error?
    let path = pathJoin [| folderPath; baseName + ".dgmauto" |]
    let data = stateToJsonString state
    writeFile path data

/// Save state to normal file. Automatically add the .dgm suffix.
let saveStateToFile folderPath baseName state = // TODO: catch error?
    let path = pathJoin [| folderPath; baseName + ".dgm" |]
    let data = stateToJsonString state
    writeFile path data

/// Create new empty diagram file. Automatically add the .dgm suffix.
let createEmptyDgmFile folderPath baseName =
    saveStateToFile folderPath baseName (([],[]), None)

let stripVertices (conn: Connection) =
    {conn with Vertices = []}

let magnifySheet magnification (comp: Component) =
    {comp with 
        X = int <| round (magnification * float (comp.X + comp.W / 2 )); 
        Y = int <| round (magnification * float (comp.Y + comp.H/2))}

/// Interface function that can read old-style circuits (without wire vertices)
/// as well as new circuits with vertices. Old circuits have an expansion parameter
/// since new symbols are larger (in units) than old ones.
let getLatestCanvas state =
    let oldCircuitMagnification = 2.0
    let stripConns canvas =
        let (comps,conns) = canvas
        let noVertexConns = List.map stripVertices conns
        let expandedComps = List.map (magnifySheet oldCircuitMagnification) comps
        expandedComps, noVertexConns
    match state  with
    | CanvasOnly canvas -> stripConns canvas
    | CanvasWithFileWaveInfo(canvas, _, _) -> stripConns canvas
    | CanvasWithFileWaveInfoAndNewConns(canvas, _, _) -> canvas


/// load a component from its canvas and other elements
let rec makeLoadedComponentFromCanvasData (canvas: CanvasState) filePath timeStamp waveInfo =
    
    let inputs, outputs = parseDiagramSignature canvas
    {
        Name = getBaseNameNoExtension filePath
        TimeStamp = timeStamp
        WaveInfo = waveInfo
        FilePath = filePath
        CanvasState = canvas
        InputLabels = inputs
        OutputLabels = outputs
    }


/// Make a loadedComponent from the file read from filePath.
/// Return the component, or an Error string.
let tryLoadComponentFromPath filePath : Result<LoadedComponent, string> =
    match tryLoadStateFromPath filePath with
    | Result.Error msg ->  Error <| sprintf "Can't load component %s because of Error: %s" (getBaseNameNoExtension filePath)  msg
    | Ok state ->
        makeLoadedComponentFromCanvasData (getLatestCanvas state) filePath state.getTimeStamp state.getWaveInfo
        |> Result.Ok



type LoadStatus =
    | Resolve  of LoadedComponent * LoadedComponent
    | OkComp of LoadedComponent
    | OkAuto of LoadedComponent

    
/// load all files in folderpath. Return Ok list of LoadStatus or a single Error.
let loadAllComponentFiles (folderPath:string)  = 
    let x = 
        try
            Ok <| fs.readdirSync (U2.Case1 folderPath)
        with
        | e -> Error <| sprintf "Error reading Issie project directory at '%s: %A" folderPath e
    match x with
    | Error msg -> Error msg
    | Ok x ->
        x
        |> Seq.toList
        |> List.filter (path.extname >> ((=) ".dgm"))
        |> List.map (fun fileName ->
                if fileNameIsBad (pathWithoutExtension fileName)
                then
                    Error <| sprintf @"Can't load file name '%s' from project '%s' because it contains incorrect characters.\n \
                    File names used as sheets must contain only alphanumeric and space characters before the '.dgm' extension" fileName folderPath
                else 
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



