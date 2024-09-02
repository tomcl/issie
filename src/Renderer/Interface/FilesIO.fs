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
open ElectronAPI

open Node
open EEExtensions
open Fable.SimpleJson
open JSHelpers
open System.IO

[<Emit("process.cwd()")>]
let getCWD (u:unit): string = jsNative

//----------------Static Asset Handling------------------------------//

(*
Static assets come from file ./static in repo but are then placed differently
in porduction and dvelopmnet builds.
*)

/// This will only work for development
[<Emit("__static")>]
let staticDirFromStatic() :string = jsNative

/// This uses a fixed directory for production as a hack.
/// it is dependent on the electron build which positions static assets there.
/// productionbuild is defined in JSHelpers to be true for production (binary) builds only
let staticDir() =
    /// This identifies macos builds (arm64 too I hope!)
    /// on MacOs we think it should be ../Resources/static
    /// we hope staticDir will give this?
    let isMac = Node.Api.``process``.platform = Node.Base.Darwin
    if productionBuild && not isMac then
        "./resources/static"
    elif productionBuild && isMac then
        path.join [|__dirname; ".."; ".."; "static"|]
    else
        staticDirFromStatic()

/// absolute path to repo directory ./static
/// NB this path is not fixed (even as relative path) between
/// production and dev builds, so this must be used to access static
/// assets.
let staticFileDirectory = staticDir()

let pathJoin args = 
    #if FABLE_COMPILER
    path.join args
    #else
    Path.Join args
    #endif

let baseName (filePath: string) =
    #if FABLE_COMPILER
    path.basename filePath
    #else
    Path.GetFileName filePath
    #endif

let dirName (filePath: string) =
    #if FABLE_COMPILER
    path.dirname filePath
    #else
    Path.GetDirectoryName filePath
    #endif

let readFile (filePath: string) =
    #if FABLE_COMPILER
    fs.readFileSync(filePath, "utf8")
    #else
    File.ReadAllText(filePath, System.Text.ASCIIEncoding.UTF8)
    #endif

let exists (filePath: string) =
    #if FABLE_COMPILER
    fs.existsSync (U2.Case1 filePath)
    #else
    File.Exists filePath
    #endif

let extName (filePath: string) =
    #if FABLE_COMPILER
    path.extname filePath
    #else
    Path.GetExtension filePath
    #endif

let mkdir (folderPath: string) =
    #if FABLE_COMPILER
    fs.mkdirSync folderPath
    #else
    let dirInfo = Directory.CreateDirectory folderPath
    printfn "created directory: %A" dirInfo
    #endif

let readdir (folderPath: string) =
    #if FABLE_COMPILER
    fs.readdirSync (U2.Case1 folderPath)
    #else
    Directory.GetFiles folderPath |> Array.map Path.GetFileName
    #endif

let unlink (folderPath: string) =
    #if FABLE_COMPILER
    fs.unlink (U2.Case1 folderPath, ignore) // Asynchronous.
    #else
    File.Delete folderPath
    #endif

let rename (oldPath: string) (newPath: string) =
    #if FABLE_COMPILER
    fs.renameSync (oldPath, newPath) // Asynchronous.
    #else
    File.Move(oldPath, newPath, false)
    #endif

let ensureDirectory dPath =
    if (not <| exists dPath) then 
        mkdir dPath

let pathWithoutExtension filePath =
    let ext = extName filePath
    filePath 
    |> Seq.rev
    |> Seq.skip ext.Length
    |> Seq.rev
    |> String.ofSeq

let baseNameWithoutExtension =
    pathWithoutExtension >> baseName

let fileNameIsBad name =
    match (name |> Seq.tryItem 0) |> Option.map (fun c -> System.Char.IsDigit c || c = '_') with
    | Some true -> true
    | Some false | None -> 
        name
        |> Seq.filter (fun ch -> not (ch = ' ' || Char.IsLetterOrDigitOrUnderscore ch))
        |> Seq.isEmpty
        |> not

let filePathIsBad = 
    baseNameWithoutExtension >> fileNameIsBad

let fileExistsWithExtn extn folderPath baseName =
    let path = pathJoin [| folderPath; baseName + extn |]
    exists path

let tryReadFileSync fPath =
    if not <| exists fPath then
        Error $"Error: file {fPath} does not exist"
    else    
    readFile fPath
    |> Ok



/// Write utf8 encoded data to file.
/// Create file if it does not exist.
let writeFile path data =
    try
        let options = createObj ["encoding" ==> "utf8"] |> Some
        #if FABLE_COMPILER
        fs.writeFileSync(path, data, options)
        #else
        File.WriteAllText(path, data, System.Text.ASCIIEncoding.UTF8)
        #endif
        Ok ()
    with
        | e -> Result.Error $"Error '{e.Message}' writing file '{path}'"

/// read file names from directory: returning [] on any error.
let readFilesFromDirectory (path:string) : string list =
    if exists path then
        try 
            readdir path
            |> Seq.toList
        with
            | e ->
                printf $"Warning: readFilesFromDirectory has used readdir on 'path'='{path}' with an exception {e.Message}"
                []
    else
        printf $"Warning: readFilesFromDirectory has 'path'='{path}' and this directory does not exist."
        []

let hasExtn extn fName =
    (String.toLower fName).EndsWith (String.toLower extn)

/// copy a sheet from some source path to a destination path
let copyFile (sourcePath: string) (newPath: string) =
    match readFile sourcePath |> writeFile newPath with
    | Ok _ -> ()
    | Error msg -> log <| msg


let readFilesFromDirectoryWithExtn (path:string) (extn:string) : string list =
    readFilesFromDirectory path
    |> List.filter (fun name -> hasExtn extn name)

let removeExtn extn fName = 
    if hasExtn extn fName
    then Some fName[0..(fName.Length - extn.Length - 1)]
    else None

/// returns the list of backup files in descending chronological order.
let backupFileData (path:string) (baseName: string) =
    readFilesFromDirectory path
    |> List.filter (fun fn -> String.startsWith (baseName + "-") fn)
    |> List.map (fun fn -> 
            String.splitString [|"-"|] fn 
            |> Array.tryItem 1
            |> Option.bind (String.tryParseWith System.Int32.TryParse)
            |> fun n -> n,fn)
    |> List.sortDescending



/// returns the sequence number and name of the most recent (highest sequence number) backup file
let latestBackupFileData (path:string) (baseName: string) =
    backupFileData path baseName
    |> List.tryHead
    |> Option.bind (function 
        | None,_ -> None 
        | Some n, fn -> Some(n, fn))



/// read canvas state from file found on filePath (which includes .dgm suffix etc).
/// return Error if file does not exist or cannot be parsed.
let private tryLoadStateFromPath (filePath: string) =
    if not (exists filePath) then
        Result.Error <| sprintf "Can't read file from %s because it does not seem to exist!" filePath      
    else
        try
            Ok (readFile filePath)
        with
            | e -> Result.Error $"Error {e.Message} reading file '{filePath}'"

        |> Result.map jsonStringToState
        |> ( function
            | Error msg  -> Result.Error <| sprintf "could not convert file '%s' to a valid issie design sheet. Details: %s" filePath msg
            | Ok res -> Ok res)

let makeData aWidth dWidth (makeFun: int -> int -> bigint) : Map<bigint,bigint>=
    let truncate n =
        match dWidth with
        | 64 -> n
        | w -> ((1I <<< w) - 1I) &&& n
       
    let a = aWidth / 2
    let inp = [|0..(1 <<< a) - 1|]
    Array.allPairs inp inp
    |> Array.map (fun (x,y) -> bigint((x <<< a) + y), truncate (makeFun x y))
    |> Map.ofArray



let makeFixedROM addr data mem =
    let signExtend w n =
        if n &&& (1 <<< (w - 1)) <> 0 then
            ((-1 <<< w) ||| n) &&& 0xFFFFFFFF
        else
            n
            
    match mem.Init, addr, data with
    | UnsignedMultiplier, a, d when a % 2 = 0 && a <= 16 ->
        Ok <| makeData a d (fun (x:int) (y:int) -> bigint((x * y) % (1 <<< d)))
    | SignedMultiplier, a, d when a % 2 = 0 && a <= 16 ->
        let w = a / 2
        Ok <| makeData a d (fun (x:int) (y:int) -> bigint((signExtend w x * signExtend w y) &&& ((1 <<< d) - 1)))
    | FromData,_, _ -> Ok mem.Data
    | _ -> failwithf $"addr={addr}, data={data}, int={mem.Init} not allowed in makeFixedROM"

let jsonStringToMem (jsonString : string) =
     Json.tryParseAs<Map<bigint,bigint>> jsonString



            


let getBaseNameNoExtension filePath =
    let name = baseName filePath
    match name.Split '.' |> Seq.toList with
    | [] -> failwithf "what? split at . in a filename should never return empty list"
    | [name] -> name // No dots found.
    | firstSplit :: splits ->
        // Quite ugly but works.
        let rest =
            ("", [0..splits.Length - 2]) ||> List.fold (fun baseName i ->
                name + "." + splits[i]
            )
        firstSplit + rest

let private makeFileFilters (name : string) (extn : string) =
    createObj !![
    "name" ==> name
    "extensions" ==> ResizeArray [ extn ]
    ] 
    |> unbox<FileFilter> 
    |> Array.singleton

/// Ask the user to choose a project file, with a dialog window.
/// Return the folder containing the chosen project file.
/// Return None if the user exits withouth selecting a path.
let askForExistingProjectPath (defaultPath: string option) : string option =
    let options = createEmpty<OpenDialogSyncOptions>
    options.filters <- Some (makeFileFilters "ISSIE project file" "dprj" |> ResizeArray)
    options.defaultPath <-
        defaultPath
        |> Option.defaultValue (electronRemote.app.getPath ElectronAPI.Electron.AppGetPath.Documents)
        |> Some
    let w = electronRemote.getCurrentWindow()
    electronRemote.dialog.showOpenDialogSync(w,options)
    |> Option.bind (
        Seq.toList
        >> function
        | [] -> Option.None
        | p :: _ -> Some <| dirName p
    )

/// ask for existing sheet paths
let askForExistingSheetPaths (defaultPath: string option) : string list option =
    let options = createEmpty<OpenDialogSyncOptions>
    options.filters <- Some (makeFileFilters "ISSIE sheet" "dgm" |> ResizeArray)
    options.defaultPath <-
        defaultPath
        |> Option.defaultValue (electronRemote.app.getPath ElectronAPI.Electron.AppGetPath.Documents)
        |> Some
    options.properties <- Some [|
        OpenDialogOptionsPropertiesArray.OpenFile
        OpenDialogOptionsPropertiesArray.MultiSelections
        |]
    let w = electronRemote.getCurrentWindow()
    electronRemote.dialog.showOpenDialogSync(w,options)
    |> Option.bind (
        Seq.toList
        >> function
        | [] -> None
        | paths -> Some <| paths
    )



/// Ask the user a new project path, with a dialog window.
/// Return None if the user exits withouth selecting a path.
let rec askForNewProjectPath (defaultPath:string option) : string option =
    let options = createEmpty<SaveDialogSyncOptions>
    options.filters <- Some (makeFileFilters "ISSIE project" "" |> ResizeArray)
    options.title <- Some "Enter new ISSIE project directory and name"
    options.nameFieldLabel <- Some "New project name"
    options.defaultPath <- defaultPath
    options.buttonLabel <- Some "Create Project"
    options.properties <- Some [|
        SaveDialogOptionsPropertiesArray.CreateDirectory
        SaveDialogOptionsPropertiesArray.ShowOverwriteConfirmation
        |]
    match electronRemote.getCurrentWindow() with
    | w ->
        electronRemote.dialog.showSaveDialogSync(options)
        |> Option.bind (fun dPath ->
            let dir = dirName dPath
            let files = readdir dir
            if Seq.exists (fun (fn:string) -> fn.EndsWith ".dprj") files
            then
                electronRemote.dialog.showErrorBox(
                    "Invalid project directory",
                    "You are trying to create a new Issie project inside an existing project directory. \
                     This is not allowed, please choose a different directory")
                askForNewProjectPath defaultPath
            
            else
                Some dPath)
    
    


    
let tryCreateFolder (path : string) =
    if Seq.exists (Char.IsLetterOrDigitOrUnderscore >> not) (baseName path) then 
        Result.Error <| "File or project names must contain only letters, digits, or underscores"
    else
        try
            Result.Ok <| mkdir path
        with
            | ex -> Result.Error <| $"Can't create folder '{path}': {ex.Message}"


/// Asyncronously remove file.
/// ignore if file does not exist
let removeFileWithExtn extn folderPath baseName  =
    let path = pathJoin [| folderPath; baseName + extn |]
    if exists path then
        try 
            unlink path // Asynchronous.
        with
            | _ -> ()
    else
        ()

let renameFile extn folderPath baseName newBaseName =
    let oldPath = pathJoin [| folderPath; baseName + extn |]
    let newPath = pathJoin [| folderPath; newBaseName + extn |]
    if exists oldPath then
        try
            Ok <| rename oldPath newPath // synchronous.
        with
            | e -> Error  $"Rename of '{baseName}' in '{folderPath}' failed"
    elif extn = ".dgm" then
        Error $"Error: The file '{baseName}{extn} appears to have been removed"
    else
        Ok ()

let removeFile (folderPath:string) (baseName:string) = removeFileWithExtn ".dgm" folderPath baseName

let removeAutoFile folderPath baseName =
    let path = pathJoin [| folderPath; baseName + ".dgmauto" |]
    unlink path // Asynchronous.

let readMemDefnLine (addressWidth:int) (wordWidth: int) (lineNo: int) (s:string) =
    let nums = String.splitRemoveEmptyEntries [|' ';'\t';',';';';'"'|] s 
    match nums with
    | [|addr;data|] ->
        let addrNum = NumberHelpers.strToIntCheckWidth addressWidth addr
        let dataNum = NumberHelpers.strToIntCheckWidth wordWidth data
        match addrNum,dataNum with
        | Ok a, Ok d -> Ok (a, d)
        | Error aErr,_ -> Error $"Line {lineNo}:'%s{s}' has invalid address ({addr}). {aErr}"
        | _, Error dErr -> Error $"Line '%s{s}' has invalid data item ({data}). {dErr}"
    | x -> Error $"Line {lineNo}:'%s{s}' has {x.Length} items: valid lines consist of two numbers"

let readMemLines (addressWidth:int) (wordWidth: int) (lines: string array) =
    let parse = 
        Array.map String.trim lines
        |> Array.filter ((<>) "")
        |> Array.mapi (readMemDefnLine addressWidth wordWidth)
    match Array.tryFind (function | Error _ -> true | _ -> false) parse with
    | None ->
        let defs = (Array.map (function |Ok x -> x | _ -> failwithf "What?") parse)
        let repeats =
            Array.groupBy fst defs
            |> Array.filter (fun (num, vals) -> vals.Length > 1)
        if repeats <> [||] then 
            repeats
            |> Array.map fst
            |> fun aLst -> Error $"Memory addresses %A{aLst} are repeated"
        else
            Ok defs

    | Some (Error firstErr) -> 
        Error firstErr
    | _ -> failwithf "What? can't happen"

let readMemDefns (addressWidth:int) (wordWidth: int) (fPath: string) =
     tryReadFileSync fPath
    |> Result.bind (
        //(fun contents -> printfn "read file:\n contents={contents}"; contents)
        String.splitRemoveEmptyEntries [|'\n';'\r'|]
        >> readMemLines addressWidth wordWidth 
        >> Result.map Map.ofArray)

    
    

let writeMemDefns (fPath: string) (mem: Memory1) =
    try
        Map.toArray mem.Data
        |> Array.sortBy fst
        |> Array.map (fun (a,b) -> $"{NumberHelpers.hexBignum a}\t{NumberHelpers.hexBignum b}")
        |> String.concat "\n"
        |> writeFile fPath
        |> Ok
    with
        | e -> Error $"Error writing file '{fPath}': {e.Message}"

/// Return data for memory if it is linked to a ram.
/// Return mem data if it is unlinked
/// Error if the read fails ot the file parse fails.
let initialiseMem (mem: Memory1) (projectPath:string) =

    let memResult =
        match mem.Init with

        | FromFile name ->
            let fPath = pathJoin [| projectPath; name + ".ram"|]
            readMemDefns mem.AddressWidth mem.WordWidth fPath

        | FromData ->
            Ok mem.Data

        | _ -> Error $"Unsupported legacy memory type '{mem.Init}'"
       
    memResult
    |> Result.map (fun data -> {mem with Data = data})





/// Save a PNG file (encoded base64, as from draw2d).
/// Overwrite existing file if needed.
/// Not used now we do not have Draw2D.
/// Probably not useful but maybe one day could be used to print schematic?
let savePngFile folderPath baseName png = // TODO: catch error?
    /// Write base64 encoded data to file.
    /// Create file if it does not exist.
    let writeFileBase64 path data =
        let options = createObj ["encoding" ==> "base64"] |> Some
        try
            #if FABLE_COMPILER
            fs.writeFileSync(path, data, options)
            #else
            File.WriteAllBytes(path, System.Convert.FromBase64String(data))
            #endif
            Ok ()
        with
            | e -> Result.Error $"Error '{e.Message}' writing file '{path}'"   
    let path = pathJoin [| folderPath; baseName + ".png" |]
    writeFileBase64 path png



/// Save state to normal file. Automatically add the .dgm suffix.
/// This version will not correctly deal with bigint numbers.
/// See svaStateToFileNew
let saveStateToFile folderPath baseName state = // TODO: catch error?
    let path = pathJoin [| folderPath; baseName + ".dgm" |]
    let data = stateToJsonString state
    writeFile path data

/// Save state to file. Automatically add the .dgm suffix.
/// This is the new version of the function that uses the new state format and copes with bigints
/// However, it seems that it is not used??
let saveStateToFileExperimental folderPath baseName state = // TODO: catch error?
    let path = pathJoin [| folderPath; baseName + ".dgmNew" |]
    let data = stateToJsonStringExperimental state
    writeFile path data

/// Create new empty diagram file. Automatically add the .dgm suffix.
let createEmptyDgmFile folderPath baseName =
    saveStateToFile folderPath baseName (([],[]), None, Some {Form=Some User;Description=None})

let stripVertices (conn: LegacyCanvas.LegacyConnection) =
    {conn with Vertices = []}

let magnifySheet magnification (comp: LegacyCanvas.LegacyComponent) =
    {comp with 
        X = magnification * (comp.X + comp.W / 2. ); 
        Y = magnification * (comp.Y + comp.H/2.)
        H = -1 // overwritten correctly by Sheet based on componnet type
        W = -1 // as above
    }


/// Update from old component types to new
/// In addition do some sanity checks
/// The standard way to add functionality to an existing component is to create a new
/// component type, keeping the old type. Then on reading sheets from disk both new and old
/// will be correctly read. This function will be called on load and will convert from the old
/// type to the new one so that the rest of issie need only process new types, but compatibility
/// with saved old types remains.
let getLatestComp (comp: Component) =
    let updateMem (mem:Memory) : Memory1 =
        {
            Init = FromData
            Data = mem.Data
            AddressWidth = mem.AddressWidth
            WordWidth = mem.WordWidth
        }
    match comp.Type with
    | RAM mem -> {comp with Type = RAM1 (updateMem mem)}
    | ROM mem -> {comp with Type = ROM1 (updateMem mem)}
    | AsyncROM mem -> { comp with Type = AsyncROM1 (updateMem mem)}
    | Constant(width,cVal) -> {comp with Type = Constant1(width, cVal, $"%A{cVal}")}
    | Input width -> { comp with Type = Input1 (width, None)}
    | _ -> comp


/// Interface function that can read old-style circuits (without wire vertices)
/// as well as new circuits with vertices. Old circuits have an expansion parameter
/// since new symbols are larger (in units) than old ones.
let getLatestCanvas state =
    let oldCircuitMagnification = 1.25
    let stripConns (canvas: LegacyCanvas.LegacyCanvasState) =
        let (comps,conns) = canvas
        let noVertexConns = List.map stripVertices conns
        let expandedComps = List.map (magnifySheet oldCircuitMagnification) comps
        (expandedComps, noVertexConns)
        |> legacyTypesConvert
    let comps,conns =
        match state  with
        | CanvasOnly canvas -> stripConns canvas
        | CanvasWithFileWaveInfo(canvas, _, _) -> stripConns canvas
        | CanvasWithFileWaveInfoAndNewConns(canvas, _, _) -> legacyTypesConvert canvas
        | NewCanvasWithFileWaveInfoAndNewConns(canvas,_,_) -> canvas
        | NewCanvasWithFileWaveSheetInfoAndNewConns (canvas,_,_,_) -> canvas
    let comps = List.map convertFromJSONComponent comps
    List.map getLatestComp comps, conns

/// If the component is a RAM update its contents based on its initialiser
let checkMemoryContents (projectPath:string) (comp: Component) : Component =
    match comp.Type with
    | RAM1 mem | ROM1 mem | AsyncROM1 mem | AsyncRAM1 mem when not (String.endsWith "backup" (String.toLower projectPath))->
        match mem.Init with
        | FromFile fName ->
            let fPath = pathJoin [|projectPath ; (fName + ".ram")|]
            let memData = readMemDefns mem.AddressWidth mem.WordWidth fPath
            match memData with
            | Ok memDat -> 
                if memDat <> mem.Data then
                    printfn "%s" $"Warning! RAM file {fPath} has changed so component {comp.Label} is now different"
                let mem = {mem with Data = memDat}
                {comp with Type = getMemType comp.Type mem}
            | Error msg ->
                printfn $"Error reloading component {comp.Label} from its file {fPath}:\n{msg}"
                comp // ignore errors for now
        | _ -> comp
    | _ -> comp

/// load a component from its canvas and other elements
let makeLoadedComponentFromCanvasData (canvas: CanvasState) filePath timeStamp waveInfo (sheetInfo:SheetInfo option) =
    let projectPath = dirName filePath
    let inputs, outputs = CanvasExtractor.parseDiagramSignature canvas
    //printfn "parsed component"
    let comps,conns = canvas
    let comps' = List.map (checkMemoryContents projectPath) comps
    //printfn "checked component"
    let canvas = comps',conns
    let ramChanges = 
        List.zip comps' comps
        |> List.filter (fun (c1,c2) -> c1.Type <> c2.Type)
        |> List.map fst
    //printfn "ram changes processed"
    let form,description = match sheetInfo with |None -> (Some User),None |Some sI -> sI.Form,sI.Description
    let ldc =
        {
            Name = getBaseNameNoExtension filePath
            TimeStamp = timeStamp
            WaveInfo = waveInfo
            FilePath = filePath
            CanvasState = canvas
            InputLabels = inputs
            OutputLabels = outputs
            Form = form
            Description = description
            LoadedComponentIsOutOfDate = false
        }
    ldc, ramChanges


/// Make a loadedComponent from the file read from filePath.
/// Return the component, or an Error string.
let tryLoadComponentFromPath filePath : Result<LoadedComponent, string> =
    match tryLoadStateFromPath filePath with
    | Result.Error msg  
    | Ok (Result.Error msg) ->
        Error <| sprintf "Can't load component %s because of Error: %s" (getBaseNameNoExtension filePath)  msg
    | Ok (Ok state) ->
        let canvas = getLatestCanvas state
        makeLoadedComponentFromCanvasData 
            canvas
            filePath 
            state.getTimeStamp 
            state.getWaveInfo
            state.getSheetInfo
        |> fst // ignore ram change info, they will always be loaded
        |> Result.Ok



type LoadStatus =
    | Resolve  of LoadedComponent * LoadedComponent
    | OkComp of LoadedComponent
    | OkAuto of LoadedComponent

    
/// load all files in folderpath. Return Ok list of LoadStatus or a single Error.
let loadAllComponentFiles (folderPath:string)  = 
    let x = 
        try
            Ok <| readdir folderPath
        with
        | e -> Error <| sprintf "Error reading Issie project directory at '%s: %A" folderPath e
    match x with
    | Error msg -> Error msg
    | Ok x ->
        x
        |> Seq.toList
        |> List.filter (extName >> ((=) ".dgm"))
        |> List.map (fun fileName ->
                if fileNameIsBad (pathWithoutExtension fileName)
                then
                    Error <| sprintf @"Can't load file name '%s' from project '%s' because it contains incorrect characters.\n \
                    File names used as sheets must contain only alphanumeric and space characters before the '.dgm' extension" fileName folderPath
                else 
                    let filePath = pathJoin [| folderPath; fileName |]
                    printfn $"loading {fileName}"
                    let ldComp =  filePath |> tryLoadComponentFromPath
                    let autoComp = filePath + "auto" |> tryLoadComponentFromPath
                    printfn $"{fileName} Loaded"
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

/// Ask the user a new project path, with a dialog window.
/// Return None if the user exits withouth selecting a path.
let rec askForNewFile (projectPath: string) : string option =
    let options = createEmpty<SaveDialogSyncOptions>
    options.filters <- Some (makeFileFilters "Memory Contents File" "ram" |> ResizeArray)
    options.defaultPath <- Some projectPath
    options.title <- Some "Enter new file name"
    options.nameFieldLabel <- Some "New file name"
    options.buttonLabel <- Some "Save memory content to file"
    options.properties <- Some [|
        SaveDialogOptionsPropertiesArray.ShowOverwriteConfirmation
        |] 
    match electronRemote.getCurrentWindow() with
    | w ->
        electronRemote.dialog.showSaveDialogSync(options)
        
let saveAllProjectFilesFromLoadedComponentsToDisk (proj: Project) =
    proj.LoadedComponents
    |> List.iter (fun ldc ->
        let name = ldc.Name
        let state = ldc.CanvasState
        let waveInfo = ldc.WaveInfo
        let sheetInfo = {Form=ldc.Form;Description=ldc.Description}
        saveStateToFile proj.ProjectPath name (state,waveInfo,Some sheetInfo) |> ignore
        removeFileWithExtn ".dgmauto" proj.ProjectPath name)

let openWriteDialogAndWriteMemory mem path =
    match askForNewFile path with
    | None -> 
        None
    | Some fpath ->
        let fpath' =
            if not (String.contains "." fpath) then
                fpath + ".ram"
            else
                fpath
        writeMemDefns fpath' mem |> ignore
        Some fpath'
    




