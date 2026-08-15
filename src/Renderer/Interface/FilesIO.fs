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

let getCWD (u:unit): string = Bridge.cwd

//----------------Static Asset Handling------------------------------//

(*
Static assets come from file ./static in repo but are then placed differently
in porduction and dvelopmnet builds.
*)

/// Absolute path to the static asset directory.
///
/// This used to be worked out here, three ways: __static in development, a bare
/// "./resources/static" relative to the working directory for production on Windows and Linux, and
/// __dirname/../../static for production on macOS. All three land on the same place as the main
/// process's process.resourcesPath, and __static in particular is a webpack substitution that
/// expands to an expression over `path` and `process` - so it could not survive contextIsolation
/// even in principle. Main resolves it once now and sends the answer; see Bridge.staticDirectory.
let staticDir() = Bridge.staticDir

/// absolute path to repo directory ./static
/// NB this path is not fixed (even as relative path) between
/// production and dev builds, so this must be used to access static
/// assets.
/// Empty under plain .NET - the tests - which have no Electron to ask and are given the directory
/// they work on explicitly.
let staticFileDirectory =
    #if FABLE_COMPILER
    Bridge.staticDir
    #else
    ""
    #endif

// Path arithmetic is pure string work with no privilege attached to it, so it is one shared
// implementation rather than node's `path` here and System.IO.Path there - see PathHelpers.fs.
let pathJoin (args: string array) = PathHelpers.join args

let baseName (filePath: string) = PathHelpers.basename filePath

let dirName (filePath: string) = PathHelpers.dirname filePath

let readFile (filePath: string) =
    #if FABLE_COMPILER
    Bridge.fsReadFile filePath
    #else
    File.ReadAllText(filePath, System.Text.Encoding.UTF8)
    #endif

/// False rather than an exception for a path main will not let Issie see. Every caller treats this
/// as a question about whether to go on, and a throw here would turn a refusal into a crash - but it
/// is logged, because "the file is not there" and "you may not look" are different problems.
let exists (filePath: string) =
    #if FABLE_COMPILER
    try
        Bridge.fsExists filePath
    with e ->
        Log.warn $"exists '{filePath}': {e.Message}"
        false
    #else
    // a directory exists too: File.Exists alone is false for one, which made every directory
    // read fail when this code is hosted on .NET (the tests)
    File.Exists filePath || Directory.Exists filePath
    #endif

/// True when the path exists and is a directory. False for a file, and for a path that is not
/// there at all, so it is safe to use as a filter before reading a directory's contents.
let isDirectory (filePath: string) =
    #if FABLE_COMPILER
    try
        Bridge.fsIsDirectory filePath
    with e ->
        Log.warn $"isDirectory '{filePath}': {e.Message}"
        false
    #else
    Directory.Exists filePath
    #endif

let extName (filePath: string) = PathHelpers.extname filePath

let mkdir (folderPath: string) =
    #if FABLE_COMPILER
    Bridge.fsMkdir folderPath
    #else
    Directory.CreateDirectory folderPath |> ignore
    #endif

let readdir (folderPath: string) =
    #if FABLE_COMPILER
    Bridge.fsReaddir folderPath
    #else
    Directory.GetFiles folderPath |> Array.map Path.GetFileName
    #endif

let unlink (folderPath: string) =
    #if FABLE_COMPILER
    Bridge.fsUnlink folderPath
    #else
    File.Delete folderPath
    #endif

let rename (oldPath: string) (newPath: string) =
    #if FABLE_COMPILER
    Bridge.fsRename oldPath newPath
    #else
    File.Move(oldPath, newPath, false)
    #endif

let ensureDirectory dPath =
    if (not <| exists dPath) then
        mkdir dPath

/// Modification time in milliseconds since the epoch, or None when the path is not there.
/// Works on a directory as well as a file. NB a directory's time changes when an entry is added
/// to it, removed from it or renamed - NOT when a file already in it is rewritten in place.
let modifiedTimeMs (filePath: string) : float option =
    #if FABLE_COMPILER
    // one round trip rather than two: main answers null for a path that is not there, so the
    // exists check that used to guard this is folded into the same call
    try
        Bridge.fsModifiedTimeMs filePath
    with e ->
        Log.warn $"modifiedTimeMs '{filePath}': {e.Message}"
        None
    #else
    match exists filePath with
    | false -> None
    | true ->
        Some (File.GetLastWriteTimeUtc filePath - System.DateTime(1970, 1, 1)).TotalMilliseconds
    #endif

/// Make a directory if it is not there, saying why if that could not be done. Creating a
/// directory can genuinely fail - a read-only or full disk, a permissions policy - and the
/// callers here are all able to carry on without it.
let tryEnsureDirectory (dPath: string) : Result<string, string> =
    try
        ensureDirectory dPath
        match exists dPath with
        | true -> Ok dPath
        | false -> Error $"could not create the directory {dPath}"
    with e ->
        Error $"could not create the directory {dPath}: {e.Message}"

/// The per-user, writable Issie directory.
///
/// Anything Issie writes for the user - demo working copies, component libraries the user makes or
/// imports - belongs here and NOT beside the installation. On macOS the app bundle
/// is signed and notarised, so writing inside it invalidates the signature and Gatekeeper can
/// then refuse to launch it. On Windows the installation is usually under Program Files, which
/// needs administrator rights to write. Both fail only for installed users, never in a
/// development build, which is exactly the kind of bug that ships.
let tryUserDataDirectory () : Result<string, string> =
    try
        Bridge.userData |> tryEnsureDirectory
    with e ->
        Error $"could not find the user data directory: {e.Message}"

let private tryUserSubdirectory (name: string) : Result<string, string> =
    tryUserDataDirectory ()
    |> Result.bind (fun root -> tryEnsureDirectory (pathJoin [| root; name |]))

/// Where a demo project is copied so that the user can edit it.
let tryUserDemosDirectory () : Result<string, string> = tryUserSubdirectory "demos"

/// Where libraries the user makes or imports live. The libraries shipped with Issie are NOT copied
/// here: they stay read-only under the installation, and are found there directly.
let tryUserLibrariesDirectory () : Result<string, string> = tryUserSubdirectory "libraries"

/// Show a directory in the platform's file manager.
///
/// On Windows this launches explorer.exe rather than calling shell.openPath, because the folder
/// window is created by the already-running explorer.exe, which has no right to take the
/// foreground away from Issie - so shell.openPath opens it *behind* the app. A process launched by
/// the foreground process does have that right and passes it on. Measured on Windows 11: through
/// shell.openPath the window lands immediately below Issie and Issie keeps focus, through the
/// spawn immediately above it and focused. Every other platform raises it already, and
/// shell.openPath is the portable route there.
///
/// onError is given a readable reason when the directory cannot be shown. On platforms other than
/// Windows it is called asynchronously, since shell.openPath reports failure by resolving with a
/// non-empty message rather than by rejecting - so saying nothing would make a failure look like
/// success.
let openFolderInFileManager (path: string) (onError: string -> unit) : unit =
    // The platform difference, and the explorer.exe spawn it needs on Windows, now live in main -
    // see Bridge.revealInFileManager, which this comment used to describe. It answers a promise of
    // the reason it could not be shown so that a failure still cannot look like success.
    Bridge.revealInFileManager path
    |> Promise.iter (fun error -> if error <> "" then onError error)

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
let writeFile (path: string) (data: string) =
    try
        #if FABLE_COMPILER
        // utf8 with no byte order mark, which is what main writes - see the .NET branch below for
        // why that matters
        Bridge.fsWriteFile path data
        #else
        // UTF8Encoding(false), not Encoding.UTF8: the latter emits a byte order mark, and
        // fs.writeFileSync on the Fable side does not. A .dgm written from .NET with a BOM is
        // rejected by the app's JSON parser with "Unexpected token '?'".
        File.WriteAllText(path, data, System.Text.UTF8Encoding false)
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
                Log.warn $"could not read the directory '{path}': {e.Message}"
                []
    else
        Log.warn $"could not read the directory '{path}': it does not exist"
        []

#if FABLE_COMPILER
/// Main asks readdirSync for Dirent entries, which each say whether they are a directory, and
/// reduces to the names of those that are. One call, no stat of anything.
let private subdirectoryNamesOf (folderPath: string) : string array =
    Bridge.fsReaddirDirectories folderPath
#endif

/// The immediate subdirectories of a folder, as full paths. [] if it cannot be read.
///
/// Deliberately not readdir: under .NET that is Directory.GetFiles, which lists files only, while
/// node's readdirSync lists directories too. Anything looking for subfolders through readdir
/// therefore works in the app and finds nothing under test - which is how this function came to
/// exist.
///
/// The directories come from the one readdir rather than from a stat of each entry, which is what
/// this used to do: existsSync and lstatSync per name, paid on every FILE in the folder before
/// discarding it. Listing C:\Windows\System32 took 244ms that way and takes 3ms this way - 4,885
/// entries, of which 4,687 were files answering a question nobody asked. Directory.GetDirectories
/// never had the problem, so only the node side changes.
let readSubdirectories (folderPath: string) : string list =
    if not (isDirectory folderPath) then
        []
    else
        try
            #if FABLE_COMPILER
            subdirectoryNamesOf folderPath
            |> Array.toList
            |> List.map (fun name -> pathJoin [| folderPath; name |])
            #else
            Directory.GetDirectories folderPath |> Array.toList
            #endif
        with e ->
            Log.warn $"could not list the subdirectories of '{folderPath}': {e.Message}"
            []

let hasExtn extn fName =
    (String.toLower fName).EndsWith (String.toLower extn)

/// copy a sheet from some source path to a destination path
let copyFile (sourcePath: string) (newPath: string) =
    match readFile sourcePath |> writeFile newPath with
    | Ok _ -> ()
    | Error msg -> Log.error msg


let readFilesFromDirectoryWithExtn (path:string) (extn:string) : string list =
    readFilesFromDirectory path
    |> List.filter (fun name -> hasExtn extn name)

/// What a directory looks like to Issie.
///
/// The .dprj marker is not needed to load a project - loadAllComponentFiles reads the .dgm files
/// and never opens it - but it is how a project is told from a folder that merely happens to have
/// sheets in it, and how "would this new project be inside an existing one?" is answered.
type ProjectDirectory =
    /// Holds the marker: a project, whatever else is in it.
    | IsProject
    /// Holds sheets but no marker, so it was a project whose marker has been lost, or a folder
    /// somebody put sheets in. Loadable either way.
    | SheetsButNoMarker
    /// Nothing here that Issie can open.
    | NotAProject

/// Which combination of marker and sheets means what. The one place that rule lives: inspectFolder
/// reads a folder to answer it, and the project browser is handed the same two facts by main
/// without either of them having to agree about the meaning separately.
let classifyFolder (hasMarker: bool) (sheetCount: int) : ProjectDirectory =
    match hasMarker, sheetCount > 0 with
    | true, _ -> IsProject
    | false, true -> SheetsButNoMarker
    | false, false -> NotAProject

/// What a directory is to Issie, and how many sheets are in it, from the one read of it. The
/// count is free once the classification has looked at the file names anyway.
let inspectFolder (path: string) : ProjectDirectory * int =
    let files = readFilesFromDirectory path
    let sheets = files |> List.filter (hasExtn ".dgm") |> List.length
    classifyFolder (files |> List.exists (hasExtn ".dprj")) sheets, sheets

let inspectProjectDirectory (path: string) : ProjectDirectory = inspectFolder path |> fst

/// The empty file that marks a directory as an Issie project, named after the directory.
let projectMarkerPath (projectPath: string) =
    pathJoin [| projectPath; baseName projectPath + ".dprj" |]

/// True when a path has no parent to go up to - a drive root, or the root of a share. dirName
/// returns such a path unchanged, which is what the browser's Up control asks.
let isFilesystemRoot (path: string) = dirName path = path

/// One folder as the project browser draws it.
type FolderEntry = {
    Path: string
    Kind: ProjectDirectory
    /// .dgm files directly inside it: what a project is worth telling apart by, without opening it.
    SheetCount: int
}

/// Ask for a folder chosen in the project browser to be made readable, and say whether it was.
///
/// Every other read here goes through a channel confined to directories Issie already has a reason
/// to trust, and a folder just picked out of the browser is not yet one of them. Main decides, by
/// looking at the folder itself rather than taking the renderer's word: it admits one that holds
/// sheets or a project marker. False therefore means "no project there", not "refused".
let admitProjectFolder (path: string) : bool =
    #if FABLE_COMPILER
    Bridge.admitProjectFolder path
    #else
    // Nothing confines a test run, so every folder is already as readable as it will ever be.
    isDirectory path
    #endif

/// Ordinary folders are included because the browser navigates into them; hidden ones are not,
/// since nobody keeps projects in them and they would bury what is worth seeing. Openable folders
/// sort first, so a project is never lost among unrelated ones.
let private forBrowsing (entries: FolderEntry list) =
    entries
    |> List.filter (fun entry -> not ((baseName entry.Path).StartsWith "."))
    |> List.sortBy (fun entry ->
        (match entry.Kind with NotAProject -> 1 | _ -> 0), String.toLower (baseName entry.Path))

/// Every immediate subdirectory of `path`, classified, for the browser to draw - or why the folder
/// could not be listed. One level only: this lists a folder, it does not search a disk.
///
/// A native folder picker draws every folder alike, so it cannot show which of them hold projects -
/// which is the whole reason Issie draws this list itself.
///
/// Deliberately NOT readSubdirectories and inspectFolder, which reach the operating system through
/// the confined filesystem channel. The folder being browsed is by definition one the user has not
/// opened yet, so every folder worth showing was refused: the dialog opened on the folder holding
/// the last project and reported that the user's Documents did not exist. The browse channel in
/// src/Main/Bridge.fs answers this one question without that confinement, and carries names and
/// counts rather than the contents of anything.
let browseFolderForOpening (path: string) : Result<FolderEntry list, string> =
    #if FABLE_COMPILER
    let listing = Bridge.browseFolder path

    if not listing.exists then
        Error "That folder is not there."
    else
        listing.entries
        |> Array.toList
        |> List.map (fun entry ->
            { Path = entry.path
              Kind = classifyFolder entry.hasMarker entry.sheetCount
              SheetCount = entry.sheetCount })
        |> forBrowsing
        |> Ok
    #else
    // Nothing confines a test run, so the ordinary directory reads answer this directly.
    if not (isDirectory path) then
        Error "That folder is not there."
    else
        readSubdirectories path
        |> List.map (fun dir ->
            let kind, sheets = inspectFolder dir
            { Path = dir; Kind = kind; SheetCount = sheets })
        |> forBrowsing
        |> Ok
    #endif

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
     Json.tryParseNativeAs<Map<bigint,bigint>> jsonString



            


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

/// Ask the user to choose a project, with a dialog window.
/// Return the chosen folder, or None if the user exits without choosing one.
///
/// A project IS a directory, so that is what the dialog asks for. It used to ask for the .dprj
/// inside one, which meant navigating into the project, past its sheets greyed out by the filter,
/// to select a file that is empty, says nothing, and is named after the folder the user was
/// already standing in - and whose directory was then all that was kept. Asking for the folder
/// also lets a project whose marker was lost or renamed be opened, which the filter made
/// impossible even though loading a project never reads it.
let askForExistingProjectPath (defaultPath: string option) : string option =
    let options = createEmpty<OpenDialogSyncOptions>
    // A folder picker draws every folder the same, so it cannot show which ones hold projects.
    // The title says that the folder they are in will do, which is what Issie then acts on.
    options.title <- Some "Choose an ISSIE project folder, or a folder containing projects"
    options.buttonLabel <- Some "Open"
    options.properties <- Some [| OpenDialogOptionsPropertiesArray.OpenDirectory |]
    options.defaultPath <-
        defaultPath
        |> Option.defaultValue Bridge.documents
        |> Some
    Bridge.dialogOpen options
    |> Array.toList
    |> function
        | [] -> Option.None
        | p :: _ -> Some p

/// Ask the user to choose a folder, for a caller that knows what it wants one for.
/// Return None if the user exits without choosing one.
let askForFolder (title: string) (buttonLabel: string) (defaultPath: string option) : string option =
    let options = createEmpty<OpenDialogSyncOptions>
    options.title <- Some title
    options.buttonLabel <- Some buttonLabel
    options.properties <- Some [|
        OpenDialogOptionsPropertiesArray.OpenDirectory
        OpenDialogOptionsPropertiesArray.CreateDirectory
        |]
    options.defaultPath <-
        defaultPath
        |> Option.defaultValue Bridge.documents
        |> Some
    Bridge.dialogOpen options
    |> Array.toList
    |> function
        | [] -> Option.None
        | p :: _ -> Some p

/// ask for existing sheet paths
let askForExistingSheetPaths (defaultPath: string option) : string list option =
    let options = createEmpty<OpenDialogSyncOptions>
    options.filters <- Some (makeFileFilters "ISSIE sheet" "dgm" |> ResizeArray)
    options.defaultPath <-
        defaultPath
        |> Option.defaultValue Bridge.documents
        |> Some
    options.properties <- Some [|
        OpenDialogOptionsPropertiesArray.OpenFile
        OpenDialogOptionsPropertiesArray.MultiSelections
        |]
    Bridge.dialogOpen options
    |> Array.toList
    |> (
        function
        | [] -> None
        | paths -> Some <| paths
    )



// askForNewProjectPath, a native SAVE dialog that asked the user to save a file which was really
// a directory, is gone: FileUpdate.newProject asks for the name and the folder in the app, where
// the naming rule and the inside-an-existing-project rule can be answered while the user is still
// typing rather than by an error box after the dialog has taken their typing away.

/// Why a project may not be called this, if it may not.
///
/// One rule, in one place: the creation form asks it of every keystroke so that the user sees the
/// objection while they can still act on it, and tryCreateFolder asks it as the last word. It used
/// to be reachable only by breaking it, after the native dialog had been dismissed, in an error
/// box that took the typing with it.
let projectNameError (name: string) : string option =
    if name = "" then
        Some "Enter a name for the project."
    elif Seq.exists (Char.IsLetterOrDigitOrUnderscore >> not) name then
        Some "Project names must contain only letters, digits, or underscores. Spaces and hyphens are not allowed."
    else
        None

let tryCreateFolder (path : string) =
    match projectNameError (baseName path) with
    | Some err -> Result.Error err
    | None ->
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

/// Split a .ram file line into the part which defines a memory location and the comment written
/// against it, if any. A comment runs from "//" to the end of the line.
let splitMemDefnComment (s: string) =
    match s.IndexOf "//" with
    | -1 -> s, None
    | i ->
        let comment = String.trim (s.Substring(i + 2))
        s.Substring(0, i), (if comment = "" then None else Some comment)

/// Parse one "address data" line of a .ram file, which may carry a comment. lineNo is the 1-based
/// line number in the file, used only to say where an error is.
let readMemDefnLine (addressWidth:int) (wordWidth: int) (lineNo: int) (s:string) =
    let defn, comment = splitMemDefnComment s
    // ':' is a separator so that the "0: 10" form people write addresses in is read as two numbers
    let nums = String.splitRemoveEmptyEntries [|' ';'\t';',';';';':';'"'|] defn
    match nums with
    | [|addr;data|] ->
        let addrNum = NumberHelpers.strToIntCheckWidth addressWidth addr
        let dataNum = NumberHelpers.strToIntCheckWidth wordWidth data
        match addrNum,dataNum with
        | Ok a, Ok d -> Ok (a, d, comment)
        | Error aErr,_ -> Error $"Line {lineNo}: '%s{s}' has an invalid address ({addr}). {aErr}"
        // the line number used to be missing here, so a bad data item said only which line
        // text was wrong, not where to find it
        | _, Error dErr -> Error $"Line {lineNo}: '%s{s}' has an invalid data item ({data}). {dErr}"
    | x -> Error $"Line {lineNo}: '%s{s}' has {x.Length} items: valid lines consist of two numbers"

/// Parse the lines of a .ram file, reporting the first bad line. lines must be the file's
/// lines as they are, blanks included, so that reported line numbers match the file.
let readMemLines (addressWidth:int) (wordWidth: int) (lines: string array) =
    let parse =
        lines
        // number the lines before dropping blank ones: the index used to be taken after the
        // filter, making it a 0-based count of non-blank lines rather than a line number
        |> Array.mapi (fun i line -> i + 1, String.trim line)
        // a line which is nothing but a comment defines no location, so it is dropped here along
        // with the blank ones rather than being read as a line with no numbers on it
        |> Array.filter (fun (_, line) -> String.trim (fst (splitMemDefnComment line)) <> "")
        |> Array.map (fun (lineNo, line) -> readMemDefnLine addressWidth wordWidth lineNo line)
    match Array.tryFind (function | Error _ -> true | _ -> false) parse with
    | None ->
        let defs = (Array.map (function |Ok x -> x | _ -> failwithf "What?") parse)
        let repeats =
            Array.groupBy (fun (addr, _, _) -> addr) defs
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

/// The locations defined by a .ram file, and the comments written against them. Locations with no
/// comment are absent from the second map.
let readMemDefns (addressWidth:int) (wordWidth: int) (fPath: string) =
     tryReadFileSync fPath
    |> Result.bind (
        // split on '\n' only, keeping blank lines, so readMemLines can report true file line
        // numbers. Splitting on both '\n' and '\r' and removing empties dropped them, and a
        // trailing '\r' is removed by the trim in readMemLines
        (fun (contents: string) -> contents.Split '\n')
        >> readMemLines addressWidth wordWidth
        >> Result.map (fun defs ->
            let data = defs |> Array.map (fun (addr, dat, _) -> addr, dat) |> Map.ofArray
            let comments =
                defs
                |> Array.choose (fun (addr, _, comment) -> comment |> Option.map (fun c -> addr, c))
                |> Map.ofArray
            data, comments))

    
    

let writeMemDefns (fPath: string) (mem: Memory1) =
    try
        // comments are written back against their locations, or writing a memory out would lose
        // whatever the .ram file it came from had to say about them
        let comments = Option.defaultValue Map.empty mem.Comments
        Map.toArray mem.Data
        |> Array.sortBy fst
        |> Array.map (fun (a,b) ->
            let defn = $"{NumberHelpers.hexBignum a}\t{NumberHelpers.hexBignum b}"
            match Map.tryFind a comments with
            | Some comment -> $"{defn}\t// {comment}"
            | None -> defn)
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
            Ok (mem.Data, Option.defaultValue Map.empty mem.Comments)

        | _ -> Error $"Unsupported legacy memory type '{mem.Init}'"

    memResult
    |> Result.map (fun (data, comments) ->
        {mem with Data = data; Comments = (if Map.isEmpty comments then None else Some comments)})

/// Save state to normal file. Automatically add the .dgm suffix.
/// This version will not correctly deal with bigint numbers.
/// See svaStateToFileNew
/// If serialisation fails the existing file is left untouched and an Error returned.
let saveStateToFile folderPath baseName state =
    let path = pathJoin [| folderPath; baseName + ".dgm" |]
    stateToJsonString state
    |> Result.bind (writeFile path)

/// Save state to file. Automatically add the .dgm suffix.
/// This is the new version of the function that uses the new state format and copes with bigints
/// However, it seems that it is not used??
let saveStateToFileExperimental folderPath baseName state =
    let path = pathJoin [| folderPath; baseName + ".dgmNew" |]
    stateToJsonStringExperimental state
    |> Result.bind (writeFile path)

/// Create new empty diagram file. Automatically add the .dgm suffix.
let createEmptyDgmFile folderPath baseName =
    saveStateToFile folderPath baseName (([],[]), None, Some {Form=Some User;Description=None;ParameterDefinitions = None; IsTopSheet = None})

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
            Comments = None
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
            | Ok (memDat, comments) ->
                if memDat <> mem.Data then
                    Log.warn $"RAM file {fPath} has changed, so component {comp.Label} is now different"
                let mem =
                    {mem with
                        Data = memDat
                        Comments = (if Map.isEmpty comments then None else Some comments)}
                {comp with Type = getMemType comp.Type mem}
            | Error msg ->
                Log.error $"reloading component {comp.Label} from its file {fPath}: {msg}"
                comp // ignore errors for now
        | _ -> comp
    | _ -> comp

/// load a component from its canvas and other elements
let makeLoadedComponentFromCanvasData (canvas: CanvasState) filePath timeStamp waveInfo (sheetInfo:SheetInfo option) =
    let projectPath = dirName filePath
    let inputs, outputs = CanvasExtractor.parseDiagramSignature canvas
    let comps,conns = canvas
    let comps' = List.map (checkMemoryContents projectPath) comps
    let canvas = comps',conns
    let ramChanges = 
        List.zip comps' comps
        |> List.filter (fun (c1,c2) -> c1.Type <> c2.Type)
        |> List.map fst
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
            LCParameterSlots = sheetInfo |> Option.bind (fun sI -> sI.ParameterDefinitions)
            IsTopSheet = sheetInfo |> Option.bind (fun sI -> sI.IsTopSheet) |> Option.defaultValue false
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



/// Copy a sheet from some source path to a destination path, giving every component, port and
/// connection in it a fresh uuid so that it cannot clash with the sheet it was copied from.
/// Falls back to a plain file copy if the source cannot be parsed.
let copySheetWithNewIds (sourcePath: string) (newPath: string) =
    match tryLoadComponentFromPath sourcePath with
    | Error msg ->
        Log.error msg
        copyFile sourcePath newPath
    | Ok ldc ->
        let ldc' = RegenerateIds.regenerateSheetIds ldc
        // a copied sheet never claims to be the top of the design it is copied into
        let sheetInfo: SheetInfo = {Form = ldc'.Form; Description = ldc'.Description; ParameterDefinitions = ldc'.LCParameterSlots; IsTopSheet = None}
        match saveStateToFile (dirName newPath) (baseNameWithoutExtension newPath) (ldc'.CanvasState, ldc'.WaveInfo, Some sheetInfo) with
        | Ok _ -> ()
        | Error msg -> Log.error msg

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
                    let ldComp =  filePath |> tryLoadComponentFromPath
                    let autoComp = filePath + "auto" |> tryLoadComponentFromPath
                    Log.dbg Log.Files $"loaded {fileName}"
                    match (ldComp, autoComp) with
                    | Ok ldComp, Ok autoComp when ldComp.TimeStamp < autoComp.TimeStamp ->
                        Resolve(ldComp,autoComp) |> Ok
                    | Ok ldComp, _ -> 
                        OkComp ldComp |> Ok
                    | Error _, Ok autoComp ->
                        OkAuto autoComp |> Ok
                    | Error msg, _ -> Error msg
            )
        |> ResultList.sequence

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
    // main answers "" for a cancelled dialog, where the remote API answered None
    match Bridge.dialogSave options with
    | "" -> Option.None
    | chosen -> Some chosen
        
let saveAllProjectFilesFromLoadedComponentsToDisk (proj: Project) =
    proj.LoadedComponents
    |> List.iter (fun ldc ->
        let name = ldc.Name
        let state = ldc.CanvasState
        let waveInfo = ldc.WaveInfo
        let sheetInfo: SheetInfo = {Form=ldc.Form;Description=ldc.Description; ParameterDefinitions=ldc.LCParameterSlots; IsTopSheet = Some ldc.IsTopSheet}
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
    




