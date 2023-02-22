module FilesIONative

open System.IO
open System.Text
open CommonTypes
open FilesIO
open Fable.SimpleJson

let pathJoin args = Path.Combine(args)

let baseName (filePath: string) : string =
    Path.GetFileNameWithoutExtension filePath

let pathWithoutExtension (filePath: string) : string =
    Path.GetFileNameWithoutExtension filePath

let getBaseNameNoExtension (filePath: string) : string =
    Path.GetFileNameWithoutExtension filePath

let jsonStringToState (jsonString: string) =
    Json.tryParseNativeAs<LegacyCanvas.LegacyCanvasState> jsonString
    |> (function
    | Ok state -> Ok(Helpers.JsonHelpers.SavedInfo.CanvasOnly state)
    | Error _ ->
        match Json.tryParseNativeAs<Helpers.JsonHelpers.SavedInfo> jsonString with
        | Ok state -> Ok state
        | Error str ->
            match
                Json.tryParseNativeAs<Helpers.JsonHelpers.SavedCanvasUnknownWaveInfo<obj>>
                    jsonString
            with
            | Ok(Helpers.JsonHelpers.SavedCanvasUnknownWaveInfo.NewCanvasWithFileWaveSheetInfoAndNewConns(cState,
                                                                                                          _,
                                                                                                          sheetInfo,
                                                                                                          time)) ->
                Ok
                <| Helpers.JsonHelpers.SavedInfo.NewCanvasWithFileWaveSheetInfoAndNewConns(
                    cState,
                    None,
                    sheetInfo,
                    time
                )
            | Error str ->
                printfn "Error in Json parse of %s : %s" jsonString str
                Error str)

// let jsonStringToState (jsonString: string) =
//     tryParseAs<LegacyCanvas.LegacyCanvasState> jsonString
//     |> (function
//     | Ok state ->
//         printfn "state: %A" state
//         Ok(Helpers.JsonHelpers.SavedInfo.CanvasOnly state)
//     | Error _ ->
//         match tryParseAs<Helpers.JsonHelpers.SavedInfo> jsonString with
//         | Ok state -> Ok state
//         | Error str ->
//             match tryParseAs<Helpers.JsonHelpers.SavedCanvasUnknownWaveInfo<obj>> jsonString with
//             | Ok(Helpers.JsonHelpers.NewCanvasWithFileWaveSheetInfoAndNewConns(cState,
//                                                                                _,
//                                                                                sheetInfo,
//                                                                                time)) ->
//                 Ok
//                 <| Helpers.JsonHelpers.SavedInfo.NewCanvasWithFileWaveSheetInfoAndNewConns(
//                     cState,
//                     None,
//                     sheetInfo,
//                     time
//                 )
//             | Error str ->
//                 printfn "Error in Json parse of %s : %s" jsonString str
//                 Error str)

let private tryLoadStateFromPath (filePath: string) =
    printfn $"====================== Loading state from {filePath}"
    if not (File.Exists filePath) then
        Result.Error
        <| sprintf "Can't read file from %s because it does not seem to exist!" filePath
    else
        try
            Ok(File.ReadAllText(filePath, Encoding.UTF8))
        with e ->
            Result.Error $"Error {e.Message} reading file '{filePath}'"

        |> Result.map jsonStringToState
        |> (function
        | Error msg ->
            Result.Error
            <| sprintf
                "could not convert file '%s' to a valid issie design sheet. Details: %s"
                filePath
                msg
        | Ok res -> Ok res)

let tryLoadComponentFromPath filePath : Result<CommonTypes.LoadedComponent, string> =
    printfn $"Loading component {filePath}"
    match tryLoadStateFromPath filePath with
    | Result.Error msg
    | Ok(Result.Error msg) ->
        Error
        <| sprintf
            "Can't load component %s because of Error: %s"
            (getBaseNameNoExtension filePath)
            msg
    | Ok(Ok state) ->
        printfn "state: %A" state
        let canvas = getLatestCanvas state
        makeLoadedComponentFromCanvasData
            canvas
            filePath
            state.getTimeStamp
            state.getWaveInfo
            state.getSheetInfo
        |> fst // ignore ram change info, they will always be loaded
        |> Result.Ok

/// load all files in folderpath. Return Ok list of LoadStatus or a single Error.
let loadAllComponentFiles (folderPath: string) =
    printfn $"Loading all components from {folderPath}"
    let x =
        try
            Ok <| Directory.GetFiles folderPath
        with e ->
            Error
            <| sprintf "Error reading Issie project directory at '%s: %A" folderPath e
    match x with
    | Error msg -> Error msg
    | Ok x ->
        x
        |> Seq.toList
        |> List.map Path.GetFileName
        |> List.filter (Path.GetExtension >> ((=) ".dgm"))
        |> List.map (fun fileName ->
            if FilesIO.fileNameIsBad (pathWithoutExtension fileName) then
                Error
                <| sprintf
                    @"Can't load file name '%s' from project '%s' because it contains incorrect characters.\n \
                    File names used as sheets must contain only alphanumeric and space characters before the '.dgm' extension"
                    fileName
                    folderPath
            else
                let filePath = pathJoin [| folderPath; fileName |]
                printfn $"loading {filePath}"
                let ldComp = filePath |> tryLoadComponentFromPath
                let autoComp = filePath + "auto" |> tryLoadComponentFromPath
                printfn $"{filePath} Loaded"
                match (ldComp, autoComp) with
                | Ok ldComp, Ok autoComp when ldComp.TimeStamp < autoComp.TimeStamp ->
                    Resolve(ldComp, autoComp) |> Ok
                | Ok ldComp, _ -> OkComp ldComp |> Ok
                | Error _, Ok autoComp -> OkAuto autoComp |> Ok
                | Error msg, _ -> Error msg)
        |> Helpers.tryFindError

let setupProjectFromComponents (sheetName: string) (ldComps: LoadedComponent list) =
    let compToSetup =
        match ldComps with
        | [] -> failwithf "setupProjectComponents must be called with at least one LoadedComponent"
        | comps ->
            // load sheetName
            match
                comps
                |> List.tryFind (fun comp -> comp.Name = sheetName)
            with
            | None ->
                failwithf
                    "What? can't find sheet %s in loaded sheets %A"
                    sheetName
                    (comps |> List.map (fun c -> c.Name))
            | Some comp -> comp

    { ProjectPath = dirName compToSetup.FilePath
      OpenFileName = compToSetup.Name
      WorkingFileName = Some compToSetup.Name
      LoadedComponents = ldComps }

// let conns = BusWire.extractConnections model.Sheet.Wire
// let comps = SymbolUpdate.extractComponents model.Sheet.Wire.Symbol
// let canvasState = comps,conns

let rec resolveComponentOpenPopup
    (pPath: string)
    (components: LoadedComponent list)
    (resolves: LoadStatus list)
    : string * LoadedComponent list
    =
    let chooseWhichToOpen comps =
        let onlyUserCreated =
            List.filter
                (fun comp ->
                    match comp.Form with
                    | Some User
                    | None -> true
                    | _ -> false)
                comps
        (List.maxBy (fun comp -> comp.TimeStamp) onlyUserCreated).Name
    match resolves with
    | [] -> (chooseWhichToOpen components), components
    | Resolve(ldComp, autoComp) :: rLst ->
        // ldComp, autocomp are from attemps to load saved file and its autosave version.
        let compChanges, connChanges = FileMenuView.quantifyChanges ldComp autoComp
        // special case when autosave data is most recent
        match compChanges + connChanges with
        | 0 ->
            failwithf
                "There were layout but no circuit changes made in sheet %s after your last save. \
                     There is an automatically saved version which is \
                     more uptodate. Do you want to keep the newer AutoSaved version or \
                     the older Saved version?"
                ldComp.Name
        | n when n < 3 ->
            failwithf
                "Warning: %d component and %d connection changes were made to sheet '%s' after your last Save. \
                     There is an automatically saved version which is \
                     more uptodate. Do you want to keep the newer AutoSaved version or \
                     the older saved version?"
                compChanges
                connChanges
                ldComp.Name
        | n ->
            failwithf
                "Warning: %d component and %d connection changes were made to sheet '%s' after your last Save. \
                     There is an automatically saved version which is \
                     more uptodate. Do you want to keep the newer AutoSaved version or \
                     the older saved version? This is a large change so the option you do not choose \
                     will be saved as file 'backup/%s.dgm'"
                compChanges
                connChanges
                ldComp.Name
                ldComp.Name
    | OkAuto autoComp :: rLst ->
        failwithf "Could not load saved project file '%s' - using autosave file instead" pPath
        resolveComponentOpenPopup pPath (autoComp :: components) rLst
    | OkComp comp :: rLst -> resolveComponentOpenPopup pPath (comp :: components) rLst

let readProjectFromPath (folderPath: string) =
    match loadAllComponentFiles folderPath with
    | Error err -> failwithf "Error loading components from %s: %s" folderPath err
    | Ok(componentsToResolve: LoadStatus list) ->
        resolveComponentOpenPopup folderPath [] componentsToResolve
