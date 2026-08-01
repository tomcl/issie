module ComponentLibraries

(*
    ComponentLibraries.fs

    Reusable parameterised components shipped with Issie, placed from the catalogue and
    materialised into a project on use.

    On disk a library is a directory of ordinary sheets:

        static/libraries/<libname>/index.json
        static/libraries/<libname>/<compname>.dgm

    The index exists so that the catalogue and the placement popup can be shown without opening a
    single .dgm: reading every sheet of every library at startup does not scale. It is derived
    data - `indexOfLibraryDirectory` rebuilds it from the sheets themselves - so it can be
    regenerated whenever the sheets change, and a library with no index is still usable, just
    slower to open.
*)

open Fable.SimpleJson
open CommonTypes
open FilesIO
open ParameterTypes

module Constants =
    /// directory under static/ holding the libraries shipped with Issie
    let librariesDirectory = "libraries"
    let indexFile = "index.json"
    /// catalogue section used when a component's sheet does not name one
    let defaultSection = "Components"
    /// How far a library directory's modification time may be after its index before the index is
    /// treated as stale. Writing index.json into the directory it describes can leave the
    /// directory a moment newer than the file, and packaging through a zip rounds times to the
    /// nearest two seconds, so a strict comparison would rebuild every index on every startup.
    let indexStaleMarginMs = 10000.0

//------------------------------------------------------------------------------------------------//
//------------------------------------------ The index -------------------------------------------//
//------------------------------------------------------------------------------------------------//

/// One parameter of a library component, copied out of the sheet's declarations so that the
/// placement popup can ask for a value without the sheet being read.
type LibraryParam = {
    Name: string
    Description: string
    Default: int
}

/// One component offered by a library. `Name` is both what the user sees and the base name of the
/// sheet's .dgm within the library directory.
type LibraryComponent = {
    Name: string
    /// shown as the catalogue tooltip; this is the sheet's own Description
    Description: string
    /// catalogue section this component is grouped under
    Section: string
    InputLabels: string list
    OutputLabels: string list
    Params: LibraryParam list
}

/// A library as offered in the catalogue.
type ComponentLibrary = {
    Name: string
    Components: LibraryComponent list
    /// absolute path of the directory holding the .dgm files
    Path: string
}

/// The parameters a sheet declares, in index form.
let private paramsOfSheet (ldc: LoadedComponent) : LibraryParam list =
    let defs =
        ldc.LCParameterSlots
        |> Option.map (fun ps -> ps.DefaultBindings)
        |> Option.defaultValue Map.empty
    defs
    |> Map.toList
    |> List.map (fun (ParamName name, definition) ->
        let value =
            evaluateParamExpression (bindingsOf defs) definition.Expression
            |> Result.toOption
            |> Option.defaultValue 1
        {Name = name; Description = definition.Description; Default = value})

/// The index entry for one sheet of a library.
/// A sheet with no description still appears, described by its own name: a library component
/// without a tooltip is worse than a dull one.
let private componentOfSheet (ldc: LoadedComponent) : LibraryComponent =
    {
        Name = ldc.Name
        Description = ldc.Description |> Option.defaultValue ldc.Name
        Section = Constants.defaultSection
        InputLabels = ldc.InputLabels |> List.map fst
        OutputLabels = ldc.OutputLabels |> List.map fst
        Params = paramsOfSheet ldc
    }

/// Build a library's index by reading every sheet in its directory. This is what the index file
/// caches, and what is used when there is no index file.
/// A sheet instantiated by another sheet of the same library is part of that component rather
/// than a component in its own right, so it is not offered separately.
let indexOfLibraryDirectory (libPath: string) : LibraryComponent list =
    let sheets =
        readFilesFromDirectoryWithExtn libPath ".dgm"
        |> List.choose (fun name ->
            match tryLoadComponentFromPath (pathJoin [| libPath; name |]) with
            | Ok ldc -> Some ldc
            | Error msg ->
                JSHelpers.log $"Skipping library sheet {name}: {msg}"
                None)
    let instantiatedWithin =
        sheets
        |> List.collect (fun ldc ->
            fst ldc.CanvasState
            |> List.choose (fun comp ->
                match comp.Type with
                | Custom cc -> Some cc.Name
                | _ -> None))
        |> Set.ofList
    sheets
    |> List.filter (fun ldc -> not (Set.contains ldc.Name instantiatedWithin))
    |> List.map componentOfSheet
    |> List.sortBy (fun comp -> comp.Name)

/// Read one library. The index file is used if it parses; otherwise the directory is scanned and
/// a warning logged, so a missing or stale index degrades speed rather than function.
let private readLibrary (librariesPath: string) (libName: string) : ComponentLibrary option =
    let libPath = pathJoin [| librariesPath; libName |]
    let fromIndex =
        match tryReadFileSync (pathJoin [| libPath; Constants.indexFile |]) with
        | Error _ -> None
        | Ok contents ->
            match Json.tryParseAs<LibraryComponent list> contents with
            | Ok comps -> Some comps
            | Error msg ->
                JSHelpers.log $"Library {libName}: could not read {Constants.indexFile} ({msg}); reading its sheets instead"
                None
    let components = fromIndex |> Option.defaultWith (fun () -> indexOfLibraryDirectory libPath)
    match components with
    | [] -> None
    | _ -> Some {Name = libName; Components = components; Path = libPath}

/// Write a library's index from its sheets. Called when the index is missing or stale, and by a
/// library author directly after editing sheets in place - see refreshIndex for why that case
/// cannot be detected.
let writeLibraryIndex (libPath: string) : Result<unit, string> =
    indexOfLibraryDirectory libPath
    |> Json.stringify
    |> writeFile (pathJoin [| libPath; Constants.indexFile |])

/// The names of the subdirectories of `path`. Anything that is not a directory - a README, say -
/// is not a library and is skipped here rather than further in, since reading a file as though it
/// were a directory logs a warning even though the answer is correctly "no components".
let private subdirectoriesOf (path: string) : string list =
    match exists path with
    | false -> []
    | true ->
        readFilesFromDirectory path
        |> List.filter (fun name -> isDirectory (pathJoin [| path; name |]))

/// Copy the libraries shipped with Issie into the user's library directory, which is where they
/// are read from and where an imported library will also go. A library already there is left
/// alone: it may have been changed, and the shipped copy is only a starting point.
/// This means a library changed by a new Issie version does not replace one already copied. That
/// is the right way round while libraries are user-editable; a version check can be added when
/// there is something to check.
let private seedShippedLibraries (userPath: string) : unit =
    let shippedPath = pathJoin [| staticFileDirectory; Constants.librariesDirectory |]
    subdirectoriesOf shippedPath
    |> List.iter (fun name ->
        let target = pathJoin [| userPath; name |]
        match exists target with
        | true -> ()
        | false ->
            ensureDirectory target
            let source = pathJoin [| shippedPath; name |]
            readFilesFromDirectory source
            |> List.filter (fun f -> hasExtn ".dgm" f || f = Constants.indexFile)
            |> List.iter (fun f -> copyFile (pathJoin [| source; f |]) (pathJoin [| target; f |])))

/// Rewrite a library's index if it is missing or older than the directory it describes.
///
/// A directory's modification time changes when a sheet is added to it, removed from it or
/// renamed, which covers importing a library, deleting a component, and a fresh checkout. It does
/// NOT change when a sheet already there is rewritten in place, so a library author who edits a
/// sheet with Issie must regenerate the index with writeLibraryIndex. Watching every file for that
/// one case is not worth it: the index is derived data, and regenerating it is one call.
let private refreshIndex (libPath: string) : unit =
    let stale =
        match modifiedTimeMs (pathJoin [| libPath; Constants.indexFile |]), modifiedTimeMs libPath with
        | None, _ -> true
        | Some indexTime, Some dirTime -> dirTime > indexTime + Constants.indexStaleMarginMs
        | Some _, None -> false
    match stale with
    | false -> ()
    | true ->
        match writeLibraryIndex libPath with
        | Ok () -> ()
        // a library that cannot be indexed is still usable, just slower to open, so this must not
        // be allowed to stop the application starting
        | Error msg -> JSHelpers.log $"Could not write the index of library {libPath}: {msg}"

/// Every component library available. Read once when the application starts: the index files make
/// this cheap, and the catalogue is a pure render function so it cannot read them itself.
/// Libraries are read from the user's writable directory, not from the installation: see
/// FilesIO.userDataDirectory for why nothing may be written beside the installation.
let readLibraries () : ComponentLibrary list =
    let librariesPath = userLibrariesDirectory ()
    seedShippedLibraries librariesPath
    let libraries = subdirectoriesOf librariesPath
    libraries |> List.iter (fun name -> refreshIndex (pathJoin [| librariesPath; name |]))
    libraries
    |> List.choose (readLibrary librariesPath)
    |> List.sortBy (fun lib -> lib.Name)

//------------------------------------------------------------------------------------------------//
//--------------------------- Naming library sheets within a project -----------------------------//
//------------------------------------------------------------------------------------------------//

(*
    A library sheet is copied into the project under the name L<n>_<compname>, where n identifies
    the library within this project. The prefix is short because the sheet name is what the user
    sees: on the canvas as the stem of every instance label, in the sheet trees, and in the
    waveform simulator. A '.' would have been worse on both counts - longer, and already reserved
    as the custom component label separator.

    No extra project state records which n belongs to which library: a sheet's Form gives the
    library name and its own name gives the index.
*)

/// The name a library component takes as a sheet of a project.
let sheetNameFor (libraryIndex: int) (compName: string) = $"L{libraryIndex}_{compName}"

/// The library and index a sheet belongs to, if it is a library sheet.
let libraryOfSheet (ldc: LoadedComponent) : (string * int) option =
    match ldc.Form with
    | Some (Library (libName, _)) ->
        // the index is the digits between the leading L and the first underscore
        match ldc.Name.Split('_') |> Array.tryHead with
        | Some prefix when prefix.Length > 1 && prefix.StartsWith "L" ->
            match System.Int32.TryParse (prefix.Substring 1) with
            | true, n -> Some (libName, n)
            | _ -> None
        | _ -> None
    | _ -> None

/// The prefix owned by a library index.
let prefixFor (libraryIndex: int) = $"L{libraryIndex}_"

/// The index to use for a library in this project: the one it already has if any of its sheets
/// are present, otherwise the lowest free index.
/// An index is free when no other library holds it and NO existing sheet begins with its prefix -
/// not merely none of the names this library would produce. User sheet names may themselves
/// contain underscores, so a project can already hold a sheet called L1_Anything, and letting a
/// library share a prefix with an unrelated sheet would be confusing even where nothing actually
/// collides. A prefix in use is skipped rather than anything being renamed or refused.
let libraryIndexFor (ldcs: LoadedComponent list) (library: ComponentLibrary) : int =
    let existing = ldcs |> List.choose libraryOfSheet
    match existing |> List.tryFind (fun (libName, _) -> libName = library.Name) with
    | Some (_, n) -> n
    | None ->
        let takenIndices = existing |> List.map snd |> Set.ofList
        let names = ldcs |> List.map (fun ldc -> ldc.Name)
        let prefixInUse n = names |> List.exists (fun name -> name.StartsWith (prefixFor n))
        Seq.initInfinite (fun i -> i + 1)
        |> Seq.find (fun n -> not (Set.contains n takenIndices) && not (prefixInUse n))

/// The prefixes the libraries used by this project own. Once a library holds a prefix no sheet may
/// be named into it, or a component of that library added later would have nowhere to go.
let reservedPrefixes (ldcs: LoadedComponent list) : string list =
    ldcs
    |> List.choose libraryOfSheet
    |> List.map (snd >> prefixFor)
    |> List.distinct

/// The reserved prefix a proposed sheet name would intrude on, if any.
/// Used to refuse the name when a sheet is created or renamed.
let reservedPrefixOf (ldcs: LoadedComponent list) (sheetName: string) : string option =
    reservedPrefixes ldcs
    |> List.tryFind (fun prefix -> sheetName.StartsWith prefix)

//------------------------------------------------------------------------------------------------//
//---------------------------------------- Cleaning up -------------------------------------------//
//------------------------------------------------------------------------------------------------//

/// True when a sheet came from a library.
let isLibrarySheet (ldc: LoadedComponent) =
    match ldc.Form with
    | Some (Library _) -> true
    | _ -> false

/// Library sheets no sheet instantiates any more, and so which should be dropped from the project.
/// A library sheet used only by another library sheet of the same component is kept, since that
/// one is reachable; the calculation is repeated until it settles so that a multi-sheet component
/// goes in one piece.
/// Deliberately NOT run when the instance is deleted: undo restores model snapshots, so deleting
/// the sheet there would leave undo unable to bring it back. Sweeping when the project is saved
/// or closed keeps deletion undoable.
let rec unusedLibrarySheets (ldcs: LoadedComponent list) : LoadedComponent list =
    let instantiated =
        ldcs
        |> List.collect (fun ldc ->
            fst ldc.CanvasState
            |> List.choose (fun comp ->
                match comp.Type with
                | Custom cc -> Some cc.Name
                | _ -> None))
        |> Set.ofList
    let unused = ldcs |> List.filter (fun ldc -> isLibrarySheet ldc && not (Set.contains ldc.Name instantiated))
    match unused with
    | [] -> []
    | _ ->
        let unusedNames = unused |> List.map (fun ldc -> ldc.Name) |> Set.ofList
        let remaining = ldcs |> List.filter (fun ldc -> not (Set.contains ldc.Name unusedNames))
        unused @ unusedLibrarySheets remaining
