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
    /// directory under static/ holding the shipped libraries
    let librariesDirectory = "libraries"
    let indexFile = "index.json"
    /// catalogue section used when a component's sheet does not name one
    let defaultSection = "Components"

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

/// Every library shipped with Issie. Read once when the application starts: the index files make
/// this cheap, and the catalogue is a pure render function so it cannot read them itself.
/// Anything in the libraries directory that is not a directory of sheets - the README, say - has
/// no components and so is simply not a library.
let readLibraries () : ComponentLibrary list =
    let librariesPath = pathJoin [| staticFileDirectory; Constants.librariesDirectory |]
    match exists librariesPath with
    | false -> []
    | true ->
        readFilesFromDirectory librariesPath
        |> List.choose (readLibrary librariesPath)
        |> List.sortBy (fun lib -> lib.Name)

/// Write a library's index from its sheets. Not called by the application: a library author runs
/// this after changing a library, because a hand-maintained index would rot.
let writeLibraryIndex (libPath: string) : Result<unit, string> =
    indexOfLibraryDirectory libPath
    |> Json.stringify
    |> writeFile (pathJoin [| libPath; Constants.indexFile |])

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

/// The index to use for a library in this project: the one it already has if any of its sheets
/// are present, otherwise the lowest free index.
/// An index is free when no other library holds it AND none of the names it would produce clashes
/// with an existing sheet - user sheet names may themselves contain underscores, so a project can
/// already hold a sheet called L1_Adder. A clashing index is skipped rather than anything being
/// renamed or refused.
let libraryIndexFor (ldcs: LoadedComponent list) (library: ComponentLibrary) : int =
    let existing = ldcs |> List.choose libraryOfSheet
    match existing |> List.tryFind (fun (libName, _) -> libName = library.Name) with
    | Some (_, n) -> n
    | None ->
        let takenIndices = existing |> List.map snd |> Set.ofList
        let existingNames = ldcs |> List.map (fun ldc -> ldc.Name) |> Set.ofList
        let clashes n =
            library.Components
            |> List.exists (fun comp -> Set.contains (sheetNameFor n comp.Name) existingNames)
        Seq.initInfinite (fun i -> i + 1)
        |> Seq.find (fun n -> not (Set.contains n takenIndices) && not (clashes n))

/// The prefix a registered library owns in this project. A user sheet may not be named into one
/// of these, or a component of that library added later would have nowhere to go.
let reservedPrefixes (ldcs: LoadedComponent list) : string list =
    ldcs
    |> List.choose libraryOfSheet
    |> List.map (fun (_, n) -> $"L{n}_")
    |> List.distinct

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
