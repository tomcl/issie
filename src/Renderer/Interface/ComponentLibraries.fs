module ComponentLibraries

(*
    ComponentLibraries.fs

    Reusable parameterised components, offered in the catalogue and materialised into a project on
    use.

    On disk a library is a directory of .ldgm files, one per component:

        <libraries>/<libname>/<compname>.ldgm

    An .ldgm is a serialised (LibraryHeader * string): a small authored header, and the component's
    sheet as the exact text of a .dgm. Two things follow from that shape, and they are the whole
    design:

    - Listing a library reads headers only. The body is one JSON string token, so nothing builds a
      canvas: no LoadedComponent, no width inference, no id regeneration. Those cost far more than
      reading the bytes do.
    - Materialising a component writes the body string out as a .dgm and hands it to the ordinary
      sheet loader. There is one canvas format in Issie, not two, and nothing here understands it.

    Nothing is derived, so nothing can go stale. An earlier design generated an index of each
    library and had to work out when that index was out of date. The header is authored instead, by
    the "save as library component" command - which is also what settles whether a component is
    offered in the catalogue, a question that otherwise needs every sheet in the library parsed to
    answer.
*)

open Fable.SimpleJson
open CommonTypes
open FilesIO

module Constants =
    /// directory under static/ holding the libraries shipped with Issie
    let librariesDirectory = "libraries"
    let componentExtension = ".ldgm"
    /// catalogue section used when a component does not name one
    let defaultSection = "Components"
    /// Written into every header and checked when one is read. The format is expected to grow - a
    /// header is cheap to extend - and a file from a later Issie should be refused with something
    /// better than a decoding error. .dgm has no version field, which is why loading one is three
    /// decode attempts in sequence; this is the one chance not to repeat that.
    let currentFormatVersion = 1

//------------------------------------------------------------------------------------------------//
//------------------------------------- The .ldgm format -----------------------------------------//
//------------------------------------------------------------------------------------------------//

/// What the catalogue needs to know about a component without reading its sheet.
///
/// Deliberately does NOT carry the component's parameters, ports, or anything else derived from
/// the sheet. All of that is read from the body, once, just before it is needed - which is why
/// this cannot drift from the sheet it describes.
type LibraryHeader = {
    FormatVersion: int
    /// what the user sees, and the base name of the file within the library directory
    Name: string
    /// the catalogue tooltip
    Description: string
    /// catalogue section this component is grouped under
    Section: string
    /// False for a sheet that exists only to be used by another component of the same library.
    /// Authored, not inferred: working it out meant parsing every sheet of the library to find
    /// which of them were instantiated by others.
    OfferedInCatalogue: bool
    /// Other components OF THE SAME LIBRARY that this one instantiates, by name. Referenced rather
    /// than embedded, so two components sharing a sub-sheet do not each carry a copy of it. A
    /// dependency is materialised alongside the component that needs it.
    Requires: string list
}

/// A component file: its header, and its sheet as the exact text of a .dgm.
/// The body is a string, not a parsed canvas, so that reading a header never builds one and
/// materialising is a file write followed by the ordinary loader.
type LibraryFile = LibraryHeader * string

/// A library as offered in the catalogue: a name and a directory. Its components are read when the
/// user opens it, never at startup.
type ComponentLibrary = {
    Name: string
    /// absolute path of the directory holding the .ldgm files
    Path: string
}

/// One component of an opened library, as listed.
type LibraryListing = {
    Header: LibraryHeader
    /// absolute path of the .ldgm
    Path: string
}

/// What opening a library found: the components it can offer, and anything that would not read.
/// A file that will not read costs that component, never the library.
type OpenedLibrary = {
    Name: string
    Path: string
    Components: LibraryListing list
    Problems: string list
}

let componentPath (libPath: string) (name: string) =
    pathJoin [| libPath; name + Constants.componentExtension |]

/// Read one .ldgm. The body comes back as text: there is little to gain from decoding less, since
/// it is a single JSON string token either way.
let tryReadComponentFile (path: string) : Result<LibraryFile, string> =
    match tryReadFileSync path with
    | Error msg -> Error msg
    | Ok contents ->
        #if FABLE_COMPILER
        let parsed = Json.tryParseNativeAs<LibraryFile> contents
        #else
        // Thoth on the .NET side, as for writing. An .ldgm holds no discriminated unions - a
        // record, a string, and a list of strings - which is the one case where the two libraries
        // agree, so a file written by either can be read by either.
        let parsed =
            Thoth.Json.Net.Decode.Auto.fromString<LibraryFile> contents
        #endif
        match parsed with
        | Error msg -> Error $"{baseName path} is not a readable library component ({msg})"
        | Ok (header, body) ->
            match header.FormatVersion > Constants.currentFormatVersion with
            | true ->
                Error $"{baseName path} was written by a later version of Issie: it uses library format {header.FormatVersion} and this version reads {Constants.currentFormatVersion}"
            | false -> Ok (header, body)

/// The header of one component.
let tryReadHeader (path: string) : Result<LibraryHeader, string> =
    tryReadComponentFile path |> Result.map fst

/// Write a component file. Used by "save as library component". `body` must be the text of a .dgm
/// exactly as the sheet was saved, since that is what is written back out when it is used.
let writeComponentFile (libPath: string) (header: LibraryHeader) (body: string) : Result<unit, string> =
    let file: LibraryFile = header, body
    #if FABLE_COMPILER
    let json = Json.stringify file
    #else
    // SimpleJson does not run on .NET - its converter is JS all the way down - so the .NET side
    // writes with Thoth, as the .dgm path does. The two disagree about unions, but not about
    // anything in an .ldgm: a tuple is an array and a record is an object in both. Fable reads
    // what .NET writes because SimpleJson's reader takes either union encoding; the reverse does
    // not hold, which is why tryReadComponentFile is still Fable only.
    let json = Thoth.Json.Net.Encode.Auto.toString (0, file)
    #endif
    writeFile (componentPath libPath header.Name) json

//------------------------------------------------------------------------------------------------//
//--------------------------------------- Finding them -------------------------------------------//
//------------------------------------------------------------------------------------------------//

/// The names of the subdirectories of `path`. Anything that is not a directory - a README, say -
/// is not a library, and is skipped here rather than further in, since reading a file as though it
/// were a directory logs a warning even though the answer is correctly "no components".
let private subdirectoriesOf (path: string) : string list =
    match exists path with
    | false -> []
    | true ->
        readFilesFromDirectory path
        |> List.filter (fun name -> isDirectory (pathJoin [| path; name |]))

/// Where libraries the user makes or imports are kept. Those shipped with Issie stay read-only
/// under the installation; this is the writable side. Error when the directory cannot be made -
/// see FilesIO.tryUserDataDirectory for why that is a real possibility and not a theoretical one.
let tryUserLibrariesDirectory () : Result<string, string> =
    tryUserDataDirectory ()
    |> Result.bind (fun root -> tryEnsureDirectory (pathJoin [| root; Constants.librariesDirectory |]))

/// The libraries available: those shipped with Issie, and those the user has made or imported.
/// Directory names only - no file is opened - so this is cheap enough for startup, which it has to
/// be: the catalogue is a pure render function and cannot read the disk itself. Everything about a
/// component is read later, when its library is opened.
/// A user library with the same name as a shipped one wins, so a library can be overridden.
let findLibraries () : ComponentLibrary list =
    let librariesIn root =
        subdirectoriesOf root |> List.map (fun name -> {Name = name; Path = pathJoin [| root; name |]})
    let shipped = librariesIn (pathJoin [| staticFileDirectory; Constants.librariesDirectory |])
    let user =
        match tryUserLibrariesDirectory () with
        | Ok path -> librariesIn path
        | Error _ -> []      // nowhere to keep user libraries just means there are none
    let userNames = user |> List.map (fun lib -> lib.Name) |> Set.ofList
    shipped
    |> List.filter (fun lib -> not (Set.contains lib.Name userNames))
    |> List.append user
    |> List.sortBy (fun lib -> lib.Name)

/// Read the headers of a library's components. Done when the user opens the library, and not kept:
/// it is one small read per component, on an action the user took, and keeping it would mean
/// deciding when it had gone wrong.
let openLibrary (library: ComponentLibrary) : OpenedLibrary =
    let read =
        readFilesFromDirectoryWithExtn library.Path Constants.componentExtension
        |> List.map (fun fileName ->
            let path = pathJoin [| library.Path; fileName |]
            path, tryReadHeader path)
    {
        Name = library.Name
        Path = library.Path
        Components =
            read
            |> List.choose (fun (path, header) ->
                match header with
                | Ok header when header.OfferedInCatalogue -> Some {Header = header; Path = path}
                | _ -> None)
            |> List.sortBy (fun listing -> listing.Header.Name)
        Problems =
            read
            |> List.choose (fun (_, header) -> match header with | Error msg -> Some msg | Ok _ -> None)
    }

/// A component and everything it needs, dependencies first - the order they must be written in.
/// Dependencies are named rather than embedded, so they are read from the same library here. A
/// name that is not there is an error rather than a silent omission: the component would otherwise
/// be placed holding a custom component that refers to a sheet which does not exist.
let readComponentAndDependencies (libPath: string) (name: string) : Result<LibraryFile list, string> =
    let rec read (got: LibraryFile list) (name: string) : Result<LibraryFile list, string> =
        match got |> List.exists (fun (header, _) -> header.Name = name) with
        | true -> Ok got        // shared sub-sheet reached twice: a diamond, not a problem
        | false ->
            tryReadComponentFile (componentPath libPath name)
            |> Result.bind (fun (header, body) ->
                (got, header.Requires)
                ||> Helpers.ResultList.fold read
                |> Result.map (fun got -> got @ [header, body]))
    read [] name

/// The sheets a sheet instantiates, by name and without duplicates. Used to fill in a header's
/// Requires when a component is saved, and to find everything that must be saved with it.
let customSheetsUsedBy (ldc: LoadedComponent) : string list =
    fst ldc.CanvasState
    |> List.choose (fun comp ->
        match comp.Type with
        | Custom cc -> Some cc.Name
        | _ -> None)
    |> List.distinct

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
let libraryIndexFor (ldcs: LoadedComponent list) (libraryName: string) : int =
    let existing = ldcs |> List.choose libraryOfSheet
    match existing |> List.tryFind (fun (libName, _) -> libName = libraryName) with
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
