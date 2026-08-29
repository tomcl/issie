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
      reading the bytes do. One component's canvas is built before it is placed - by
      tryReadComponentShape, so that the catalogue can draw it being dragged - but only the one
      being carried, and only once the user has taken hold of it.
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
        // agree, so a file written by either can be read by either. The body is not decoded here,
        // so its own encoding does not matter until it is loaded as a sheet.
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

/// Write a component file to a path chosen by the caller. `body` must be the text of a .dgm
/// exactly as the sheet was saved, since that is what is written back out when it is used.
let writeComponentFileAt (path: string) (header: LibraryHeader) (body: string) : Result<unit, string> =
    let file: LibraryFile = header, body
    #if FABLE_COMPILER
    let json = Json.stringify file
    #else
    // SimpleJson does not run on .NET - its converter is JS all the way down - so the .NET side
    // writes with Thoth, as the .dgm path does. The two disagree about unions, but not about
    // anything in an .ldgm: a tuple is an array and a record is an object in both, so either side
    // reads what the other wrote. The .dgm body inside does hold unions; reading that on .NET is
    // SimpleJsonDotNet's job.
    let json = Thoth.Json.Net.Encode.Auto.toString (0, file)
    #endif
    writeFile path json

/// Write a component into a library, under the name its header carries. What "save as library
/// component" uses; a library opened as a project writes to the file each sheet came from, which
/// is writeComponentFileAt above.
let writeComponentFile (libPath: string) (header: LibraryHeader) (body: string) : Result<unit, string> =
    writeComponentFileAt (componentPath libPath header.Name) header body

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

//------------------------------------------------------------------------------------------------//
//----------------------------- A library opened as a project ------------------------------------//
//------------------------------------------------------------------------------------------------//

(*
    A library is MAINTAINED as well as used, and the only way to change a component used to be to
    place it in a project, edit it there and save it back out - which renames its sheets, gives them
    new ids, and produces a copy rather than a change. So a library directory opens as a project in
    its own right: its .ldgm files are its sheets, kept where they are.

    A multi-sheet component needs nothing special. Its helper sheets are .ldgm files in the same
    directory, named by its header's Requires, so opening the directory brings them in with it and
    the component is a design of several sheets exactly as it was authored.

    What makes this work with the rest of Issie is that an .ldgm holds the text of a .dgm. Loading
    one unwraps it and hands the text to the ordinary sheet decode; saving one wraps it back up
    beside the header the file already had. Nothing in between knows which of the two it is looking
    at, and a sheet's FilePath is where that fact lives.
*)

/// Whether a sheet's file is a library component rather than an ordinary .dgm.
let isLibraryComponentFile (filePath: string) = hasExtn Constants.componentExtension filePath

/// Whether this project IS a library, opened to be edited in place.
///
/// Read off the sheets' own files rather than carried in the model: the two forms differ only in
/// how each sheet is stored, so the files are where the difference actually is.
let isLibraryProject (project: Project) =
    project.LoadedComponents |> List.exists (fun ldc -> isLibraryComponentFile ldc.FilePath)

/// The extension a project's sheets are stored with. A project is all of one form or all of the
/// other, so this is a fact about the project and not about each sheet.
let sheetExtension (project: Project) =
    match isLibraryProject project with
    | true -> Constants.componentExtension
    | false -> ".dgm"

/// The file one sheet of a project is kept in - including a sheet that is about to be added, which
/// takes the form its siblings are in rather than becoming a stray .dgm the library loader would
/// never read.
let sheetFilePath (project: Project) (sheetName: string) : string =
    pathJoin [| project.ProjectPath; sheetName + sheetExtension project |]

/// The sheets a canvas instantiates, by name and without duplicates: a component's Requires.
let private customSheetsOnCanvas ((comps, _): CanvasState) =
    comps
    |> List.choose (fun comp -> match comp.Type with | Custom cc -> Some cc.Name | _ -> None)
    |> List.distinct

/// The header to write beside a sheet being saved into a library.
///
/// Everything the author declared is kept: which section the catalogue files it under, and whether
/// it is offered there at all. Those are decisions about the LIBRARY rather than about the sheet,
/// and nothing in the editor asks for them - so a sheet newly added to a library is a helper, and
/// becomes a component in its own right when "Save as library component" says so.
///
/// Requires is the exception, and has to be: it is what the sheet instantiates, so it is a fact
/// about the canvas being saved and is recomputed from it. Left alone, a component that gained a
/// sub-sheet would be placed missing it.
let private headerForSheet (path: string) (description: string option) (canvas: CanvasState) : LibraryHeader =
    let existing = tryReadHeader path
    let orExisting (pick: LibraryHeader -> 'a) (fallback: 'a) =
        match existing with
        | Ok header -> pick header
        | Error _ -> fallback
    let name = baseNameWithoutExtension path
    { FormatVersion = Constants.currentFormatVersion
      Name = name
      Description = description |> Option.defaultValue (orExisting (fun h -> h.Description) name)
      Section = orExisting (fun h -> h.Section) Constants.defaultSection
      OfferedInCatalogue = orExisting (fun h -> h.OfferedInCatalogue) false
      Requires = customSheetsOnCanvas canvas }

/// Put the serialised sheet in the file, in whichever of the two forms that file is. An ordinary
/// .dgm is the text and nothing more; an .ldgm is that same text wrapped in the header the file
/// already carried.
let private putSheetInFile
        (filePath: string)
        ((canvas, _, sheetInfo): CanvasState * SavedWaveInfo option * SheetInfo option)
        (json: string)
        : Result<unit, string> =
    match isLibraryComponentFile filePath with
    | false -> writeFile filePath json
    | true ->
        let description = sheetInfo |> Option.bind (fun si -> si.Description)
        writeComponentFileAt filePath (headerForSheet filePath description canvas) json

/// Write one sheet to its own file. The single funnel every sheet save goes through, so that
/// "which kind of project is this" is asked once and in one place.
let writeSheetFile
        (filePath: string)
        (state: CanvasState * SavedWaveInfo option * SheetInfo option)
        : Result<unit, string> =
    Helpers.JsonHelpers.stateToJsonString state
    |> Result.bind (putSheetInFile filePath state)

/// As writeSheetFile, keeping a timestamp the caller already has rather than stamping the moment
/// of writing. For a rewrite the user did not ask for - the id conversion done on load - where the
/// stamp is what says which sheet they were last working on.
let writeSheetFileAt
        (timeStamp: System.DateTime)
        (filePath: string)
        (state: CanvasState * SavedWaveInfo option * SheetInfo option)
        : Result<unit, string> =
    Helpers.JsonHelpers.stateToJsonStringAt timeStamp state
    |> Result.bind (putSheetInFile filePath state)

/// The text of the .dgm a sheet's file holds, unwrapped from its header where it has one. What
/// "save as library component" copies: an .ldgm's body IS a .dgm, so a component can be written
/// into another library from a library opened as a project as readily as from an ordinary sheet.
let trySheetFileBody (filePath: string) : Result<string, string> =
    match isLibraryComponentFile filePath with
    | false -> tryReadFileSync filePath
    | true -> tryReadComponentFile filePath |> Result.map snd

/// Read a sheet from its file, in whichever of the two forms that file is.
let tryLoadSheetFile (filePath: string) : Result<LoadedComponent, string> =
    match isLibraryComponentFile filePath with
    | false -> tryLoadComponentFromPath filePath
    | true ->
        tryReadComponentFile filePath
        |> Result.bind (fun (_, body) -> tryLoadComponentFromText filePath body)

/// Create the file for a sheet a project does not have yet, empty.
let createEmptySheetFile (project: Project) (name: string) =
    writeSheetFile
        (sheetFilePath project name)
        (([], []), None, Some {Form = Some User; Description = None; ParameterDefinitions = None; IsTopSheet = None})

/// Write every sheet of a project to disk. Used where a change reaches sheets other than the open
/// one - a custom component's ports changing shape, and a project being renamed.
let writeAllSheetFiles (project: Project) =
    project.LoadedComponents
    |> List.iter (fun ldc ->
        let sheetInfo: SheetInfo =
            {Form = ldc.Form; Description = ldc.Description
             ParameterDefinitions = ldc.LCParameterSlots; IsTopSheet = Some ldc.IsTopSheet}
        writeSheetFile (sheetFilePath project ldc.Name) (ldc.CanvasState, ldc.WaveInfo, Some sheetInfo) |> ignore
        removeFileWithExtn ".dgmauto" project.ProjectPath ldc.Name)

/// Copy a sheet from some source path to a destination path, giving every component, port and
/// connection in it a fresh id so that it cannot clash with the sheet it was copied from. Either
/// path may be a library component or an ordinary sheet, so this is also how a component is copied
/// out of a library into a project and back.
/// Falls back to a plain file copy if the source cannot be read as a sheet.
let copySheetWithNewIds (sourcePath: string) (newPath: string) =
    match tryLoadSheetFile sourcePath with
    | Error msg ->
        Log.error msg
        copyFile sourcePath newPath
    | Ok ldc ->
        let ldc' = Helpers.RegenerateIds.regenerateSheetIds ldc
        // a copied sheet never claims to be the top of the design it is copied into
        let sheetInfo: SheetInfo =
            {Form = ldc'.Form; Description = ldc'.Description
             ParameterDefinitions = ldc'.LCParameterSlots; IsTopSheet = None}
        match writeSheetFile newPath (ldc'.CanvasState, ldc'.WaveInfo, Some sheetInfo) with
        | Ok () -> ()
        | Error msg -> Log.error msg

/// Load a library directory as a project: every component in it becomes a sheet, and the file it
/// came from is where it is saved back to.
///
/// The sheets come back as User whatever the .ldgm says. A library sheet materialised INTO a
/// project is marked Library so that it is hidden and held read-only - it is one thing the user
/// placed, not a sheet of their own design. Here the sheets ARE the design, and marking them that
/// way would open the library into an editor that refuses to edit it.
///
/// Nor is any of them the top: which component of a library is "the" design is not a question a
/// library answers, and a flag left over from the project a component was authored in would make
/// one of them the answer at random.
let tryLoadLibraryProject (libPath: string) : Result<LoadStatus list, string> =
    match readFilesFromDirectoryWithExtn libPath Constants.componentExtension with
    | [] -> Error $"{baseName libPath} holds no library components"
    | fileNames ->
        fileNames
        |> List.map (fun fileName ->
            let path = pathJoin [| libPath; fileName |]
            tryReadComponentFile path
            |> Result.bind (fun (header, body) ->
                tryLoadComponentFromText path body |> Result.map (fun ldc -> header, ldc))
            |> Result.map (fun (header, ldc) ->
                OkComp
                    { ldc with
                        Name = header.Name
                        Description = Some header.Description
                        Form = Some User
                        IsTopSheet = false }))
        |> Helpers.ResultList.sequence

/// Whether a library may be opened as a project - which is to say, whether Issie could save it
/// again afterwards.
///
/// The libraries the user made or imported live in their own writable directory and always may be.
/// The ones shipped with Issie sit inside the installation, which is read-only for anyone who
/// installed it, and opening a library that could never be saved is worse than not offering it at
/// all: the work is done before the refusal arrives. A development run has the checkout writable
/// (see Main/Bridge.fs) and is where a shipped library is actually maintained, so there it may.
let libraryIsEditable (library: ComponentLibrary) : bool =
    match Bridge.isDev with
    | true -> true
    | false ->
        match tryUserLibrariesDirectory () with
        | Error _ -> false
        | Ok userRoot -> PathHelpers.isWithin userRoot library.Path

//------------------------------------------------------------------------------------------------//
//------------------------------ What a component will look like ---------------------------------//
//------------------------------------------------------------------------------------------------//

/// What a library component becomes on the canvas, without materialising it: the ports its symbol
/// will have, and whether that symbol is drawn as clocked.
///
/// This is what the catalogue needs to draw a component being carried to the sheet. It is read
/// from the sheet rather than declared in the header for the reason the header gives: nothing
/// derived is stored, so nothing can be out of date with the sheet it describes.
type ComponentShape = {
    InputLabels: (string * int) list
    OutputLabels: (string * int) list
    IsClocked: bool
}

/// The canvas held in a component file's body. The body is the text of a .dgm, so this is the
/// ordinary sheet decode and there is nothing library-specific in it.
let private tryCanvasOfBody (name: string) (body: string) : Result<CanvasState, string> =
    match Helpers.JsonHelpers.jsonStringToState body with
    | Error msg -> Error $"{name} does not hold a readable sheet ({msg})"
    | Ok state -> Ok (FilesIO.getLatestCanvas state)

/// Whether a sheet is clocked, which for a sheet that uses other sheets is a question about all of
/// them. CommonTypes.isClocked answers it from a project's LoadedComponents; a library component
/// belongs to no project until it is placed, so the sheets it came with are what is searched here.
let rec private canvasIsClocked
        (sheets: (string * CanvasState) list)
        (visited: string list)
        ((comps, _): CanvasState)
        : bool =
    comps
    |> List.exists (fun comp ->
        match comp.Type with
        | Custom cc when not (List.contains cc.Name visited) ->
            sheets
            |> List.tryFind (fun (name, _) -> name = cc.Name)
            |> Option.map (fun (_, canvas) -> canvasIsClocked sheets (cc.Name :: visited) canvas)
            |> Option.defaultValue false
        | Custom _ -> false     // a sheet reached twice: the first visit answered for it
        // asked with no LoadedComponents, isClocked answers for the primitives and says false for
        // any Custom - which is the case above, where the sheets to search are the ones read here
        | _ -> isClocked [] [] comp)

/// The shape of one component of a library, read from the same files placing it will read.
///
/// Every sheet is decoded, not just the component's own: whether it is clocked can depend on a
/// sheet it uses, and its ports come from the last one, which is the component itself.
let tryReadComponentShape (libPath: string) (name: string) : Result<ComponentShape, string> =
    readComponentAndDependencies libPath name
    |> Result.bind (
        Helpers.ResultList.traverse (fun ((header, body): LibraryFile) ->
            tryCanvasOfBody header.Name body |> Result.map (fun canvas -> header.Name, canvas)))
    |> Result.bind (fun sheets ->
        match List.tryLast sheets with
        | None -> Error $"Library component {name} has no sheet"
        | Some (_, canvas) ->
            let inputs, outputs = CanvasExtractor.parseDiagramSignature canvas
            Ok {
                InputLabels = inputs
                OutputLabels = outputs
                IsClocked = canvasIsClocked sheets [] canvas
            })

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
