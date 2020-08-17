// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#nowarn "0213"
#r "paket: groupref FakeBuild //"
#load "./tools/FSharpLint.fs"
#load "./tools/ElectronTools.fs"
#load "./tools/Updating.fs"
#load "./.fake/build.fsx/intellisense.fsx"

open BlackFox.CommandLine
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.JavaScript
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Net.Http
open Fake.Tools
open Fantomas.FakeHelpers
open Fantomas.FormatConfig
open Tools.Electron
open Tools.Linting
open Tools.Updating
open System
open System.IO

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "issie"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Schematic editor and Simulator"

// Author(s) of the project
let author = "tomcl"

// File system information
let solutionFile  = "issie.sln"

// Build docs website root
let website = "https://tomcl.github.io/issie/"

// Github repository
let repo = @"https://github.com/tomcl/ISSIE"

// Web or JS related fs projects
// Projects that have bindings to other languages where name linting needs to be more relaxed.
let relaxedNameLinting = 
    [ (__SOURCE_DIRECTORY__ @@ "src/Electron/**/*.fs") ]

let netCoreVersions = [ "netcoreapp3.1" ]

// OS runtime targets
let runTimes = ["win-x64";"linux-x64"]

// Read additional information from the release notes document
let release = ReleaseNotes.load (__SOURCE_DIRECTORY__ @@ "RELEASE_NOTES.md")

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

let tools      = __SOURCE_DIRECTORY__ @@ "tools"
let srcGlob    = __SOURCE_DIRECTORY__ @@ "src/**/*.??proj"
let fsSrcGlob  = __SOURCE_DIRECTORY__ @@ "src/**/*.fs"
let fsTestGlob = __SOURCE_DIRECTORY__ @@ "tests/**/*.fs"
let bin        = __SOURCE_DIRECTORY__ @@ "bin"
let temp       = __SOURCE_DIRECTORY__ @@ "temp"
let objFolder  = __SOURCE_DIRECTORY__ @@ "obj"
let dist       = __SOURCE_DIRECTORY__ @@ "dist"
let fable      = __SOURCE_DIRECTORY__ @@ ".fable"
let fsProjGlob =
    !! (__SOURCE_DIRECTORY__  @@ "src/**/*.fsproj")
    ++ (__SOURCE_DIRECTORY__  @@ "tests/**/*.fsproj")

let foldExcludeGlobs (g: IGlobbingPattern) (d: string) = g -- d
let foldIncludeGlobs (g: IGlobbingPattern) (d: string) = g ++ d

let fsSrcAndTest =
    !! fsSrcGlob
    ++ fsTestGlob
    -- (__SOURCE_DIRECTORY__  @@ "src/**/obj/**")
    -- (__SOURCE_DIRECTORY__  @@ "tests/**/obj/**")
    -- (__SOURCE_DIRECTORY__  @@ "src/**/AssemblyInfo.*")
    -- (__SOURCE_DIRECTORY__  @@ "src/**/**/AssemblyInfo.*")

let fsRelaxedNameLinting =
    let baseGlob s =
        !! s
        -- (__SOURCE_DIRECTORY__  @@ "src/**/AssemblyInfo.*")
        -- (__SOURCE_DIRECTORY__  @@ "src/**/obj/**")
        -- (__SOURCE_DIRECTORY__  @@ "tests/**/obj/**")
    match relaxedNameLinting with
    | [h] when relaxedNameLinting.Length = 1 -> baseGlob h |> Some
    | h::t -> List.fold foldIncludeGlobs (baseGlob h) t |> Some
    | _ -> None

let failOnBadExitAndPrint (p : ProcessResult) =
    if p.ExitCode <> 0 then
        p.Errors |> Seq.iter Trace.traceError
        failwithf "failed with exitcode %d" p.ExitCode

module dotnet =
    let tool optionConfig command args =
        DotNet.exec (fun p -> { p with WorkingDirectory = tools} |> optionConfig ) (sprintf "%s" command) args
        |> failOnBadExitAndPrint

    let femto optionConfig args =
        tool optionConfig (!!(__SOURCE_DIRECTORY__ @@ "packages/tooling/Femto/tools/**/**/Femto.dll") |> Seq.head) args

let setCmd f args =
    match Environment.isWindows with
    | true -> Command.RawCommand(f, Arguments.OfArgs args)
    | false -> Command.RawCommand("mono", Arguments.OfArgs (f::args))

let configuration() =
    FakeVar.getOrDefault "configuration" "Release"

let getEnvFromAllOrNone (s: string) =
    let envOpt (envVar: string) =
        if String.isNullOrEmpty envVar then None
        else Some(envVar)

    let procVar = Environment.GetEnvironmentVariable(s) |> envOpt
    let userVar = Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.User) |> envOpt
    let machVar = Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.Machine) |> envOpt

    match procVar,userVar,machVar with
    | Some(v), _, _
    | _, Some(v), _
    | _, _, Some(v)
        -> Some(v)
    | _ -> None


// --------------------------------------------------------------------------------------
// Initialisation and fixup actions

Target.create "Init" <| fun _ ->
    Fake.IO.File.delete (__SOURCE_DIRECTORY__ @@ ".paket/Paket.Restore.Targets")
    Fake.IO.Directory.delete (__SOURCE_DIRECTORY__ @@ "packet_files")

Target.create "KillCreated" <| fun _ ->
    Fake.Core.Process.killAllCreatedProcesses()

Target.create "KillZombies" <| fun _ ->
    Fake.Core.Process.killAllByName "issie.exe"
    Fake.Core.Process.killAllByName "node"
    Fake.Core.Process.killAllByName "dotnet"



// --------------------------------------------------------------------------------------
// Set configuration mode based on target

Target.create "ConfigDebug" <| fun _ ->
    FakeVar.set "configuration" "Debug"

Target.create "ConfigRelease" <| fun _ ->
    FakeVar.set "configuration" "Release"

// --------------------------------------------------------------------------------------
// Generate assembly info files with the right version & up-to-date information

Target.create "AssemblyInfo" <| fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ AssemblyInfo.Title (projectName)
          AssemblyInfo.Product project
          AssemblyInfo.Description summary
          AssemblyInfo.Version release.AssemblyVersion
          AssemblyInfo.FileVersion release.AssemblyVersion
          AssemblyInfo.Configuration <| configuration()
          AssemblyInfo.InternalsVisibleTo (sprintf "%s.Tests" projectName) ]

    let getProjectDetails projectPath =
        let projectName = Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! srcGlob
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
        match projFileName with
        | Fsproj -> AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
        | Csproj -> AssemblyInfoFile.createCSharp ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
        | Vbproj -> AssemblyInfoFile.createVisualBasic ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
        | Shproj -> () )

// --------------------------------------------------------------------------------------
// Update package.json version & name      

Target.create "PackageJson" <| fun _ ->
    let setValues (current: Json.JsonPackage) =
        { current with
            Name = Str.toKebabCase project |> Some
            Description = summary |> Some
            Version = release.NugetVersion |> Some
            Repository = 
                match current.Repository with
                | Some(r) ->
                    { r with
                        Json.RepositoryValue.Type = "git" |> Some
                        Json.RepositoryValue.Url = repo + ".git" |> Some }
                | _ ->
                    { Json.RepositoryValue.Type = "git" |> Some
                      Json.RepositoryValue.Url = repo + ".git" |> Some
                      Json.RepositoryValue.Directory = None }
                |> Some
            Author = author |> Some
            License = (File.readLine(__SOURCE_DIRECTORY__ @@ "LICENSE.md").Split(' ')) |> Array.head |> Some
            Bugs = { Json.BugsValue.Url = (repo + "/issues" |> Some) } |> Some
            Homepage = repo |> Some }
    
    Json.setJsonPkg setValues

// --------------------------------------------------------------------------------------
// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the
// src folder to support multiple project outputs

Target.create "CopyBinaries" <| fun _ ->
    !! srcGlob
    -- (__SOURCE_DIRECTORY__ @@ "src/**/*.shproj")
    -- (__SOURCE_DIRECTORY__ @@ "src/**/Electron.Fable.fsproj")
    |> Seq.map (fun f -> ((Path.getDirectory f) @@ "bin" @@ configuration(), "bin" @@ (Path.GetFileNameWithoutExtension f)))
    |> Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))

// --------------------------------------------------------------------------------------
// Clean tasks

Target.create "Clean" <| fun _ ->
    let clean() =
        !! (__SOURCE_DIRECTORY__  @@ "tests/**/bin")
        ++ (__SOURCE_DIRECTORY__  @@ "tests/**/obj")
        ++ (__SOURCE_DIRECTORY__  @@ "tools/bin")
        ++ (__SOURCE_DIRECTORY__  @@ "tools/obj")
        ++ (__SOURCE_DIRECTORY__  @@ "src/**/bin")
        ++ (__SOURCE_DIRECTORY__  @@ "src/**/obj")
        |> Seq.toList
        |> List.append [bin; temp; objFolder; dist; fable]
        |> Shell.cleanDirs
    TaskRunner.runWithRetries clean 99

Target.create "CleanDocs" <| fun _ ->
    let clean() =
        Shell.cleanDirs ["docs"]
    TaskRunner.runWithRetries clean 99

Target.create "PostBuildClean" <| fun _ ->
    let clean() =
        !! srcGlob
        -- (__SOURCE_DIRECTORY__ @@ "src/**/*.shproj")
        |> Seq.map (
            (fun f -> (Path.getDirectory f) @@ "bin" @@ configuration()) 
            >> (fun f -> Directory.EnumerateDirectories(f) |> Seq.toList )
            >> (fun fL -> fL |> List.map (fun f -> Directory.EnumerateDirectories(f) |> Seq.toList)))
        |> (Seq.concat >> Seq.concat)
        |> Seq.iter Directory.delete
    TaskRunner.runWithRetries clean 99

Target.create "PostPublishClean" <| fun _ ->
    let clean() =
        !! (__SOURCE_DIRECTORY__ @@ "src/**/bin" @@ configuration() @@ "/**/publish")
        |> Seq.iter Directory.delete
    TaskRunner.runWithRetries clean 99

Target.create "CleanElectronBin"  <| fun _ ->
    let netCores =
        netCoreVersions
        |> List.map (fun s -> __SOURCE_DIRECTORY__ @@ "bin/Core" @@ s)

    let clean() =
        !! (__SOURCE_DIRECTORY__  @@ "bin/Main")
        ++ (__SOURCE_DIRECTORY__  @@ "bin/Renderer")
        ++ (__SOURCE_DIRECTORY__  @@ "bin/Common")
        ++ (__SOURCE_DIRECTORY__  @@ "bin/Simulator")
        ++ (__SOURCE_DIRECTORY__  @@ "bin/WidthInferer")
        |> List.ofSeq
        |> Shell.deleteDirs

        let runtimeDirs =
            runTimes 
            |> List.map (fun s ->
                netCores 
                |> List.map (fun coreVer -> coreVer @@ s @@ "**"))
            |> List.concat

        match netCores |> List.tryHead with
        | Some(h) ->
            !! (h @@ "**")
            |> (fun src -> List.fold foldIncludeGlobs src netCores.Tail)
            |> (fun src -> List.fold foldExcludeGlobs src runtimeDirs)
            |> Seq.iter Shell.rm
        | _ -> ()
    TaskRunner.runWithRetries clean 99

// --------------------------------------------------------------------------------------
// Restore tasks

let restoreSolution () =
    solutionFile
    |> DotNet.restore id

Target.create "Restore" <| fun _ ->
    TaskRunner.runWithRetries restoreSolution 5

// Add task to make Node.js cli ready
Target.create "YarnInstall" <| fun _ ->
    let setParams (defaults:Yarn.YarnParams) =
        if Environment.isLinux then
            defaults
        else
            { defaults with
                Yarn.YarnParams.YarnFilePath = (__SOURCE_DIRECTORY__ @@ "packages/tooling/Yarnpkg.Yarn/content/bin/yarn.cmd")
            }
    Yarn.install setParams

// Build Key.json if necessary from GD_KEY env variable
Target.create "BuildKeys" <| fun _ ->
    let jsonPath = 
        __SOURCE_DIRECTORY__ @@ "src/Core/Key.json"
        |> FileInfo.ofPath
    let key = 
        let insertKey s = sprintf "{ \"key\": \"%s\" }" s

        getEnvFromAllOrNone "GD_KEY"
        |> Option.map (insertKey)
        |> defaultArg <| ""
    TraceSecrets.register "<GD_KEY Json>" key
    
    if key <> "" && jsonPath.Exists |> not then
        File.writeString false jsonPath.FullName key
    else ()

// --------------------------------------------------------------------------------------
// Build tasks

Target.create "Build" <| fun _ ->
    let setParams (defaults:MSBuildParams) =
        { defaults with
            Verbosity = Some(Quiet)
            Targets = ["Build"]
            Properties =
                [
                    "Optimize", "True"
                    "DebugSymbols", "True"
                    "Configuration", configuration()
                    "Version", release.AssemblyVersion
                    "GenerateDocumentationFile", "true"
                    "DependsOnNETStandard", "true"
                ]
         }
    restoreSolution()
    MSBuild.build setParams solutionFile

Target.create "BuildElectron" <| fun _ ->
    Npm.exec "rebuild node-sass" id
    Yarn.exec "compile" id

// Run Dev mode
Target.create "Dev" <| fun _ ->
    Yarn.exec "dev" id

// Build artifacts
Target.create "DistWin" <| fun _ ->
    Yarn.exec "distwin" id

// Gets latest docker image to build for linux
Target.create "PullDockerImage" <| fun _ ->
    CmdLine.Empty
    |> CmdLine.append "pull"
    |> CmdLine.append "electronuserland/builder"
    |> CmdLine.toString
    |> CreateProcess.fromRawCommandLine "docker"
    |> CreateProcess.ensureExitCodeWithMessage "Pulling electronuserland/builder docker container failed."
    |> Proc.run
    |> ignore

// Build artifacts
Target.create "DistLinux" <| fun _ ->
    let sandboxPath = @"./node_modules/electron/dist/chrome-sandbox"

    CmdLine.Empty
    |> CmdLine.append "run"
    |> CmdLine.append "--rm"
    |> CmdLine.appendRaw (sprintf "-v %s:/project" __SOURCE_DIRECTORY__)
    |> CmdLine.appendRaw "-v electron:/root/.cache/electron"
    |> CmdLine.appendRaw "-v electron-builder:/root/.cache/electron-builder electronuserland/builder"
    |> CmdLine.appendRaw (sprintf "/bin/bash -c \"chown -R root %s && chmod 4755 %s && yarn --link-duplicates --pure-lockfile && yarn distLinux\"" sandboxPath sandboxPath)
    |> CmdLine.toString
    |> CreateProcess.fromRawCommandLine "docker"
    |> CreateProcess.ensureExitCodeWithMessage "Failed to build linux image."
    |> Proc.run
    |> ignore

// --------------------------------------------------------------------------------------
// Create differentials for updating

Target.create "CreateDiffs" <| fun _ ->
    let latestTag = (Git.CommandHelper.runSimpleGitCommand __SOURCE_DIRECTORY__ "describe --tag").Split('-').[0]
    let latestRelease =
        if Version(release.NugetVersion) <= Version(latestTag.Substring(1)) then 
            failwith "Cannot create diff of older version"
        else
            sprintf "https://github.com/Shmew/MordhauBuddy/releases/download/%s" latestTag

    let downloadSignature (fi: FileInfo) =
        let file =
            match fi.Extension with
            | ext when ext = ".exe" -> sprintf "MordhauBuddy.Setup.%s%s.sig" (latestTag.Substring(1)) ext
            | ext when ext = ".AppImage" -> sprintf "MordhauBuddy-%s%s.sig" (latestTag.Substring(1)) ext
            | _ -> failwith "Invalid file extention for generating delta"
        
        sprintf "%s/%s" latestRelease file
        |> downloadFile (fi.Directory.FullName @@ file)

    !! (__SOURCE_DIRECTORY__ @@ "dist/linux-x64/*.AppImage")
    ++ (__SOURCE_DIRECTORY__ @@ "dist/win-x64/*.exe")
    |> List.ofSeq
    |> List.distinct
    |> List.iter (fun f ->
        let fi = FileInfo.ofPath(f)
        genSigNew (sprintf "%s.sig" fi.Name) fi
        
        downloadSignature fi
        |> FileInfo.ofPath
        |> genDelta fi)

// --------------------------------------------------------------------------------------
// Publish net core applications

Target.create "PublishDotNet" <| fun _ ->
    let runPublish (project: string) (framework: string) =
        let buildWithRunTime (rt: string) =
            let setParams (defaults:MSBuildParams) =
                { defaults with
                    Verbosity = Some(Quiet)
                    Targets = ["Publish"]
                    Properties =
                        [
                            "Optimize", "True"
                            "DebugSymbols", "True"
                            "Configuration", configuration()
                            "Version", release.AssemblyVersion
                            "GenerateDocumentationFile", "true"
                            "TargetFramework", framework
                            "SelfContained", "true"
                            "RuntimeIdentifier", rt
                        ]
                }
            MSBuild.build setParams project
        runTimes |> List.iter buildWithRunTime

    !! srcGlob
    -- (__SOURCE_DIRECTORY__ @@ "src/**/*.shproj")
    -- (__SOURCE_DIRECTORY__ @@ "src/**/*.vbproj")
    |> Seq.map
        ((fun f -> (((Path.getDirectory f) @@ "bin" @@ configuration()), f) )
        >>
        (fun f ->
            Directory.EnumerateDirectories(fst f) 
            |> Seq.filter (fun frFolder -> frFolder.Contains("netcoreapp"))
            |> Seq.map (fun frFolder -> DirectoryInfo(frFolder).Name), snd f))
    |> Seq.iter (fun (l,p) -> l |> Seq.iter (runPublish p))

// --------------------------------------------------------------------------------------
// Lint and format source code to ensure consistency

Target.create "Format" <| fun _ ->
    let config =
        { FormatConfig.Default with
            PageWidth = 120
            SpaceBeforeColon = false }
        
    fsSrcAndTest
    |> List.ofSeq
    |> formatCode config
    |> Async.RunSynchronously
    |> printfn "Formatted files: %A"

Target.create "Lint" <| fun _ ->
    fsSrcAndTest
    -- (__SOURCE_DIRECTORY__  @@ "src/**/AssemblyInfo.*")
    |> (fun src -> List.fold foldExcludeGlobs src relaxedNameLinting)
    |> (fun fGlob ->
        match fsRelaxedNameLinting with
        | Some(glob) ->
            [(false, fGlob); (true, glob)]
        | None -> [(false, fGlob)])
    |> Seq.map (fun (b,glob) -> (b,glob |> List.ofSeq))
    |> List.ofSeq
    |> FSharpLinter.lintFiles (__SOURCE_DIRECTORY__ @@ "bin/LintResults.xml")

// --------------------------------------------------------------------------------------
// Validate JavaScript dependencies

Target.create "ValidateJSPackages" <| fun _ ->
    let validate () =
        fsProjGlob
        |> Seq.iter (fun file ->
            dotnet.femto id
                (sprintf "--resolve %s" file))
    TaskRunner.runWithRetries validate 5

// --------------------------------------------------------------------------------------
// Run the unit test binaries

Target.create "RunTests" <| fun _ ->
    !! ("tests/**/bin" @@ configuration() @@ "**" @@ "*Tests.exe")
    |> Seq.iter (fun f ->
        CreateProcess.fromCommand(setCmd f [])
        |> CreateProcess.withTimeout (TimeSpan.MaxValue)
        |> CreateProcess.ensureExitCodeWithMessage "Tests failed."
        |> Proc.run
        |> ignore)

// --------------------------------------------------------------------------------------
// Generate Paket load scripts
Target.create "LoadScripts" <| fun _ ->
    let frameworks =
        __SOURCE_DIRECTORY__ @@ "bin"
        |> Directory.EnumerateDirectories
        |> Seq.map (fun d ->
            Directory.EnumerateDirectories d
            |> Seq.map (fun f -> DirectoryInfo(f).Name)
            |> List.ofSeq)
        |> List.ofSeq
        |> List.reduce List.append
        |> List.distinct
        |> List.reduce (fun acc elem -> sprintf "%s --framework %s" elem acc)
        |> function
        | e when e.Length > 0 ->
            Some (sprintf "--framework %s" e)
        | _ -> None

    let arguments =
        [Some("generate-load-scripts"); frameworks]
        |> List.choose id
        |> List.reduce (fun acc elem -> sprintf "%s %s" acc elem)

    arguments
    |> CreateProcess.fromRawCommandLine ((__SOURCE_DIRECTORY__ @@ ".paket") @@ "paket.exe")
    |> CreateProcess.withTimeout (TimeSpan.MaxValue)
    |> CreateProcess.ensureExitCodeWithMessage "Failed to generate paket load scripts."
    |> Proc.run
    |> ignore

// --------------------------------------------------------------------------------------
// Generate the documentation


// Paths with template/source/output locations
let content     = __SOURCE_DIRECTORY__ @@ "docsrc/content"
let output      = __SOURCE_DIRECTORY__ @@ "docs"
let files       = __SOURCE_DIRECTORY__ @@ "docsrc/files"
let templates   = __SOURCE_DIRECTORY__ @@ "docsrc/tools/templates"
let formatting  = __SOURCE_DIRECTORY__ @@ "packages/formatting/FSharp.Formatting"
let toolPath    = __SOURCE_DIRECTORY__ @@ "packages/formatting/FSharp.Formatting.CommandTool/tools/fsformatting.exe"
let docTemplate = "docpage.cshtml"

Target.create "LocalDocs" <| fun _ ->
    FakeVar.set "Website" website // for now we never generate local docs
Target.create "ReleaseDocs" <| fun _ ->
    FakeVar.set "Website" website

// Specify more information about your project
let info () =
  [ "project-name", project
    "project-author", author
    "project-summary", summary
    "project-repo", repo
    "root", FakeVar.getOrDefault "Website" website ]

let referenceBinaries = []

let layoutRootsAll = new Collections.Generic.Dictionary<string, string list>()
layoutRootsAll.Add("en",[   templates;
                            formatting @@ "templates"
                            formatting @@ "templates/reference" ])

Target.create "ReferenceDocs" <| fun _ ->
    Directory.ensure (output @@ "reference")

    let lDirs = 
        DirectoryInfo.getSubDirectories <| DirectoryInfo bin
        |> Array.map DirectoryInfo.getSubDirectories
        |> Array.reduce Array.append
        |> Array.map (fun x -> x.FullName.ToLower())
        |> List.ofArray
    printfn "lDirs=%A" lDirs
    let binaries () =
        let manuallyAdded =
            referenceBinaries
            |> List.map (fun b -> bin @@ b)

        let conventionBased =
            DirectoryInfo.getSubDirectories <| DirectoryInfo bin
            |> Array.collect (fun d ->
                let name, dInfo =
                    let netFrameworkBin =
                        DirectoryInfo.getSubDirectories d |> Array.filter(fun x -> x.FullName.ToLower().Contains("net4"))
                    let netCoreBin =
                        DirectoryInfo.getSubDirectories d |> Array.filter(fun x -> x.FullName.ToLower().Contains("netcoreapp"))

                    match netFrameworkBin.Length > 0 with
                    | true ->
                        d.Name, netFrameworkBin |> Array.head
                    | false ->
                        d.Name, netCoreBin |> Array.head

                dInfo.GetFiles()
                |> Array.filter (fun x ->
                    x.Name.ToLower() = (sprintf "%s.dll" name).ToLower() 
                        || x.Name.ToLower() = (sprintf "%s.exe" name).ToLower())
                |> Array.map (fun x -> x.FullName))
            |> List.ofArray

        conventionBased @ manuallyAdded
    printfn "Binaries=%A" (binaries())
    binaries()
    |> FSFormatting.createDocsForDlls (fun args ->
        { args with
            OutputDirectory = output @@ "reference"
            LayoutRoots =  layoutRootsAll.["en"]
            ProjectParameters =  info()
            LibDirs = lDirs
            ToolPath = toolPath
            SourceRepository = repo @@ "tree/master" })

let copyFiles () =
    Shell.copyRecursive files output true
    |> Trace.logItems "Copying file: "
    Directory.ensure (output @@ "content")
    Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true
    |> Trace.logItems "Copying styles and scripts: "

Target.create "Docs" <| fun _ ->
    File.delete "docsrc/content/release-notes.md"
    Shell.copyFile "docsrc/content/" "RELEASE_NOTES.md"
    Shell.rename "docsrc/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

    DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath templates)
    |> Seq.iter (fun d ->
                    let name = d.Name
                    if name.Length = 2 || name.Length = 3 then
                        layoutRootsAll.Add(
                                name, [templates @@ name
                                       formatting @@ "templates"
                                       formatting @@ "templates/reference" ]))
    copyFiles ()

    for dir in  [ content ] do
        let langSpecificPath(lang, path:string) =
            path.Split([|'/'; '\\'|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.exists(fun i -> i = lang)
        let layoutRoots =
            let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, dir))
            match key with
            | Some lang -> layoutRootsAll.[lang]
            | None -> layoutRootsAll.["en"] // "en" is the default language

        FSFormatting.createDocs (fun args ->
            { args with
                Source = content
                OutputDirectory = output
                LayoutRoots = layoutRoots
                ProjectParameters  = info()
                Template = docTemplate })

Target.create "GenerateDocs" ignore

// --------------------------------------------------------------------------------------
// Release Scripts

Target.create "GitPush" <| fun p ->
    let msg =
        p.Context.Arguments
        |> List.choose (fun s ->
            match s.StartsWith("--Msg=") with
            | true -> Some(s.Substring 6)
            | false -> None)
        |> List.tryHead
        |> function
        | Some(s) -> s
        | None -> (sprintf "Bump version to %s" release.NugetVersion)

    Git.Staging.stageAll ""
    Git.Commit.exec "" msg
    Git.Branches.push ""

Target.create "GitTag" <| fun _ ->
    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" "origin" release.NugetVersion

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build -t <Target>' to override

Target.create "All" ignore
Target.create "Release" ignore
Target.create "UpdateDocs" ignore
Target.create "AllDev" ignore
Target.create "QDev" <| fun _ ->
    Yarn.exec "dev" id

"Clean"
  ==> "AssemblyInfo"
  ==> "Restore"
  ==> "PackageJson"
  ==> "YarnInstall"
  ==> "Build"
  ==> "BuildElectron"
  ==> "PostBuildClean" 
  ==> "CopyBinaries"

"Build" ==> "RunTests"

"Build"
  ==> "PostBuildClean"
  ==> "PublishDotNet"
  ==> "PostPublishClean"
  ==> "CopyBinaries"

"Restore" ==> "Lint"
"Restore" ==> "Format"

"Lint" 
  ?=> "Build"
  ?=> "RunTests"
  ?=> "CleanDocs"

"CopyBinaries"
  ==> "CleanDocs"
  ==> "Docs"
  ==> "ReferenceDocs"
  ==> "GenerateDocs"
  ?=> "CleanElectronBin"

"Clean" 
  ==> "GitPush"
  ?=> "GitTag"

"All" <== ["RunTests"; "GenerateDocs"; "CleanElectronBin"]

"AllDev" <== ["RunTests"; "CleanElectronBin"]

"All" ?=> "Release"

"LocalDocs" ?=> "All"
"ReleaseDocs" ?=> "All"

"ConfigDebug" ?=> "Clean"
"ConfigRelease" ?=> "Clean"

"PullDockerImage"
  ==> "DistLinux"
  ?=> "CreateDiffs"

"YarnInstall"
  ==> "DistWin"
  ?=> "CreateDiffs"

"DistWin" ?=> "DistLinux"

"Dev" <== ["All"; "LocalDocs"; "ConfigDebug"]

"DistWin" <== ["All"; "ReleaseDocs"; "ConfigRelease"]
"DistLinux" <== ["All"; "ReleaseDocs"; "ConfigRelease"]
"Release" <== ["All"; "ReleaseDocs"; "ConfigRelease"; "DistWin"; "DistLinux"; "CreateDiffs"]
"UpdateDocs" <== ["All"; "ReleaseDocs"; "ConfigRelease"]

Target.runOrDefaultWithArguments "Dev"
