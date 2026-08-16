#!/usr/bin/env -S dotnet fsi
#r "nuget: Fake.Core.Target"
#r "nuget: Fake.JavaScript.Npm, 6.0.0"
//
// Boilerplate to run FAKE under fsi allowing it to work with Dotnet 7
// this seems going forward to be the best way to run FAKE
//
System.Environment.GetCommandLineArgs()
|> Array.skip 2 // skip fsi.exe; build.fsx
|> Array.toList
|> Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__
|> Fake.Core.Context.RuntimeContext.Fake
|> Fake.Core.Context.setExecutionContext
// end of boilerplate

open System
open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.JavaScript

Target.create "CleanDev" (fun _ ->
  printfn "starting clean..."
  let dirsToClean =
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "dist*"
    ++ ".fable*"
    ++ "src/Renderer/fable_modules"
    ++ "src/Main/fable_modules"
  printfn "Cleaning directories: %A" dirsToClean
  Shell.cleanDirs dirsToClean
  printfn "Clean complete."
)

Target.create "Clean" (fun _ ->
  !! "src/**/bin"
  ++ "src/**/obj"
  ++ "dist"
  ++ ".fable"
  |> Shell.cleanDirs
  Target.run 1 "KillZombies" []
)

Target.create "CleanFableJS" <| fun _ ->
    !! (__SOURCE_DIRECTORY__  @@ "src/**/*.fs.js")
    ++ (__SOURCE_DIRECTORY__  @@ "src/**/*.map")
    |> Seq.toList
    |> File.deleteAll

Target.create "CleanNode" <| fun _ ->
    Shell.cleanDir (__SOURCE_DIRECTORY__  @@ "node_modules")
    File.delete (__SOURCE_DIRECTORY__ @@ "package-lock.json")

Target.create "DotnetRestore" (fun _ ->
    // One project at a time. MSBuild restores a solution's projects on as many nodes as it has
    // cores, and every project's restore spawns a `dotnet paket restore` of its own, so a whole
    // set of them run at once - which the arm64 macOS CI runner does not have the memory for. It
    // killed one during the v6.0.21 release build: `Paket.Restore.targets: error MSB3073: the
    // command "dotnet paket restore" exited with code 137`, 137 being 128 + SIGKILL. Nothing was
    // wrong with the restore itself; the same one succeeds on Linux, on Windows and in the Tests
    // workflow.
    //
    // -m:1 is what stops the project restores overlapping. --disable-parallel is NuGet's own
    // switch, covering the parallelism inside a single restore. Serial costs seconds here, since
    // this is bounded by downloading packages rather than by the CPU.
    Shell.Exec("dotnet","restore issie.sln -m:1 --disable-parallel") |> ignore)

Target.create "NpmInstall" (fun _ ->
  Npm.exec "ci" id
)

Target.create "Build" (fun _ ->
  Npm.run "compile" id
)

Target.create "Dev" (fun _ ->
  Npm.run "dev" id
)

Target.create "Dist" (fun _ ->
  Npm.run "dist" id
)

Target.create "DistDir" (fun _ ->
  // "pack" is the --dir (unpacked directory, no installer) variant of "dist"
  Npm.run "pack" id
)

// Kill what an interrupted dev session leaves behind, via scripts/clean-dev.js.
//
// This used to be three killAllByName calls - "issie.exe", "node" and "dotnet" - which match on
// the executable name alone. So it killed the `dotnet fsi` running this script (and, through the
// Clean target, killed itself partway through a clean), the npm that may have started it, and
// every unrelated dotnet and node process on the machine: another project's dev server, an IDE's
// compiler server. clean-dev.js matches on what a process IS - fable, Electron, the webpack dev
// server - kills the supervisors last, and never matches itself.
Target.create "KillZombies" <| fun _ ->
    let code = Shell.Exec("node", "scripts/clean-dev.js", __SOURCE_DIRECTORY__)
    if code <> 0 then failwithf "scripts/clean-dev.js exited with %d" code

// Build order

"CleanFableJS"
  ==> "Clean"

"CleanFableJS"
  ==> "CleanDev"

"CleanDev"
  ==> "DotnetRestore"
  ==> "NpmInstall"
  ==> "Build"

"NpmInstall"
  ==> "Dev"

"NpmInstall"
  ==> "Dist"

"NpmInstall"
  ==> "DistDir"

// start build
Target.runOrDefaultWithArguments "Dev"
