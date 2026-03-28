module Build

open System
open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.JavaScript

let initFakeContext (args: string[]) =
    args
    |> Array.skip 1 // skip executable path
    |> Array.toList
    |> Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__
    |> Fake.Core.Context.RuntimeContext.Fake
    |> Fake.Core.Context.setExecutionContext

[<EntryPoint>]
let main args =
    initFakeContext (Environment.GetCommandLineArgs())

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
        !! (__SOURCE_DIRECTORY__ @@ "src/**/*.fs.js")
        ++ (__SOURCE_DIRECTORY__ @@ "src/**/*.map")
        |> Seq.toList
        |> File.deleteAll

    Target.create "CleanNode" <| fun _ ->
        Shell.cleanDir (__SOURCE_DIRECTORY__ @@ "node_modules")
        File.delete (__SOURCE_DIRECTORY__ @@ "package-lock.json")

    Target.create "DotnetRestore" (fun _ ->
        Shell.Exec("dotnet", "restore issie.sln") |> ignore)

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
      Npm.run "dist:dir" id
    )

    Target.create "KillZombies" <| fun _ ->
        Fake.Core.Process.killAllByName "issie.exe"
        Fake.Core.Process.killAllByName "node"
        Fake.Core.Process.killAllByName "dotnet"

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

    "CleanFableJS"
     ==> "dev"

    // start build
    Target.runOrDefaultWithArguments "Dev"
    0
