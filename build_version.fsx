#!/usr/bin/env -S dotnet fsi
#r "nuget: Fake.Core.Target"

open System
open System.IO
open System.Text.RegularExpressions
open Fake.Core

System.Environment.GetCommandLineArgs()
|> Array.skip 2 // skip fsi.exe; build.fsx
|> Array.toList
|> Fake.Core.Context.FakeExecutionContext.Create false __SOURCE_FILE__
|> Fake.Core.Context.RuntimeContext.Fake
|> Fake.Core.Context.setExecutionContext

(* ---------------------------------------------------- Constants --------------------------------------------------- *)

let VersionfsPath = "src/Renderer/Interface/Version.fs"
let PackagejsonPath = "package.json"
let IntRegex = Regex "[0-9]+"
let VerStrRegex = Regex "[0-9]+\.[0-9]+\.[0-9]+"

(* ----------------------------------------------------- Utility ---------------------------------------------------- *)

/// <summary>Returns version tuple from given Version.fs file.</summary>
/// <param name="path">Path to Version.fs file.</param>
/// <returns>Version tuple recorded in file.</returns>
let getVerTupFromVersionfs (path: string): int*int*int =
    File.ReadAllLines (__SOURCE_DIRECTORY__ + "/" + path)
    |> Seq.tryFind (fun line -> line.Contains "let VERSION = [")
    |> function
        | Some line -> line
        | None -> failwithf "%s does not contain version string" path
    |> String.split '\"'
    |> List.filter IntRegex.IsMatch
    |> List.map int
    |> function
        | [ major; minor; patch] -> major, minor, patch
        | _ -> failwithf "%s has incorrect version format" path

/// <summary>Returns version line in Version.fs format.</summary>
/// <param name="verTup">Version tuple.</param>
/// <returns>Formatted version line.</returns>
let getVersionfsStrFromVerTup (verTup: int*int*int): string =
    let major, minor, patch = verTup
    sprintf "let VERSION = [ \"%d\" ; \"%d\" ; \"%d\" ]" major minor patch

/// <summary>Writes version numver in Version.fs file. Function has side effect!</summary>
/// <param name="path">Path to Version.fs file.</param>
/// <param name="vt">Version tuple.</param>
let setVersionfsFromVerTup (path: string) (vt: int*int*int): unit =
    let verstr = getVersionfsStrFromVerTup vt
    File.ReadAllLines (__SOURCE_DIRECTORY__ + "/" + path)
    |> Seq.map (fun line -> if line.Contains "let VERSION = [" then verstr else line)
    |> (fun seq -> ((__SOURCE_DIRECTORY__ + "/" + path), seq))
    |> File.WriteAllLines 

/// <summary>Returns version tuple from given package.json file.</summary>
/// <param name="path">Path to package.json file.</param>
/// <returns>Version tuple recorded in file.</returns>
let getVerTupFromPackagejson (path: string): int*int*int =
    File.ReadAllLines (__SOURCE_DIRECTORY__ + "/" + path)
    |> Seq.tryFind (fun line -> line.Contains "\"version\": ")
    |> function
        | Some line -> line
        | None -> failwithf "%s does not contain version string" path
    |> String.split '\"'
    |> List.filter VerStrRegex.IsMatch
    |> function
        | [ verstr] -> verstr
        | _ -> failwithf "%s has incorrect version format" path
    |> String.split '.'
    |> function
        | [ major; minor; patch ] -> (int major), (int minor), (int patch)
        | _ -> failwithf "%s has incorrect version format" path

/// <summary>Returns version line in package.json format.</summary>
/// <param name="verTup">Version tuple.</param>
/// <returns>Formatted version line.</returns>
let getPackagejsonStrFromVerTup (verTup: int*int*int): string =
    let major, minor, patch = verTup
    sprintf "  \"version\": \"%d.%d.%d\"," major minor patch

/// <summary>Writes version numver in package.json file. Function has side effect!</summary>
/// <param name="path">Path to package.json file.</param>
/// <param name="vt">Version tuple.</param>
let setPackagejsonFromVerTup (path: string) (vt: int*int*int): unit =
    let verstr = getPackagejsonStrFromVerTup vt
    File.ReadAllLines (__SOURCE_DIRECTORY__ + "/" + path)
    |> Seq.map (fun line -> if line.Contains "\"version\": " then verstr else line)
    |> (fun seq -> ((__SOURCE_DIRECTORY__ + "/" + path), seq))
    |> File.WriteAllLines 


(* -------------------------------------------------- Build Targets ------------------------------------------------- *)

Target.create "GetVersion" (fun _ ->
    Trace.log "--- Get Current Version ---"
    let vvt = getVerTupFromVersionfs VersionfsPath
    let pvt = getVerTupFromPackagejson PackagejsonPath
    Trace.log (sprintf "* Version.fs version   : %A" vvt)
    Trace.log (sprintf "* package.json version : %A" pvt)
    match vvt = pvt with
    | true -> Trace.log "* version string matched"
    | false -> failwithf "version tuple mismatch, terminate"
)

Target.create "SetVersion" (fun _ ->
    Trace.log "--- SetVersion ---"
    let getVerEnvVar (verType: string) (verDefault: int)= 
        verType
        |> Fake.Core.Environment.environVarOrNone
        |> function
            | Some vs -> if IntRegex.IsMatch vs then Some (int vs) else None
            | None -> None
        |> Option.defaultValue verDefault
    let omajor, ominor, opatch = getVerTupFromVersionfs VersionfsPath
    let nvt = getVerEnvVar "major" omajor, getVerEnvVar "minor" ominor, getVerEnvVar "patch" opatch
    Trace.log (sprintf "* new version tuple: %A" nvt)
    setVersionfsFromVerTup VersionfsPath nvt
    setPackagejsonFromVerTup PackagejsonPath nvt
)

Target.create "BumpMajor" (fun _ ->
    Trace.log "--- Bump Major Version Number ---"
    let major, minor, patch = getVerTupFromVersionfs VersionfsPath
    let nvt = major+1, 0, 0
    Trace.log (sprintf "* new version tuple: %A" nvt)
    setVersionfsFromVerTup VersionfsPath nvt
    setPackagejsonFromVerTup PackagejsonPath nvt
)

Target.create "BumpMinor" (fun _ ->
    Trace.log "--- Bump Minor Version Number ---"
    let major, minor, patch = getVerTupFromVersionfs VersionfsPath
    let nvt = major, minor+1, 0
    Trace.log (sprintf "* new version tuple: %A" nvt)
    setVersionfsFromVerTup VersionfsPath nvt
    setPackagejsonFromVerTup PackagejsonPath nvt
)

Target.create "BumpPatch" (fun _ ->
    Trace.log "--- Bump Patch Version Number ---"
    let major, minor, patch = getVerTupFromVersionfs VersionfsPath
    let nvt = major, minor, patch+1
    Trace.log (sprintf "* new version tuple: %A" nvt)
    setVersionfsFromVerTup VersionfsPath nvt
    setPackagejsonFromVerTup PackagejsonPath nvt
)

(* ----------------------------------------------- Define Dependancies ---------------------------------------------- *)

open Fake.Core.TargetOperators

"GetVersion"
    ==> "SetVersion"

"GetVersion"
    ==> "BumpMajor"

"GetVersion"
    ==> "BumpMinor"

"GetVersion"
    ==> "BumpPatch"

(* -------------------------------------------------- Default Build ------------------------------------------------- *)

Target.runOrDefault "GetVersion"
