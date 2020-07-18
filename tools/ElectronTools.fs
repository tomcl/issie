namespace Tools.Electron

[<RequireQualifiedAccess>]
module Json =
    open FSharp.Json
    open Fake.IO
    open Fake.IO.FileSystemOperators

    [<RequireQualifiedAccess>]
    type RepositoryValue =
        { Type : string option
          Url : string option
          Directory : string option }

    type BugsValue =
        { Url : string option }

    [<NoComparison>]
    type BinValue =
        | OnlyOne of string
        | Multiple of obj []

    type ManValue =
        | OnlyOne of string
        | Multiple of string []

    type DirectoriesValue =
        { Lib : string option
          Bin : string option
          Man : string option
          Doc : string option
          Example : string option
          Test : string option }

    type DistValue =
        { Shasum : string option
          Tarball : string option }

    type EsnextValue =
        { Main : string
          Browser : string }

    [<NoComparison>]
    type JsonPackage =
        { Name : string option
          Version : string option
          Description : string option
          Keywords : string [] option
          Homepage : string option
          Bugs : BugsValue option
          License : string option
          Author : string option
          Contributors : string [] option
          Maintainers : string [] option
          Files : string [] option
          Main : string option
          Bin : BinValue option
          Type : string option
          Typings : string option
          Man : ManValue option
          Directories : DirectoriesValue option
          Repository : RepositoryValue option
          Scripts : Map<string, string> option
          Config : obj option
          ElectronWebpack : obj option
          Build : obj option
          Dependencies : obj option
          DevDependencies : obj option
          OptionalDependencies : obj option
          PeerDependencies : obj option
          Resolution : obj option
          Engines : obj option
          Os : string [] option
          Cpu : string [] option
          Private : bool option
          PublishConfig : obj option
          Dist : DistValue option
          Readme : string option
          Module : string option
          Browser : string option
          Proxy : string option
          Eslint : obj option
          Jest : obj option
          Esnext : EsnextValue option
          Jspm : obj option
          Workspaces : string [] option }

    let config =
        JsonConfig.create
            (jsonFieldNaming = Json.lowerCamelCase, allowUntyped = true, serializeNone = SerializeNone.Omit)
    let jsonPath = (__SOURCE_DIRECTORY__ @@ "../package.json")
    let getJsonPkg() = File.readAsString jsonPath |> Json.deserializeEx<JsonPackage> config

    let setJsonPkg (f : JsonPackage -> JsonPackage) =
        getJsonPkg()
        |> f
        |> Json.serializeEx config
        |> File.writeString false jsonPath

module Str =
    open System.Text.RegularExpressions

    let setLowerFirst (s : string) = sprintf "%s%s" (s.Substring(0, 1).ToLower()) (s.Substring(1))
    let toKebabCase (s : string) =
        MatchEvaluator(fun m -> "-" + m.Value.ToLower()) |> (fun m -> Regex.Replace(setLowerFirst s, "[A-Z]", m))
