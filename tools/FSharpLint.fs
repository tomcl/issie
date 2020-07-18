namespace Tools.Linting

open System
open FSharpLint.Application.ConfigurationManager

type internal XmlResult =
    | Success
    | Failure

type internal FileLintResult =
    { FileName : string
      FileResult : XmlResult
      FileSuccess : bool
      FileTime : float
      LintResponse : string option }

type internal LintResult =
    { Date : DateTime
      FileTotal : int
      ErrorCount : int
      FailureCount : int
      Time : TimeSpan
      LintVersion : Version
      Name : string
      Result : XmlResult
      Success : bool
      Duration : float
      Files : FileLintResult list }

module internal LintResults =
    open System.IO
    open System.Xml
    open System.Xml.Linq

    /// Generate test results using NUnit v2 schema.
    /// http://nunit.org/docs/files/TestResult.xml
    let writeNUnitSummary (file : string) (lResult : LintResult) =
        let testCaseElements =
            lResult.Files
            |> Seq.map (fun fileRes ->
                   let content =
                       let fElem =
                           [| XAttribute(XName.Get "executed", "True") |> box
                              XAttribute(XName.Get "result", fileRes.FileResult.ToString()) |> box
                              XAttribute(XName.Get "success", fileRes.FileSuccess.ToString()) |> box

                              XAttribute
                                  (XName.Get "time",
                                   System.String.Format
                                       (Globalization.CultureInfo.InvariantCulture, "{0:0.000}", fileRes.FileTime))
                              |> box
                              XAttribute(XName.Get "asserts", "0") |> box |]
                       match fileRes.LintResponse with
                       | Some(response) ->
                           [| XElement(XName.Get "failure", [| XElement(XName.Get "message", response) |]) |> box |]
                       | _ -> [||]
                       |> Array.append fElem
                   XElement(XName.Get "test-case",
                            [| yield XAttribute(XName.Get "name", fileRes.FileName) |> box
                               yield! content |]))

        let element =
            XElement(XName.Get "test-results",
                     [| yield XAttribute(XName.Get "date", lResult.Date.ToString("yyyy-MM-dd")) |> box
                        yield XAttribute(XName.Get "name", lResult.Name) |> box
                        yield XAttribute(XName.Get "total", lResult.FileTotal) |> box
                        yield XAttribute(XName.Get "errors", lResult.ErrorCount) |> box
                        yield XAttribute(XName.Get "failures", lResult.FailureCount) |> box
                        yield XAttribute(XName.Get "ignored", "0") |> box
                        yield XAttribute(XName.Get "not-run", "0") |> box
                        yield XAttribute(XName.Get "inconclusive", "0") |> box
                        yield XAttribute(XName.Get "skipped", "0") |> box
                        yield XAttribute(XName.Get "invalid", "0") |> box
                        yield XAttribute(XName.Get "time", lResult.Time.ToString("hh':'mm':'ss")) |> box
                        yield XElement
                                  (XName.Get "environment",
                                   [| yield XAttribute(XName.Get "fsharplint-version", string lResult.LintVersion)
                                            |> box
                                      yield XAttribute(XName.Get "clr-version", string Environment.Version) |> box
                                      yield XAttribute(XName.Get "os-version", string Environment.OSVersion) |> box

                                      yield XAttribute(XName.Get "platform", string Environment.OSVersion.Platform)
                                            |> box
                                      yield XAttribute(XName.Get "cwd", Environment.CurrentDirectory) |> box
                                      yield XAttribute(XName.Get "machine-name", Environment.MachineName) |> box
                                      yield XAttribute(XName.Get "user", Environment.UserName) |> box
                                      yield XAttribute(XName.Get "user-domain", Environment.UserDomainName) |> box |])
                              |> box
                        yield XElement
                                  (XName.Get "culture-info",
                                   [| yield XAttribute
                                                (XName.Get "current-culture",
                                                 string Globalization.CultureInfo.CurrentCulture) |> box

                                      yield XAttribute
                                                (XName.Get "current-uiculture",
                                                 string Globalization.CultureInfo.CurrentUICulture) |> box |])
                              |> box
                        yield XElement(XName.Get "test-suite",
                                       [| yield XAttribute(XName.Get "type", "Assembly") |> box
                                          yield XAttribute(XName.Get "name", lResult.Name) |> box
                                          yield XAttribute(XName.Get "executed", "True") |> box
                                          yield XAttribute(XName.Get "result", string lResult.Result) |> box
                                          yield XAttribute(XName.Get "success", string lResult.Success) |> box

                                          yield XAttribute
                                                    (XName.Get "time",
                                                     System.String.Format
                                                         (Globalization.CultureInfo.InvariantCulture, "{0:0.000}",
                                                          lResult.Duration)) |> box
                                          yield XAttribute(XName.Get "asserts", "0") |> box
                                          yield XElement(XName.Get "results", [| yield! testCaseElements |]) |> box |])
                              |> box |])

        let doc = XDocument([| element |])
        let path = Path.GetFullPath file
        Path.GetDirectoryName path
        |> Directory.CreateDirectory
        |> ignore
        let settings = XmlWriterSettings()
        settings.CheckCharacters <- false
        use writer = XmlWriter.Create(path, settings)
        doc.Save writer

module FSharpLinter =
    open FSharpLint.Application
    open FSharpLint.Framework

    type private FileLint =
        { Path : string
          Warnings : string List option
          Error : string option
          Duration : float }

    let private writeLine (str : string) (color : ConsoleColor) (writer : IO.TextWriter) =
        let originalColour = Console.ForegroundColor
        Console.ForegroundColor <- color
        writer.WriteLine str
        Console.ForegroundColor <- originalColour

    let private writeInfoLine (str : string) = writeLine str ConsoleColor.White Console.Out
    let private writeWarningLine (str : string) = writeLine str ConsoleColor.Yellow Console.Out
    let private writeErrorLine (str : string) = writeLine str ConsoleColor.Red Console.Error

    let private parserProgress =
        function
        | Starting(file) -> String.Format(Resources.GetString("ConsoleStartingFile"), file) |> writeInfoLine
        | ReachedEnd(_, warnings) ->
            String.Format(Resources.GetString("ConsoleFinishedFile"), List.length warnings) |> writeInfoLine
        | Failed(file, parseException) ->
            String.Format(Resources.GetString("ConsoleFailedToParseFile"), file) |> writeErrorLine
            "Exception Message:" + Environment.NewLine + parseException.Message + Environment.NewLine
            + "Exception Stack Trace:" + Environment.NewLine + parseException.StackTrace + Environment.NewLine
            |> writeErrorLine

    let mutable private collectWarning = List.empty<string>

    let private getWarnings() =
        match collectWarning.Length = 0 with
        | true -> None
        | false ->
            let warns = collectWarning
            collectWarning <- List.empty<string>
            Some(warns)

    let private writeLintWarning (warn : LintWarning.Warning) =
        let warnMsg = warn.Info + Environment.NewLine + LintWarning.getWarningWithLocation warn.Range warn.Input
        collectWarning <- ((if collectWarning.Length > 0 then Environment.NewLine + warnMsg
                            else warnMsg)
                           :: collectWarning)
        warnMsg |> writeWarningLine
        String.replicate 80 "*" |> writeInfoLine

    let private handleError (str : string) = writeErrorLine str

    let private handleLintResult =
        function
        | LintResult.Failure(failure) -> handleError failure.Description
        | _ -> ()

    let private parseInfo (webFile : bool) =
        let config =
            if webFile then
                Some { Configuration.formatting = None
                       Configuration.conventions =
                           Some { recursiveAsyncFunction = None
                                  redundantNewKeyword = None
                                  nestedStatements = None
                                  reimplementsFunction = None
                                  canBeReplacedWithComposition = None
                                  raiseWithTooManyArgs = None
                                  sourceLength = None
                                  naming =
                                      Some { interfaceNames =
                                                 Some { RuleConfig.enabled = false
                                                        config = None }
                                             exceptionNames = None
                                             typeNames = None
                                             recordFieldNames = None
                                             enumCasesNames = None
                                             unionCasesNames = None
                                             moduleNames = None
                                             literalNames = None
                                             namespaceNames = None
                                             memberNames =
                                                 Some { RuleConfig.enabled = false
                                                        config = None }
                                             parameterNames = None
                                             measureTypeNames = None
                                             activePatternNames = None
                                             publicValuesNames = None
                                             nonPublicValuesNames = None }
                                  numberOfItems = None
                                  binding = None }
                       Configuration.typography = None
                       ignoreFiles = None
                       hints = None }
            else None
        { CancellationToken = None
          ReceivedWarning = Some writeLintWarning
          Configuration = config
          ReportLinterProgress = Some parserProgress }

    let private buildLintResults (fileList : FileLint list) =
        let errors =
            fileList
            |> List.choose (fun fl -> fl.Error)
            |> List.length

        let failures =
            fileList
            |> List.choose (fun fl -> fl.Warnings)
            |> List.length

        let result =
            match errors + failures = 0 with
            | true -> Success
            | false -> Failure

        let success =
            match result with
            | Success -> true
            | _ -> false

        let fileElements =
            let fileSuccess (f : FileLint) = f.Error.IsNone && f.Warnings.IsNone

            let fileResult (f : FileLint) =
                match fileSuccess f with
                | true -> Success
                | false -> Failure

            let failMsg (f : FileLint) =
                match f.Warnings, f.Error with
                | Some(w), Some(e) -> (w |> List.reduce (+)) + Environment.NewLine + e
                | Some(w), None -> w |> List.reduce (+)
                | None, Some(e) -> e
                | _ -> "No valid data"

            fileList
            |> List.map (fun f ->
                   match fileSuccess f with
                   | true ->
                       { FileName = f.Path
                         FileResult = fileResult f
                         FileSuccess = fileSuccess f
                         FileTime = f.Duration
                         LintResponse = None }
                   | false ->
                       { FileName = f.Path
                         FileResult = fileResult f
                         FileSuccess = fileSuccess f
                         FileTime = f.Duration
                         LintResponse = Some(failMsg f) })

        { Date =
              let d = DateTime.Today in d.Date
          FileTotal = fileList.Length
          ErrorCount = errors
          FailureCount = failures
          Time =
              let d = DateTime.Now in d.TimeOfDay
          LintVersion = typeof<FSharpLint.Application.Lint.OptionalLintParameters>.Assembly.GetName().Version
          Name = "FSharpLint.Application"
          Result = result
          Success = success
          Duration = fileList |> List.sumBy (fun fl -> fl.Duration)
          Files = fileElements }

    let lintFiles (resultFile : string) (fileList : (bool * string list) list) =
        let lintFile (webFile : bool) (file : string) =
            let sw = Diagnostics.Stopwatch.StartNew()
            try
                Lint.lintFile (parseInfo webFile) file |> handleLintResult
                sw.Stop()
                { Path = file
                  Warnings = getWarnings()
                  Error = None
                  Duration =
                      let s = sw.Elapsed in s.TotalSeconds }
            with e ->
                sw.Stop()
                let error =
                    "Lint failed while analysing " + file + "." + Environment.NewLine + "Failed with: " + e.Message
                    + Environment.NewLine + "Stack trace:" + e.StackTrace
                error |> handleError
                { Path = file
                  Warnings = getWarnings()
                  Error = Some(error)
                  Duration =
                      let s = sw.Elapsed in s.TotalSeconds }
        fileList
        |> List.map (fun (webFile, fList) -> fList |> List.map (lintFile webFile))
        |> List.reduce (fun acc elem -> List.append acc elem)
        |> buildLintResults
        |> LintResults.writeNUnitSummary resultFile
