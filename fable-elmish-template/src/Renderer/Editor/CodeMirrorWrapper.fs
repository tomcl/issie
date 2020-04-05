module CodeMirrorWrapper

open EditorTypes
open JSHelpers
open EditorStyle

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

// Interface with JS library.

[<Emit("
CodeMirror(document.getElementById($0), {
  lineNumbers: true,
  lineWrapping: true,
  matchBrackets: true,
  theme: \"mbo\", // Not working?
  mode: {name: \"verilog\"}
});")>]
let private createEditor (id : string) : JSEditor = jsNative

[<Emit("$0.getValue()")>]
let private getCode (editor : JSEditor) : string = jsNative

[<Emit("$0.setValue($1)")>]
let private setCode (editor : JSEditor) (code : string) : unit = jsNative

// React wrapper.

type DisplayModeType = Hidden | Visible

type CodeMirrorReactProps = {
    Dispatch : JSEditorMsg -> unit
    DisplayMode : DisplayModeType
}

type CodeMirrorReact(initialProps) =
    inherit PureStatelessComponent<CodeMirrorReactProps>(initialProps)

    let divId = "codeMirrorEditor"

    override this.componentDidMount() =
        log "Mounting CodeMirrorReact component"
        createEditor divId |> InitEditor |> this.props.Dispatch 

    override this.render() =
        let style = match this.props.DisplayMode with
                    | Hidden -> editorHiddenStyle
                    | Visible -> editorVisibleStyle
        div [ Id divId; style ] []

let inline private createCodeMirrorReact props = ofType<CodeMirrorReact,_,_> props []

type CodeMirrorWrapper() =
    let mutable editor : JSEditor option = None

    /// Returns a react element containing the editor.
    /// The dispatch function has to be: JSEditorMsg >> dispatch
    member this.EditorReactElement jsEditorMsgDispatch displayMode =
        // Return react element with relevant props.
        createCodeMirrorReact {
            Dispatch = jsEditorMsgDispatch
            DisplayMode = displayMode
        }

    member this.InitEditor newEditor =
        match editor with
        | None -> editor <- Some newEditor
        | Some _ -> failwithf "what? InitEditor should never be called when editor is already created" 

    member this.GetCode () =
        match editor with
        | None ->
            log "Warning: CodeMirrorWrapper.GetCode called when editor is None"
            ""
        | Some e ->
            getCode e
