module CodeMirrorWrapper

open EditorTypes
open JSHelpers

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

[<Emit("
CodeMirror(document.getElementById($0), {
  lineNumbers: true,
  lineWrapping: true,
  matchBrackets: true,
  theme: \"mbo\", // Not working?
  mode: {name: \"verilog\"}
});")>]
let private createEditor (id : string) : Editor = jsNative

[<Emit("$0.getValue()")>]
let getCode (editor : Editor) : string = jsNative

[<Emit("$0.setValue($1)")>]
let setCode (editor : Editor) (code : string) : unit = jsNative

type DisplayModeType = Hidden | Visible

type CodeMirrorReactProps = {
    DispatchFunc : Editor -> unit
    DisplayMode : DisplayModeType
}

type CodeMirrorReact(initialProps) =
    inherit PureStatelessComponent<CodeMirrorReactProps>(initialProps)

    let hiddenStyle = Style [ Height "0px"; Width "0px" ]
    let visibleStyle = Style [ Height "auto"; Width "auto" ]

    override this.componentDidMount() =
        log "Mounting CodeMirrorReact component"
        this.props.DispatchFunc <| createEditor "editor"

    override this.render() =
        let style = match this.props.DisplayMode with
                    | Hidden -> hiddenStyle
                    | Visible -> visibleStyle
        div [ Id "editor"; style ] []

let inline createCodeMirrorReact props = ofType<CodeMirrorReact,_,_> props []

type CodeMirrorWrapper() =
    let mutable editor : Editor option = None

    /// Returns a react element containing the editor.
    /// The dispatch function has to be: InitEditor >> dispatch
    member this.EditorReactElement dispatch displayMode =
        createCodeMirrorReact { DispatchFunc = dispatch; DisplayMode = displayMode } // Already created.

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
