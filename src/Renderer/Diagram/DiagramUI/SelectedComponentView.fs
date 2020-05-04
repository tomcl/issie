(*
    SelectedComponentView.fs

    View for the selected component in the right tab.
*)

module SelectedComponentView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.Browser

open JSHelpers
open DiagramModelType

let viewSelectedComponent model =
    match model.SelectedComponent with
    | None -> div [] [ str "Select a component in the diagram" ]
    | Some comp ->
        let formId = "component-properties-form-" + comp.Id
        let readOnlyFormField name value =
            Field.div [] [
                Label.label [] [ str name ]
                Control.div [] [ Input.text [ Input.Props [ ReadOnly true; Name name ]; Input.IsStatic true; Input.Value value ] ] ]
        let formField name defaultValue =
            // Use comp.Id as key to refresh. DefaultValue is only updated when
            // the form is created and not anymore. The Key force the re-rendering
            // of the element every time the Key value changes.
            Field.div [] [
                Label.label [] [ str name ]
                Control.div [] [ Input.text [ Input.Props [ Name name; Key comp.Id ]; Input.DefaultValue defaultValue ] ] ]
        let formButton text onClick =
            Field.div [ Field.IsGrouped ] [
                Control.div [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick (fun e ->
                            e.preventDefault()
                            onClick ()
                        )
                    ] [ str text ]
                ]
            ]
        form [ Id formId ] [
            readOnlyFormField "Id" comp.Id
            readOnlyFormField "Type" <| sprintf "%A" comp.Type
            formField "Label" comp.Label
            readOnlyFormField "Input ports" <| sprintf "%d" comp.InputPorts.Length
            readOnlyFormField "Output ports" <| sprintf "%d" comp.OutputPorts.Length
            // Submit.
            formButton "Update" (fun _ ->
                // TODO: dont think this is the right way to do it.
                let form = document.getElementById <| formId
                let label : string = getFailIfNull form ["elements"; "Label"; "value"]
                model.Diagram.EditComponent comp.Id label
            )
        ]
