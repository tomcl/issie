/// A sheet is more than its canvas, and Issie rebuilds sheets from canvases in several places.
///
/// Its array settings say how many copies its components are and give it ports that no component
/// on it draws; its parameters give those ports their widths; being the top sheet decides which
/// design is simulated. None of that is on the canvas. Every path that rebuilds a LoadedComponent
/// from one has therefore to put it back, and three did not - the model's per-frame copy of the
/// open sheet, the design sent to the .NET sidecar, and the sheet the shim rebuilds from that. Each
/// silently produced an ordinary sheet where an array component had been, with the wrong ports.
///
/// What is pinned here is the invariant rather than those three bugs: the fields of
/// LoadedComponent, split between the ones a canvas can say and the ones only the sheet's own copy
/// can, so that a field added to the record fails this test until somebody decides which it is.
module SheetIdentity

open Expecto
open CommonTypes
open ParameterTypes
open CanvasBuilder
open Microsoft.FSharp.Reflection

/// Where a field's value comes from when a sheet is rebuilt from a canvas.
type private Source =
    /// the canvas, or the name the caller asked for
    | FromCanvas
    /// only the sheet's existing copy can say it, and losing it changes what the sheet MEANS
    | Carried
    /// deliberately not carried, for the reason given
    | Reset of Why: string

let private policy =
    [ "Name", FromCanvas
      "CanvasState", FromCanvas
      "InputLabels", FromCanvas
      "OutputLabels", FromCanvas

      "LCParameterSlots", Carried
      "IsTopSheet", Carried
      "ArrayInfo", Carried

      "LoadedComponentIsOutOfDate", Reset "built from the canvas, so by construction it matches it"
      "TimeStamp", Reset "this copy is being made now"
      "FilePath", Reset "belongs to the file; this copy is never written"
      "WaveInfo", Reset "belongs to the file; a wave selection is not part of the circuit"
      "Form", Reset "library machinery, and does not change what circuit this is"
      "Description", Reset "shown in the properties pane, and does not change what circuit this is" ]

/// A sheet with something distinctive in every carried field, so that losing one is visible.
let private distinctive () =
    let comp = makeComp 1 0 1 (Input1(4, None)) "A"
    let defs =
        { DefaultBindings =
            Map [ ParamName "W", { Expression = PInt 4I; Description = "the width" } ]
          ParamSlots = Map.empty }
    { makeLdc "sheet" (Some defs) ([ comp ], []) with
        IsTopSheet = true
        ArrayInfo = Some { LoopParam = ParamName "i"; Copies = 3 } }

let tests =
    testList "SheetIdentity" [

        test "every field of LoadedComponent is classified" {
            // the enforcement: a field added to the record is in neither list until someone puts it
            // in one, and which list it goes in is the whole question this module exists to ask
            let actual =
                FSharpType.GetRecordFields typeof<LoadedComponent>
                |> Array.map (fun f -> f.Name)
                |> Set.ofArray
            let classified = policy |> List.map fst |> Set.ofList
            Expect.isEmpty (Set.difference actual classified)
                "LoadedComponent has a field this module does not classify: decide whether \
                 rebuilding a sheet from its canvas must carry it, and add it to the policy"
            Expect.isEmpty (Set.difference classified actual)
                "the policy names a field LoadedComponent no longer has"
        }

        test "rebuilding a sheet from its canvas keeps what the canvas cannot say" {
            // exactly what the model does every frame for the sheet being edited
            let sheet = distinctive ()
            let rebuilt =
                [ sheet ]
                |> CanvasExtractor.addStateToLoadedComponents sheet.Name sheet.CanvasState
                |> List.find (fun l -> l.Name = sheet.Name)
            for name, source in policy do
                match source with
                | Carried ->
                    let field (l: LoadedComponent) =
                        (FSharpValue.GetRecordFields l, FSharpType.GetRecordFields typeof<LoadedComponent>)
                        ||> Array.zip
                        |> Array.pick (fun (v, f) -> if f.Name = name then Some v else None)
                    Expect.equal (field rebuilt) (field sheet)
                        $"'{name}' was lost: the sheet rebuilt from the canvas is a DIFFERENT sheet \
                          that happens to be drawn the same way"
                | FromCanvas | Reset _ -> ()
        }

        test "a caller that leaves the sheet out of the list is the way this was lost" {
            // Not a rule the compiler can state, so it is stated here: the whole design goes in,
            // the named sheet included, because the lookup that carries these fields forward is a
            // lookup in that list. Filtering first - which three callers did - makes it dead.
            let sheet = distinctive ()
            let withoutIt =
                []
                |> CanvasExtractor.addStateToLoadedComponents sheet.Name sheet.CanvasState
                |> List.find (fun l -> l.Name = sheet.Name)
            Expect.isNone withoutIt.ArrayInfo
                "there is nowhere else it could come from - which is why the caller must pass it"
            Expect.equal (fst withoutIt.CanvasState |> List.length) 1
                "the canvas still arrives; it is only what the canvas cannot say that is lost"
        }

        test "the design sent to the sidecar carries them too" {
            // the same invariant across the wire: SimpleSheet is a projection of a sheet, and a
            // projection that drops these describes a different circuit at the other end
            let sheet = distinctive ()
            let shimmed =
                [ sheet ]
                |> CanvasExtractor.simpleDesignOfLoadedComponents
                |> SimpleDesignShim.designToLoadedComponents
                |> List.find (fun l -> l.Name = sheet.Name)
            Expect.equal shimmed.ArrayInfo sheet.ArrayInfo "array settings must cross the wire"
            Expect.equal
                (shimmed.LCParameterSlots |> Option.map (fun d -> d.DefaultBindings))
                (sheet.LCParameterSlots |> Option.map (fun d -> d.DefaultBindings))
                "and so must the parameters that give its ports their widths"
        }
    ]
