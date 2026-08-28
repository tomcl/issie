/// How much of the parameter feature the UI shows, and the invariant that keeps it simple.
///
/// Parameters are an advanced feature with three levels of use, and a user at one level must not
/// have to understand the next:
///
///   1. no parameters at all - library components do not count
///   2. parameters with a single settled value throughout
///   3. a parameter bound to different values in different instances
///
/// Two gates separate them, and both are pure functions of the loaded components, so they are
/// tested here directly rather than through the pane. The fourth state the feature could have had -
/// an instance parameter bound to nothing, viewed at the sheet's own value - is designed away
/// rather than presented, which is what addParamToInstances and the retargeted bind offer are for.
module ParameterUI

open Expecto
open CommonTypes
open ParameterTypes
open CanvasBuilder
open Optics
open Optics.Operators

/// A parameter declaration. Descriptions are compulsory but say nothing these tests depend on.
let private declares (name: string) (expr: ParamExpression) =
    ParamName name, { Expression = expr; Description = $"test parameter {name}" }

let private paramDefs (defaults: (ParamName * ParamDefinition) list) (slots: (ParamSlot * ConstrainedExpr) list) =
    { DefaultBindings = Map defaults; ParamSlots = Map slots }

/// A sheet whose one output width is its parameter W, defaulting to 4.
let private widthSheet (name: string) =
    let out = makeComp 1 1 0 (Output 4) "O"
    let wExpr = { Expression = PParameter(ParamName "W"); Constraints = [] }
    makeLdc name
        (Some (paramDefs [ declares "W" (PInt 4I) ] [ {CompId = ComponentId 1; CompSlot = IO "O" }, wExpr ]))
        ([ out ], [])

/// An instance of `child` on a parent sheet, binding W to the given expression (or to nothing).
let private instance (child: LoadedComponent) (id: int) (binding: ParamExpression option) =
    let bindings = binding |> Option.map (fun e -> Map [ ParamName "W", e ])
    makeComp id 0 1 (customOf child [] [ "O", 4 ] bindings) $"I{id}"

let private sheetOf (name: string) (comps: Component list) =
    makeLdc name None (comps, [])

let private asLibrary (libName: string) (compName: string) (ldc: LoadedComponent) =
    { ldc with Form = Some (Library(libName, compName)) }

let tests =
    testList "ParameterUI" [

        // --- Gate A: is the parameter vocabulary shown at all? ---

        test "a project with no parameters does not use parameters" {
            let top = sheetOf "top" []
            Expect.isFalse (ParameterAnalysis.projectDeclaresParams [ top ]) "nothing declares one"
        }

        test "a parameter the user declared turns the vocabulary on" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 8I)) ]
            Expect.isTrue (ParameterAnalysis.projectDeclaresParams [ top; child ]) "child declares W"
        }

        test "a library component's own parameters do not count as the user's" {
            // Placing a parameterised library component must not expose the vocabulary to someone
            // who never asked for it: on the instance those values are presented as settings.
            let child = widthSheet "L1_reg" |> asLibrary "stdlib" "reg"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 8I)) ]
            Expect.isFalse (ParameterAnalysis.projectDeclaresParams [ top; child ])
                "the parameter arrived with the library"
        }

        // --- Gate B: is a top sheet needed? ---

        test "one instance is not ambiguous" {
            // The old test was the mere presence of a parameter, which turned the whole top-sheet
            // apparatus on here, where there is nothing whatever to settle.
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 8I)) ]
            Expect.isFalse (ParameterAnalysis.projectHasAmbiguousDisplay [ top; child ])
                "a single instance settles the value by itself"
        }

        test "instances that agree are not ambiguous" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 8I))
                                      instance child 2 (Some (PInt 8I)) ]
            Expect.isFalse (ParameterAnalysis.projectHasAmbiguousDisplay [ top; child ])
                "both instances give the same value"
        }

        // Two instances in ONE design setting a sheet differently is ordinary, and settled by a
        // rule: the sheet is drawn at the largest and the others are shown beside it. This used to
        // be reported as ambiguous, which raised the top-sheet question for a design that had
        // nothing to choose between - the two paths are both part of the same design.
        test "one design using a sheet at two sizes is not ambiguous" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 8I))
                                      instance child 2 (Some (PInt 16I)) ]
            Expect.isFalse (ParameterAnalysis.projectHasAmbiguousDisplay [ top; child ])
                "there is one design, so nothing to ask the user"
        }

        test "a sheet with no instances is not ambiguous" {
            // Its declared value is simply the value, and there is no top sheet to choose.
            let child = widthSheet "child"
            Expect.isFalse (ParameterAnalysis.projectHasAmbiguousDisplay [ child ])
                "nothing instantiates it, so nothing disagrees"
        }

        test "disagreeing instances of a library sheet are not ambiguous by themselves" {
            // A library sheet is never displayed, so there is no question of which value to draw
            // it at. What would make this ambiguous is the PARENT varying, and the parent is
            // caught on its own account.
            let child = widthSheet "L1_reg" |> asLibrary "stdlib" "reg"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 8I))
                                      instance child 2 (Some (PInt 16I)) ]
            Expect.isFalse (ParameterAnalysis.projectHasAmbiguousDisplay [ top; child ])
                "the sheet the instances disagree about cannot be opened"
        }

        test "two independent designs using one sheet at different widths are ambiguous" {
            // Merging across forest roots is deliberate: opening child, which design is it part of?
            let child = widthSheet "child"
            let topA = sheetOf "topA" [ instance child 1 (Some (PInt 8I)) ]
            let topB = sheetOf "topB" [ instance child 2 (Some (PInt 16I)) ]
            Expect.isTrue (ParameterAnalysis.projectHasAmbiguousDisplay [ topA; topB; child ])
                "the two roots disagree about child"
        }

        // --- what a parameter of a sheet is set to ---
        //
        // A parameter's value is what the instances of its sheet set it to. The stored value is a
        // placeholder for the case where nothing does - a sheet not used anywhere yet - and is
        // shown as provisional rather than as a value the design has settled on.

        let displayOf (ldcs: LoadedComponent list) (top: string) (sheet: string) (param: string) =
            ParameterAnalysis.displayValues ldcs top sheet |> Map.tryFind (ParamName param)

        test "one instance setting a value gives that value, not the stored one" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 8I)) ]
            Expect.equal (displayOf [ top; child ] "top" "child" "W")
                (Some (ParameterAnalysis.Values [ 8I ]))
                "the sheet stores 4 and the instance binds 8: 8 is what the sheet is used at"
        }

        // This used to be indistinguishable from the case above: an unbound instance took the
        // child's stored value, and it was reported in the same shape as one an instance had set.
        test "an instance that binds nothing leaves the value unsettled" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 None ]
            Expect.equal (displayOf [ top; child ] "top" "child" "W")
                (Some (ParameterAnalysis.NotUsed 4I))
                "nothing sets W, so the stored value stands in - which is what greys it in the pane"
        }

        test "a sheet nothing instantiates keeps its stored value, as a placeholder" {
            let child = widthSheet "child"
            let other = sheetOf "top" []
            Expect.equal (displayOf [ other; child ] "top" "child" "W")
                (Some (ParameterAnalysis.NotUsed 4I))
                "nothing uses the sheet, so the stored value is all there is"
        }

        // The top sheet is an instance of nothing, so its own values are always unsettled - they
        // are the primary state everything below is derived from. analyseUnderTop records the top
        // as its own instance for the membership question, which used to make its parameters read
        // as though they were set by something.
        test "the top sheet's own values are unsettled, not set by itself" {
            let top = widthSheet "top"
            Expect.equal (displayOf [ top ] "top" "top" "W")
                (Some (ParameterAnalysis.NotUsed 4I))
                "the top is instantiated by nothing"
            Expect.isTrue (Set.contains "top" (ParameterAnalysis.sheetsUnderTop [ top ] "top"))
                "while still being a member of its own design"
        }

        // One design reaching a sheet by paths that set it differently is allowed. The largest is
        // taken so that the sheet has one definite value, and the others are shown beside it.
        test "instances that differ give every value, largest first" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 8I))
                                      instance child 2 (Some (PInt 16I)) ]
            Expect.equal (displayOf [ top; child ] "top" "child" "W")
                (Some (ParameterAnalysis.Values [ 16I; 8I ]))
                "distinct and descending, so the head is the one the sheet is drawn at"
            Expect.isFalse (ParameterAnalysis.projectHasAmbiguousDisplay [ top; child ])
                "one design using a sheet at two sizes has nothing for the user to choose between"
        }

        // --- which design a sheet belongs to ---

        test "two designs setting a sheet differently is the case that needs a choice" {
            let child = widthSheet "child"
            let topA = sheetOf "topA" [ instance child 1 (Some (PInt 8I)) ]
            let topB = sheetOf "topB" [ instance child 2 (Some (PInt 16I)) ]
            Expect.isTrue (ParameterAnalysis.projectHasAmbiguousDisplay [ topA; topB; child ])
                "the two designs are both right and no rule can pick between them"
        }

        // The bug this replaced: effectiveTopSheet asked whether the PROJECT had one design, so a
        // scratch sheet nobody instantiates was a second root and the whole project lost its top.
        // Every parameter row in every sheet then fell back to its stored value with nothing said,
        // and no popup fired, because nothing was ambiguous.
        test "a stray sheet does not stop other sheets knowing their design" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 8I)) ]
            let scratch = sheetOf "scratch" []
            Expect.equal (ParameterAnalysis.effectiveTopSheetFor [ top; child; scratch ] "child") "top"
                "child sits under exactly one root, whatever else the project holds"
            Expect.equal (displayOf [ top; child; scratch ] "top" "child" "W")
                (Some (ParameterAnalysis.Values [ 8I ]))
                "so its value is the one its design gives it"
            Expect.isFalse (ParameterAnalysis.projectHasAmbiguousDisplay [ top; child; scratch ])
                "and there is still nothing to ask the user"
        }

        test "a sheet in no design is its own top" {
            let child = widthSheet "child"
            let scratch = sheetOf "scratch" []
            Expect.equal (ParameterAnalysis.effectiveTopSheetFor [ child; scratch ] "scratch") "scratch"
                "nothing contains it, so it heads its own design"
        }

        // --- bringing every sheet into line with what its design sets ---
        //
        // propagateParameterValues is a pure recomputation from the primary state, not an
        // incremental edit. Everything below follows from that: it can be run after anything, in
        // any order, as many times as you like.

        let paramOf (ldcs: LoadedComponent list) (sheet: string) (name: string) =
            ldcs
            |> List.tryFind (fun ldc -> ldc.Name = sheet)
            |> Option.bind (fun ldc -> ldc.LCParameterSlots)
            |> Option.bind (fun defs -> Map.tryFind (ParamName name) defs.DefaultBindings)
            |> Option.map (fun d -> d.Expression)

        let widthOf (ldcs: LoadedComponent list) (sheet: string) (label: string) =
            ldcs
            |> List.tryFind (fun ldc -> ldc.Name = sheet)
            |> Option.bind (fun ldc ->
                fst ldc.CanvasState
                |> List.tryFind (fun c -> c.Label = label)
                |> Option.map (fun c -> c.Type))

        test "a sheet takes the value its design sets, in its parameters and on its canvas" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 16I)) ]
            let after = ParameterAnalysis.propagateParameterValues [ top; child ]
            Expect.equal (paramOf after "child" "W") (Some (PInt 16I))
                "the stored value follows the design rather than staying at 4"
            Expect.equal (widthOf after "child" "O") (Some (Output 16))
                "and the slot on its canvas is rewritten to match, so what is drawn is what is stored"
        }

        test "propagation is idempotent" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 16I)) ]
            let once = ParameterAnalysis.propagateParameterValues [ top; child ]
            let twice = ParameterAnalysis.propagateParameterValues once
            Expect.equal (List.map (fun (l: LoadedComponent) -> l.CanvasState) twice)
                (List.map (fun (l: LoadedComponent) -> l.CanvasState) once)
                "running it again changes nothing, which is what lets it run after anything"
            Expect.equal (paramOf twice "child" "W") (paramOf once "child" "W") "including the values"
        }

        test "a sheet nothing sets keeps its own value, which is the only copy of it" {
            let child = widthSheet "child"
            let after = ParameterAnalysis.propagateParameterValues [ child ]
            Expect.equal (paramOf after "child" "W") (Some (PInt 4I))
                "overwriting it would destroy the primary state of a sheet not yet used"
            Expect.equal (widthOf after "child" "O") (Some (Output 4)) "and its canvas is left alone"
        }

        test "differing instances leave the sheet at the largest, with the rest recorded" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 8I))
                                      instance child 2 (Some (PInt 16I))
                                      instance child 3 (Some (PInt 4I)) ]
            let after = ParameterAnalysis.propagateParameterValues [ top; child ]
            Expect.equal (paramOf after "child" "W") (Some (PInt 16I)) "drawn at the largest"
            Expect.equal (widthOf after "child" "O") (Some (Output 16)) "canvas to match"
            // the others are not stored on the sheet - the properties pane asks displayValues for
            // them when it draws, which is the same question and one fewer copy of the answer
            Expect.equal (displayOf after "top" "child" "W")
                (Some (ParameterAnalysis.Values [ 16I; 8I; 4I ]))
                "descending, for the pane to show as '16 (also 8, 4)'"
        }

        test "the top sheet's own value is primary and is never rewritten" {
            let child = widthSheet "child"
            let top = { widthSheet "top" with
                            CanvasState = [ instance child 1 (Some (PParameter (ParamName "W"))) ], [] }
            let after = ParameterAnalysis.propagateParameterValues [ top; child ]
            Expect.equal (paramOf after "top" "W") (Some (PInt 4I))
                "nothing sets the top, so its stored value stands"
            Expect.equal (paramOf after "child" "W") (Some (PInt 4I))
                "and the child takes it through the binding"
        }

        test "a flagged top sheet wins over what the instance forest says" {
            let child = widthSheet "child"
            let top = { sheetOf "top" [ instance child 1 (Some (PInt 8I)) ] with IsTopSheet = true }
            Expect.equal (ParameterAnalysis.effectiveTopSheetFor [ top; child ] "child") "top"
                "the user's choice is the design"
        }

        // --- what a property input box holds while it is being edited ---
        //
        // The box's text is model state, as it is for every other input box in Issie. Two things
        // follow, and both used to be wrong: the box can be set from elsewhere by an ordinary
        // dispatch, and an entry that will not parse stays on screen with its message instead of
        // reaching the component.

        test "a box's key names the component, not just the slot" {
            // Every component with a width has a Buswidth slot, so keying on the slot name alone
            // meant one component's box read another's entry - and showed its error, in red, on a
            // component whose own value was fine.
            let a = ParameterTypes.paramBoxKey (Some(ComponentId 1)) Buswidth
            let b = ParameterTypes.paramBoxKey (Some(ComponentId 2)) Buswidth
            Expect.notEqual a b "two components' width boxes are different boxes"
            Expect.equal (Map.ofList [a, 1; b, 2] |> Map.count) 2 "so they do not share an entry"
            Expect.equal (ParameterTypes.paramBoxKey None Buswidth).CompId (ComponentId 0)
                "a popup has no component, and its entries go when the popup closes"
        }

        // What the box records is the text, whatever it means. Only a Spec that is Ok is ever
        // applied, so an unparseable entry is visible and inert rather than silently dropped.
        test "text that will not parse is held with its message, and yields no value" {
            let bindings = Map [ ParamName "W", PInt 8I ]
            let stateOf (text: string) =
                let spec =
                    parseExpression text
                    |> Result.bind (evaluateParamExpression bindings)
                    |> Result.map (fun v -> {CompSlot = Buswidth; Expression = PInt v; Constraints = []; Value = v})
                { Text = text; Spec = spec }
            let bad = stateOf "W +* 2"
            Expect.equal bad.Text "W +* 2" "the text stays exactly as typed, so the box still shows it"
            Expect.isError bad.Spec "and nothing is derived from it to apply"
            let good = stateOf "W*2"
            Expect.equal good.Text "W*2" "a valid entry keeps its text too"
            match good.Spec with
            | Ok spec -> Expect.equal spec.Value 16I "and carries the value that reaches the component"
            | Error e -> failtest $"expected a value, got {e}"
        }

        // --- repairing the totality invariant on load ---

        let bindingOf (ldcs: LoadedComponent list) (sheet: string) (instId: ComponentId) (name: string) =
            ldcs
            |> List.tryFind (fun ldc -> ldc.Name = sheet)
            |> Option.map (fst << (fun ldc -> ldc.CanvasState))
            |> Option.bind (List.tryFind (fun c -> c.Id = instId))
            |> Option.bind (fun c ->
                match c.Type with
                | Custom cc -> cc.ParameterBindings |> Option.bind (Map.tryFind (ParamName name))
                | _ -> None)

        test "an instance binding nothing is given the sheet's own value" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 None ]
            let repaired = ParameterAnalysis.bindMissingInstanceParams [ top; child ]
            Expect.equal (bindingOf repaired "top" (ComponentId 1) "W") (Some (PInt 4I))
                "written down at the value it was already resolving to, so no design changes"
            Expect.isTrue (ParameterAnalysis.everyInstanceBindsEveryParam repaired)
                "and the invariant the rest of the system is written against now holds"
        }

        test "repairing leaves an instance that already binds alone, and is idempotent" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 16I)) ]
            let once = ParameterAnalysis.bindMissingInstanceParams [ top; child ]
            Expect.equal (bindingOf once "top" (ComponentId 1) "W") (Some (PInt 16I)) "the instance's own value stands"
            Expect.equal (List.map (fun (l: LoadedComponent) -> l.CanvasState) once)
                (List.map (fun (l: LoadedComponent) -> l.CanvasState) [ top; child ])
                "a project that already holds the invariant is untouched"
            let twice = ParameterAnalysis.bindMissingInstanceParams once
            Expect.equal (List.map (fun (l: LoadedComponent) -> l.CanvasState) twice)
                (List.map (fun (l: LoadedComponent) -> l.CanvasState) once) "and running it again does nothing"
        }

        // --- the bind offer, asked the same way whether an instance exists or not ---

        test "an ancestor two sheets up is offered at placement, as it is after placement" {
            // top declares W; mid does not. Placing an instance of child onto mid should still
            // offer to follow top.W - the chain materialises W on mid - which the old test
            // (does the IMMEDIATE parent declare it?) could not see.
            let child = widthSheet "child"
            let mid = sheetOf "mid" [ instance child 1 (Some (PInt 4I)) ]
            let top = { widthSheet "top" with
                            CanvasState = [ makeComp 1 0 0 (customOf mid [] [] None) "MID" ], [] }
            match ParameterAnalysis.bindOfferForPlacement [ top; mid; child ] "top" "mid" (ParamName "W") with
            | Some (bindsTo, actions) ->
                Expect.equal bindsTo "top" "the offer describes itself as following the declaring ancestor"
                Expect.isTrue
                    (actions |> List.exists (function
                        | ParameterAnalysis.AddSheetParam (s, ParamName "W", _, _) -> s = "mid"
                        | _ -> false))
                    "and accepting would create W on the sheet in between"
            | None -> failtest "expected an offer to follow top.W"
        }

        test "no offer where no ancestor declares the name" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 4I)) ]
            Expect.isNone (ParameterAnalysis.bindOfferForPlacement [ top; child ] "top" "top" (ParamName "W"))
                "nothing to follow, so nothing offered"
        }

        // --- totality: an instance parameter is never unbound ---

        test "a parameter added to a sheet with instances leaves none of them unbound" {
            // Placing an instance asks for every parameter, but adding one to a sheet that already
            // has instances is the hole that would otherwise leave them all binding nothing.
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 None; instance child 2 None ]
            Expect.isFalse (ParameterAnalysis.everyInstanceBindsEveryParam [ top; child ])
                "the instances start out bound to nothing"
            let fixed' = ParameterAnalysis.bindParamOnInstances "child" (ParamName "W") 4I [ top; child ]
            Expect.isTrue (ParameterAnalysis.everyInstanceBindsEveryParam fixed')
                "every instance now binds W"
        }

        test "binding instances leaves an instance that already had a value alone" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 16I)); instance child 2 None ]
            let fixed' = ParameterAnalysis.bindParamOnInstances "child" (ParamName "W") 4I [ top; child ]
            let bindingOf id =
                fixed'
                |> List.pick (fun ldc -> if ldc.Name = "top" then Some (fst ldc.CanvasState) else None)
                |> List.pick (fun comp ->
                    match comp.Id = id, comp.Type with
                    | true, Custom cc -> cc.ParameterBindings |> Option.bind (Map.tryFind (ParamName "W"))
                    | _ -> None)
            Expect.equal (bindingOf (ComponentId 1)) (PInt 16I) "the chosen value survives"
            Expect.equal (bindingOf (ComponentId 2)) (PInt 4I) "the one with none takes the declared value"
        }

        test "the sheet gaining the parameter is not touched" {
            // A sheet cannot instantiate itself, but the skip is what makes that explicit.
            let child = widthSheet "child"
            let before = [ child ]
            Expect.equal (ParameterAnalysis.bindParamOnInstances "child" (ParamName "W") 4I before) before
                "nothing to bind, nothing changed"
        }

        // --- the bind-to-top offer, retargeted from unbound to literal ---

        test "an instance bound to a literal is offered a chain to a same-named ancestor" {
            // The trigger used to be an unbound parameter. Every instance now binds every
            // parameter, so that could never fire; a literal is the state the offer is useful in,
            // and typing the name by hand is what single-level scoping defeats.
            let child = widthSheet "child"
            let top =
                makeLdc "top"
                    (Some (paramDefs [ declares "W" (PInt 8I) ] []))
                    ([ instance child 1 (Some (PInt 8I)) ], [])
            let offers = ParameterAnalysis.findBindOffers [ top; child ] "top" None
            Expect.equal (List.length offers) 1 "one instance, one offer"
            Expect.equal offers[0].Param (ParamName "W") "for the parameter of that name"
            Expect.equal offers[0].BindsTo "top" "bound up to the sheet that declares it"
        }

        test "an instance already following a parameter is not offered anything" {
            let child = widthSheet "child"
            let top =
                makeLdc "top"
                    (Some (paramDefs [ declares "W" (PInt 8I) ] []))
                    ([ instance child 1 (Some (PParameter(ParamName "W"))) ], [])
            Expect.isEmpty (ParameterAnalysis.findBindOffers [ top; child ] "top" None)
                "it already stays in step"
        }

        test "the evidence gate keeps the offer quiet with no ancestor declarer" {
            // A literal binding is not on its own evidence of a design constant.
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child 1 (Some (PInt 8I)) ]
            Expect.isEmpty (ParameterAnalysis.findBindOffers [ top; child ] "top" None)
                "no sheet above declares W"
        }

        // --- writing what a sheet declares ---
        //
        // A sheet that declares nothing holds LCParameterSlots = None, and that is exactly the
        // sheet a FIRST declaration is written to. Reached through lcParameterSlots_, a prism, the
        // write was silently dropped: composing onto a prism gives a setter that does nothing when
        // the outer get is None. A second, hand-written write path existed only to cover that hole.

        test "a first declaration reaches a sheet that had no parameter data at all" {
            let sheet = sheetOf "fresh" []
            Expect.isNone sheet.LCParameterSlots "nothing is declared yet"
            let written =
                sheet
                |> Optic.set (lcParameterDefs_ >-> defaultBindings_)
                       (Map.ofList [ declares "W" (PInt 8I) ])
            Expect.equal (ParameterAnalysis.declaredParams written |> Map.tryFind (ParamName "W"))
                (Some (PInt 8I)) "the declaration is there, and the record was created to hold it"
        }

        test "a sheet with no parameter data reads as empty rather than failing" {
            let defs = sheetOf "fresh" [] ^. lcParameterDefs_
            Expect.isEmpty defs.DefaultBindings "no declarations"
            Expect.isEmpty defs.ParamSlots "and no slots"
        }

        test "writing a slot to a sheet that declares nothing also lands" {
            // the same hole seen from the other field: updateParamSlot writes through this lens too
            let slot = {CompId = ComponentId 1; CompSlot = Buswidth}
            let written =
                sheetOf "fresh" []
                |> Optic.map (lcParameterDefs_ >-> paramSlots_)
                       (Map.add slot {Expression = PParameter (ParamName "W"); Constraints = []})
            Expect.equal (ParameterAnalysis.sheetParamSlots written |> Map.count) 1 "the slot is stored"
        }

        // --- the bounds a value has to satisfy, wherever it comes from ---
        //
        // These were built inline at each properties box, which had two consequences: a bound
        // computed from the component's width was frozen at the width showing when the expression
        // was typed, and a value arriving any other way was bounded by nothing at all.

        let maxWidth = bigint CommonTypes.Constants.maxIssieBusWidth

        let messages (constraints: ParamConstraint list) =
            constraints |> List.map (function MinVal (_, m) | MaxVal (_, m) -> m)

        let bounds (constraints: ParamConstraint list) =
            constraints
            |> List.choose (function
                | MinVal (PInt v, _) -> Some ("min", v)
                | MaxVal (PInt v, _) -> Some ("max", v)
                | _ -> None)

        test "a width slot is bounded at one bit and at the largest bus Issie allows" {
            Expect.equal (bounds (ComponentSlots.constraintsFor Buswidth (Register 8)))
                [ "min", 1I; "max", maxWidth ]
                "the bound CatalogueView enforces on a typed width, now wherever the value comes from"
        }

        test "an input's default value is bounded by the input's CURRENT width" {
            // the staleness this replaces: the bound was computed when the expression was typed,
            // so parameterising the width and then changing it left "must fit in 8 bits" behind
            Expect.equal (bounds (ComponentSlots.constraintsFor InputDefault (Input1 (8, None))))
                [ "min", 0I; "max", 255I ] "eight bits"
            Expect.equal (bounds (ComponentSlots.constraintsFor InputDefault (Input1 (40, None))))
                [ "min", 0I; "max", (1I <<< 40) - 1I ]
                "and the bound still holds past 32, where the old `1 <<< width` wrapped"
        }

        test "a BusSelection LSB is bounded below and not above" {
            // IO on a BusSelection is its LSB, not a width: Buswidth is already its own width
            Expect.equal (bounds (ComponentSlots.constraintsFor (IO "sel") (BusSelection (8, 0))))
                [ "min", 0I ] "a bit position, with no width to exceed"
        }

        test "a slot the component does not have is bounded by nothing" {
            Expect.isEmpty (ComponentSlots.constraintsFor InputDefault (Register 8))
                "nothing can be written there, so there is nothing to bound"
        }

        test "a custom component's property is bounded by the sheet inside it, not here" {
            let child = widthSheet "child"
            let inst = instance child 1 (Some (PInt 8I))
            Expect.isEmpty (ComponentSlots.constraintsFor (CustomCompParam "W") inst.Type)
                "its bounds are the child sheet's slots, which are expressions in the child's own \
                 properties and so cannot be stated here"
        }

        // --- and how that bound reaches an instance binding ---

        /// The bindings an instance of `child` resolves at before a candidate value is applied.
        let childBaseline (child: LoadedComponent) =
            ParameterAnalysis.declaredParams child

        test "a binding that would make an impossible bus is refused in the child's words" {
            let child = widthSheet "child"
            match ParameterView.instanceBindingProblem [ child ] "child" (childBaseline child)
                      (ParamName "W") 100000I with
            | Ok () -> failtest "a hundred thousand bits was accepted"
            | Error message ->
                Expect.stringContains message (string CommonTypes.Constants.maxIssieBusWidth)
                    "the message is the one written for the width it would set"
        }

        test "a binding of zero is refused, since a width of zero is not a width" {
            let child = widthSheet "child"
            Expect.isError
                (ParameterView.instanceBindingProblem [ child ] "child" (childBaseline child)
                     (ParamName "W") 0I)
                "the child's output cannot be zero bits wide"
        }

        test "an ordinary binding passes" {
            let child = widthSheet "child"
            Expect.isOk
                (ParameterView.instanceBindingProblem [ child ] "child" (childBaseline child)
                     (ParamName "W") 16I)
                "16 bits is a bus like any other"
        }

        test "a property no slot uses is not complained about" {
            // only the slots that USE the property are checked: a complaint from any other would
            // name the box the user is not editing
            let child = widthSheet "child"
            Expect.isOk
                (ParameterView.instanceBindingProblem [ child ] "child" (childBaseline child)
                     (ParamName "unused") 100000I)
                "nothing on the child sheet depends on it"
        }
    ]
