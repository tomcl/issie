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

/// A parameter declaration. Descriptions are compulsory but say nothing these tests depend on.
let private declares (name: string) (expr: ParamExpression) =
    ParamName name, { Expression = expr; Description = $"test parameter {name}" }

let private paramDefs (defaults: (ParamName * ParamDefinition) list) (slots: (ParamSlot * ConstrainedExpr) list) =
    { DefaultBindings = Map defaults; ParamSlots = Map slots }

/// A sheet whose one output width is its parameter W, defaulting to 4.
let private widthSheet (name: string) =
    let out = makeComp "o" 1 0 (Output 4) "O"
    let wExpr = { Expression = PParameter(ParamName "W"); Constraints = [] }
    makeLdc name
        (Some (paramDefs [ declares "W" (PInt 4) ] [ { CompId = "o"; CompSlot = IO "O" }, wExpr ]))
        ([ out ], [])

/// An instance of `child` on a parent sheet, binding W to the given expression (or to nothing).
let private instance (child: LoadedComponent) (id: string) (binding: ParamExpression option) =
    let bindings = binding |> Option.map (fun e -> Map [ ParamName "W", e ])
    makeComp id 0 1 (customOf child [] [ "O", 4 ] bindings) (id.ToUpper())

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
            let top = sheetOf "top" [ instance child "i1" (Some (PInt 8)) ]
            Expect.isTrue (ParameterAnalysis.projectDeclaresParams [ top; child ]) "child declares W"
        }

        test "a library component's own parameters do not count as the user's" {
            // Placing a parameterised library component must not expose the vocabulary to someone
            // who never asked for it: on the instance those values are presented as settings.
            let child = widthSheet "L1_reg" |> asLibrary "stdlib" "reg"
            let top = sheetOf "top" [ instance child "i1" (Some (PInt 8)) ]
            Expect.isFalse (ParameterAnalysis.projectDeclaresParams [ top; child ])
                "the parameter arrived with the library"
        }

        // --- Gate B: is a top sheet needed? ---

        test "one instance is not ambiguous" {
            // The old test was the mere presence of a parameter, which turned the whole top-sheet
            // apparatus on here, where there is nothing whatever to settle.
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child "i1" (Some (PInt 8)) ]
            Expect.isFalse (ParameterAnalysis.projectHasAmbiguousDisplay [ top; child ])
                "a single instance settles the value by itself"
        }

        test "instances that agree are not ambiguous" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child "i1" (Some (PInt 8))
                                      instance child "i2" (Some (PInt 8)) ]
            Expect.isFalse (ParameterAnalysis.projectHasAmbiguousDisplay [ top; child ])
                "both instances give the same value"
        }

        test "instances that disagree are ambiguous" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child "i1" (Some (PInt 8))
                                      instance child "i2" (Some (PInt 16)) ]
            Expect.isTrue (ParameterAnalysis.projectHasAmbiguousDisplay [ top; child ])
                "the editor cannot draw child at both widths"
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
            let top = sheetOf "top" [ instance child "i1" (Some (PInt 8))
                                      instance child "i2" (Some (PInt 16)) ]
            Expect.isFalse (ParameterAnalysis.projectHasAmbiguousDisplay [ top; child ])
                "the sheet the instances disagree about cannot be opened"
        }

        test "two independent designs using one sheet at different widths are ambiguous" {
            // Merging across forest roots is deliberate: opening child, which design is it part of?
            let child = widthSheet "child"
            let topA = sheetOf "topA" [ instance child "i1" (Some (PInt 8)) ]
            let topB = sheetOf "topB" [ instance child "i2" (Some (PInt 16)) ]
            Expect.isTrue (ParameterAnalysis.projectHasAmbiguousDisplay [ topA; topB; child ])
                "the two roots disagree about child"
        }

        // --- totality: an instance parameter is never unbound ---

        test "a parameter added to a sheet with instances leaves none of them unbound" {
            // Placing an instance asks for every parameter, but adding one to a sheet that already
            // has instances is the hole that would otherwise leave them all binding nothing.
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child "i1" None; instance child "i2" None ]
            Expect.isFalse (ParameterAnalysis.everyInstanceBindsEveryParam [ top; child ])
                "the instances start out bound to nothing"
            let fixed' = ParameterAnalysis.bindParamOnInstances "child" (ParamName "W") 4 [ top; child ]
            Expect.isTrue (ParameterAnalysis.everyInstanceBindsEveryParam fixed')
                "every instance now binds W"
        }

        test "binding instances leaves an instance that already had a value alone" {
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child "i1" (Some (PInt 16)); instance child "i2" None ]
            let fixed' = ParameterAnalysis.bindParamOnInstances "child" (ParamName "W") 4 [ top; child ]
            let bindingOf id =
                fixed'
                |> List.pick (fun ldc -> if ldc.Name = "top" then Some (fst ldc.CanvasState) else None)
                |> List.pick (fun comp ->
                    match comp.Id = id, comp.Type with
                    | true, Custom cc -> cc.ParameterBindings |> Option.bind (Map.tryFind (ParamName "W"))
                    | _ -> None)
            Expect.equal (bindingOf "i1") (PInt 16) "the chosen value survives"
            Expect.equal (bindingOf "i2") (PInt 4) "the one with none takes the declared value"
        }

        test "the sheet gaining the parameter is not touched" {
            // A sheet cannot instantiate itself, but the skip is what makes that explicit.
            let child = widthSheet "child"
            let before = [ child ]
            Expect.equal (ParameterAnalysis.bindParamOnInstances "child" (ParamName "W") 4 before) before
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
                    (Some (paramDefs [ declares "W" (PInt 8) ] []))
                    ([ instance child "i1" (Some (PInt 8)) ], [])
            let offers = ParameterAnalysis.findBindOffers [ top; child ] "top" None
            Expect.equal (List.length offers) 1 "one instance, one offer"
            Expect.equal offers[0].Param (ParamName "W") "for the parameter of that name"
            Expect.equal offers[0].BindsTo "top" "bound up to the sheet that declares it"
        }

        test "an instance already following a parameter is not offered anything" {
            let child = widthSheet "child"
            let top =
                makeLdc "top"
                    (Some (paramDefs [ declares "W" (PInt 8) ] []))
                    ([ instance child "i1" (Some (PParameter(ParamName "W"))) ], [])
            Expect.isEmpty (ParameterAnalysis.findBindOffers [ top; child ] "top" None)
                "it already stays in step"
        }

        test "the evidence gate keeps the offer quiet with no ancestor declarer" {
            // A literal binding is not on its own evidence of a design constant.
            let child = widthSheet "child"
            let top = sheetOf "top" [ instance child "i1" (Some (PInt 8)) ]
            Expect.isEmpty (ParameterAnalysis.findBindOffers [ top; child ] "top" None)
                "no sheet above declares W"
        }
    ]
