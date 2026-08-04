/// What a custom component instance's ports are, and keeping instances in step with the sheet
/// inside them.
///
/// A sheet that declares parameters has no single signature: it has a family of them, one per set
/// of bindings, and two instances of it may legitimately be at different widths. CustomCompPorts
/// used to assume the opposite - that every instance must equal the sheet - so it reported every
/// parameterised design as changed, and "fixing" one forced all its instances to the sheet's
/// declared widths while leaving their bindings alone. These tests pin the replacement invariant:
///
///     an instance is out of date exactly when it differs from what ITS OWN bindings give it,
///     which is CanvasExtractor.signatureOfInstance.
module InstanceSignatures

open Expecto
open CommonTypes
open ParameterTypes
open CanvasBuilder

/// A parameter declaration. Descriptions are compulsory but say nothing these tests depend on.
let private declares (name: string) (expr: ParamExpression) =
    ParamName name, {Expression = expr; Description = $"test parameter {name}"}

let private paramDefs (defaults: (ParamName * ParamDefinition) list) (slots: (ParamSlot * ConstrainedExpr) list) =
    {DefaultBindings = Map defaults; ParamSlots = Map slots}

let private wExpr = {Expression = PParameter (ParamName "W"); Constraints = []}

/// The child sheet under test: one input A and one output S, both W bits wide, W defaulting to 4.
/// A is above S on the canvas, so the ordered signature is ([A], [S]).
let private childLdc =
    let a = {makeComp "a" 0 1 (Input1 (4, None)) "A" with Y = 0.}
    let s = {makeComp "s" 1 0 (Output 4) "S" with Y = 100.}
    makeLdc "child"
        (Some (paramDefs [declares "W" (PInt 4)]
                         [{CompId = "a"; CompSlot = IO "A"}, wExpr
                          {CompId = "s"; CompSlot = IO "S"}, wExpr]))
        ([a; s], [conn a 0 s 0])

/// An instance of the child, whose stored ports may deliberately be wrong so that the code under
/// test has something to correct.
let private instanceOf (id: string) (storedWidth: int) (bindings: ParamBindings option) =
    makeComp id 1 1 (customOf childLdc ["A", storedWidth] ["S", storedWidth] bindings) (id.ToUpper())

let private bindingOf (w: int) = Some (Map [ParamName "W", PInt w])

/// The instance record CustomCompPorts works on, built the way getInstancesOfCurrentSheet builds
/// it: the instance's stored ports against the ports its own bindings give it.
let private instanceUnderTest
        (ldcs: LoadedComponent list)
        (parentLdc: LoadedComponent)
        (comp: Component)
        : CustomCompPorts.Instance =
    match comp.Type with
    | Custom cc ->
        let bindingExprs =
            ParameterAnalysis.instanceBindingExprs (ParameterAnalysis.sheetParamSlots parentLdc) comp cc
        let expected =
            CanvasExtractor.signatureOfInstance ldcs (ParameterAnalysis.declaredParams parentLdc) cc.Name bindingExprs
            |> Option.defaultWith (fun () -> failtest $"no signature for {comp.Label}")
        { Sheet = parentLdc.Name
          CompId = comp.Id
          Label = comp.Label
          Old = (cc.InputLabels, cc.OutputLabels)
          Expected = expected }
    | _ -> failtest $"{comp.Label} is not a custom component"

/// The custom component type of `cid` on `sheet` after the project has been updated.
let private customOn (p: Project) (sheet: string) (cid: string) =
    p.LoadedComponents
    |> List.find (fun ldc -> ldc.Name = sheet)
    |> fun ldc -> fst ldc.CanvasState
    |> List.find (fun comp -> comp.Id = cid)
    |> fun comp ->
        match comp.Type with
        | Custom cc -> cc
        | t -> failtest $"expected a custom component, got {t}"

let tests =
    testList "InstanceSignatures" [

        // --- the signature an instance ought to have ---

        test "an instance at the sheet's declared value has the sheet's own widths" {
            let widths =
                CanvasExtractor.signatureOfInstance [childLdc] Map.empty "child" (Map [ParamName "W", PInt 4])
            Expect.equal widths (Some (["A", 4], ["S", 4])) "W = 4 is what the sheet declares"
        }

        test "an instance at another value has that value's widths" {
            let widths =
                CanvasExtractor.signatureOfInstance [childLdc] Map.empty "child" (Map [ParamName "W", PInt 16])
            Expect.equal widths (Some (["A", 16], ["S", 16])) "the instance's own binding sizes its ports"
        }

        test "a binding that is a parent parameter is evaluated on the parent" {
            // the whole point of binding to an enclosing sheet's parameter: evaluated in the
            // child's own environment it looks self-referential, and the width came out as zero
            let parentBindings = Map [ParamName "W", PInt 12]
            let widths =
                CanvasExtractor.signatureOfInstance [childLdc] parentBindings "child"
                    (Map [ParamName "W", PParameter (ParamName "W")])
            Expect.equal widths (Some (["A", 12], ["S", 12])) "the instance follows the parent's W"
        }

        test "a parameter the instance leaves alone takes the sheet's default" {
            let widths = CanvasExtractor.signatureOfInstance [childLdc] Map.empty "child" Map.empty
            Expect.equal widths (Some (["A", 4], ["S", 4])) "nothing bound, so the declared value"
        }

        test "widths are inexact when a binding cannot be evaluated here" {
            // a canvas checked on its own knows nothing of the sheet that contains it, so the
            // widths must not then be compared - CanvasStateAnalyser reports port names only
            let _, exact =
                CanvasExtractor.signatureOfInstanceWithCertainty [childLdc] Map.empty "child"
                    (Map [ParamName "W", PParameter (ParamName "OUTER")])
                |> Option.defaultWith (fun () -> failtest "no signature")
            Expect.isFalse exact "OUTER cannot be resolved without the parent's bindings"
        }

        test "widths are exact when every binding resolves" {
            let _, exact =
                CanvasExtractor.signatureOfInstanceWithCertainty [childLdc] Map.empty "child"
                    (Map [ParamName "W", PInt 16])
                |> Option.defaultWith (fun () -> failtest "no signature")
            Expect.isTrue exact "a literal needs no environment"
        }

        // --- out of date means out of step with ITS OWN bindings ---

        test "an instance bound away from the default is not out of date" {
            // the regression this whole reconciliation exists for: this raised the "you have
            // changed the inputs or outputs" dialog on every save of any parameterised design
            let inst = instanceOf "i16" 16 (bindingOf 16)
            let parent = makeLdc "parent" None ([inst], [])
            let under = instanceUnderTest [childLdc; parent] parent inst
            Expect.equal under.Old under.Expected "16-bit instance of a sheet declared at 4 is correct"
        }

        test "instances at different widths are both up to date" {
            let i8 = instanceOf "i8" 8 (bindingOf 8)
            let i16 = instanceOf "i16" 16 (bindingOf 16)
            let parent = makeLdc "parent" None ([i8; i16], [])
            let ldcs = [childLdc; parent]
            Expect.equal (instanceUnderTest ldcs parent i8).Old (instanceUnderTest ldcs parent i8).Expected "8"
            Expect.equal (instanceUnderTest ldcs parent i16).Old (instanceUnderTest ldcs parent i16).Expected "16"
        }

        test "an instance whose stored ports contradict its bindings is out of date" {
            let stale = instanceOf "stale" 4 (bindingOf 8)
            let parent = makeLdc "parent" None ([stale], [])
            let under = instanceUnderTest [childLdc; parent] parent stale
            Expect.notEqual under.Old under.Expected "stored at 4 while bound to 8"
            Expect.equal under.Expected (["A", 8], ["S", 8]) "and 8 is what it should be"
        }

        // --- each instance is brought to its OWN signature, not to a shared one ---

        test "updating instances gives each one the widths its own bindings give it" {
            let stale8 = instanceOf "stale8" 4 (bindingOf 8)
            let stale16 = instanceOf "stale16" 4 (bindingOf 16)
            let parent = makeLdc "parent" None ([stale8; stale16], [])
            let ldcs = [childLdc; parent]
            let project =
                { ProjectPath = ""; OpenFileName = "child"; WorkingFileName = None; LoadedComponents = ldcs }
            let updated =
                (project, [instanceUnderTest ldcs parent stale8; instanceUnderTest ldcs parent stale16])
                ||> List.fold (fun p inst -> CustomCompPorts.updateInstance inst p)
            Expect.equal (customOn updated "parent" "stale8").InputLabels ["A", 8]
                "the 8-bit instance goes to 8, not to the sheet's declared 4"
            Expect.equal (customOn updated "parent" "stale16").InputLabels ["A", 16]
                "and the 16-bit instance to 16, not to the other instance's 8"
        }

        test "a port added to the sheet reaches each instance at that instance's own width" {
            // the case this dialog has always existed for, and the one the reconciliation must not
            // lose: the port is the sheet's, the width is the instance's
            let grownChild =
                let a = {makeComp "a" 0 1 (Input1 (4, None)) "A" with Y = 0.}
                let b = {makeComp "b" 0 1 (Input1 (4, None)) "B" with Y = 50.}
                let s = {makeComp "s" 1 0 (Output 4) "S" with Y = 100.}
                makeLdc "child"
                    (Some (paramDefs [declares "W" (PInt 4)]
                                     [{CompId = "a"; CompSlot = IO "A"}, wExpr
                                      {CompId = "b"; CompSlot = IO "B"}, wExpr
                                      {CompId = "s"; CompSlot = IO "S"}, wExpr]))
                    ([a; b; s], [conn a 0 s 0])
            let i16 = instanceOf "i16" 16 (bindingOf 16)
            let parent = makeLdc "parent" None ([i16], [])
            let ldcs = [grownChild; parent]
            let project =
                { ProjectPath = ""; OpenFileName = "child"; WorkingFileName = None; LoadedComponents = ldcs }
            let updated = CustomCompPorts.updateInstance (instanceUnderTest ldcs parent i16) project
            let cc = customOn updated "parent" "i16"
            Expect.equal cc.InputLabels ["A", 16; "B", 16] "B is added, at the instance's 16 rather than the sheet's 4"
            let comp =
                updated.LoadedComponents
                |> List.find (fun l -> l.Name = "parent")
                |> fun l -> fst l.CanvasState |> List.find (fun c -> c.Id = "i16")
            Expect.equal (comp.InputPorts |> List.map (fun p -> p.PortNumber)) [Some 0; Some 1]
                "and the instance has a port for it, numbered contiguously"
        }

        test "a port deleted from the sheet is deleted from each instance" {
            let shrunkChild =
                let s = {makeComp "s" 1 0 (Output 4) "S" with Y = 100.}
                makeLdc "child"
                    (Some (paramDefs [declares "W" (PInt 4)] [{CompId = "s"; CompSlot = IO "S"}, wExpr]))
                    ([s], [])
            let i16 = instanceOf "i16" 16 (bindingOf 16)
            let parent = makeLdc "parent" None ([i16], [])
            let ldcs = [shrunkChild; parent]
            let project =
                { ProjectPath = ""; OpenFileName = "child"; WorkingFileName = None; LoadedComponents = ldcs }
            let updated = CustomCompPorts.updateInstance (instanceUnderTest ldcs parent i16) project
            let cc = customOn updated "parent" "i16"
            Expect.isEmpty cc.InputLabels "A is gone"
            Expect.equal cc.OutputLabels ["S", 16] "and S keeps the instance's width"
        }

        test "updating an instance leaves its parameter bindings alone" {
            // forcing the ports while leaving the bindings is what produced an instance the
            // simulator rejects: the two must stay in step
            let stale = instanceOf "stale" 4 (bindingOf 8)
            let parent = makeLdc "parent" None ([stale], [])
            let ldcs = [childLdc; parent]
            let project =
                { ProjectPath = ""; OpenFileName = "child"; WorkingFileName = None; LoadedComponents = ldcs }
            let updated = CustomCompPorts.updateInstance (instanceUnderTest ldcs parent stale) project
            let cc = customOn updated "parent" "stale"
            Expect.equal cc.ParameterBindings (bindingOf 8) "the binding is untouched"
            Expect.equal (cc.InputLabels, cc.OutputLabels) (["A", 8], ["S", 8]) "and the ports now agree with it"
        }

        test "a sheet needing an update is flagged so that it reaches disk" {
            let stale = instanceOf "stale" 4 (bindingOf 8)
            let parent = makeLdc "parent" None ([stale], [])
            let ldcs = [childLdc; parent]
            let project =
                { ProjectPath = ""; OpenFileName = "child"; WorkingFileName = None; LoadedComponents = ldcs }
            let updated = CustomCompPorts.updateInstance (instanceUnderTest ldcs parent stale) project
            Expect.isTrue
                (updated.LoadedComponents |> List.find (fun l -> l.Name = "parent")).LoadedComponentIsOutOfDate
                "the sheet holding the instance has unsaved changes"
        }

        // --- what the dialog reports ---

        test "a width-only difference produces no port change to confirm" {
            // widths are a fact about each instance; listing one instance's numbers as the
            // sheet's is what made the dialog describe a parameterised design wrongly
            let stale = instanceOf "stale" 4 (bindingOf 8)
            let parent = makeLdc "parent" None ([stale], [])
            let structural, widthOnly =
                CustomCompPorts.reportedChanges [instanceUnderTest [childLdc; parent] parent stale]
            Expect.isEmpty structural "no port was added, deleted or renamed"
            Expect.equal (widthOnly |> List.map (fun i -> i.Label)) ["STALE"]
                "the one instance changing in width alone is named"
        }

        test "a renamed port is reported once however many instances there are" {
            let renamedChild =
                let a = {makeComp "a" 0 1 (Input1 (4, None)) "AA" with Y = 0.}
                let s = {makeComp "s" 1 0 (Output 4) "S" with Y = 100.}
                makeLdc "child"
                    (Some (paramDefs [declares "W" (PInt 4)]
                                     [{CompId = "a"; CompSlot = IO "A"}, wExpr
                                      {CompId = "s"; CompSlot = IO "S"}, wExpr]))
                    ([a; s], [conn a 0 s 0])
            let i8 = instanceOf "i8" 8 (bindingOf 8)
            let i16 = instanceOf "i16" 16 (bindingOf 16)
            let parent = makeLdc "parent" None ([i8; i16], [])
            let ldcs = [renamedChild; parent]
            let structural, _ =
                CustomCompPorts.reportedChanges
                    [instanceUnderTest ldcs parent i8; instanceUnderTest ldcs parent i16]
            Expect.equal (List.length structural) 1 "one rename, not one per instance"
            Expect.equal structural[0].OldName (Some "A") "from A"
            Expect.equal structural[0].NewName (Some "AA") "to AA"
        }

        test "a rename is not guessed between ports on opposite sides" {
            // keyed on width alone, an input added at width 8 was paired with an output deleted at
            // width 8, and the instance was left half-changed
            let changes =
                CustomCompPorts.ioCompareSigs (["NEW", 8], []) ([], ["OLD", 8])
                |> CustomCompPorts.guessAtRenamedPorts
                |> Array.toList
            Expect.equal (List.length changes) 2 "an added input and a deleted output, not one rename"
            Expect.isTrue
                (changes |> List.exists (fun c -> c.New = Some ("NEW", 8) && c.Old = None))
                "the input is added"
            Expect.isTrue
                (changes |> List.exists (fun c -> c.Old = Some ("OLD", 8) && c.New = None))
                "the output is deleted"
        }

        test "a rename is still guessed between ports on the same side" {
            let changes =
                CustomCompPorts.ioCompareSigs (["NEW", 8], []) (["OLD", 8], [])
                |> CustomCompPorts.guessAtRenamedPorts
                |> Array.toList
            Expect.equal (List.length changes) 1 "one rename"
            Expect.equal changes[0].Old (Some ("OLD", 8)) "from OLD"
            Expect.equal changes[0].New (Some ("NEW", 8)) "to NEW"
        }

        // --- a slot survives its component being renamed ---

        test "an IO slot is the same slot after its component is renamed" {
            let stored = {CompId = "a"; CompSlot = IO "A"}
            let asked = {CompId = "a"; CompSlot = IO "RENAMED"}
            Expect.isTrue (ParameterTypes.sameSlot stored asked) "the label is not part of a slot's identity"
            Expect.isFalse (ParameterTypes.sameSlot stored {CompId = "b"; CompSlot = IO "A"})
                "but the component is"
            Expect.isFalse
                (ParameterTypes.sameSlot {CompId = "a"; CompSlot = Buswidth} {CompId = "a"; CompSlot = InputDefault})
                "and so is which field of it"
        }

        test "a slot is found under the component's old label" {
            let slots = Map [{CompId = "a"; CompSlot = IO "A"}, wExpr]
            Expect.equal
                (ParameterTypes.tryFindSlot {CompId = "a"; CompSlot = IO "RENAMED"} slots)
                (Some wExpr)
                "the expression is still this field's, so the properties pane still shows it"
        }

        test "setting a renamed component's slot replaces the old one rather than adding a second" {
            // two slots for one field left them to fight over the component's width in Map key order
            let newExpr = {Expression = PParameter (ParamName "N"); Constraints = []}
            let slots =
                Map [{CompId = "a"; CompSlot = IO "A"}, wExpr]
                |> ParameterTypes.addSlot {CompId = "a"; CompSlot = IO "RENAMED"} newExpr
            Expect.equal (Map.count slots) 1 "one slot for one field"
            Expect.equal (Map.toList slots |> List.head |> snd) newExpr "and it is the new expression"
        }

        test "clearing a renamed component's slot clears the one stored under the old label" {
            let slots =
                Map [{CompId = "a"; CompSlot = IO "A"}, wExpr]
                |> ParameterTypes.removeSlot {CompId = "a"; CompSlot = IO "RENAMED"}
            Expect.isEmpty slots "the field goes back to being an ordinary number"
        }

        test "a renamed component's slot still sizes the instance's port" {
            // the slot was created as IO "A"; the sheet now calls that input RENAMED
            let a = {makeComp "a" 0 1 (Input1 (4, None)) "RENAMED" with Y = 0.}
            let s = {makeComp "s" 1 0 (Output 4) "S" with Y = 100.}
            let ldc =
                makeLdc "child"
                    (Some (paramDefs [declares "W" (PInt 4)]
                                     [{CompId = "a"; CompSlot = IO "A"}, wExpr
                                      {CompId = "s"; CompSlot = IO "S"}, wExpr]))
                    ([a; s], [conn a 0 s 0])
            let widths = CanvasExtractor.signatureOfInstance [ldc] Map.empty "child" (Map [ParamName "W", PInt 16])
            Expect.equal widths (Some (["RENAMED", 16], ["S", 16]))
                "the width follows the slot, and the name the component"
        }

        test "saving repoints a slot at its component's current label and drops dead ones" {
            let a = {makeComp "a" 0 1 (Input1 (4, None)) "RENAMED" with Y = 0.}
            let defs =
                paramDefs [declares "W" (PInt 4)]
                          [{CompId = "a"; CompSlot = IO "A"}, wExpr
                           {CompId = "gone"; CompSlot = IO "GONE"}, wExpr]
            let tidied =
                CanvasExtractor.tidyParamSlots ([a], []) (Some defs)
                |> Option.defaultWith (fun () -> failtest "no definitions")
            Expect.equal (Map.toList tidied.ParamSlots |> List.map fst)
                [{CompId = "a"; CompSlot = IO "RENAMED"}]
                "the live slot is repointed and the slot of a deleted component is gone"
        }

        // --- a parameter-only edit is still an unsaved change ---

        test "a sheet whose parameters were edited is flagged as needing saving" {
            // declaring a parameter, describing it, or entering an expression that works out to the
            // width already shown all leave the canvas identical, and canvas comparison is how
            // Issie decides whether to save
            let other = makeLdc "other" None ([], [])
            let marked = ParameterAnalysis.markSheetOutOfDate "child" [childLdc; other]
            Expect.isTrue (marked |> List.find (fun l -> l.Name = "child")).LoadedComponentIsOutOfDate
                "the edited sheet needs saving"
            Expect.isFalse (marked |> List.find (fun l -> l.Name = "other")).LoadedComponentIsOutOfDate
                "and no other sheet is disturbed"
        }

        test "a parameter-only change is invisible to canvas comparison" {
            // the premise the flag exists for: without it nothing whatever would notice
            let withParam =
                {childLdc with
                    LCParameterSlots =
                        Some (paramDefs [declares "W" (PInt 4); declares "N" (PInt 2)] [])}
            Expect.isTrue (CanvasExtractor.stateIsEqual childLdc.CanvasState withParam.CanvasState)
                "declaring a parameter changes no component"
        }
    ]
