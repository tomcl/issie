module ArrayExpand

(*
    ArrayExpand.fs

    An ARRAY DESIGN SHEET is a sheet whose hardware is several copies of what is drawn on it, one
    per value of a loop variable. This module is what that means: which copy joins to which, what
    ports the sheet therefore has, and (later) the expansion itself.

    See CommonTypes.ArrayInfo for the settings a sheet carries, and CommonTypes.ComponentType for
    the four components that say how the copies join up - BusOut, MuxOut, JoinOut and JoinIn.

    Compiled before CanvasExtractor, because parseDiagramSignature asks this module what an array
    sheet's ports are. It therefore depends on nothing but CommonTypes and the parameter types,
    which is also what makes it testable on its own.

    NOTHING HERE THROWS OR REFUSES. A sheet being edited passes through states whose joins do not
    work out, and its ports still have to be drawn while that is true. So every function gives its
    best reading of the sheet along with a list of what is wrong with it; CanvasStateAnalyser is
    what turns those into an error and refuses to simulate.
*)

open CommonTypes
open ParameterTypes

/// How many bits the select input of an array multiplexer has: enough to index the copies, and
/// never zero, since a width of zero is not a width.
///
/// ceil(log2 copies), counted by shifting an int rather than through ParameterTypes.clog2, which
/// takes a bigint. A copy count is small by construction - ArrayElaborate.Constants.maxArrayCopies
/// bounds it, as a bus width is bounded - so converting one to a bigint to count its bits would be
/// work for nothing. The same answer as clog2 for every value this is asked about.
let arraySelectWidth (copies: int) =
    let rec bits v acc = if v <= 1 then acc else bits (v >>> 1) (acc + 1)
    max 1 (if copies <= 1 then 0 else bits (copies - 1) 0 + 1)

/// The name of the sheet port an unmatched JoinIn becomes. The direction is in the name as well as
/// in the port list so that the two ends of one channel read apart, and so that the generated names
/// sit further from anything a user would write.
let joinInPortName (label: string) (num: int) = $"{label}_in_{num}"

/// The name of the sheet port an unmatched JoinOut becomes.
let joinOutPortName (label: string) (num: int) = $"{label}_out_{num}"

/// The name of the select input a MuxOut adds to the sheet, beside the output of its own name.
let muxSelectPortName (label: string) = label + "_sel"

/// Something wrong with an array component's sheet: what to say, and what to point at.
///
/// The message says WHAT IS NOT ALLOWED and stops. Why it is not allowed belongs in the
/// documentation - a user reading an error is trying to fix a sheet, not to learn the design of the
/// feature, and a sentence of reasoning after the fault buries it.
///
/// Components are what the simulator highlights in red, so a problem names the ones a user has to
/// look at. Empty only where the fault is the sheet's own settings and no component is at fault.
type ArrayProblem = {
    Message: string
    Components: ComponentId list
}

/// One end of a channel: a join component, in one copy, on the channel that copy puts it on.
type JoinEnd = {
    /// The join component as drawn on the array sheet.
    Comp: Component
    /// Which copy this end belongs to, 0 .. copies-1.
    Copy: int
    /// The channel it is on in that copy - its number expression evaluated with the loop variable
    /// set to Copy.
    Num: int
}

/// What the joins of an array design sheet come to.
type JoinWiring = {
    /// A JoinOut end and the JoinIn end taking from it: one wire between two copies.
    Matched: (JoinEnd * JoinEnd) list
    /// JoinOut ends no copy takes from. Each becomes an OUTPUT on the sheet's outline.
    UnmatchedOut: JoinEnd list
    /// JoinIn ends no copy supplies. Each becomes an INPUT on the sheet's outline.
    UnmatchedIn: JoinEnd list
    /// EVERY end, matched or not, in every copy: which channel each join component is on in each
    /// copy of the array. What that is for is naming - a copy's ports are the channels it is
    /// actually joined by, and copy 3's carry in is not the one drawn on the sheet.
    Ends: JoinEnd list
    /// What is wrong with the joins, if anything. The wiring above is still the best reading of
    /// them, so a sheet being edited can still be drawn; analyseState is what refuses to simulate.
    Problems: ArrayProblem list
}

/// The joins drawn on a sheet, in the (Y, X) order everything about a sheet's ports follows.
let private joinComps (isOut: bool) ((comps, _): CanvasState) =
    comps
    |> List.filter (fun comp ->
        match comp.Type, isOut with
        | JoinOut _, true | JoinIn _, false -> true
        | _ -> false)
    |> List.sortBy (fun comp -> comp.Y, comp.X)

/// The width and stored channel number of a join.
let private joinWidthAndNum (comp: Component) =
    match comp.Type with
    | JoinOut (w, n) | JoinIn (w, n) -> w, n
    | t -> failwithf $"joinWidthAndNum called on {t}, which is not a join"

/// Which channel one join is on in one copy.
///
/// The number is a parameter slot, so what decides it is the EXPRESSION the sheet stores for that
/// slot, evaluated with the loop variable set to this copy - not the integer on the component,
/// which is only what the sheet is drawn at. The loop variable is the only name in scope, which is
/// what makes the answer a fact about the sheet rather than about whoever instantiated it.
let private channelOf (info: ArrayInfo) (slots: ComponentSlotExpr) (comp: Component) (copy: int) =
    let _, stored = joinWidthAndNum comp
    match tryFindSlot {CompId = comp.Id; CompSlot = JoinNum} slots with
    | None -> Ok stored
    | Some exprSpec ->
        let bindings = Map [info.LoopParam, PInt (bigint copy)]
        match evaluateParamExpression bindings exprSpec.Expression with
        | Ok value ->
            match tryIntOfParamInt value with
            | Some n -> Ok n
            | None -> Error $"is too large to be a channel number"
        // A name the evaluator does not know is reported HERE rather than passed on, because its
        // message names what is in scope as "properties of this sheet" - and the one name in scope
        // for a channel number is the LOOP VARIABLE, which is deliberately not a property. Its
        // wording would name the right word and call it the wrong thing. Anything else the
        // evaluator objects to - a shift too far, a division by zero - it says better than this
        // could, so that is passed through with the sentence made to read.
        | Error msg ->
            let (ParamName loop) = info.LoopParam
            let outOfScope =
                paramNamesOfExpr exprSpec.Expression
                |> List.filter (fun name -> name <> info.LoopParam)
                |> List.distinct
                |> List.map (fun (ParamName n) -> $"'{n}'")
            match outOfScope with
            | [] -> Error $"cannot be worked out - {msg}"
            | names ->
                let named = String.concat ", " names
                Error $"names {named}; only the loop variable '{loop}' may be used"

/// Every end of every join, copy by copy, with whatever could not be worked out reported and the
/// number the sheet is drawn at used in its place.
let private endsOf (info: ArrayInfo) (slots: ComponentSlotExpr) (canvas: CanvasState) (isOut: bool) =
    let side = if isOut then "Join out" else "Join in"
    ((([]: JoinEnd list), ([]: ArrayProblem list)), joinComps isOut canvas)
    ||> List.fold (fun (ends, problems) comp ->
        (((ends, problems)), [0 .. copiesOfArray info - 1])
        ||> List.fold (fun (ends, problems) copy ->
            match channelOf info slots comp copy with
            | Ok num ->
                {Comp = comp; Copy = copy; Num = num} :: ends, problems
            | Error msg ->
                let _, stored = joinWidthAndNum comp
                {Comp = comp; Copy = copy; Num = stored} :: ends,
                {Message = $"{side} '{comp.Label}': channel number {msg}"
                 Components = [comp.Id]} :: problems))
    |> fun (ends, problems) -> List.rev ends, List.rev problems

/// Labels that must be distinct within one side of the joins.
///
/// A join's label names a CHANNEL, so a JoinOut and the JoinIn reading it share one - which is why
/// checkComponentNamesAreOk exempts joins from the sheet-wide unique-name rule. What it does not
/// exempt them from is being distinct among themselves on each side: each join is one port of the
/// copy, so two on a side sharing a label are two ports with one name.
let private duplicateLabels (side: string) (comps: Component list) =
    comps
    |> List.countBy (fun comp -> comp.Label, snd (joinWidthAndNum comp))
    |> List.filter (fun (_, n) -> n > 1)
    |> List.map (fun ((label, num), n) ->
        { Message = $"{n} {side} components are on channel {num} of '{label}'"
          Components =
            comps
            |> List.filter (fun c -> c.Label = label && snd (joinWidthAndNum c) = num)
            |> List.map (fun c -> c.Id) })

/// Which JoinOut in which copy drives which JoinIn in which copy, and which ends are left over.
///
/// The one place join semantics live. parseDiagramSignature asks it what ports the sheet has, and
/// the expansion asks it what wires to draw between the copies, so the two cannot disagree about
/// which end of a chain is loose.
///
/// A JoinOut end and a JoinIn end MATCH when they carry the same label and the same channel number;
/// a match is a wire from one copy to another. Everything left over becomes a port on the sheet's
/// outline - an unmatched JoinOut an output, an unmatched JoinIn an input - which is what makes the
/// ends of a chain the array's own connections without anything having to say which copies they are.
let joinsOf (info: ArrayInfo) (paramDefs: ParameterDefs option) (canvas: CanvasState) : JoinWiring =
    let slots = paramDefs |> Option.map (fun defs -> defs.ParamSlots) |> Option.defaultValue Map.empty
    let outEnds, outProblems = endsOf info slots canvas true
    let inEnds, inProblems = endsOf info slots canvas false

    let negatives =
        outEnds @ inEnds
        |> List.filter (fun e -> e.Num < 0)
        |> List.map (fun e ->
            { Message = $"'{e.Comp.Label}' is on negative channel {e.Num} in copy {e.Copy}"
              Components = [e.Comp.Id] })

    let nameProblems =
        duplicateLabels "Join out" (joinComps true canvas)
        @ duplicateLabels "Join in" (joinComps false canvas)

    /// The end DRIVING each channel. One only: two JoinOut ends on one channel are two drivers of
    /// one wire, which is the mistake a net with two sources always is.
    ///
    /// Its twin does not exist, and deliberately. Several JoinIn ends may sit on one channel and
    /// that is ordinary fan-out - two copies both reading the carry that a third produces - so the
    /// in ends are kept as a list and every one of them is wired.
    let grouped = outEnds |> List.groupBy (fun e -> e.Comp.Label, e.Num)

    /// One COMPONENT on one channel in several copies. Several components on one channel is the
    /// other shape of the same fault, and duplicateLabels has already said so in words that fit it -
    /// counted here as well, this printed the same copy twice ("in more than one copy (0, 0)") and
    /// said it once per channel, so one mistake arrived as four messages none of which read right.
    let outClashes =
        grouped
        |> List.filter (fun (_, es) ->
            List.length es > 1 && (es |> List.distinctBy (fun e -> e.Comp.Id) |> List.length) = 1)
        |> List.map (fun ((label, num), es) ->
            let copies = es |> List.map (fun e -> string e.Copy) |> String.concat ", "
            { Message = $"Join out '{label}' drives channel {num} in copies {copies}"
              Components = es |> List.map (fun e -> e.Comp.Id) |> List.distinct })

    let outByChannel =
        grouped
        |> List.choose (fun (key, es) -> es |> List.tryHead |> Option.map (fun e -> key, e))
        |> Map.ofList

    /// Every in end paired with the out end driving its channel: one wire each, so a channel read
    /// by two copies gives two wires from the one that drives it.
    let matched =
        inEnds
        |> List.choose (fun inEnd ->
            Map.tryFind (inEnd.Comp.Label, inEnd.Num) outByChannel
            |> Option.map (fun outEnd -> outEnd, inEnd))
        |> List.sortBy (fun (o, i) -> o.Comp.Y, o.Comp.X, o.Copy, i.Copy)

    /// The channels something reads, so that a driver with no reader can be told from one with two.
    let channelsRead = inEnds |> List.map (fun e -> e.Comp.Label, e.Num) |> Set.ofList

    { Matched = matched
      Ends = outEnds @ inEnds
      UnmatchedOut = outEnds |> List.filter (fun e -> not (Set.contains (e.Comp.Label, e.Num) channelsRead))
      UnmatchedIn = inEnds |> List.filter (fun e -> not (Map.containsKey (e.Comp.Label, e.Num) outByChannel))
      Problems = nameProblems @ outProblems @ inProblems @ negatives @ outClashes }

/// The unmatched ends of one join component, grouped by the channel they are on and in channel
/// order, so that a sheet with several loose ends lists them predictably.
///
/// One PORT per channel, and EVERY end on that channel with it. A port is named after the channel,
/// so a channel several copies are loose on can only be one port - but it is a port of all of them,
/// and each has to be wired to it. A number that does not vary with the loop variable is the
/// extreme case (every copy on one channel); `i/2` is the ordinary one, giving the array one input
/// per pair of copies. Keeping one end per channel and dropping the rest wired the first copy and
/// left the others' body ports dangling, which came out as an unconnected port on a sheet the user
/// cannot open.
let looseEndsOf (ends: JoinEnd list) (comp: Component) : (int * JoinEnd list) list =
    ends
    |> List.filter (fun e -> e.Comp.Id = comp.Id)
    |> List.groupBy (fun e -> e.Num)
    |> List.sortBy fst

/// What the array does with one of its own ports - which is also how the wrapper must wire it.
///
/// The reason this is a type rather than two functions: the sheet's SIGNATURE and the wrapper that
/// realises it are the same seven rules read two ways, and stating them twice is how a sheet's
/// declared ports and its wrapper's actual ports come to disagree. They are stated once, here, and
/// arrayOutlineOf takes the names and widths off them while ArrayElaborate builds the components.
type PortRole =
    /// an ordinary Input: one port, driven to every copy
    | ToEveryCopy
    /// an unmatched JoinIn: into every copy whose end is loose on this channel. Usually one, and a
    /// list because it need not be - a channel number that does not vary over some copies leaves
    /// them all reading one channel, which is one port of the array feeding each of them
    | ToCopies of int list
    /// a MuxOut's select input, which drives no copy - the multiplexer reads it
    | SelectOf of Component
    /// an ordinary Output, or an unmatched JoinOut: out of the one copy named
    | FromCopy of int
    /// a BusOut: every copy's value concatenated, copy 0 least significant
    | Concatenated
    /// a MuxOut's output: the value of whichever copy its select names, and 0 where it names none
    | Multiplexed

/// One port of an array component's outline: what it is called, how wide it is, which drawn
/// component it comes from, and what the array does with it.
type OutlinePort = {
    Name: string
    /// The width the PORT has, which is not always the width of the component it comes from: a
    /// BusOut's port is as wide as all the copies together.
    Width: int
    /// The array IO component on the sheet that generates this port. What ArrayElaborate needs it
    /// for is the width EXPRESSION of that component, which is keyed by component id.
    Comp: Component
    Role: PortRole
}

/// The ports an array design sheet has, and what is wrong with the sheet if anything.
///
/// DERIVED from the sheet's contents and its copy count, rather than being its Input1 and Output
/// components - which is the whole of what makes an array sheet different from every other sheet
/// as far as anything outside this module is concerned. The copy count is a plain integer on the
/// sheet, so this is a fact about the SHEET, exactly as an ordinary sheet's ports are.
///
/// Ports come in the (Y, X) order of the components that generate them - the order
/// getOrderedCompLabels already uses. A MuxOut contributes BOTH an input and an output, so the two
/// lists are over the same components in the same order and a mux select lands in the middle of the
/// inputs, which is exactly where the outline puts it.
///
/// The canvas passed in must already be resolved at whatever bindings are wanted, as
/// signatureOfInstance resolves it: the widths here are read off the components.
let outlinePortsOf
        (info: ArrayInfo)
        (paramDefs: ParameterDefs option)
        (canvas: CanvasState)
        : (OutlinePort list * OutlinePort list) * JoinWiring =
    let comps, _ = canvas
    let copies = copiesOfArray info
    let wiring = joinsOf info paramDefs canvas
    let ordered = comps |> List.sortBy (fun comp -> comp.Y, comp.X)
    let port name width comp role = { Name = name; Width = width; Comp = comp; Role = role }

    let inputs =
        ordered
        |> List.collect (fun comp ->
            match comp.Type with
            | Input1 (w, _) -> [port comp.Label w comp ToEveryCopy]
            | JoinIn (w, _) ->
                looseEndsOf wiring.UnmatchedIn comp
                |> List.map (fun (num, es) ->
                    port (joinInPortName comp.Label num) w comp (ToCopies (es |> List.map (fun e -> e.Copy))))
            // A plain number of bits and never an expression: it follows the copy count, which is a
            // plain number on the sheet.
            | MuxOut _ -> [port (muxSelectPortName comp.Label) (arraySelectWidth copies) comp (SelectOf comp)]
            | _ -> [])

    let outputs =
        ordered
        |> List.collect (fun comp ->
            match comp.Type with
            | Output w -> [for i in 0 .. copies - 1 -> port $"{comp.Label}_{i}" w comp (FromCopy i)]
            | BusOut w -> [port comp.Label (w * copies) comp Concatenated]
            | JoinOut (w, _) ->
                // The head, where several copies are loose on one channel. Unlike the in side that
                // is not a shape to support: a channel carries one signal, so copies both driving
                // one is outClashes above and the sheet is already refused. Taking the first is the
                // best reading of it while it is being fixed.
                looseEndsOf wiring.UnmatchedOut comp
                |> List.map (fun (num, es) ->
                    port (joinOutPortName comp.Label num) w comp (FromCopy (List.head es).Copy))
            | MuxOut w -> [port comp.Label w comp Multiplexed]
            | _ -> [])

    (inputs, outputs), wiring

/// The array component's ports as names and widths - what every sheet signature is - and what is
/// wrong with the sheet if anything.
let arrayOutlineOf
        (info: ArrayInfo)
        (paramDefs: ParameterDefs option)
        (canvas: CanvasState)
        : ((string * int) list * (string * int) list) * ArrayProblem list =
    let (inPorts, outPorts), wiring = outlinePortsOf info paramDefs canvas
    let named (ports: OutlinePort list) = ports |> List.map (fun p -> p.Name, p.Width)
    let inputs, outputs = named inPorts, named outPorts

    /// Every port name the sheet derives, and the ones it derives twice.
    ///
    /// A derived name is built from a user's label, so it can equal another user's label: an Output
    /// labelled A on a three-copy sheet gives A_0 A_1 A_2, and a BusOut labelled A_1 gives A_1.
    /// Guaranteeing distinctness by construction would need a separator no label may contain, which
    /// puts an ugly character in a port name read on the parent sheet - so it is checked instead,
    /// exactly, against the names actually derived rather than by a pattern over them.
    let collisions (side: string) (ports: OutlinePort list) =
        ports
        |> List.countBy (fun p -> p.Name)
        |> List.filter (fun (_, n) -> n > 1)
        |> List.map (fun (name, _) ->
            { Message = $"This sheet derives two {side}s called '{name}'"
              Components =
                ports
                |> List.filter (fun p -> p.Name = name)
                |> List.map (fun p -> p.Comp.Id)
                |> List.distinct })

    // Only where the joins work out. A sheet whose joins are wrong has an outline that is only the
    // best reading of a broken sheet, and the names it derives twice are a consequence of that
    // rather than a second thing to fix: two Join outs sharing a channel reported one clash and
    // then one derived-name collision per copy, which is one mistake said four times.
    match wiring.Problems with
    | [] -> (inputs, outputs), collisions "input" inPorts @ collisions "output" outPorts
    | joinProblems -> (inputs, outputs), joinProblems
