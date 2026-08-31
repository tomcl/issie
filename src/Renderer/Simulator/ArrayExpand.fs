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
    /// What is wrong with the joins, if anything. The wiring above is still the best reading of
    /// them, so a sheet being edited can still be drawn; analyseState is what refuses to simulate.
    Problems: string list
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
        | Error msg -> Error msg

/// Every end of every join, copy by copy, with whatever could not be worked out reported and the
/// number the sheet is drawn at used in its place.
let private endsOf (info: ArrayInfo) (slots: ComponentSlotExpr) (canvas: CanvasState) (isOut: bool) =
    let side = if isOut then "Join out" else "Join in"
    ((([]: JoinEnd list), ([]: string list)), joinComps isOut canvas)
    ||> List.fold (fun (ends, problems) comp ->
        (((ends, problems)), [0 .. copiesOfArray info - 1])
        ||> List.fold (fun (ends, problems) copy ->
            match channelOf info slots comp copy with
            | Ok num ->
                {Comp = comp; Copy = copy; Num = num} :: ends, problems
            | Error msg ->
                let _, stored = joinWidthAndNum comp
                {Comp = comp; Copy = copy; Num = stored} :: ends,
                $"{side} '{comp.Label}': its channel number {msg}" :: problems))
    |> fun (ends, problems) -> List.rev ends, List.rev problems

/// Labels that must be distinct within one side of the joins.
///
/// A join's label names a CHANNEL, so a JoinOut and the JoinIn reading it share one - which is why
/// checkComponentNamesAreOk exempts joins from the sheet-wide unique-name rule. What it does not
/// exempt them from is being distinct among themselves on each side: each join is one port of the
/// copy, so two on a side sharing a label are two ports with one name.
let private duplicateLabels (side: string) (comps: Component list) =
    comps
    |> List.countBy (fun comp -> comp.Label)
    |> List.filter (fun (_, n) -> n > 1)
    |> List.map (fun (label, n) -> $"{n} {side} components are labelled '{label}': each is one port of the copy, so they must have different names")

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
            $"'{e.Comp.Label}' is on channel {e.Num} in copy {e.Copy}: a channel number must never \
              be negative, because the sheet port an unmatched join becomes is named after it")

    let nameProblems =
        duplicateLabels "Join out" (joinComps true canvas)
        @ duplicateLabels "Join in" (joinComps false canvas)

    /// Ends by channel, and the channels a side puts two ends on - two JoinOuts on one channel are
    /// two drivers of one wire, and two JoinIns on one are two names for the same value.
    let byChannel (side: string) (ends: JoinEnd list) =
        let grouped = ends |> List.groupBy (fun e -> e.Comp.Label, e.Num)
        let clashes =
            grouped
            |> List.filter (fun (_, es) -> List.length es > 1)
            |> List.map (fun ((label, num), es) ->
                let copies = es |> List.map (fun e -> string e.Copy) |> String.concat ", "
                $"{side} '{label}' is on channel {num} in more than one copy ({copies}): each channel \
                  joins exactly two copies, so its number must differ from copy to copy")
        grouped |> List.choose (fun (key, es) -> es |> List.tryHead |> Option.map (fun e -> key, e)) |> Map.ofList,
        clashes

    let outByChannel, outClashes = byChannel "Join out" outEnds
    let inByChannel, inClashes = byChannel "Join in" inEnds

    let matched =
        outByChannel
        |> Map.toList
        |> List.choose (fun (key, outEnd) -> Map.tryFind key inByChannel |> Option.map (fun inEnd -> outEnd, inEnd))
        |> List.sortBy (fun (o, _) -> o.Comp.Y, o.Comp.X, o.Copy)

    { Matched = matched
      UnmatchedOut = outEnds |> List.filter (fun e -> not (Map.containsKey (e.Comp.Label, e.Num) inByChannel))
      UnmatchedIn = inEnds |> List.filter (fun e -> not (Map.containsKey (e.Comp.Label, e.Num) outByChannel))
      Problems = nameProblems @ outProblems @ inProblems @ negatives @ outClashes @ inClashes }

/// The ports an array design sheet has, and what is wrong with the sheet if anything.
///
/// DERIVED from the sheet's contents and its copy count, rather than being its Input1 and Output
/// components - which is the whole of what makes an array sheet different from every other sheet
/// as far as anything outside this module is concerned. The copy count is a plain integer on the
/// sheet, so this is a fact about the SHEET, exactly as an ordinary sheet's ports are.
///
/// Ports come in the (Y, X) order of the components that generate them - the order
/// getOrderedCompLabels already uses - with the multiplexers' ports last, in the order they are
/// declared in the sheet's settings.
///
/// The canvas passed in must already be resolved at whatever bindings are wanted, as
/// signatureOfInstance resolves it: the widths here are read off the components.
let arrayOutlineOf
        (info: ArrayInfo)
        (paramDefs: ParameterDefs option)
        (canvas: CanvasState)
        : ((string * int) list * (string * int) list) * string list =
    let comps, _ = canvas
    let copies = copiesOfArray info
    let wiring = joinsOf info paramDefs canvas

    /// The unmatched ends of one join component, in channel order so that a sheet with several
    /// loose ends lists them predictably.
    ///
    /// One PORT per channel, not per end. A join whose number does not vary with the loop variable
    /// puts every copy on one channel, so every copy's end is loose on it - and a port is named
    /// after the channel, so those would be one name repeated. That sheet is already reported as
    /// wrong (each channel joins exactly two copies), and this is the best reading of it meanwhile.
    let looseEndsOf (ends: JoinEnd list) (comp: Component) =
        ends
        |> List.filter (fun e -> e.Comp.Id = comp.Id)
        |> List.distinctBy (fun e -> e.Num)
        |> List.sortBy (fun e -> e.Num)

    let ordered = comps |> List.sortBy (fun comp -> comp.Y, comp.X)

    let inputs =
        ordered
        |> List.collect (fun comp ->
            match comp.Type with
            // an ordinary Input goes to EVERY copy, so it is one port however many copies there are
            | Input1 (w, _) -> [comp.Label, w]
            | JoinIn (w, _) ->
                looseEndsOf wiring.UnmatchedIn comp
                |> List.map (fun e -> joinInPortName comp.Label e.Num, w)
            // a MuxOut reads one copy's value back, so the select saying which copy is an input
            | MuxOut _ -> [muxSelectPortName comp.Label, arraySelectWidth copies]
            | _ -> [])

    let outputs =
        ordered
        |> List.collect (fun comp ->
            match comp.Type with
            // an ordinary Output is one port PER COPY
            | Output w -> [for i in 0 .. copies - 1 -> $"{comp.Label}_{i}", w]
            // the copies' values concatenated, copy 0 least significant
            | BusOut w -> [comp.Label, w * copies]
            | JoinOut (w, _) ->
                looseEndsOf wiring.UnmatchedOut comp
                |> List.map (fun e -> joinOutPortName comp.Label e.Num, w)
            // the value of whichever copy the select names, and 0 where it names none
            | MuxOut w -> [comp.Label, w]
            | _ -> [])

    /// Every port name the sheet derives, and the ones it derives twice.
    ///
    /// A derived name is built from a user's label, so it can equal another user's label: an Output
    /// labelled A on a three-copy sheet gives A_0 A_1 A_2, and a BusOut labelled A_1 gives A_1.
    /// Guaranteeing distinctness by construction would need a separator no label may contain, which
    /// puts an ugly character in a port name read on the parent sheet - so it is checked instead,
    /// exactly, against the names actually derived rather than by a pattern over them.
    let collisions (side: string) (ports: (string * int) list) =
        ports
        |> List.countBy fst
        |> List.filter (fun (_, n) -> n > 1)
        |> List.map (fun (name, _) ->
            $"this sheet derives two {side}s called '{name}': rename one of the components they \
              come from, since a sheet cannot have two ports of one name")

    (inputs, outputs),
    wiring.Problems @ collisions "input" inputs @ collisions "output" outputs
