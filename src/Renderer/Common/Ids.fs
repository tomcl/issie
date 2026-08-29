/// The id types, and nothing else.
///
/// A file of their own, compiled before everything else, for one reason: ParameterTypes holds a
/// component id and could not name the type - which its own comment said for as long as the two
/// were in the wrong order. Ids depend on nothing, so this is where they belong.
///
/// [<AutoOpen>], like Shared/ListPairs.fs, so that `ComponentId` resolves in every file that
/// already says `open CommonTypes` and nothing has to be changed to find it.
[<AutoOpen>]
module Ids

open Fable.Core

// [<Struct>] on an id: when it helps, when it costs, and how that was settled.
//
// Under FABLE it is a no-op. [<Erase>] has already made every id below a bare number in the
// emitted JavaScript, so struct changes nothing there, ever. This is a .NET question only, and
// .NET here means the sidecar, which runs the whole build path (Sidecar/SimSession.build).
//
// The mechanism is that F# Map BOXES a struct key on every comparison. Measured directly (.NET,
// 200,000 Map.containsKey lookups into a 10,000-entry map, keys built outside the timed loop, so
// the only thing measured is the comparison):
//
//     raw int key                            11.1 ms      0 MB
//     RefId of int          (reference DU)    26.5 ms      0 MB
//     [<Struct>] StructId of int              46.7 ms    113 MB
//
// About 570 bytes per lookup, roughly 44 bytes on each of the ~13 comparisons a tree that size
// needs, both operands boxed every time - and SLOWER than the reference wrapper, not faster. Map
// does not reach IComparable<'T> through a devirtualised constrained call: it takes its comparer
// from LanguagePrimitives.FastGenericComparer<'T>, which has hard-coded fast paths for genuine
// primitives and otherwise falls back to a path taking obj. A struct wrapper gets neither the
// primitive fast path nor the reference type's property of already being an object. (The table
// also prices the wrappers themselves: a raw int key is 2.4x faster than the reference DU. That
// is the standing cost of type-safe ids in an F# Map, it is already being paid, and [<Struct>]
// does not recover it.)
//
// So struct pays off where an id is CONSTRUCTED a lot - no heap object per id - and costs where it
// is a Map KEY. Which of those dominates depends on the design, and on nothing else:
//
//   ComponentId       FIndexOf, FCustomOutputCompLookup, FIOActive - EXPANSION-sized, one entry
//                     per component per instance - plus DesignComponentsById and the draw block
//   OutputPortNumber  FCustomOutputCompLookup's key - EXPANSION-sized
//   PortId, InputPortId, OutputPortId, ConnectionId, InputPortNumber
//                     WidthInferer, GraphBuilder, CanvasStateAnalyser, SymbolInfo.PortOrientation
//                     and the draw block - all DESIGN-sized, none expansion-sized
//
// Making exactly those last five struct, interleaved A/B pairs against a worktree of the commit
// before, fastest build of each, tiered compilation off, the same binaries for both designs:
//
//     3cpu/eep1            378 fast components   10.98 -> 12.65 ms   +15%, lost 6 pairs of 6
//     largeTest/main4   30,020 fast components   354.9 -> 348.0 ms   -1.9%, won 9 pairs of 10
//
// Opposite signs, both real, and the reason is the whole point. A small design's build IS its
// per-sheet work - width inference, graph building - and those maps are keyed by exactly these
// five, so boxing their keys costs a sixth of it. A design that EXPANDS spends its build making
// hundreds of thousands of FastComponents, where these ids are values and array elements rather
// than keys, and not allocating one per id wins more than the maps lose. The per-sheet penalty is
// the same 1.6 ms in both cases; beside 350 ms it disappears.
//
// Taken, because the sidecar exists for the designs that expand. But note what it cost to find
// out: 3cpu places no sheet twice, so its design-sized and expansion-sized structures are the same
// size and it cannot tell the two apart. DESIGN-SIZED IS NOT COLD - expansion-sized is what makes
// a structure big, not what makes it hot - so measure a design that expands before trusting the
// table above. SimBenchmark takes ISSIE_BENCH_PROJECT and ISSIE_BENCH_TOP for that.
//
// ComponentId and OutputPortNumber stay reference DUs: their maps grow with the expansion, so
// boxing would be paid per component per instance. Freeing them means taking those maps off
// generic comparison, as SimulationGraph already is (SimGraph, an encapsulated int-keyed map,
// measured neutral).
//
// On unwrapping at a map boundary: a member (`port.Id.PToInt`) and a function (`pToInt port.Id`)
// compile to the SAME thing under Fable - both to a free function returning its argument - so the
// choice is F#'s type inference, and it decides for the function. A member on an unannotated
// lambda parameter is FS0072, "the type of this expression could not be inferred before accessing
// its members"; `List.map pToInt` infers. Hence one short named function per id type below.

// The next types are not strictly necessary, but help in understanding what is what.
// Used consistently they provide type protection that greatly reduces coding errors

/// The id of a port, undirected: see InputPortId and OutputPortId below for the directed
/// forms the code uses where it knows which side of a connection it is on.
[<Erase; Struct>]
type PortId = | PortId of int

/// The integer inside a port id, for the seams that must speak in bare ids - the file writers and
/// the generators that derive port ids from a component's.
let pToInt (PortId n) = n

/// Unique integer id of a component. Unique across the whole DESIGN - the one id namespace
/// with a global invariant, allocated densely from 1 by Helpers.IdAllocator so a design's
/// components can index arrays directly. 0 and negatives are sentinels, never allocated.
[<Erase>]
type ComponentId = | ComponentId of int

/// The integer inside a component id. For the seams that must speak in bare ids - the file
/// writers, the SimpleDesign wire format, and the parameter types, which compile before this file
/// and so cannot name the type - and nowhere else: in between, the type is the point.
let cToInt (ComponentId n) = n

/// The DESIGN-time name of a sheet.
///
/// A sheet's name and the name of one INSTANCE of that sheet are different things which have had
/// the same type - bare string - for as long as the waveform simulator has existed. That is why
/// SimTypes and ModelType each carry a long comment warning about the confusion, and why
/// FastCreate needs a collision hack for it. Wrapping the design-time one is half of telling
/// them apart; SimSheetId below is the other half.
///
/// Wrapped at the simulator interface only. It is deliberately NOT pushed into
/// LoadedComponent.Name, CustomComponentType.Name or SimpleSheet.SheetName: those cross the .dgm
/// persistence boundary and the SimpleJsonDotNet wire boundary, and [<Erase>] does not mean the
/// same thing under Fable as under .NET.
[<Erase>]
type SheetName = | SheetName of string

/// The chain of custom-component instances between one instance and the simulated top sheet,
/// INNERMOST FIRST - so it names one ELABORATED copy of a sheet.
///
/// These are design-time ComponentIds, unique across the design, so a path is stable under
/// relabelling and means the same thing whichever side computed it. It is not a new value: the
/// simulator builds exactly this as FastComponent.AccessPath and the design side builds exactly
/// this as SheetTree.SheetAccessPath. This gives it a name.
///
/// Innermost first, although a path is READ the other way round - `top.alu.adder` - because that
/// is where the work is. A path is only ever built by descending the design from the top sheet,
/// and it is only ever taken apart at the deep end: the id of the instance itself, and the path
/// of the sheet that instance is drawn on. Both are a cons here; root first they were an
/// `@ [cid]` at seventeen sites and a `path[0 .. path.Length - 2]` at three.
///
/// FOUR places pay for it, and they are all of them: SheetOfInstance resolves a path against the
/// design from the top down, so it folds back; SimulatedDesign.LabelsOfInstance and getFullSimName
/// turn one into text; and WavePath.pathOfComponent reverses before walking. Anything else that
/// needs a path root first should go through LabelsOfInstance rather than reverse one of its own.
///
/// Two consequences worth knowing before writing against it. The instances a path sits INSIDE are
/// its tails, not its prefixes - so containment is a suffix test (WaveSimSelectHelpers.isSubSheetOf)
/// and walking outwards is `List.tail`. And sorting is unaffected: the paths anything sorts are
/// siblings sharing one parent, so both orders compare the single element that differs.
///
/// The string paths beside it - SheetTree.SheetPath and LabelPath, the wave selector's NodeKey,
/// and WavePath.WPLabels, which is the form saved in a .dgm - stay ROOT first. They are read
/// rather than taken apart, and one of them is in the file format. So the rule is: id paths
/// innermost first, name paths root first, and the reversal happens where a path becomes text.
[<Erase>]
type InstancePath = | InstancePath of ComponentId list

/// A path as a person reads it: the labels of the custom components passed through, root first.
///
/// DISPLAY ONLY, never an identity. A shown path may be shortened where that is unambiguous -
/// which is a rendering decision, and must not reach anything that compares paths.
[<Erase>]
type LabelPath = | LabelPath of string list

/// Unique identifier for a component of a running simulation: which component, and which
/// elaborated copy of its sheet it belongs to.
///
/// The list is the access path - the containing custom component instances between it and the top
/// sheet of the simulation, innermost first. It is an InstancePath, the same list under the same
/// rule; a pair rather than one list because the component's own id always exists while the path
/// may be empty.
///
/// THREE NAMES FOR A COMPONENT, AND NO OTHERS. ComponentId names one of the DESIGN - unique across
/// it, and the same one in every instance of its sheet. This names one of a SIMULATION: the design
/// component plus which copy. FastCompIndex names one inside a particular BUILD, and means nothing
/// in the next. This is the durable one, and so what the renderer holds, what a saved selection
/// resolves to, and what FIndexOf translates to a FastCompIndex. There used to be a fourth,
/// SimComponentId, which was an abbreviation of this same tuple under another name.
///
/// A bare tuple, deliberately. Tagging it is one line - `[<Erase>] type FComponentId = FComponentId
/// of ComponentId * ComponentId list` - and was measured on this branch to break 71 sites, 38 of
/// them in FastCreate and FastExtract, which destructure it as a tuple. Worth doing with a change
/// that needs it rather than as a sweep of the simulator core. If it is done, tag it [<Erase>] and
/// NOT [<Struct>]: it is the key type of FIndexOf, the one map a built simulation keeps, and the
/// note above the id types prices what a struct key costs in an F# Map.
type FComponentId = ComponentId * ComponentId list

// An instance of a sheet in a running simulation is named by its InstancePath and nothing more,
// so no separate type for it: a wrapper carrying exactly the same information would be a layer to
// unwrap at every use and a second name for one idea.

/// Unique integer id of a connection, unique within its SHEET only - nothing resolves a
/// connection id outside the sheet it belongs to (error highlighting is sheet-guarded).
[<Erase; Struct>]
type ConnectionId     = | ConnectionId of int

/// The integer inside a connection id, for the seams that must speak in bare ids - the file
/// writers, and the sorts that need a total order over wires.
let connToInt (ConnectionId n) = n

/// type to uniquely identify a segment
type SegmentId      = int * ConnectionId


/// Human-readable name of component as displayed on sheet.
/// For I/O/labelIO components a width indication eg (7:0) is also displayed, but NOT included here
[<Erase>]
type ComponentLabel   = | ComponentLabel of string

/// Integer id of a component port, unique within its SHEET.
/// Connection ports and connected component ports have the same port Id
/// InputPortId and OutputPortID wrap the id to distinguish component
/// inputs and outputs some times (e.g. in simulation)
[<Erase; Struct>]
type InputPortId      = | InputPortId of PortId

/// Integer id of a component port, unique within its SHEET.
/// Connection ports and connected component ports have the same port Id
/// InputPortId and OutputPortID wrap the id to distinguish component
/// inputs and outputs some times (e.g. in simulation)
[<Erase; Struct>]
type OutputPortId     = | OutputPortId of PortId

/// Port numbers are sequential unique with port lists.
/// Inputs and Outputs are both numberd from 0 up.
[<Erase; Struct>]
type InputPortNumber  = | InputPortNumber of int

/// Port numbers are sequential unique with port lists.
/// Inputs and Outputs are both numberd from 0 up.
[<Erase>]
type OutputPortNumber = | OutputPortNumber of int

/// Where a FastComponent sits in the build that made it: the slot LookupArray stamped it with as
/// the flatten created it, and the identity of a fast component INSIDE a simulation.
///
/// The design-time name a component also has - its ComponentId and the access path of the instance
/// it belongs to - is carried by the FastComponent itself (cId, AccessPath) and read from there
/// when something needs it. That name is what survives a rebuild, so it is what the renderer holds
/// and what a saved selection resolves through; the index is what the simulation uses, and it is
/// meaningless in the next build.
///
/// [<Struct>] as well as [<Erase>], and safely so: it is only ever an ARRAY index, never the key of
/// an F# Map, never in a Set and never sorted - which are the things the note above prices a struct
/// id at. It is a map VALUE (FIndexOf, FCustomOutputCompLookup), which costs nothing, since a value
/// is never compared. Every read unwraps it to the bare int first - `FCompsByIndex[fcToInt
/// index]`, `LookupArray.item (fcToInt i)` - and the -1 sentinel is tested as
/// `fcToInt fc.CustomOutIndex < 0`, an int comparison. Keep it that way: a generic `=` on
/// a struct DU boxes both sides.
[<Erase; Struct>]
type FastCompIndex = | FastCompIndex of int

let fcToInt (FastCompIndex n) = n

/// Where a driven signal sits in the build's array of drivers.
///
/// [<Struct>] as well as [<Erase>]. Unlike FastCompIndex this one IS a Map key - WaveData's fetch
/// cache and WaveDrawn's memo of the SVGs, plus a Set of them for pruning - so the note above does
/// price it. It costs nothing as shipped because both of those are RENDERER stores, where [<Erase>]
/// has already made the key a bare number and the Map is an ordinary int-keyed one. Moving either
/// store to the .NET side would start paying for it; nothing else here would notice.
///
/// SignalHandle is this index PLUS the build it belongs to: a handle can be quoted back by a
/// reader, an index cannot, which is what stops one simulation's index reading another's data.
[<Erase; Struct>]
type DriverIndex = | DriverIndex of int

let dToInt (DriverIndex n) = n
