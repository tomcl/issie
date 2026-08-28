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

// Why none of the id types here are [<Struct>], although they look like textbook cases for it.
//
// Because F# Map BOXES a struct key on every comparison. Measured directly (.NET, 200,000
// Map.containsKey lookups into a 10,000-entry map, keys built outside the timed loop, so the
// only thing measured is the comparison):
//
//     raw int key                                 11.1 ms      0 MB
//     RefId of int        (reference DU, as here) 26.5 ms      0 MB
//     [<Struct>] StructId of int                  46.7 ms    113 MB
//
// That is about 570 bytes per lookup, roughly 44 bytes on each of the ~13 comparisons a tree of
// that size needs - both operands boxed every time. It is also SLOWER than the reference wrapper,
// not faster. The reason is that Map does not reach IComparable<'T> through a devirtualised
// constrained call: it takes its comparer from LanguagePrimitives.FastGenericComparer<'T>, which
// has hard-coded fast paths for genuine primitives and otherwise falls back to a path taking obj.
// A struct wrapper gets neither the primitive fast path nor the reference type's property of
// already being an object.
//
// The same effect is visible in the application. Allocation per build of the 3cpu demo, from
// SimLog's AllocMb, which repeats to within 0.1%:
//
//     nothing struct (as here)                    270.26 MB
//     port NUMBERS struct - never Map keys        270.44 MB   no change
//     ComponentId struct - the dominant Map key   275.38 MB   +1.9%
//     all seven struct                            278.11 MB   +2.9%
//
// So: [<Struct>] is free for an id that never becomes a Map key, and costs allocation and time
// for one that does. Every id below is Map-key material. Do not add it without measuring.
//
// The table also prices the wrappers themselves: a raw int key is 2.4x faster than the reference
// DU. That is the standing cost of type-safe ids in an F# Map, it is already being paid, and
// [<Struct>] does not recover it.

// The next types are not strictly necessary, but help in understanding what is what.
// Used consistently they provide type protection that greatly reduces coding errors

/// The id of a port, undirected: see InputPortId and OutputPortId below for the directed
/// forms the code uses where it knows which side of a connection it is on.
[<Erase>]
type PortId = | PortId of int

/// The integer inside a port id, for the seams that must speak in bare ids - the file writers and
/// the generators that derive port ids from a component's.
let portIdValue (PortId n) = n

/// Unique integer id of a component. Unique across the whole DESIGN - the one id namespace
/// with a global invariant, allocated densely from 1 by Helpers.IdAllocator so a design's
/// components can index arrays directly. 0 and negatives are sentinels, never allocated.
[<Erase>]
type ComponentId = | ComponentId of int

/// The integer inside a component id. For the seams that must speak in bare ids - the file
/// writers, the SimpleDesign wire format, and the parameter types, which compile before this file
/// and so cannot name the type - and nowhere else: in between, the type is the point.
let componentIdValue (ComponentId n) = n

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

/// The chain of custom-component instances from the simulated top sheet down to one instance,
/// root first - so it names one ELABORATED copy of a sheet.
///
/// These are design-time ComponentIds, unique across the design, so a path is stable under
/// relabelling and means the same thing whichever side computed it. It is not a new value: the
/// simulator already builds exactly this as FastComponent.AccessPath (`ap @ [cid]` in
/// FastCreate) and the design side already builds exactly this as SheetTree.SheetAccessPath
/// (`accessPath @ [inst.InstId]` in MenuHelpers). This gives it a name.
[<Erase>]
type InstancePath = | InstancePath of ComponentId list

/// A path as a person reads it: the labels of the custom components passed through, root first.
///
/// DISPLAY ONLY, never an identity. A shown path may be shortened where that is unambiguous -
/// which is a rendering decision, and must not reach anything that compares paths.
[<Erase>]
type LabelPath = | LabelPath of string list

/// Unique identifier for a fast component.
/// The list is the access path, a list of all the containing custom components 
/// from the top sheet of the simulation (root first)
type SimComponentId = ComponentId * ComponentId list

/// The old name for SimComponentId, kept while the ~70 sites that destructure it as a bare tuple
/// are still doing so.
///
/// Both are abbreviations of the same tuple today, so this costs nothing and changes nothing.
/// Making the identity a tagged type is one line here - `[<Erase>] type SimComponentId =
/// SimComponentId of ComponentId * ComponentId list` - and it was measured, on this branch, to
/// break 71 sites, 38 of them in FastCreate and FastExtract. THAT is the reason to wait: it is
/// worth doing with the change that needs it (the per-instance port enumeration, which factors a
/// predicate out of FastCreate anyway) rather than as a sweep of the simulator core which buys
/// nothing on its own.
///
/// When it is done, tag it [<Erase>] and NOT [<Struct>]: it is the key type of FIndexOf, the one
/// map a built simulation keeps, and the note above the id types prices what a struct key costs in
/// an F# Map. A plain reference wrapper is what the rest of them are and what this should be.
type FComponentId = SimComponentId

// An instance of a sheet in a running simulation is named by its InstancePath and nothing more,
// so no separate type for it: a wrapper carrying exactly the same information would be a layer to
// unwrap at every use and a second name for one idea.

/// Unique integer id of a connection, unique within its SHEET only - nothing resolves a
/// connection id outside the sheet it belongs to (error highlighting is sheet-guarded).
[<Erase>]
type ConnectionId     = | ConnectionId of int

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
[<Erase>]
type InputPortId      = | InputPortId of PortId

/// Integer id of a component port, unique within its SHEET.
/// Connection ports and connected component ports have the same port Id
/// InputPortId and OutputPortID wrap the id to distinguish component
/// inputs and outputs some times (e.g. in simulation)
[<Erase>]
type OutputPortId     = | OutputPortId of PortId

/// Port numbers are sequential unique with port lists.
/// Inputs and Outputs are both numberd from 0 up.
[<Erase>]
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
/// [<Struct>] as well as [<Erase>], and safely so: this is only ever an ARRAY index, never the key
/// of an F# Map - which is the one thing the note above prices a struct id at.
[<Erase; Struct>]
type FastCompIndex = | FastCompIndex of int

let fastCompIndexValue (FastCompIndex n) = n

/// Where a driven signal sits in the build's array of drivers.
///
/// [<Struct>] as well as [<Erase>], and safely so: this is only ever an ARRAY index, never the key
/// of an F# Map - which is the one thing the note above prices a struct id at. Erased to the bare
/// integer under Fable, a value type carrying one under .NET, so it costs nothing on either.
///
/// SignalHandle is this index PLUS the build it belongs to: a handle can be quoted back by a
/// reader, an index cannot, which is what stops one simulation's index reading another's data.
[<Erase; Struct>]
type DriverIndex = | DriverIndex of int

let driverIndexValue (DriverIndex n) = n
