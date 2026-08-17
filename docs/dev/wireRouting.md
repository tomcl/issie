# How a wire gets its shape

A map of the two passes that decide where a wire is drawn: **routing**, which turns a pair of ports
into a list of segments, and **separation**, which afterwards moves whole groups of segments apart
so that parallel wires do not sit on top of each other. Written for someone about to change
`BusWireRoute.fs` or `BusWireSeparate.fs`.

The two are quite different kinds of code. Routing works on one wire at a time and is pure.
Separation works on the whole sheet at once, builds a mutable array as its working representation,
and is the only place in the draw block where that is deliberate.

## What a wire is

```
Wire = { StartPos; InitialOrientation; Segments: Segment list; ... }
Segment = { Index; Length: float; Mode: Auto | Manual; Draggable; ... }
```

A segment has a **signed length and no position**. Its direction alternates: segment 0 has
`InitialOrientation`, segment 1 is perpendicular, and so on, so a segment's orientation is
`Index % 2` and the absolute positions come from folding the lengths onto `StartPos`
(`BlockHelpers.foldOverSegs`, `getAbsSegments`). Every geometric fact about a wire is derived, and
the only stored coordinate is where it starts.

Three consequences run through everything below.

- **A zero-length segment is not nothing.** It joins two parallel segments, which is how a wire
  gets a straight run that can still be dragged in the middle, and how the segment count stays
  aligned with the alternating-orientation rule. `IsZero` is used far more than `Length = 0`.
- **The first and last segments are nubs**: short, `Draggable = false`, perpendicular to the symbol
  edge they leave, at least `BusWire.Constants.nubLength` (10) long so a wire visibly emerges from
  its port. Both separation and routing take care never to move them. Every separation distance
  constant is smaller than `nubLength` for this reason.
- **Changing a middle segment's position never moves the wire's ends.** Moving segment *i*
  sideways is `segs[i-1].Length += d; segs[i+1].Length -= d` (`BusWireRoutingHelpers.moveSegment`),
  which leaves the total displacement — and so the endpoint — untouched. That invariant is what
  lets separation cache each segment's absolute position once and then move many segments in one
  pass.

`getStartAndEndWirePos` deserves a warning: it returns the **second-to-last** vertex as the end,
not the last. That is deliberate — its callers in `tryShiftHorizontalSeg` want the point before the
final nub — but it means it is the wrong function for "where does this wire end".

## Why it is split into two passes

The split is a response to a user-facing budget, not an implementation accident.

**While a symbol is being dragged, only routing runs.** Every mouse move sends `UpdateWires`,
which re-routes the affected wires and nothing else; separation runs on mouse-*up*. The reason is
that someone dragging a component is judging the placement by the wiring they can see, so the
route has to appear within a few tens of milliseconds of the symbol moving, and it has to be the
same route they will end up with. Separation is then a **fine adjustment**: it moves segments
sideways by tens of pixels, which reads as the drawing settling rather than as it changing.

That is also why routing is deliberately *not* idempotent. A wire is re-routed from where the
symbol now is, so dragging a symbol away and back does not always give the wiring you started
with — see the round-trip test in `WireQuality.fs`. Making routing canonical (one wiring per set of
positions, whatever the history) would cost the drag-time budget, and would take away the property
that what you see mid-drag is what you get.

**Separation, on the other hand, should settle.** It is applied to an already-separated sheet after
every drag, paste and rotate, so if a second application moves wires then what the user is left
with depends on how many times the pass happened to run. It does settle on ordinary sheets, and on
two of the corpus sheets in `WireQuality.fs` it does not — see
[openIssues](openIssues.md#wire-routing-and-separation).

## The cases the complexity is for

Most of the machinery in `BusWireSeparate` — clustering, same-net linking, the repeated passes,
corner removal — exists for two shapes that are common in real designs and pathological for a
local heuristic.

- **An array of ports facing an array of ports**, with some of the connections crossed. Every wire
  in the bundle wants the same channel, the order they should be in is decided by where they end,
  and one wire's choice constrains every other's. This is what segment ordering
  (`orderPairwiseToMinimiseCrossings`) is for.
- **The same, but the two arrays are on opposite sides of a symbol**, so the whole bundle has to
  turn back and go round it. Now the bundle competes with itself on both sides of the obstacle, and
  the segments doing the going-round are exactly the ones that clustering will try to spread.

`WireQuality.fs` holds both (`crossedArrays`, `wrappedArrays`). They are worth building first when
changing anything here: sheets where every wire runs left to right will not show a difference.

## Routing

### The initial shape

`BusWireUpdateHelpers.autoroute` is the whole of the initial layout and does three things.

1. **Normalise.** `rotateStartDest Right` rotates both ports 90° at a time until the *source* port
   faces Right. So the case analysis only has to cope with a wire leaving rightwards, and the
   rotated result is rotated back afterwards (`rotateSegments`). Everything downstream of this,
   including the comments in `BusWireRoute.fs` that talk about "the vertical segment", is written
   as if no symbol were ever rotated.
2. **Pick a shape** from a 16-way table in `BusWire.makeInitialWireVerticesList`: (target left or
   right of source) × (target above or below) × (which edge the target port is on). Each entry is a
   literal vertex list. Most start with `rightNub` — the stick out of the port plus a zero-length
   segment — and end with a matching stick into the target port.
3. **Convert to segments** by pairwise differencing the vertices.

The table produces wires of **6, 7, 8 or 9 segments** depending on the case, and anything
downstream that matches on segment count has to cover all four. `tryShiftHorizontalSeg` covered
every count but 8 for a long time and silently returned the wire it was handed, which is a failure
mode worth recognising: a `| _ -> wire.Segments` fallthrough in this code does not fail, it routes
badly. Which count a wire has is not obvious from the sheet, so a test that pins one
(`DrawBlockTests`, "a wire into a bottom-edge port") is worth more than reading the table.

### Getting out of the way of symbols

`BusWireRoute.smartAutoroute` wraps `autoroute` with obstacle avoidance:

```
autoroute
  └─ findWireSymbolIntersections → []  → done
                                → boxes → ensureBothNubs
                                          ├─ tryShiftVerticalSeg  (left, then right)
                                          └─ tryShiftHorizontalSeg (up/down, recursive)
                                          └─ neither worked → keep the intersecting wire
```

`findWireSymbolIntersections` expands every symbol's bounding box by `minWireSeparation` (7) and
asks which boxes any **interior** segment crosses — the nubs are excluded from the scan, and the
box of the symbol a nub belongs to is not expanded, because a wire must be allowed to touch the
symbol it connects to.

One exemption is worth knowing about because it is the sharpest edge here. A multiplexer's SEL port
sits *inside* its own bounding box — the body is a trapezoid, so the port is drawn in from the
edge — and the last segments of a wire reaching one therefore have to be allowed into that box.
Allowed into **that** box: the exemption used to apply to every mux and demux on the sheet, so a
wire climbing to a SEL port past a *different* mux could not see it. Nothing was reported as
intersecting, so no shift was attempted either, and the wire was drawn straight through. On a
register file — a column of muxes with address wires arriving from below — that is not an edge
case, it is most of them. **Anything which suppresses the intersection check suppresses the
avoidance as well**, which makes an over-broad exemption much more expensive than it looks.

`tryShiftVerticalSeg` slides the wire's one crossing (post-rotation: vertical) segment to just
outside the leftmost, then the rightmost, edge of the obstacles, by adding to segment 2 and
subtracting the same amount from segment 4. It accepts the first shift that clears **all**
intersections, re-running the full check rather than trusting the geometry.

`tryShiftHorizontalSeg` is the fallback and is recursive, bounded by
`maxCallsToShiftHorizontalSeg` (5). For each direction it builds a short list of candidate wires —
one per movable horizontal segment, each moved to just clear the extreme obstacle — and takes the
first that is clear. If either direction yields one it takes the shorter, and if neither does it
recurses on whichever side has the smaller remaining vertical distance to escape. The bound exists
because there are configurations with no clean route at all — a symbol dragged on top of another,
for instance — and without it a drag would hang.

The candidate list is ordered, and the order is the whole point. The shape the segment-count table
picks comes first, so a wire that routes today routes identically and this is only ever a fallback;
then the segments which `findWireSymbolIntersectionsBySegment` says are *actually obstructed*; then
the rest. Choosing the segment to move by the wire's segment count — which is what this did — is a
guess in a place where the obstacle geometry is the answer, and it is why wires into a top or
bottom edge port used to be left drawn across a component.

When every one of those still crosses something, a last family is tried which moves **the crossing
and the turn together**: the horizontal run goes onto the chosen row *and* the turn which follows it
goes to a chosen column - hard against either side of the obstacles, or at the destination itself.
Neither shift can do this alone, because moving a segment sideways cannot change where the wire
turns and moving the turn cannot change which row the wire crosses on. An obstacle blocking both
the row a wire leaves on and the row it arrives on needs both at once, and that is the ordinary
case for reaching a port on a top or bottom edge from beyond an obstacle: the crossing has to
happen past the obstacle, next to the destination rather than next to the source.

Putting the crossing next to the destination is what makes these work, and it is worth knowing why
it is not the natural thing for this code to do: the shape comes from a table indexed on where the
two ports are, and its intermediate coordinate is a midpoint. Everything the shift code does is a
correction to that midpoint.

`ensureBothNubs` runs first because both shift functions address segments by absolute index, and a
short wire may not have an interior segment there yet; it splits the end segment into
nub / zero / remainder so the indices mean what the shift code assumes.

### Not re-routing: partial autoroute

When a symbol moves, `updateWire` tries `partialAutoroute` before falling back to
`smartAutoroute`. It preserves manual routing: it finds the first `Manual` segment, treats the
vertex before it as a fixed point, and translates only the segments between the moved port and that
point. It refuses (returns `None`, forcing a full re-route) if the port has ended up on a different
side of the fixed point than it started, or if the wire would now leave its port on the wrong edge.

`updateWires` classifies wires by which end moved: both ends moved means translate the wire
rigidly, one end moved means re-route from that end (`reverseWire` lets the same code run from
either end), neither means leave it alone.

### Dead code to know about

`snapToNet` — 80 lines that make a new wire copy the shape of an existing wire in the same net — is
unreachable: `smartAutoroute` matches `model.SnapToNet` with a wildcard that always keeps the
unsnapped wire, and the real branch is commented out. `copySegments` and `generateEndSegments`
exist only to serve it.

## Separation

`separateAndOrderModelSegments` runs after routing, at the end of a drag or a wire creation, when
there is time to look at the whole sheet. Its job is that parallel segments of different wires
should be spread `maxSegmentSeparation` (30) apart, ordered so as to minimise crossings, and *not*
spread apart when they belong to the same net (overlapping same-net segments are one visible line,
which is what you want).

### The working representation

Horizontal and vertical segments are separated independently, and each pass flattens the problem to
one dimension. A `Line` (in `BusWireRoutingHelpers.fs`) is a segment or a symbol edge reduced to:

- `P` — the coordinate perpendicular to it, the only thing separation changes;
- `B` — the interval it spans along itself, which decides whether two lines are "the same place";
- `LType` — what may be done to it;
- `SameNetLink` — other lines that must move with it.

`P` and `LType` are `mutable`, the lines live in an array sorted by `P`, and `Lid` is that array
index. The module then works by mutation, which is against the grain of the rest of the draw block
and is deliberate: this runs on every drag and the alternative is rebuilding the array per move.
Read the mutation as scoped — the array is built, mutated and consumed inside one call, and
`adjustSegmentsInModel` is the single point where the result re-enters the immutable model.

`makeLines` builds the array for one orientation, per pass:

| `LType` | comes from | may it move? |
|---|---|---|
| `BARRIERPOS` / `BARRIERNEG` | the two edges of a symbol's bounding box | never; stops a cluster |
| `FIXEDMANUALSEG` | a `Manual` segment, or any segment of a wire not being routed | never |
| `FIXEDSEG` | a segment next to a zero-length segment — i.e. one continuing the straight line out of a nub | not by clustering; only by `separateFixedSegments` |
| `NORMSEG` | anything else interior and non-zero | yes |
| `LINKEDSEG` | a same-net segment linked to another | it follows its link |

Nubs (index 0 and last) and zero-length segments are never lines at all, and neither is any segment
of a wire shorter than `minWireLengthToSeparate` (10) end to end — short wires stay straight rather
than becoming squiggly.

### The four steps

**1. Link same-net segments.** `linkSameNetLines` groups lines by output port and, within a group,
merges lines that are close in `P` and overlapping in `B`: one line keeps the union of the bounds
and a list of the others in `SameNetLink`, and the others become `LINKEDSEG` so clustering ignores
them. This is what keeps a fan-out drawn as a single trunk instead of *n* parallel lines 30 apart.

**This is the only thing in either pass that knows a net exists**, so how far it reaches decides how
well a net is drawn. Its capture distance is `sameNetTrunkCapture` (10), and it was
`modernCirclePositionTolerance` (2) — which answers a different question. That constant is how close
two coordinates must be to count as the *same point*, when `updateCirclesOnNet` works out where a
net crosses itself: a circle is drawn at a cross-roads to say that the four wires meeting there are
one net and not two crossing. Two units is the right answer to "is this the same place"; it is the
wrong answer to "are these close enough to be one line".

And two units is nothing: a sink port a couple of units off its neighbours' gives that wire a trunk
segment 2 away from theirs, which is then not linked, is `NORMSEG` where theirs are `FIXEDSEG`, and
so is the only one of them that can move. Separation duly pushed it a full `maxSegmentSeparation`
clear of a trunk it was 2 units from. `staggeredFanout` in `WireQuality.fs` is that sheet, and it is
what a fan-out looks like whenever the components are not in a column.

Note what linking can and cannot do. It **merges lines that are already nearly coincident** — it is
an attraction of last resort, not a construction. It cannot bring together two wires of a net whose
routes bend in different places, because their trunk segments are nowhere near each other to start
with. That would need routing to know about the net, and it does not: `snapToNet` was written to
make a new wire follow an existing one in its net and is unreachable (see
[openIssues](openIssues.md#wire-routing-and-separation)). Measured against the half-perimeter of a
net's terminals - a lower bound on any rectilinear Steiner tree joining them - what the two passes
produce today is about 1.3x the bound at eight sinks, against 2.1x for wires routed with no sharing
at all. So most of the available gain is already taken, by this one function.

**2. Cluster.** `makeClusters` walks the `P`-sorted array and repeatedly grows a cluster around the
lowest not-yet-clustered movable line: `expandCluster` searches upward until it hits a gap larger
than the cluster could possibly need, or a barrier (symbol edge or fixed segment) which is recorded
as `UpperFix`; then searches downward from the top of what it found, using the union of bounds
gathered on the way up, which can pick up lines the upward pass could not see. Only lines whose
`B` overlaps the running cluster bound join. Every movable line ends in exactly one cluster —
`makeClusters` asserts this and logs (once) if a line is ever orphaned.

A cluster's `Bound` is the union of its members' spans, and a union is as wide as its widest
member — so it says almost nothing about where any particular segment is. Two things follow, and
between them they are the whole subtlety of this code. The downward search deliberately reuses the
wider bound from the upward one, because that is how it finds segments the first search could not
see. But a bound carried over to a cluster with *different membership* silently changes which
barriers apply: a symbol edge that only ever obstructed a segment now in another cluster will stop
this one's search, and every segment beyond it is dropped. Recompute the bound whenever the
membership changes.

**3. Order and place.** For each cluster, `orderPairwiseToMinimiseCrossings` sorts its segments into
the order that produces the fewest crossings. The criterion is local: `numCrossingsSign` looks at
which way each wire turns at the two ends of a segment and decides, for a pair, which of the two
should have the larger `P`. That is fed to a bubble sort over adjacent pairs — deliberately not a
full optimisation, since it has to be fast, and adjacent-pair ordering is right whenever a correct
order exists at all.

`calcSegPositions` then assigns the actual coordinates, and the interesting part is what happens
near a boundary: segments are placed a full `maxSegmentSeparation` away from a **fixed** bound (a
symbol edge), but only spread symmetrically about their own midpoint when free. Between two fixed
bounds too close together, the spacing shrinks to fit rather than the cluster overflowing.

**4. Write back.** `adjustSegmentsInModel` first copies each line's new `P` onto everything in its
`SameNetLink`, then converts each `P` change into a `moveSegment` on the wire — which, by the
invariant at the top of this page, cannot move any port.

### The pass sequence

```
separate Horizontal → Vertical → Horizontal → Vertical → Horizontal
separateFixedSegments Horizontal → Vertical
removeModelCorners
```

Five alternating passes, not one, because moving vertical segments changes which horizontal
segments overlap, and clusters merge across iterations. The comment in the source says one pass
each "should be enough in theory" and is honest that it is not; the count is empirical.

`separateFixedSegments` then handles what clustering deliberately would not touch: `FIXEDSEG`s that
are exactly on top of each other, which it nudges apart into whatever space it can find nearby.

`removeModelCorners` is cleanup. Separation can leave a wire with a small useless staircase, and
`findWireCorner` looks for four consecutive segments where the middle two can be deleted and the
outer two extended, then checks with `isSegmentExtensionOk` that the extended segments would not
run into a symbol or another wire. At most one corner is removed per wire per pass, and the check
runs against a `LineInfo` snapshot taken before any corner was removed. `removeWireSpikes` (called
from elsewhere) removes the other artifact: a segment that doubles straight back on itself with a
zero-length segment between.

## Where it is called from

| trigger | entry point |
|---|---|
| a new wire is drawn | `BusWireUpdate.newWire` → `smartAutoroute` → `updateWireSegmentJumpsAndSeparations` |
| a symbol is dragged, rotated, scaled | `routeAndSeparateSymbolWires` (routes affected wires, then separates all) |
| Ctrl-Shift-W / menu "separate wires" | `reSeparateWiresFrom` — makes segments `Auto` again, keeps positions, re-separates |
| Ctrl-Shift-R / menu "reroute wires" | `reRouteWiresFrom` — full `smartAutoroute` then separate |
| a sheet is opened | the load path routes each wire, then one global separation |

`separateAndOrderModelSegments` takes a list of wires to route, but generates lines for **all**
wires and then discards only those clusters containing none of them. A wire not in the list can
therefore still be moved, if it shares a cluster with one that is. That is intentional: the
alternative is unrouted segments pinning newly routed ones in place.

## Measuring it

The user-facing requirement is that for *any* component positions the wiring looks good, which
breaks down into objectives that conflict:

- **Wire drawn** — the length of the *union* of what is on the canvas, so same-net segments lying
  on top of each other count once. This is the metric to minimise, and the reason to define it as a
  union is that it makes "short wires" and "same-net wires share a trunk" the same objective
  instead of two that have to be traded off by hand.
- **Bends**, **crossings between different nets**, and **overlap between different nets** (which
  must be zero — it is the one thing separation guarantees).

`Tests/Issie.Tests/WireQuality.fs` computes all of these over a corpus of the hard sheets and
records the current scores, so a change to either pass has to say which way each number moved. Two
traps it encodes:

- **Crossings are meaningless before separation.** Unseparated wires do not cross, because they are
  lying on top of each other. Anything comparing before and after must read the overlap column
  alongside, or it will conclude that separation makes sheets worse.
- **Separation is not the place most quality is won or lost.** Measured over the corpus it removes
  every cross-net overlap while adding under 5% to the wire drawn, and adds bends only in the two
  array cases. Length, bends and crossings are decided by the initial route — a 16-entry table with
  no knowledge of other wires — and by where the symbols are.

## Testing it

`Tests/Issie.Tests/DrawBlockTests.fs` builds symbols, routes wires and runs the whole separation
pass under plain .NET — no browser, no Electron — via the sheet-description DSL. Assert *structure*
(a wire still joins the ports it names; every segment is axis-aligned; no wire was lost) rather
than coordinates. `node scripts/inspect-canvas.js` will show what the running app actually drew
when a test and the app seem to disagree.

The DSL lays a sheet out for readability, which is the wrong layout for a routing test: `movedTo`
puts the symbols where the case under test needs them. Beyond that, two things bite when writing
one.

- **`string` on a port id is not the port id under .NET.** `InputPortId` and `OutputPortId` are
  `[<Erase>]`, so `string wire.InputPort` gives the bare id in the app and `InputPortId "…"` under
  .NET, where a `Ports` lookup then throws. Use `inputPortStr` / `outputPortStr`. This is the
  reason routing looked untestable outside Electron for so long.
- **`getStartAndEndWirePos` returns the second-to-last vertex**, so it is not the function for
  checking where a wire ends. `getAbsSegments` is.
