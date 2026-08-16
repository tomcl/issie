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
symbol it connects to. A wire into a multiplexer's SEL port skips the check on its last segments
entirely; that special case exists to stop the SEL wire fighting the MUX body.

`tryShiftVerticalSeg` slides the wire's one crossing (post-rotation: vertical) segment to just
outside the leftmost, then the rightmost, edge of the obstacles, by adding to segment 2 and
subtracting the same amount from segment 4. It accepts the first shift that clears **all**
intersections, re-running the full check rather than trusting the geometry.

`tryShiftHorizontalSeg` is the fallback and is recursive, bounded by
`maxCallsToShiftHorizontalSeg` (5). It builds both the up-shifted and down-shifted wire; if either
is clear it takes the shorter, and if neither is, it recurses on whichever side has the smaller
remaining vertical distance to escape. The bound exists because there are configurations with no
clean route at all — a symbol dragged on top of another, for instance — and without it a drag would
hang.

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

**2. Cluster.** `makeClusters` walks the `P`-sorted array and repeatedly grows a cluster around the
lowest not-yet-clustered movable line: `expandCluster` searches upward until it hits a gap larger
than the cluster could possibly need, or a barrier (symbol edge or fixed segment) which is recorded
as `UpperFix`; then searches downward from the top of what it found, using the union of bounds
gathered on the way up, which can pick up lines the upward pass could not see. Only lines whose
`B` overlaps the running cluster bound join. Every movable line ends in exactly one cluster —
`makeClusters` asserts this and logs (once) if a line is ever orphaned.

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
