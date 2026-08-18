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

Mid-drag routing is deliberately quick rather than canonical — a wire is re-routed from where the
symbol now is, against whatever else is there. What makes the END of the drag canonical is that
mouse-up runs the floating redraw: every wire not routed by hand is re-routed from scratch and the
whole sheet separated, exactly the Edit-menu "redraw floating wires". The wiring after a drag is
therefore a function of the positions alone, a drag leaves nothing for a redraw to improve, and a
drag round trip restores the wiring wire-for-wire — the round-trip test in `WireQuality.fs`
asserts exact equality, with no tolerance.

This is also what catches the wires a drag cannot reach by re-routing its own symbol's wires: a
wire ROUTED AROUND the moved symbol is not connected to it, and its detour belongs to routing,
which no amount of separation can undo. Before mouse-up ran the redraw, such a wire kept its
detour after the obstacle had gone.

**Separation, on the other hand, settles, and that is enforced rather than hoped for.** It is
applied to an already-separated sheet after every drag, paste and rotate, so if a second
application moved wires then what the user is left with would depend on how many times the pass
happened to run. See "the settling loop" below.

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

### Branching off the same net

A wire is routed as a branch off a wire of its own net wherever that is legal:
`BusWireRoute.sameNetRoutes` offers one candidate per vertex of every already-routed wire of the
net, and `smartAutoroute` takes the first that crosses no symbol.

The construction is the point. The two wires start at the same port, so the leading segments are
simply **copied**, and routing carries on from the branch point as though it were a port facing the
way that wire was going — so the first segment it generates runs *along* the wire it left before
turning off. That first segment, and the one after the zero-length segment which follows it, are
**merged into the shared segment**: a route begins nub, zero, rest, and leaving that zero where it
falls would put two coincident vertices in the middle of the wire. A zero-length segment belongs
beside a nub and nowhere else — a separation move which crosses one draws the wire back over
itself, which is what a spike is. That overlap costs nothing: it is the same net, so `linkSameNetLines` merges the two
and they are drawn as one line. It is also why offering only the *ends* of segments as branch
points loses nothing — a branch that ought to leave from the middle of a segment leaves at the end
of the one before and runs back along it.

Candidates are ordered by how far the branch point is from the destination, nearest first, and the
ordinary route is one of them: it is the branch at the driver port, where nothing is shared. So the
wire follows its net for as long as it legally can.

Two things make this stable rather than a gamble. A shared run **cannot afterwards be separated** —
separation links same-net segments and moves them as one — so the saving stays saved. And branching
at a vertex and running parallel produces T-junctions and overlaps, never a **cross-roads**: the net
does not cross itself, so this does not manufacture the ambiguity that circles exist to resolve.

**Order matters now, and it did not before.** A wire can only branch off a wire that is already
routed, so what a wire can see depends on what was routed first. `redrawWires` routes shortest
first, by straight-line port distance: a short wire has the least freedom in where it can go, so it
should be in place when a longer wire of the same net comes looking for something to join. Measured
on the `fanout` sheet, the same wires routed in an arbitrary order are drawn with **10971** units of
wire against **9470** shortest-first — a bigger difference than the feature itself makes.

What this is for is **not** the average sheet. It is the occasional long wire with several
destinations, where the failure is not that the drawing is 20% longer but that three long wires
running nearly in parallel across a sheet cannot be read as one signal. `longFanout` in
`WireQuality.fs` is that case.

### Not re-routing: partial autoroute

When a symbol moves, `updateWire` tries `partialAutoroute` before falling back to
`smartAutoroute`. It preserves manual routing: it finds the first `Manual` segment, treats the
vertex before it as a fixed point, and translates only the segments between the moved port and that
point. It refuses (returns `None`, forcing a full re-route) if the port has ended up on a different
side of the fixed point than it started, or if the wire would now leave its port on the wrong edge.

`rerouteMovedWires` — which `updateWires` and `routeAndSeparateSymbolWires` both are — classifies
wires by which end moved: both ends moved means keep the shape and move it with them, one end moved
means re-route from that end (`reverseWire` lets the same code run from either end), neither means
leave it alone.

**It takes the routing off every wire it is going to autoroute before it routes any of them**, as
`redrawWires` does. Without that, a wire routed as a branch off a wire of its own net could follow
one that had not been re-routed yet — and a branch takes over the reference wire's start position,
so the new route was drawn from where the driver port used to be, joined to nothing. Dragging a mux
in `3cpu`'s `addsub` left both of its output wires behind exactly so. It is not a rare corner: a
drag of the driver of a 24-wire net detached all 24, and "the wiring is fine until you drag
something, and *redraw all wires* fixes it" is what it looks like from the outside.

Stripping first is also what makes the drag as good as a redraw rather than merely correct. Each
wire can then see the wires of its net that have already been re-routed, so the net is commoned up
during the drag instead of being left to separation. Measured on the corpus, after dragging the
busiest driver:

| sheet | wire drawn for the fanned nets | if the whole sheet were redrawn |
|---|---|---|
| `fanout` | 2938 | 2938 |
| `longFanout` | 3850 | 3850 |
| `reg16x8` | 12554 | 12597 |

Two other orderings were tried and are worse. Routing every wire against the model as it was, with
`sameNetRoutes` refusing to follow a wire that is not attached to the driver port, is correct but
gives up commoning entirely during a drag: `fanout` 3283 against 2938, and 12 crossings against 4 on
`staggeredFanout`. Feeding each result back in **without** stripping first lets a wire follow a
route that is about to be thrown away: `reg16x8` 13839 against 12554, and 20 crossings against 8 on
`longFanout`.

Hand-routed wires are the exception and are not stripped: `partialAutoroute` holds the shape the
user dragged into place by working from the segments that are already there. They are re-routed
first so the rest of their net can follow them.

The other shape this bug takes is worth recognising, because it does not look like a detached wire
at all. Where the stale wire being followed is one whose destination is on the *other* side of the
port, the branch runs to that wire's old midline and turns back, and the net is drawn as a long
thin loop out of the port with its two wires going opposite ways — visibly not following each
other, but attached at both ends and so passing every check that looks at endpoints. `customPair`
in `WireQuality.fs` is that case: two custom components 60 apart, one output driving two inputs on
the far side, and dragging one of them down and left used to produce it.

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
with. That needs routing to know about the net, which is what `sameNetRoutes` is for — below.

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
the order that produces the fewest crossings. Whether two wires cross depends on which of the two
is placed first and on nothing else, so a cluster's crossings are the **sum over its pairs**, and
`numCrossingsSign` is one term of that sum: which way round a pair is cheaper, by looking at which
way each wire turns at the two ends of its segment.

It returns **zero for most pairs**, and that is not a gap in it — a pair whose spans *nest* costs
one crossing whichever way round it goes, and in a bundle fanning out from one place most pairs
nest (22 of the 36 pairs in one cluster on `reg16x8`). What comes back is therefore a **partial
order**: definite about some pairs, genuinely indifferent about the rest.

It is treated as one. The relation is completed to a partial order, the longest chain in it is
found, and that chain's minimal element is placed — nothing else can go before it, since a chain of
that length hangs off it — then removed, and the process repeats. A segment the relation is silent
about sits in no long chain and falls out wherever its own preferences allow, which is the right
answer to a genuine indifference.

This was a bubble sort over **adjacent** pairs, which cannot carry a segment past a run of segments
it ties with to reach the one that has an opinion about it, so every tie was settled by wherever
routing happened to leave the segment. On `reg16x8` that cost 22 crossings out of 103.

Three orderings that sound plausible are worse, and each was measured before being discarded.
Barycentre — place each segment near the mean of what its two arms reach to, which is what layered
graph drawing would use — doubles the crossings on `wrappedArrays`, where wires double back and
the arms predict nothing. Scoring each segment by the sum of its preferences against all the others
gets `reg16x8` only to 87 and costs 2% more wire. Taking the largest subset the relation orders
completely and fitting the others around it costs `tangle` six crossings and its ability to settle
in one pass.

`calcSegPositions` then assigns the actual coordinates, and the interesting part is what happens
near a boundary: segments are placed a full `maxSegmentSeparation` away from a **fixed** bound (a
symbol edge), but only spread symmetrically about their own midpoint when free. Between two fixed
bounds too close together, the spacing shrinks to fit rather than the cluster overflowing.

**4. Write back.** `adjustSegmentsInModel` first copies each line's new `P` onto everything in its
`SameNetLink`, then converts each `P` change into a `moveSegment` on the wire — which, by the
invariant at the top of this page, cannot move any port.

### The settling loop

```
repeat (up to maxSettlingRounds):  separate Horizontal → separate Vertical
                               keep the round only if wiringCost improved
separateFixedSegments Horizontal → Vertical
removeModelCorners
```

The two orientations are separated **independently**, and that is what makes the pass fast: each
is a one-dimensional problem over a sorted array. They are not independent in the answer, though —
where a horizontal segment can go depends on where the vertical segments it joins ended up — so the
passes alternate, and repeating the alternation resolves most of the coupling.

Not all of it. Some pairs of decisions are mutually exclusive: doing what H wants means undoing
what V did, and the other way about. Alternating then **oscillates** instead of converging. This
was a fixed sequence of five passes, which on such a sheet landed on whichever phase the count
ended on — so the wiring depended on how many passes there happened to be, and running the pass
again flipped it.

The loop replaces the count with a decision. `wiringCost` scores the sheet — wire drawn, plus a
heavy penalty for two nets drawn on top of each other — and a round is kept only if it improved
that score by more than `settlingTolerance`. Three properties follow, and they are the reason for
the shape:

- **Idempotence.** The pass returns what it was given unless it can show it improved it, so
  applying it again changes nothing. That is a property of the acceptance rule, not of the round
  count.
- **An oscillation resolves to its better phase** rather than flipping for ever.
- **Ties break towards not moving**, which is what makes a drawing feel stable to someone dragging
  components.

A round that moves nothing at all is detected without costing anything (`adjustSegmentsInModel`
reports whether any segment moved), which is the common case after a drag and the reason the pass
is now *faster* than the fixed five: measured on a 192-wire sheet, a drag end went from 62 ms to
36 ms and re-separating a settled sheet from 41 ms to 16 ms. The first separation of a whole
unseparated sheet — sheet open, or Ctrl-Shift-R — is slower, 64 ms to 79 ms, because that is where
the extra rounds are actually earned.

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
| Edit > Redraw floating wires | `redrawFloatingWires` — every wire without a hand-routed segment is thrown away and routed again, then the sheet is separated |
| Edit > Redraw all wires | `redrawAllWires` — the same, hand routing included |

The two redraw items exist so that a change to routing can be seen over a whole sheet at once:
everything else re-routes only the wires a drag happens to touch, so a sheet laid out before a
change keeps most of its old routing and says almost nothing about whether the change helped.
Neither changes routing or separation — they are the ordinary pair of passes over many wires at
once. Two details of *how* they do it are there for what comes next rather than for now:

- the routing comes off **all** the chosen wires before any of them is routed, so that no wire is
  routed against a route which is about to be discarded;
- they are routed **shortest first**, by the straight-line distance between the two ports.

Both are immaterial while routing considers only symbols. They stop being immaterial the moment a
wire is routed with any regard for the wires already there — and shortest-first is the order to
have then, because a short wire has the least freedom in where it can go, so it should be the one
already in place when a longer wire of the same net is routed and looking for something to join.
For the record, the re-route these replace went in `Map` order, which is by `ConnectionId` — a
GUID, so no order at all, and not the same one twice.

`separateAndOrderModelSegments` takes a list of wires as its SCOPE: only clusters holding one of
them are touched, so a short list is a local adjustment and every wire id is a whole-sheet pass.
An empty list means nothing changed and the pass is skipped. Which scope a caller uses follows
from what changed:

- **Adding a wire, dragging a wire segment, un-fixing a wire** — local: the clusters that wire
  runs through. The rest of the sheet was settled a moment ago and owes nothing to this change.
  Manual segments stay fixed either way — `makeLines` marks them `FIXEDMANUALSEG`, and no pass
  moves those.
- **Moving, dropping, rotating, scaling or editing a symbol** — the floating redraw: every wire
  not routed by hand re-routed from scratch, then the whole sheet separated. The symbol's wires
  are not enough: a wire routed AROUND the symbol is not connected to it, and the space the moved
  wires vacate is space other wires should take up. Hand-routed wires are re-attached first
  (`rerouteMovedWires`, partial routing) and the redraw then leaves them alone.

Whole-sheet separation is safe because the settling loop makes the pass idempotent — a round that
cannot show it improved the sheet is discarded — so a cluster which is already settled costs a
little time and changes nothing, while one that is not gets the adjustment it was owed. It is also
what every test in `WireQuality.fs` measures, so the whole-sheet behaviour is the one the recorded
numbers describe; a test pins the local scope too, by putting two circuits far apart and checking
a local pass leaves the far one alone.

### The line array is ordered totally, and has to be

`makeLines` sorts its lines and then hands out `Lid` from the position in the array. Everything
after that reads the array by index: clusters grow by walking neighbours, `calcSegPositions` works
along it, and the ordering that minimises crossings breaks ties by arrival. So which of two
coincident lines comes first is not a presentational detail — it decides where wires end up.

Lines at the same `P` are the normal case rather than an edge case: two same-net segments leaving
one port start life exactly on top of each other. Sorting by `P` alone left their relative order to
the sort — and **F#'s `Array` sorts are stable under Fable but not under .NET**. Fable compiles
them to JavaScript's `Array.prototype.sort`, stable since ES2019; on .NET they run on
`System.Array.Sort`, an introsort that reorders equal elements. The same sheet, with byte-identical
routing going in — Map order, generation order and port geometry were all checked equal to the
last bit — separated one way in the test suite and another way in the application: on the eep1
`TEST1`/`TEST2` pair the tests drew the two-wire net as a single trunk while the app split it into
a long thin loop out of the port. Which is worse than either answer on its own, because it means
the recorded numbers in `WireQuality.fs` described a layout no user ever saw.

Two fixes, one general and one local. `src/Shared/ArraySorts.fs` shadows the `Array` sorts with
stable ones in both runtimes, the same treatment `ListPairs` gives the pairwise list functions, so
no other tying key can reopen the gap. And this sort key is `(P, B.MinB, B.MaxB, wire id)`, which
no two lines can share: geometry first, and the wire only as a last resort — better than relying
on stability, because it ties the order to the drawing rather than to the incidental order the
lines were generated in.

### Turns are read through zero-length joints, and tied ends are broken by depth

Three related defects in the ordering machinery were found through one sheet — two custom
components with wires wrapping over and under them (`customPair` in `WireQuality.fs`) — and each
had the same symptom: a pair of segments whose order the algorithm should have decided fell to
arrival order instead.

- **`turnDirs` read only the immediately adjacent segment**, and the segment beside a port's nub
  stack is zero-length on nearly every wire. A zero segment is a joint, not a turn: the wire's
  real departure is the first non-zero perpendicular segment beyond it. Reading a joint as
  "sign 0" made the ordering blind at exactly the ends that decide how wires running into a
  column of ports should nest.
- **`numCrossingsSign` was undefined on tied bounds.** Its case analysis keys on strict bound
  comparison to find the inner end; when two lines' ends coincide - which is what routing produces
  for two wires wrapping the same symbols - both orders fell into the same arm, the function
  answered the same sign for `(a, b)` and `(b, a)`, and the preference relation got a 2-cycle.
  What decides a tied end is how *deep* each wire turns: the one turning further encloses the
  other, so the shorter turn is the inner one (`turnLengths`).
- **`separateFixedSegments`' space search counted the escaped net as a wall.** When it moves a
  fixed segment out of an overlap, it looks each way for the nearest obstacle; a line lying on top
  of the overlap - the other wire of the very net being escaped - read as an obstacle at distance
  zero, so that side always looked full and the move went the other way regardless of what was
  actually there. Lines within `overlapTolerance` of the current position are now ignored: they
  are the overlap, not the wall.

And one invariant restored: **two wires of a net that routing commoned into one trunk can no
longer be split by a later pass.** The trunk's port-adjacent segments are FIXEDSEG, and
`linkSameNetLines` refused a FIXEDSEG follower, so the twins were never tied together and the
fixed-segment resolver could move each one alone - which is how a two-wire net came to be drawn as
a long thin loop out of its port. Coincident same-net FIXEDSEGs now link, and a linked pair moves
as one wherever it is moved from.

None of the seven corpus sheets moved on any metric when these landed; the `customPair` sheet went
from 7 crossings to its topological minimum of 3.

### What separation costs

Measured under .NET (Release, median of 5, warm; the app's JavaScript runs the same code roughly
2-3x slower). Every sheet of the `3cpu` project separates in **1.4-7.5 ms** - `dpdecode`, the
largest at 78 wires / 375 segments, is 7.5 ms. The end of a drag pays for the floating redraw -
routing every auto wire plus this separation - which is 32 ms on `dpdecode`, paid once per drag;
during the drag itself only the moved symbol's wires are routed, per mouse move. If mouse-up ever
needs to be cheaper on very large sheets, the targeted cut is to re-route only wires whose routes
intersect the moved symbol's old or new footprint instead of all of them.

The cost scales with the **largest cluster**, not the wire count. A synthetic 40-way fan-out (120
wires, one 80-wire net) takes ~65 ms, of which one vertical pass is 17.5 ms against 3.2 ms for the
horizontal: the net's segments form a single cluster, and `orderPairwiseToMinimiseCrossings` -
pairwise signs, partial-order completion, longest-chain placement - is roughly cubic in cluster
size (and its full mouse-up redraw is 165 ms). If a real design ever makes that pathological, the
ordering pass is the thing to optimise;
`makeLines`, `wiringCost`, the fixed-segment resolver and corner removal are all a few ms even
there, and jump recomputation is negligible.

### Port-anchored runs keep minimum separation

Two wires whose ports happen to sit nearly level run alongside each other at whatever distance the
ports dictate - both runs are FIXEDSEG (port-adjacent), which no cluster pass may move. The
fixed-segment resolver is the one pass that moves them, and its trigger used to be
`overlapTolerance` (2px): near-coincidence was mended, while a pair 4.6px apart - visually
touching for hundreds of pixels - was nobody's job. Its trigger is now `minWireSeparation`, and
because moving a FIXEDSEG puts a jog beside a port, the move is the MINIMUM outward nudge that
restores minimum separation, split between the two lines by the room each side has - not a leap to
the roomiest spot. The cost is a small jog where the ports disagree; the regfile8 pair that showed
this is pinned as a test.

### How far a wire clears a symbol it passes

A wire routed around a symbol clears it by `wireSeparationFromSymbol` (15px - the pitch between
two 2-input gate inputs) when there is room. The obstacle boxes the shift sites aim past are
already expanded by `minWireSeparation` (7), so the sites clear a further 8 beyond them; for years
they aimed `smallOffset` past the expanded box instead, which put every symbol-passing wire at
exactly 7px - visually touching - while wires in channels sat 15-30 apart. Separation leaves a
lone wire where routing put it (the settling cost gate rejects pure moves away from a symbol, as
they only add ink), so routing's clearance is what the user sees; channels may still squeeze
wires below 15 where space demands, which is the channel machinery's job and unchanged.

### No movement may take a segment into a symbol

Separation's movers - the cluster pass, linked-net propagation, the fixed-segment resolver - all
turn their decisions into wire changes through `adjustSegmentsInModel`, and that is where the one
rule they all answer to is enforced: **a move that would take a segment into a symbol box its span
overlaps, which it is not already inside, is refused** (linked groups as a unit, so a refusal
cannot split a net; the wire's own two endpoint symbols exempt, since a mux SEL wire legitimately
enters its own box; already-inside wires may still move, else they could never move out).

This is an invariant, not a patch, and it is needed because the passes' own bounds cannot express
it: a cluster carries ONE bound, while which symbol edges apply is a fact about each SEGMENT's
span - so a segment sharing a cluster with others can be placed across an edge its own span
overlaps while the cluster's bound, taken from a different member, allows it. That is exactly how
a wire was dragged through a multiplexer 23px past the bound its own cluster had computed (below).
The corner-removal pass checks its extensions against the barrier lines itself, and the T-junction
pass holds the whole moved wire to no-more-symbols-than-before; everything else goes through the
choke point. The corpus test pins the pipeline-wide consequence: separation never increases
`SymbolCrossings` over what routing delivered.

### Sub-visible segments are joints, not structure

A separate concern from the invariant above, though one bug involved both. Ports can be misaligned
by a fraction of a pixel (custom-component port arithmetic), so a "straight" wire can carry a
0.04px jog - far longer than `IsZero`'s 1e-7 epsilon, invisible on screen. Such a segment must not
be structure: `minVisibleSegmentLength` (0.5px) governs what `makeLines` will make a line of (so a
sub-visible jog cannot be moved, linked, or made the head of a same-net link), what `turnFrom`
reads as a turn, and what the T-junction pass treats as a departure. On the eep1 `alu` sheet a
0.04px jog became the head of a link whose union bounds were 359px long, and the cluster moved a
real riser through a multiplexer by moving the invisible stub - the crossing is stopped
independently by the invariant above, and the stub is kept out of the machinery by this rule.

### T junctions: same-net departures merged after separation

The last thing separation does is merge same-net turns. Where two wires of a net run along a
shared trunk and turn off it at different points, one turn is slid along the trunk onto the other,
so the net leaves the trunk once, as a T junction. The departures may head the same way - two
risers to destinations on the same side, which merge along their shared length - or opposite ways,
one up and one down, the register-file shape where a Q output feeds the same input of two muxes:
the trunk then ends where the two leave together. `alignSameNetDepartures` does it by adjusting
the moved riser's two neighbour segments, exactly as a segment drag would, and only ever when the
net's visible drawn length strictly falls - which both chooses the better of the two directions
and guarantees termination.

Five guards: the riser's neighbours must keep their directions (a neighbour pushed past zero folds
the wire back over itself); the riser at its new position must stay `minWireSeparation` clear of
every same-orientation segment of any other net - the merged riser can be longer than the one it
joins; it must stay the same distance clear of every symbol; and the moved wire must not cross
more than it did - sliding a long riser along its trunk sweeps it across other nets'
perpendicular segments, which neither the drawn length nor the parallel clearance can see, and on
`reg16x8` the unguarded version bought its ink with fifteen new crossings. And the junction a
merge would leave may have at most THREE arms: evening up two side branches where the trunk also
continues straight ahead makes a same-net cross-roads - four ways meeting at a point - which
modern wires cannot draw legally (a four-way meet is exactly what the junction circles exist to
disambiguate) and which reads badly in every style, so such a merge is refused even when it
shortens the drawing. Hand-routed wires are left alone entirely.

Separation and the merge run to a joint fixed point: a merge frees the space its riser vacated,
the next separation is entitled to take it up, and the loop repeats (bounded) until neither moves
anything. That is what keeps the combined pass idempotent, which the settling tests demand.

On `longFanout` this is worth 1100 units of drawn wire (9740 to 8615, and the fanned nets from
3960 to 2835) - more than same-net branch routing itself saves there. On `reg16x8` it saves 284
with crossings unchanged, and every one of the eight register-file nets leaves its trunk as a
single T. A departure that cannot merge is rejected for cause: the merge would lengthen the
drawing, cross more, or land on another net's line.

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
