# Invariants of the .NET simulator and its protocol

What must be true for the sidecar and the renderer to agree about a simulation, why each thing is
true, and which of them a running Issie can check for itself.

This is not a description of the protocol — that is
[sidecarSimulation.md](sidecarSimulation.md) and the two `Protocol.fs`/`SidecarClient.fs` headers,
which are the wire format's own documentation. This is the shorter and harder list: the properties
the code relies on without saying so.

An entry earns its place here only if breaking it produces a **wrong answer or a hang** rather than
a slow one. Performance is elsewhere. Each is marked:

- **holds** — true today, by construction or by a check that exists
- **violated** — not true today, with what goes wrong
- **to build** — the code is being changed to make it true

---

## The fact everything else rests on

**At most one simulation exists at a time.** Starting a step simulation, a truth table or a
waveform simulation calls `removeAllSimulationsFromModel`, which drops the other two. The three are
mutually exclusive, and `ModelHelpers.simulationIsOpen` is the predicate that already says so.

That is why the sidecar needs **one session slot** and not one per simulator, why the epoch of
section C is a counter rather than an identifier, and why "requests do not overlap" is a question
about one caller at a time rather than about several callers agreeing.

---

## 0. What wall-clock time may decide

Several invariants below depend on getting this line in the right place, so it comes first.

> **Wall time may decide what is DISPLAYED and when work is INTERRUPTED. It may never decide what
> an answer IS.**

The licence for the second half is that every reply is **self-describing**. `SimRun` answers
`{clockTick, done}`: the caller learns exactly how far the simulation got, whatever caused the
chunk to end. A clock that jumps — a laptop resuming from sleep, mid-chunk — cuts the chunk short
and costs one extra round trip. It cannot produce a wrong number, because no number is inferred
from the timing.

What is forbidden is the inverse: **inferring elapsed time from work done**. That is what
`runFastSimulationCore` does today when it returns cycles-per-millisecond for the renderer to
divide by, and it is brittle in exactly the way the rule describes — after a resume the rate
collapses towards zero and the caller concludes that instant work will take minutes.

Display uses of time are safe almost by construction. If a machine sleeps for eight hours with a
fetch outstanding, an indicator saying "waiting eight hours" is *correct*: the data really is that
stale.

---

## A. Transport

`SidecarClient.fs` on one side, the `serve` loop in `Program.fs` on the other. One loopback
WebSocket; the renderer connects directly and the main process is not in the data path.

| # | Invariant | Status |
|---|---|---|
| A1 | There is at most one socket. `connect` returns the existing one if open; `request` rejects when there is none. | **holds** |
| A2 | A correlation id is unique among requests in flight. `nextCorrId` only increments, and it is a JS float, so it does not wrap in any realistic session. | **holds** |
| A3 | Every reply matches a request in flight. | **holds** — an unmatched reply is logged rather than ignored |
| A4 | Every request eventually settles — resolves or rejects. | **violated**, see below |
| A5 | Replies arrive in the order the requests were sent. | **holds**, and is deliberately *not* relied on: correlation ids mean it could stop being true without breaking anything |
| A6 | Every request is answered within a bounded number of ticks, except commands declared long. | **to build** |

### A4 — requests that never settle

`onclose` does `pending.Clear()`. The resolvers are dropped without being called, so a caller
awaiting a request when the socket closes waits forever, and the promise chain it belongs to
neither completes nor errors. Nothing above it can tell the difference between "still working" and
"gone".

**Fix**: reject them, with the reason, and log how many were dropped. A closed socket is a normal
event — the sidecar can be restarted — and every caller already handles an error.

### A6 — the reply-time bound

The bound is per command and stated in section E. The check is in section F.

---

## B. Inside the sidecar

| # | Invariant | Status |
|---|---|---|
| B1 | One message is handled at a time; no handler runs concurrently with another. | **holds** — `serve` is a sequential `while` loop, `receive → handle → reply` |
| B2 | One connection is served at a time. | **holds** — the accept loop blocks inside `serve` |
| B3 | `SimSession.session` is written only from the serve loop. | **holds**, and follows from B1 |
| B4 | `SimRun`, `SimRead` and `SimSetInputs` require a built session and error without one. | **holds** — checked, returns `{"error":...}` |
| B5 | `sheetCache` and `lastDesign` are written only by `SendDesign`. | **holds** |

B1 is the load-bearing one. It is why the sidecar's module-level mutable state needs no locking and
why its behaviour is worth reasoning about at all. **Anything that introduces a second thread
touching session state destroys every invariant in this section**, which is why the run loop polls
the clock itself rather than being interrupted by a timer thread.

The consequence to keep in mind is that a long handler blocks *everything*, including the command
that would cancel it. That is not a defect to be fixed with concurrency; it is the reason work per
request is bounded instead (section E).

---

## C. Agreement between renderer and sidecar

These are the ones that matter. The renderer holds beliefs about a process it cannot see inside,
and nothing currently checks that the beliefs are true.

| # | Invariant | Status |
|---|---|---|
| C1 | `WaveProvider.built` describes the session the sidecar actually holds. | **violated** — a belief with no verification |
| C2 | `sidecarClockTick` equals the sidecar's clock. | **violated** — same |
| C3 | At most one fetch chain runs against a session at a time. | **violated** — unguarded |
| C4 | A reply is applied only to the simulation it was asked of. | **violated** — an in-flight reply from a superseded design can still land |

### C1, C2 and C4 — one mechanism: the session epoch

`built` is set after a successful `SimBuild` and cleared by `forget`. Nothing else keeps the two
sides in step, so they diverge whenever something happens that the renderer does not see: the
sidecar process restarting, a build failing after `built` was set, a second Issie window building
over the top.

`sidecarClockTick` has the same shape — it is written from `SimRun` chunk replies and is right only
because nothing else advances the clock.

C4 is the timing version of the same problem. `forget` empties the renderer's cache when the design
changes, but a `SimRead` already in flight resolves afterwards and writes data from the old
simulation into the new one's cache.

**Fix**: the sidecar issues a **session epoch** — an integer, bumped on every successful
`SimBuild`. It is returned by the build reply, and every session-dependent request (`SimRun`,
`SimRead`, `SimSetInputs`, `SimEnd`) carries the epoch it believes it is talking to. A mismatch is
an explicit error reply, not silence and not wrong data.

That single number converts three unverifiable beliefs into one runtime check, and the check is
exact rather than heuristic: there is no case where the epochs agree and the sides do not.

### C3 — one fetch at a time

`refreshWaveSim` issues `fetchThisView()` whenever `covers` says the data is not held, and that is
every checkbox tick, scroll step and cursor move. Nothing prevents a second chain starting before
the first has finished. Two chains then interleave against one session, and the sidecar — being
strictly serial — will happily serve them in whatever order they arrive:

    chain A:  ensureBuilt ──────────► runTo ─────────────────► simRead
    chain B:            ensureBuilt ──────► runTo ────► simRead
                              ▲
                              └─ rebuilds the session under A, so A's read is of a
                                 simulation that no longer exists at the cycle it wants

**Fix**: single-flight. One outstanding fetch; a request arriving while one is in progress
supersedes it rather than racing it — the newer view is the one the user is looking at, so the
older one has nothing to contribute. With the epoch in place, a superseded chain's replies are
rejected by the sidecar anyway, which makes single-flight an optimisation of a correctness property
rather than the correctness property itself. Both are worth having: the epoch stops wrong data, the
guard stops wasted work.

---

## D. The waveform cache

`WaveData.fs`. Module state, deliberately: it is read synchronously from `view` on every render,
per wave, and is megabytes in size.

| # | Invariant | Status |
|---|---|---|
| D1 | Every handle in `Rows` is also in `Asked`. | **holds** by construction; unchecked |
| D2 | `slice` answers only for the exact window the data was fetched for. | **holds** — an equality test, deliberately not a containment test |
| D3 | The data received is `signals × samples` values long. | **taken on trust** |

D2 is stricter than it needs to be and that is intentional: a containment test would let the viewer
draw a sub-window of stale data without anything noticing. The cost is a refetch on every view
change, which is what the fetch is for.

D3 is worth checking on receipt because the failure is silent — a short reply gives every wave after
the truncation point somebody else's data, drawn confidently.

---

## E. Bounded work per request

This is what makes A6 achievable. Each command is either **bounded** — its work limited by a count
the caller sets — or **declared long**, and the declared-long list has one entry.

| Command | Bound | Status |
|---|---|---|
| `Echo`, `Upload`, `Download` | payload size | **holds** |
| `SendDesign` | one sheet per message | **to build** — framed per sheet, but all sheets travel in one message |
| `SimBuild` | **declared long** | the one carve-out |
| `SimRun` | one polling interval of wall time | **to build** — see below |
| `SimDigest` | component count, refused above a limit | **to build** — unbounded today; it builds and runs a simulation of its own |
| `SimRam` | the mode: a count that short-circuits, a sparse limit, or a window length | **to build** |
| `SimSetInputs`, `SimRead`, `SimLog`, `SimEnd` | payload size | **holds** |

### `SimRun` — the chunk boundary

The run loop polls the clock every **N cycles, with N chosen from the component count** so that a
roughly constant amount of *work* passes between polls — on the order of a thousand component
evaluations.

That single rule bounds both things that matter:

- **overhead**: a clock read is tens of nanoseconds against microseconds of work, so well under 1%;
- **overshoot**: because N is chosen by work rather than by cycles, the time between polls is
  roughly constant whatever the design's size, so a chunk overruns its budget by a bounded amount
  of *wall time*.

The existing `let stepsBeforeCheck = 100` bounds neither. On a two-component sheet it reads the
clock far too often; on a 480,000-component design a hundred cycles is on the order of a hundred
milliseconds, so a hundred-millisecond budget overshoots by about double. That one constant is most
of why the current chunking behaves badly at both ends of the size range — and it appears twice,
because the loop it guards is written out twice.

`runFastSimulation` is shared code: the sidecar's `SimSession.run` and the renderer's own simulator
call the same function. Its return type stops being `float option`, whose `None` means both
"nothing to do" and "finished" and whose `Some` is a rate nobody should be inferring anything from.

### `SimBuild` — why it is carved out

A build has no cycle loop to poll in; it is a sequence of phases. Bounding it means either yielding
mid-phase, which needs partial construction state, or moving it off the serve loop, which needs a
thread and costs every invariant in section B. Neither is worth it before the build itself is made
faster, so it is **declared long** and the reply-time check exempts it by name — an exemption that
is visible in the code rather than an unexplained outlier in a log.

---

## F. What a running Issie can check for itself

An invariant that cannot be checked is a comment. These can be, cheaply.

| Check | Where | Catches |
|---|---|---|
| epoch mismatch | sidecar, per session-dependent command | C1, C2, C4 |
| more than one fetch chain in flight | `WaveProvider` | C3 |
| request outstanding for longer than its command's bound | renderer tick, over `pending` | A6, and every hang above it |
| pending entries dropped on close | `SidecarClient.onclose` | A4 |
| `Rows ⊆ Asked`, data length = signals × samples | `WaveData.setFetched` | D1, D3 |
| correlation id already in `pending` | `SidecarClient.request` | A2 |

### The tick

The reply-time check needs to know that time has passed while nothing has happened, and nothing
happening dispatches no message and causes no render. So time arrives as a message: **one
continuous tick for the whole application**, an Elmish subscription started once and never stopped.

It is deliberately not started per wait. A recurrence started when a wait begins has, as its
failure mode, *silence* — and silence is indistinguishable from success in the one mechanism whose
job is to report that something has gone wrong. One always-on tick makes "has it started" a single
application-level fact instead of one per wait.

The same tick carries the memory check, which currently reads the clock on every render to decide
whether to dispatch `CheckMemory`. That gives the tick a second customer whose failure is
noticeable, so a tick that stops is a tick somebody discovers.

### What is displayed

Progress and staleness are the same condition seen at different lengths: *what you are looking at
is not what you asked for*.

- **Progress bar** — driven by `SimRun` replies, not by the tick. Issue a chunk; if the first reply
  comes back `done`, no bar ever appears; if it comes back not done, show a bar and keep chunking.
  The chunk interval *is* the delay before a bar appears, so "start a bar a fixed time after
  pressing run if it has not finished" is not a rule anyone implements — it is what the protocol
  already does. The level is `clockTick / target`, where the target is **this run's** last needed
  cycle and not the configured last clock: the waveform simulator runs lazily, so a bar measured
  against a four-million-cycle configuration would sit at zero forever.
- **The stale screen** — one visual form of an invariant violation, and nothing more. It should
  never appear. It exists so that a violation is something a user sees rather than a log line
  nobody reads.

### What cannot be checked

- **A hung renderer.** Every check here runs in the renderer, so a renderer blocked inside its own
  simulation reports nothing. That is not a gap to be closed here; it is the reason the renderer no
  longer runs a simulation the sidecar has already run.
- **The sidecar being wrong rather than absent.** The epoch catches a session that is not the one
  the renderer thinks. It does not catch a session that is the right one and has computed the wrong
  answer — that is what the cross-runtime digest comparison (`simCompare`) is for, and it belongs in
  the test suite rather than in a check on every request.

---

## G. The step simulator

The step simulator should use the sidecar too, and sections A, B and C then apply to it. But it is
**command-response, not cached**, and that is a difference in kind rather than of degree.

The cache exists to mediate what the WAVEFORM simulator needs: a window of many cycles, for a
subset of signals the user chose, fetched ahead of being drawn and then read synchronously from
`view` on every render. None of those three properties is true of the step simulator:

- it shows **all** top-level outputs, viewers and state of **one sheet**, so there is nothing to
  select and the payload cannot be large;
- it only ever asks about **the current cycle**;
- it **writes**.

So it needs no window, no handles, no coverage test and no prefetch - and putting it behind a cache
designed for those would be machinery in the way of four round trips.

| # | Invariant | Status |
|---|---|---|
| G1 | The panel shows values for the inputs the user set. | **violated** - a restart replaces them with the design's defaults |
| G2 | A step backwards inside the live range costs no re-run. | **holds** - it is a read of cycles already computed |
| G3 | A RAM is read at one cycle, in a shape bounded by how much it holds. | **to build** |

### The commands it needs

| purpose | payload | reply |
|---|---|---|
| build | design, array size | ok, or an error message |
| set inputs | a subset of top-level inputs | the panel at the current cycle |
| advance clock | target cycle `n` | the panel at cycle `n` |
| RAM contents | see below | see below |

"The panel" is one value: all top-level outputs, all viewers, and the state of every clocked
component, at one cycle. It is one reply because it is one screen - the panel currently re-reads
inputs, outputs, viewers and state separately on every render, which over a wire would be four
round trips per repaint.

Making the reply to a **write** be the panel is what keeps the step simulator to one round trip per
user action: setting an input is not a write followed by a read, and neither is stepping.

### G1 - inputs are sticky, and the history is not remembered

An input set on the sidecar **stays set**. It is state of the session, not a value written into a
step array at a cycle. That is right because inputs generally do not change; the step simulator is
the one place a user may change them on any clock tick, and a changed value affects how the
simulation evolves **from there onwards**.

Two things follow.

**A write is cheap and local.** Setting an input redoes only the combinational part of the
simulation at the current cycle. The clocked history is untouched, so a write costs one cycle's
combinational evaluation, not a re-run.

**Going back does not restore the inputs that were in force then.** Past inputs are not remembered.
What is seen when stepping back is the data still held in the buffer, which *was* computed under
those inputs - so their effect is visible for as long as the cycles they produced are still alive,
and not afterwards.

**This is not entirely consistent, and that is accepted.** Stepping back inside the live buffer
shows the real history; stepping back beyond it re-runs from zero under the inputs as they are now,
which is a different history reaching the same cycle. Recording and replaying every input change
would make the two agree, and would be more machinery than the inconsistency costs. It is written
down here so that the next person to notice it finds a decision rather than a bug.

### G2 - what the step simulator has to know about its own arrays

Three numbers, not one:

| | |
|---|---|
| the circular array's **size** | fixed at build |
| the **current clock tick** | where the panel is looking |
| the cycle **after which the data is correct**, up to the current tick | a low-water mark |

Stepping forwards and backwards moves the last two. The live range is not simply
`(ClockTick - size, ClockTick]`: going back and then forward again with a changed input overwrites
what follows, so how far back the data can be trusted is its own quantity and has to be carried
rather than computed from the other two.

`stepIndexOf` is `numStep % maxArraySize`, so **both** simulators index a circular buffer. The
waveform simulator never reaches the wrap, because its array is sized for the whole configured run;
the step simulator does, because it is sized short and stepping never has to stop.

Stepping backwards inside the live range is ordinary and cheap - those cycles have been computed,
so showing one is a read. Only a step past the low-water mark re-runs.

All three numbers belong in the replies that change them, because over a wire "that cycle has been
overwritten", "that cycle has not been reached" and "that cycle was never correct" would otherwise
arrive as the same silence.

### G3 - RAM contents, for both simulators

A RAM is interrogated at one cycle - the current one for the step simulator, the cursor's for the
waveform viewer - and the question is the same in both cases, so it is one set of commands used by
both:

| command | answer |
|---|---|
| count | how many locations the store actually holds |
| sparse *N* | every live location, as (address, value) pairs, when the count is below *N* |
| window (start, n) | *n* contiguous locations from `start` |

The renderer chooses between the last two using the count. That is the decision
`RamStore.liveCountExceeds` makes locally today, and it has to move because it needs the store: a
64K RAM displayed densely means materialising 65,536 words to show fifty.

### What this leaves the cache doing

Mediating the waveform simulator's reads, and nothing else. The step simulator's commands are
command-response sequences - read the panel, advance, step back, restart, stop - and the invariants
that apply to them are those in sections A, B and C, unchanged.

Because the two simulators are mutually exclusive, one session slot serves both and section C3 is
unaffected: there is never a second caller, only a second *kind* of caller, one at a time.

---

## H. Tooltips - two of them, and only one is a cache read

They are easy to conflate and have different answers.

### The waveform tooltip

Hovering a drawn waveform shows the value at that point. The signal is one the user selected, and
the cycle is inside the window being drawn - both by construction, since the thing under the
pointer is a waveform that was fetched to be drawn.

**So the cache already holds it, and there is no command and no new invariant.** It is a read of
data that had to be there for the pixel under the mouse to exist. `D2` covers it: the cache answers
for the window it fetched, which is the window being pointed at.

### The schematic tooltip

Resting the mouse on a wire in the draw block shows the value on it. This is a different question
with the same shape as the step simulator's reads:

- **one signal, the current cycle** - the step simulator's tick, or the waveform simulator's cursor;
- **any signal on the sheet.** The wire under the pointer is whatever the user is pointing at, so
  it may well be one the cache does not track - nobody selected it as a wave;
- **under either simulator.**

**So it is a point read and not a cache read**, and it must not be written as one that happens to
work when the signal is selected. Component, port, cycle: the degenerate `SimRead` of one signal
and one sample, which the cursor column already sends.

Caching it would be wrong as well as useless - a value under a moving mouse, wanted once, keyed by
a signal that changes with every wire the pointer crosses.

| # | Invariant | Status |
|---|---|---|
| H1 | A schematic tooltip is offered only for a design-time component with exactly ONE runtime instance. | **holds** - `copiesOfCanvasComp` returns a single id, or nothing is shown |
| H2 | The value shown is from the simulator that is running. | **violated in .NET mode** |

H1 is what makes the read expressible: a component on a sheet instantiated twice has two runtime
signals and no answer to "the value on this wire", so the tooltip declines rather than choosing
one. Worth noticing that this is a **design-time** question - how many times a sheet is
instantiated - so the renderer answers it from the design alone, with no simulation and nothing
asked of the sidecar.

H2 is a consequence of the renderer no longer running a simulation the sidecar has already run. The
probe read step arrays that are never written, so it showed what an unrun simulation holds; it has
been made to show nothing instead, which is honest but is not the requirement.

---

## I. The whole command set

Everything above asks for seven commands. Nothing needs an eighth, and each of the collapses below
is worth stating because the obvious design has more.

| command | payload | reply | used by |
|---|---|---|---|
| `SendDesign` | one sheet | ok | both |
| `SimBuild` | top sheet, array size | epoch, array size, ok or error | both |
| `SimRun` | target cycle, time budget | epoch, clock tick, done, first valid cycle, **and the panel when done** | both |
| `SimRead` | start, rep, samples, signals | values, signal-major | both |
| `SimSetInputs` | inputs | epoch, **the panel** | step |
| `SimRam` | component, cycle, mode | count, or sparse pairs, or a window | both |
| `SimEnd` | - | ok | both |

Plus `Echo`/`Upload`/`Download`, which measure the channel, and `SimDigest`/`SimLog`, which exist
for development. None of the five participates in a simulation, and they are outside these
invariants.

### Why there is no separate "read the panel" command

`SimRun` to the tick it is already on completes immediately and answers with the panel. So the
first display after a build, a step forwards, a step backwards and a jump are one command, and the
step simulator needs one round trip per user action rather than a move followed by four reads.

The panel rides on the reply only **when the run is done**, so a chunked waveform run does not carry
one per chunk. It is bounded by the top sheet - all top-level outputs, all top-level viewers, and
the clocked state the panel shows - which is one sheet's worth however large the design.

### Why there is no separate point-read command

A schematic tooltip is `SimRead` with one signal and one sample. So is the waveform viewer's cursor
column. The window read, the cursor column and the tooltip are one command at three sizes.

### Why RAM is one command and not three

Count, sparse and window differ in what comes back, not in what is being asked - the contents of one
RAM at one cycle. One command with a mode keeps the two sides in step where three would drift, and
the caller still chooses: ask the count, then ask for whichever shape fits it.

### What the cache must expose

Unchanged, and small - it serves the waveform viewer's drawing and nothing else:

| | |
|---|---|
| `covers window handles cursor` | is the data for this view here yet |
| `slice handle window` | the samples to draw |
| `valueAt handle cycle` | the cursor column, and the waveform tooltip |
| `setFetched` | fill it |

The interface does not change; what changes is that nothing else goes through it. The step
simulator, the RAM tables and the schematic tooltip are command-response, because each wants one
answer once rather than a window read repeatedly from `view`.

### What this leaves unbounded

`SimBuild`, and only `SimBuild`. Every other command is bounded by something the caller sets: a
sheet, a time budget, a sample count, an input count, a window length. That is what makes the
reply-time invariant of section A6 a property of the protocol rather than a hope about it.

---

## The truth table

Stays in the renderer, permanently. It is a different simulation - algebraic, so it uses the
`FData` backend rather than the numeric one - it is combinational only, and it is always small,
because a truth table with enough inputs to be large is refused before it is built. There is
nothing for the sidecar to do that the renderer cannot do faster than the round trip.

That its `Simulator.simCache` is shared with the step simulator is then a renderer-side concern
about two things wanting one cache slot, not a protocol question, and it does not belong in this
document.
