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
| A1 | There is at most one socket. `connect` returns the existing one if open, joins one being made, and `request` makes one when there is none. | **holds** |
| A2 | A correlation id is unique among requests in flight. `nextCorrId` only increments, and it is a JS float, so it does not wrap in any realistic session. | **holds** — and is checked |
| A3 | Every reply matches a request in flight. | **holds** — an unmatched reply is logged rather than ignored |
| A4 | Every request eventually settles — resolves or rejects. | **holds** — failed on close, and failed on a deadline when there is no close |
| A7 | A closed connection is recoverable without a rebuild. | **holds** — `request` connects when there is no socket |
| A5 | Replies arrive in the order the requests were sent. | **holds**, and is deliberately *not* relied on: correlation ids mean it could stop being true without breaking anything |
| A6 | Every request is answered within its command's budget, except the two declared long. | **holds** — checked, see F |

### A4 — requests that never settle

`onclose` used to do `pending.Clear()`. The resolvers were dropped without being called, so a
caller awaiting a request when the socket closed waited forever, and the promise chain it belonged
to neither completed nor errored. Nothing above it could tell the difference between "still
working" and "gone". They are rejected now, with the reason, and how many were dropped is logged.

**A close is not the only way a reply fails to arrive.** A socket that goes quiet without closing
produces the same hang by a route `onclose` cannot cover, and it is the worse one: the entry stays
in `pending`, so the operation stays in `Model.SidecarInFlight`, so — one operation at a time being
the rule everything is sequenced by (section J) — *nothing is ever issued on this wire again*. So
every bounded request also carries a deadline, armed when it goes out, at ten times its section-E
budget. Ten times because the two numbers answer different questions: the budget is the line past
which a command is suspect and is drawn tight so the warning is worth reading, this is the line past
which it is not coming and has to be loose enough never to kill a legitimate reply. Firing it fails
that one caller and drops the socket, which A7 then makes recoverable.

It is a timer per bounded request and not an application tick — section F's objection stands. It is
armed with the request, needs no cancelling (the entry it looks for is gone by then, so it finds
nothing), and the two declared-long commands, having no budget, arm nothing.

### A6 — the reply-time bound

The bound is per command and stated in section E. The check is in section F.

### A7 — a dropped connection is not the end of the session

The sidecar drops a connection when a handler faults (section B) and keeps its session; the process
is still there and the epoch it issued is still valid. But `connect` was called only by a BUILD, so
after a drop every run and every read failed on "not connected", the viewer retried on its backoff
for ever, and simulation stayed dead until the user happened to press Refresh.

`request` connects when there is no socket, which costs one match when there is one. It cannot
storm: `connect` is single-flight, so concurrent callers join one attempt rather than racing (which
is its own failure — the sidecar serves one connection at a time, so a second handshake is not
refused but sits unanswered in the listener's queue), and it rejects rather than looping when the
sidecar is genuinely gone.

What this does **not** recover is the sidecar process dying: main does not respawn it and reports no
port, so `connect` waits out its startup budget and fails. That is a skeleton limitation and is
named in `Main/Bridge.fs` rather than here.

---

## B. Inside the sidecar

| # | Invariant | Status |
|---|---|---|
| B1 | One message is handled at a time; no handler runs concurrently with another. | **holds** — `serve` is a sequential `while` loop, `receive → handle → reply` |
| B2 | One connection is served at a time. | **holds** — the accept loop blocks inside `serve` |
| B3 | `SimSession.session` is written only from the serve loop. | **holds**, and follows from B1 |
| B4 | `SimRun`, `SimRead` and `SimSetInputs` require a built session and error without one. | **holds** — checked, returns `{"error":...}` |
| B5 | `sheetCache` and `lastDesign` are written only by `SendDesign`. | **holds** |
| B6 | A handler that throws costs its own request and no other. | **holds** — the dispatch is wrapped; the fault is answered as an error reply |

B1 is the load-bearing one. It is why the sidecar's module-level mutable state needs no locking and
why its behaviour is worth reasoning about at all. **Anything that introduces a second thread
touching session state destroys every invariant in this section**, which is why the run loop polls
the clock itself rather than being interrupted by a timer thread.

The consequence to keep in mind is that a long handler blocks *everything*, including the command
that would cancel it. That is not a defect to be fixed with concurrency; it is the reason work per
request is bounded instead (section E).

### B6 — a fault is one request's, not the connection's

The simulator this process runs is full of `failwithf`s about states that are not supposed to arise
— a memory component that is not a memory, a step outside an array, a state of the wrong shape —
and every one of them is reachable from a request, because a request names a component and a cycle
and this side chose neither. Reaching one unwound out of the serve loop: the socket was dropped and
every request in flight failed with it, for a fault that concerned one of them, leaving the renderer
with no way back to a session this process was still holding perfectly well. It is not
hypothetical — a RAM table asking for a cycle the step arrays had wrapped past did exactly this.

The dispatch is wrapped, so the fault becomes an error reply to the command that caused it and the
next command is served. It is caught around the whole dispatch rather than inside each handler
precisely because what it is for is what no handler expected. A send that then fails is still let
out, because at that point there is nothing left to answer on.

---

## C. Agreement between renderer and sidecar

These are the ones that matter. The renderer holds beliefs about a process it cannot see inside,
and nothing currently checks that the beliefs are true.

| # | Invariant | Status |
|---|---|---|
| C1 | `Model.SidecarSession` describes the session the sidecar actually holds. | **holds** — the epoch is checked on every session-dependent command |
| C2 | The session's recorded clock equals the sidecar's. | **holds** — it is written only from replies the epoch has already vouched for |
| C3 | At most one fetch chain runs against a session at a time. | **holds** — enforced by single-flight, not merely checked |
| C4 | A reply is applied only to the simulation it was asked of. | **holds** — a superseded epoch is refused |

### C1, C2 and C4 — one mechanism: the session epoch

`Model.SidecarSession` becomes `Session(top, size, epoch, clock)` when `AnsBuilt(Ok epoch)`
lands and `NoSession` when a simulation ends. Nothing else keeps the two sides in step, so they
would diverge whenever something happens the renderer does not see: the sidecar process
restarting, a build failing after the session was recorded, a second Issie window building over
the top.

The session's clock has the same shape — it is written from run-chunk replies (`AnsRan`,
`AnsSteppedTo`) as the value the sidecar REPORTED, never incremented towards, and is right only
because nothing else advances it.

C4 is the timing version of the same problem. Ending a simulation empties the caches and the
in-flight table, but a `SimRead` already in flight resolves afterwards - finding its number gone
from the table is what discards it, and the epoch check is the second lock on the same door.

**Fix**: the sidecar issues a **session epoch** — an integer, bumped on every successful
`SimBuild`. It is returned by the build reply, and every session-dependent request (`SimRun`,
`SimRead`, `SimSetInputs`, `SimEnd`) carries the epoch it believes it is talking to. A mismatch is
an explicit error reply, not silence and not wrong data.

That single number converts three unverifiable beliefs into one runtime check, and the check is
exact rather than heuristic: there is no case where the epochs agree and the sides do not.

### C3 — one fetch at a time

A fetch is asked for whenever a wave being drawn has not got the window it is drawn over, and that
is every checkbox tick and scroll step. Nothing in the protocol prevents a second chain starting
before the first has finished. Two chains then interleave against one session, and the sidecar —
being strictly serial — will happily serve them in whatever order they arrive:

    chain A:  ensureBuilt ──────────► runTo ─────────────────► simRead
    chain B:            ensureBuilt ──────► runTo ────► simRead
                              ▲
                              └─ rebuilds the session under A, so A's read is of a
                                 simulation that no longer exists at the cycle it wants

**Holds, by the in-flight table in the model.** `Model.SidecarInFlight` maps an operation number
to what was asked (`SidecarOp`); every issuer - the fetch checks, the run chunks, the builds the
start paths issue, the step-run cascade - adds its entry there, every reply
(`SidecarReply(seq, answer)`) removes it, and nothing is issued while the table is non-empty.
Emptying the table when a simulation ends is also what discards stale replies: an answer whose
number is no longer there belonged to a simulation that has gone. What to issue is decided by
viewport equality at the end of update - section J - so a check that finds the wire held does
nothing at all, and is re-entered by the completion that frees it.

The table is in the model because whether a request is in the air is a fact about the outside
world that cannot be derived - and because the UI draws from it (the run-progress strip, the
Start button's spinner while a build is in flight).

The other piece is `FetchFailedAtMs`, and it is there because deriving the ask makes failure
self-perpetuating: a fetch that fails leaves exactly the state that asks for another, and the two
spin as fast as the message queue will carry them. It paces failed RUN chunks for the same reason
- an errored chunk frees the wire, and an unpaced re-issue against a dead sidecar was measured as
thousands of identical errors inside one drain. The same asks are not made again for a couple of
seconds, and a failed fetch also latches its snapshot (`Model.FailedFetch`) so an UNCHANGED
viewport does not retry at all - any change to it is a different snapshot and tries again. It is a timestamp and not a flag because the fetch must be tried again
eventually - the commonest failure is asking while the sidecar is still starting, which fixes itself
a moment later, and a user action after that is what picks it up.

A wave the simulation cannot name is recorded as such - `NoDriver`, against the window it was asked
over - rather than left missing, for the same reason: a missing wave is what asks for a fetch, so
one that can never be fetched would be asked for by every update for as long as it stayed selected.
It draws blank and says so once. The rest of the view is fetched as usual; refusing the whole
request over it left the viewer white.

With the epoch in place, a superseded chain's replies are rejected anyway, which makes the bit an
optimisation of a correctness property rather than the correctness property itself. Both are worth
having: the epoch stops wrong data, the bit stops wasted work.

---

## D. The waveform cache

`WaveData.fs`. Module state, deliberately: it is read synchronously from `view` on every render,
per wave, and is megabytes in size.

**Keyed by wave, not by view - for READS.** Drawing asks "has this wave got the cycles it is
drawn over", per wave, per render, and one entry per wave answers it without a special case.
What the keying no longer serves is the fetch DECISION: that is viewport equality (section J),
which deliberately refetches the whole snapshot when anything in it changes - a lying cache then
costs a redundant read instead of a silently starved view, and the caches are pure memoisation
the decision never consults.

| # | Invariant | Status |
|---|---|---|
| D1 | A wave answers only for the exact window it was fetched for. | **holds** — an equality test, deliberately not a containment test |
| D2 | A wave is in the cache only if its data is. | **holds** — by construction: the entry and the data are put there together |
| D3 | The data received is `signals × samples × wordsPerSample` values long. | **holds** — and is checked, because a short reply is silent |
| D5 | A wave asked for over a window holds an answer for it - samples, or "no driver". | **holds** — by construction, and it is what stops an unfetchable wave being asked for for ever |
| D4 | Nothing fetched under a session that has ended is ever written. | **holds** — the cache carries the session it is of, and a write naming another is refused |

D1 is stricter than it needs to be and that is intentional: a containment test would let the viewer
draw a sub-window of stale data without anything noticing. The cost is a refetch when the view
changes, which is what the fetch is for.

D3 is worth checking on receipt because the failure is silent — a short reply gives every wave after
the truncation point somebody else's data, drawn confidently.

D4 matters because the cache is emptied when the design changes but a fetch already in the air is
not: without the check its reply would land beside waves of the design that replaced it, each
looking exactly as trustworthy as the other. Worse than beside — a driver index names a different
signal in the next build, so the old samples land *under the new signal's name*.

**It is checked where the writing happens, and it has to be.** This was written unconditionally for
a while, on the argument that which session is live is a fact about the model and so the question
belongs where the model is, when the completion message lands. That is a real principle and it is
the wrong one here: a promise finishes *before* its completion message is handled, and view code
reads this cache on every render in between. There was no later that came before the next render,
and the check the code named had never been written. So `Source.Fetched` carries the epoch it is of
and a write naming another is dropped.

The same number is what empties the cache: `selectSimulator` compares the session on the model with
the session the cache holds, so a build empties it on the next refresh. It used to take a
`newSimulation` flag from the caller saying the same thing less reliably.

### What is drawn, and what that means for the tooltip

A wave whose data has not arrived keeps the waveform it has. That is deliberate — waveforms a moment
out of date are what a viewer over a wire looks like, a viewer that blanks itself on every scroll is
what a broken one looks like — and it is why `WaveDrawn` remembers, beside each drawn waveform, the
exact view it is of.

The consequence to hold onto: **everything that describes what is on screen must be answered from
the window that waveform was drawn from, not from the window the controls ask for.** The value
column and the hover tooltip both do — `valueAt` reads the window the wave holds, and the tooltip is
given the gaps of the waveform it is over — so neither can disagree with the picture beside it.

There is no separate fetch for the cursor. The cursor is always on a drawn sample
(`CursorExactClkCycle = CursorDisplayCycle × SamplingZoom`), so the samples the waveform is drawn
from already contain it.

---

## E. Bounded work per request

This is what makes A6 achievable. Each command is either **bounded** — its work limited by a count
the caller sets — or **declared long**, and the declared-long list has one entry.

| Command | Bound | Status |
|---|---|---|
| `Echo`, `Upload`, `Download` | payload size | **holds** |
| `SendDesign` | one sheet per message | **holds** |
| `SimBuild` | **declared long** | see below |
| `SimRun` | one polling interval of wall time | **holds** |
| `SimDigest` | **declared long** | see below |
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

### `SendDesign` — one sheet per message, and what that settles

Decoding is the cost - the whole 18-sheet 3cpu design takes ~300ms, its largest single sheet ~25ms
- and it happens on the serve loop, which handles one message at a time. One message per sheet
makes each handler bounded by the largest sheet rather than by the largest design.

The sheets become a design only when the last of them has arrived, so the sidecar stages them and
`lastDesign` is replaced in one step. That is not defensive: **a design is only ever sent with every
simulation closed** - Start and Refresh both do it on a closed simulation, Refresh by stopping
first - so an upload never races a session, and nothing can observe a half-built design. The
staging exists because an upload is several messages, not because something might look at it in the
middle.

The first sheet of an upload therefore also **drops the session**. Nothing is taken from a caller
that is using it, and afterwards a command left over from before the design changed names an epoch
that no longer exists and is refused - rather than being answered from a simulation of a design
that has been replaced.

### `SimBuild` and `SimDigest` — why they are carved out

A build has no cycle loop to poll in; it is a sequence of phases. Bounding it means either yielding
mid-phase, which needs partial construction state, or moving it off the serve loop, which needs a
thread and costs every invariant in section B. Neither is worth it before the build itself is made
faster, so it is **declared long** and the reply-time check exempts it by name — an exemption that
is visible in the code rather than an unexplained outlier in a log.

`SimDigest` is declared long for a different reason, and permanently. It renders a design's whole
observable behaviour as text for the two runtimes to be compared byte for byte, which means
building and running a simulation of its own. It is a **development and test command** - `simCompare`
and the golden-model tests are its only callers - and bounding it would refuse exactly the large
designs a divergence hunt most wants to check. It touches no session, so however long it runs it
can only occupy the serve loop, never disturb a simulation.

---

## F. What a running Issie can check for itself

An invariant that cannot be checked is a comment. These can be, cheaply.

| Check | Where | Catches |
|---|---|---|
| epoch mismatch | sidecar, per session-dependent command | C1, C2, C4 |
| request outstanding, or answered, past its budget | `SidecarClient`, on send and on receive | A6, and every hang above it |
| pending requests failed rather than dropped on close | `SidecarClient.onclose` | A4 |
| a request unanswered past ten times its budget failed, and the socket dropped | `SidecarClient.request`, armed per request | A4, where there is no close to notice |
| a handler that threw answered as an error rather than unwinding | the sidecar's serve loop | B6 |
| data length = signals × samples × wordsPerSample | `WaveData.setFetched` | D3 |
| the session that answered is the session the cache is of | `WaveData.setFetched`, at the write | D4 |
| the viewport unserved for longer than a fetch takes, or an error latched | the viewer, per render | C3, D1 |
| correlation id already in `pending` | `SidecarClient.request` | A2 |

C3 has no check because it is **enforced**: nothing issues while `SidecarInFlight` is
non-empty. An invariant that cannot be broken needs no detector - what the viewer's banner
catches is the other half, a view that stays unserved.

### Timestamps, not a tick

Every check here is a timestamp compared against the clock, or a comparison over data already in
hand. Nothing is scheduled, nothing is counted, and no check holds state beyond what the thing being
checked already carries.

For A6 that means a pending request records when it was sent and which command it was. Anything past
its budget is reported, and so is a reply that arrives late - both when something HAPPENS, a request
going out or a reply coming in, rather than on a timer. That is enough: an application with nothing
happening reports nothing, and has nobody being misled by it either.

A continuous application tick was considered for this and rejected. It would have to run always,
costing a render of the whole application each time, to serve a check that a timestamp answers for
free - and "has the tick been started" is itself a piece of state that can be wrong, in the one
mechanism whose job is to notice when something is.

The budget is one number, two seconds, and it is deliberately not a latency budget: a round trip is
under a millisecond and a run chunk a tenth of a second, so this is the line past which a command is
not slow but stuck. The interesting part is the exemptions - the two declared-long commands, and the
three that measure the channel and can be asked for 64MB.

Each warning is said once per request, by a flag inside the pending entry, which disappears when the
request does.

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
  never appear in normal operation, and it comes up when there is an error. Behind is the SAME
  comparison that starts a fetch - the current viewport differs from the one the last completed
  fetch was for, an error reply counting as still outstanding since it never updates that record
  - and the age is measured from when THIS viewport appeared (`ViewportChangedAtMs`, stamped by
  the checks when the derived viewport changes) or a build or run last finished. Starting a
  fetch resets the clock, so the in-flight frames of a scroll or a hover can never flash it; a
  latched error shows at once and steadily, with no seconds figure that paced retries would
  falsify.

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
| G3 | A RAM is read at one cycle, in a shape bounded by how much it holds. | **holds** - `ReadRam` with a sparse/window reply, see docs/dev/ramOverTheWire.md |

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

### As implemented

The section above is the design; what shipped differs in one respect worth naming. The panel's
values are not the reply to a write or a step - they ride the SAME fetch mechanism as everything
else: the panel's cycle is part of the data viewport, so a step or a goto changes the viewport
and the next bundle reads the panel's signals along with whatever else changed. A poke
(`SimSetInputs`) bumps `Model.StimulusGeneration`, which is in the viewport, so everything
computed under the old stimulus refetches - G1's stickiness is unchanged, but the re-read is the
ordinary convergence rather than a special reply shape. A single step (+1/-1) issues one
budgeted run chunk through `SimulationView.advanceTo`; a goto is the `StartStepRun` cascade.

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
the cycle is inside the window that waveform was drawn from - both by construction, since the thing
under the pointer is a waveform made from data that had to be there for it to exist.

**So the cache already holds it, and there is no command and no new invariant.** The window it
answers for is the window that was drawn, which where data is still on its way is an older one than
the controls show - and that is right: the tooltip describes what is under the pointer. A row with
nothing drawn has no tooltip at all, so there is never a "waiting for data" to display.

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

## J. Who may ask, and how asking is decided

The renderer's side of every operation obeys one sequencing model, stated here because the code
that obeys it is spread over three files (`WaveSimTop.sidecarChecks`,
`SimulationView.continueStepRun` and the start paths) and because its one recorded violation - a
goto loop whose busy path re-dispatched itself synchronously, freezing the renderer inside a
single Elmish drain - looked innocent at every call site.

**The issuers are enumerable.** Exactly these put an operation on the wire:

- the start paths - `StartWaveSimulation` and `StartSimulation`/`StartStepRun` issue `SimBuild`,
  and nothing else ever builds. A missing session anywhere else is a stopped simulation, not a
  build somebody forgot;
- `WaveSimTop.sidecarChecks`, the end-of-update checks: one run chunk when the viewport needs a
  cycle the session has not reached, else one read bundle when a viewport differs from what the
  last completed fetch was for;
- the step-run cascade: `StartStepRun` and the reply handlers (`AnsSteppedTo`, `AnsBuilt`,
  `AnsFetched`) that continue it;
- a single step of the step simulator (`SimulationView.advanceTo`): one budgeted run chunk per
  click, gated on the same table, its panel read riding the ordinary fetch.

**One operation in flight.** `SidecarInFlight` is the record and every issuer's gate. Whoever
finds the wire held does NOTHING - no retry, no delay, no self-dispatch - because the held
operation's completion is a message, every check runs after every message, and so the waiter is
resumed the moment the wire is free. A loop that re-dispatches itself against a busy wire is the
forbidden shape: it spins the drain that the freeing reply needs to end.

**Decisions are state comparisons, never event tracking and never cache interrogation.** What
the view needs from the simulation is derived as one pure function of the model - the
`DataViewport` and `StructureViewport` records; their field lists are the enumeration that has
to be right, in one reviewable place. The fetch decision is `current viewport <> the viewport
the last COMPLETED fetch was for` (`FetchedData`/`FetchedStructure`, recorded from the snapshot
the operation carried, never from what is current when it lands). Nothing is cleared on rebuild:
the epoch inside each viewport makes a rebuild an inequality. The stimulus is in the viewport as
a poke counter (`StimulusGeneration`), because the poked values live in the simulator, which the
model cannot hold - the count is model state, and bumping it is what refetches everything
computed under the old stimulus.

**The MVU reading.** The built simulation is an implicit, immutable-for-its-lifetime component
of the model, named by its epoch. The only places the implicit part lags the explicit part are
the two bounded gaps - needs-running and needs-fetching - each visible as in-flight state, each
converging by construction. A fetch is a sequence of reads chained inside one promise (wire
speed, no Elmish round trip between reads) under one in-flight entry, answered by one message:
fast by construction, so it gets no progress display. Run chunks each go through Elmish, which
is where the progress displays come from.

**The memoisation obligation.** `FetchedData = snapshot` is a claim that the caches hold
everything the snapshot needs, so **every read in the bundle must reach the bundle's result**, and
every read's result must be memoised, keyed by state the viewport covers - and every cache
EVICTION policy must be a function of that same state.

The first half is the one that was got wrong. A bundle is several reads - waves, each memory, the
step panel, the port slices - and it answered with the WAVE read's result alone: the others logged
a warning and were dropped, so a fetch that lost a memory reported `Ok`, the snapshot was recorded
as served, and the comparison that decides fetches is against that record. Nothing asked again
until the view moved for some other reason. Reporting them costs no retry storm, because
`FailedFetch` latches the snapshot; what it buys is that the stale banner tells the truth. What DID
arrive is still returned and still shown - a memory whose rows came back is worth drawing whether
or not the one beside it failed.
`WaveDrawn.keepOnly` on the selection is sound because the selection is in the viewport, so the
eviction always coincides with an inequality that refetches. An eviction keyed on anything
outside the viewport recreates silent staleness; that is the review rule for a new cache, and
the viewport field lists are the thing to extend when a new kind of simulation-derived display
is added.

**Failure is latched by state too.** A failed fetch leaves the fetched-records alone and stores
its snapshot in `FailedFetch`; the fetch check requires the current snapshot to differ from it,
so an unchanged viewport does not retry at wire speed and any change tries again. The old shape
- the fetch itself issuing `SimBuild` when it found no session - is what once turned one refused
build into a retry storm; it is deleted by construction now, since nothing but a start builds.

---

## I. The whole command set

Everything above asks for seven commands. Nothing needs an eighth, and each of the collapses below
is worth stating because the obvious design has more.

| command | payload | reply | used by |
|---|---|---|---|
| `SendDesign` | sheet index, sheet count, top sheet name, one sheet's JSON | which sheet, whether it decoded, whether the design is complete | both |
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
| `needFetching handles window` | which of these waves has not got this window (diagnostics; the fetch decision is viewport equality, section J) |
| `slice handle window` | the samples to draw |
| `valueAt handle cycle` | the cursor column, and the waveform tooltip |
| `setFetched` | fill it |
| `holdNothing` | hold nothing, and ask for everything: a new design |

The interface does not change; what changes is that nothing else goes through it. The step
simulator, the RAM tables and the schematic tooltip are command-response, because each wants one
answer once rather than a window read repeatedly from `view`.

### What this leaves unbounded

`SimBuild` and `SimDigest`, both declared. Every other command is bounded by something the caller
sets: a sheet, a time budget, a sample count, an input count, a window length. That is what makes
the reply-time invariant of section A6 a property of the protocol rather than a hope about it - and
what makes the two exemptions worth naming in the code rather than discovering in a log.

---

## The truth table

Stays in the renderer, permanently. It is a different simulation - algebraic, so it uses the
`FData` backend rather than the numeric one - it is combinational only, and it is always small,
because a truth table with enough inputs to be large is refused before it is built. There is
nothing for the sidecar to do that the renderer cannot do faster than the round trip.

That its `Simulator.simCache` is shared with the step simulator is then a renderer-side concern
about two things wanting one cache slot, not a protocol question, and it does not belong in this
document.
