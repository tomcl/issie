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

The step simulator runs entirely in the renderer today, on `Simulator.simCache` - a different cache
from the waveform simulator's `simCacheWS`, though the two share a type and a build path. It should
also use the sidecar, and everything above then applies to it. What follows is what is different
about it, because the differences are where the invariants are.

| # | Invariant | Status |
|---|---|---|
| G1 | A session can be read only for cycles it still holds. | **implicit** - true, but written down nowhere and not the same range for the two simulators |
| G2 | The values shown are for the inputs the user set. | **violated** - a restart replaces them with the design's defaults |
| G3 | The sidecar holds one session, so two users of it must agree about it. | **to build** - only one user exists today |

### G1 - the arrays are circular, and how much that matters differs

`stepIndexOf` is `numStep % maxArraySize`, so **both** simulators index a circular buffer. The
waveform simulator never reaches the wrap because its array is sized for the whole configured run;
the step simulator does, because it is sized for a short one and the user can keep clicking.

So a step session can answer only for cycles in `(ClockTick - MaxArraySize, ClockTick]`, and a
waveform session for `[0, ClockTick]`. `SimulationView.restartsimrequired` computes exactly this
boundary, so the code knows where it is - but nothing carries the range with the data, and over a
wire the difference between "that cycle has been overwritten" and "that cycle has not been reached"
would arrive as the same silence.

Whatever route is taken, **the range a session can answer for is part of what a session is** and
should be in the build reply beside the epoch. It is one integer and it makes G1 checkable rather
than remembered.

Worth noticing: the circular buffer exists because the renderer could not afford long arrays. The
sidecar is bounded by machine memory rather than by V8's cage, so a step session could be sized far
longer than one in the renderer ever could - not unbounded, since stepping never has to stop, but
long enough that reaching the wrap becomes rare rather than routine. That does not remove G1; it
changes how often it bites.

### G2 - a restart silently discards the input history

When a step needs a cycle the buffer no longer holds, `runFastSimulationCore` calls
`restartSimulation`, which begins with `setInputstoDefault`. That sets every `Input1` back to the
**design's** default value.

The user's inputs are not replayed. They were never recorded anywhere to replay: changing an input
writes a value into a step array at a cycle, and the arrays are what the restart is discarding. So
stepping back far enough silently produces a simulation of a different input history from the one
the user built, presented as though it were theirs.

This is a renderer bug that exists today, not something the sidecar introduces. It is here because
moving the step simulator over the wire without fixing it reproduces it at a distance, where it
will be harder to see - and because the fix is the thing that makes the step simulator expressible
as a session at all: **the input history is state of the session**, a list of (cycle, component,
value), and a restart replays it. Once it exists, `SimSetInputs` is a description of the session
rather than a mutation of an array, and a session can be rebuilt from its design plus its input
history alone.

### G3 - one session, or two

`SimSession.session` is a single slot. Whichever route the step simulator takes, it and the
waveform simulator are two independent things wanting a built simulation, and the sidecar has room
for one. That is the structural question, and it is worth settling before either route is built,
because both hit it:

- **Sharing one session** means agreeing on array size and on circularity, which they currently
  disagree about (G1), and on the cycle each is looking at, which they do not share.
- **Two sessions** means the epoch of section C becomes a session *identifier* rather than a
  counter, and every session-dependent command names which one it means. That is a small change to
  make now and an awkward one to retrofit.

### Route 1 - the step simulator reads through the cache

The step panel needs one cycle's worth of values for a fixed set of signals. That is the shape the
cache already has: the cursor column is exactly a one-sample, one-cycle read.

Two things it needs that the cache does not do:

- **Component state**, not just port values - a register's contents, a RAM's store. `SimRead`
  carries port data only, so this needs a command of its own, and it is what
  `StepPanelSnapshot` in `SimInterface.fs` was sketched for: everything the panel shows at one
  cycle, in one request, because the panel currently re-reads inputs, outputs, viewers and state
  separately on every render.
- **Writes.** Setting an input mutates the session. The cache is read-only and has no notion of
  invalidation, so a write must empty whatever the cache holds for cycles at or after the one
  written. That is a new invariant and an easy one to get wrong quietly: **any command that changes
  the session invalidates every cached read of it**, and with the epoch in place the cheapest
  correct implementation is for a write to bump the epoch.

### Route 2 - the step simulator issues commands directly

Then sections A, B and C apply to it unchanged, and C3 - one fetch chain at a time - becomes the
harder question rather than the easier one, because the two simulators are genuinely independent
callers rather than one caller that can be serialised at a single point.

That is the argument for route 1: not that the cache is a better place to read from, but that
having exactly one thing in the renderer that talks to the sidecar is what makes "requests do not
overlap" a property of one module instead of a convention between two.

---

## Not yet examined

The truth table shares `Simulator.simCache` with the step simulator and builds an algebraic
simulation of its own. Nothing here covers what that sharing requires.
