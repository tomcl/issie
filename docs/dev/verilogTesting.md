# Testing the Verilog subsystem

Issie's Verilog subsystem is two independent halves — an **emitter** (`VerilogComponent/Verilog.fs`,
writing classic Verilog-2001 aimed at yosys/Icarus) and an **input compiler**
(`VerilogGrammar.ne` + `ErrorCheck*.fs` + `SheetCreator.fs`, reading a SystemVerilog-flavoured
subset with `bit`, `always_comb`, `always_ff`). They were developed separately and speak different
dialects.

This page is what is tested today, what is not, and the three routes to closing the gap. The
largest of those gaps is that **no external Verilog tool ever runs**, in the test suite or in CI.

## What runs automatically today

Both groups are plain Expecto under .NET, in `npm run test`.

| Group | Tests | What it does |
|---|---:|---|
| `Issie.VerilogOutput` | 45 | Emits synthesis Verilog for 59 component cases in isolation — 41 listed explicitly, plus every gate type at 2, 3 and 4 inputs — via `dutCanvas` (the component with one Input per input and one Output per output), and for mixed sheets, then reads the text back. |
| `Issie.VerilogCompiler` | 11 | Source → real nearley parse (`node run_parser.mjs`, the same parse the editor runs) → `ErrorCheck.getSemanticErrors` → `SheetCreator.createSheet` → simulate the resulting sheet and assert on outputs. |

`VerilogOutput` makes three kinds of check, in increasing order of usefulness:

- **structural invariants over every component** — sized literals fit their declared width, no
  identifier over 50 characters, `module`/`endmodule` balanced, instance names unique, each
  component written exactly once, no undeclared net used, every declared net driven exactly once;
- **semantic checks** — for gates, the emitted expression is parsed out and evaluated against the
  same reference `Issie.ComponentSemantics` holds the simulator to;
- **regression assertions** pinning individual constructs (constants in hex, bus compare against a
  hex constant, `$signed` on ASR, memory module well-formedness, the synchronous RAM reading its
  pre-write value, the debug-profile uart).

`VerilogCompiler` covers operators (`~^`/`^~` as the complement of xor, `~&`/`~|` reductions),
identifiers (`$` and leading `_` accepted, leading `$` rejected), whitespace rules, arrays whose
vector width differs from their word count, and a corpus check that every `.sv` in
`test/input/valid` still parses.

## What is not covered

- **No external Verilog tool runs anywhere in the suite or in CI.** The emitter is checked against
  Issie's own reference evaluator, so a misunderstanding of Verilog semantics *shared* by the
  emitter and that reference is invisible. Nothing establishes that a real simulator or synthesiser
  agrees with Issie about what the emitted text means.
- **The emitter's `ForSimulation` mode is barely covered** — one test that inputs are assigned
  procedurally and one that a clock and testbench are declared. The testbench itself is never run.
- **Most of the input compiler's corpus is unreachable from the suite.** `test/input/valid`
  (24 files) is checked only for "still parses"; the 75-file `input/codegen` corpus, whose expected
  outputs came from Icarus, and the 52-file `input/semantic` error corpus are not read at all.
- **Nothing checks the two halves against each other.** See [the round trip](#route-c-the-round-trip)
  below; today 0 of 48 emitted files parse.
- The subsystem is ~8,000 lines of F# plus the Nearley grammar. Both groups pin specific
  behaviours; neither is broad coverage.

## The manual machinery that already exists

`src/Renderer/VerilogComponent/TestParser.fs` is a test runner that predates the Expecto suite and
is reachable only from **Development > Verilog** in a debug build. Nothing in `npm run test` reads
any of it — `grep -rn "VerilogComponent/test" Tests/` is empty.

Its corpus, under `src/Renderer/VerilogComponent/test/`:

| Directory | Files | What it is |
|---|---:|---|
| `input/codegen/single` | 75 `.sv` + 66 `.json` | single-module Verilog sources for the input compiler, each with the **input vectors** to drive it beside it (`TestParser.fs:193` reads the `.json` from the source's own directory) |
| `input/codegen/multiple` | 4 dirs | multi-module cases, one directory each |
| `input/driver` | 71 `.sv` | generated testbenches: drive N input vectors, `$display` the outputs as JSON |
| `input/semantic` | 52 `.sv` | sources whose expected *errors* are the thing under test |
| `input/valid` | 24 `.sv` | sources that must parse — the only part the Expecto suite uses |
| `ref/codegen` | 66 `.json` | expected outputs, **produced by Icarus** |
| `ref/semantic` | 54 `.json` | expected error lists |

The two 66s are a coincidence, and neither is 75. Taking the set differences rather than
subtracting the counts:

- **13 sources have no `ref/codegen` entry** — `array2`, `dual_ram`, `dual_ram2`, `fifo`,
  `forloop2`, `fsm6`, `fsm7`, `parameters`, `parameters2`, `ram`, `ram2`, `ram3`, `shifter2`.
  `runCodeGenTests` fails on each with "Couldn't open codegen reference output!" rather than
  reporting it as uncovered.
- **4 references have no source** — `002-fulladder`, `002-modinst`, `002-ripplecarryadder`,
  `counter2`. Nothing reads them.
- **9 sources have no input vectors** beside them, which is what makes that count 66: the nine are
  `dual_ram`, `dual_ram2`, `fifo`, `fsm6`, `fsm7`, `ram`, `ram2`, `ram3`, `shifter2` — the memory
  and state-machine cases, all of which also lack a reference.

**Icarus is already the oracle for the input compiler — by hand.** The four menu items are:

- *Generate Driver Modules* → `genDriverFiles ()` writes a `top_module` testbench per source,
  holding input vectors as arrays and printing each output's values as JSON;
- *Icarus Compile Testcases* → `iverilog -Wall -g 2012 -o <bin> -s top_module <driver> <src>`;
- *Icarus Run testcases* → `vvp <bin>`, stdout redirected into `ref/codegen/<name>.json`;
- *Run Verilog Tests* → `runCompilerTests ()`: the semantic suite against `ref/semantic`, then
  `runCodeGenTests ()`, which compiles each source **through Issie** and simulates the resulting
  sheet, comparing its per-cycle outputs against `ref/codegen`.

So the differential test — *same source, Icarus versus Issie's compiler* — exists and works. It is
just not automated, and it is fragile in specific ways:

- `executeCommand` spawns the child process and returns immediately, so compile and run are two
  separate menu clicks with no sequencing between them and no way to tell when either finished;
- failures print to a console and are counted, but nothing exits non-zero;
- paths are relative to the repo root, so it only works from a dev build;
- **the `.json` references are committed with no record of the Icarus version or the flags that
  produced them**, so nothing distinguishes "the compiler regressed" from "the reference was
  generated by a different tool";
- `TestParser.fs` compiles into the shipped renderer, and is on the `printf` allowlist in
  `Tests/Issie.Tests/SourceHygiene.fs` because of it.

## Route A: automate the existing Icarus differential test

Cheapest by a wide margin — the corpus, the drivers and the comparison logic all exist. What is
needed:

1. **Move the runner into Expecto.** `runCodeGenTests`/`semanticErrorTests` become an
   `Issie.VerilogCompilerCorpus` group. The parse step already has a .NET route
   (`VerilogCompiler.parseVerilog` shells out to `node run_parser.mjs`), and
   `SheetCreator.createSheet` plus `Simulator.startCircuitSimulation` are reachable under .NET, so
   the Issie side needs no app. Resolve corpus paths from `__SOURCE_DIRECTORY__` as
   `VerilogCompiler.fs` does, not from the process working directory.
2. **Make Icarus a checked prerequisite, not an assumption.** Probe for `iverilog` and `vvp` once;
   if absent, skip the group with a message naming them — the pattern `Issie.VerilogCompiler`
   already needs for `node`, and the same pattern as the `CI` skip.
3. **Replace `executeCommand` with a synchronous, exit-code-checked run.** Under .NET this is
   `Process.Start` + `WaitForExit` + assert on `ExitCode`, capturing stderr into the failure
   message. The async-spawn-and-hope shape cannot be made reliable.
4. **Regenerate references as part of the test, not ahead of it.** Run Icarus and Issie on the same
   source in the same test and compare, rather than comparing against a committed file. That
   removes the version-provenance problem entirely and makes `ref/codegen` unnecessary. Keep a
   `ISSIE_UPDATE_GOLDEN`-style escape only if a committed reference is wanted for the
   Icarus-less case.
5. **Resolve the three corpus mismatches above**: give the 13 sources with no reference one (which
   step 4 does by construction), write input vectors for the nine that have none, and delete the
   four orphaned references. Failing with "Couldn't open codegen reference output!" is worse than
   either covering the case or dropping it.
6. **Decide what runs in CI.** Icarus is one `apt-get install iverilog` on Linux and a package on
   the other two platforms, but 75 sources × (compile + run) is not a per-push cost. The natural
   split is: skipped by default like `VerilogCompiler` is, run on a schedule or a label.
7. **Then delete `TestParser.fs`** and its Development menu items. It is 937 lines compiled into
   the shipped binary, reading paths that do not exist in a packaged build, and its only reason to
   exist is that this work has not been done.

## Route B: an Icarus check of the *emitter*

This does not exist in any form, and it is the emitter's only possible independent oracle. Route A
validates the input compiler; nothing validates that what Issie *writes* means what Issie thinks.

What it needs:

1. **A driver generator for an emitted sheet.** `TestParser.genDriver` already writes exactly this
   shape — input vectors as arrays, a clock loop, outputs printed as JSON — but from the input
   compiler's port information. It needs a version that takes a `FastSimulation`'s top-level ports,
   which is the same information `Verilog.getVerilog` itself works from.
2. **Stimulus shared with the Issie side.** Generate one set of input vectors, drive both the
   emitted Verilog under `vvp` and the `FastSimulation` with it, compare outputs cycle by cycle —
   the pattern `Tests/Issie.Tests/GoldenModel.fs` uses, with Icarus in place of the stored file.
3. **A decision about `ForSimulation` versus `ForSynthesis`.** `ForSimulation` already emits a
   testbench with `$display`/`$time` and `#5` delays, so it is closer to runnable — but the mode
   that matters to users is `ForSynthesis`, since that is what goes to an FPGA toolchain. Test
   `ForSynthesis` with a generated driver; treat `ForSimulation` as a second, cheaper case that
   needs no driver at all.
4. **A fixture set.** `Tests/fixtures/` already holds three whole projects (`1fulladder`, `adder4`,
   `3cpu`) that give 24 sheets covering gates, adders, muxes, comparators, shifts, registers,
   memories and custom-component hierarchy — and `GoldenModel.fs` already simulates them. Emitting
   and checking each one is the natural scope.
5. **Memories are where this will first fail.** The emitter writes one module per memory with
   `initial` blocks and `for` fill loops; Icarus handles that, but a 64K-word ROM like `3cpu`'s is
   a large elaboration. Cap the fixture memories or accept the runtime.

Doing Route B would also settle, with evidence rather than reading, whether yosys accepts the
synthesis output — the claim on which [Verilog Output](../verilogGenerate.html) rests.

## Route C: the round trip

*Can Issie's Verilog output be fed back into its Verilog input?* **No — 0 of 48 tried files
parse** — and the reasons are systematic, not incidental.

A working round trip would give the subsystem a self-checking harness needing no external tool at
all: every fixture project becomes a test of *both* halves, with the `FastSimulation` of the
original sheet as the reference for the recompiled one.

### How to re-run the experiment

Emission runs under plain .NET (`Verilog.getVerilog` is pure F#; `Issie.VerilogOutput` already does
this), so no app is needed:

1. Build the tests: `dotnet build Tests/Issie.Tests/Issie.Tests.fsproj -c Release`.
2. In an fsx referencing `Tests/Issie.Tests/bin/Release/net10.0/{Renderer,Issie.Tests,Expecto,Newtonsoft.Json}.dll`:
   for each sheet of each fixture project (`TestFixtures.loadProject`), run
   `Simulator.startCircuitSimulation` and `Verilog.getVerilog` in both `ForSynthesis` and
   `ForSimulation` modes, and write the results to files. The three fixture projects give 24 sheets, so 48 files.
3. Parse each file with the input compiler's own parser:
   `require("src/Renderer/VerilogComponent/parser.js").parseFromFile(src)` under node.

Every file fails at the parse stage, and gap 1 below alone accounts for all of them.

### The gaps, in blocking order

Emitted construct (left) vs what the input grammar accepts (right). Each row was re-checked
against the current parser with a minimal source file, by the method in
[Quick checks](#quick-checks-while-working):

| # | Emitter writes | Input compiler accepts | Affects |
|---|---|---|---|
| 1 | `input clk;` / `input [7:0] X;` | `bit` keyword mandatory: `input bit [7:0] X;` | every file |
| 2 | `reg [7:0] RG = 8'h0;` declarations | no `reg` at all; `bit`/`wire`, no reset-value initialisers | every register, every simulation-mode input |
| 3 | `always @(posedge clk) X <= …;` | only `always_ff @(posedge clk)` (clock literally named `clk`) | every sequential sheet |
| 4 | `assign { COUT,SUM } = A + B + CIN;` | concatenation not allowed on an LHS | every adder with carry-out |
| 5 | nested ternaries for `Mux4`/`Mux8` | a `?:` arm cannot contain another `?:` (nor can parens: `(a ? b : c)` is also rejected) | 4- and 8-way muxes |
| 6 | `assign O = !(A & B);` for Nand/Nor/Xnor | bare `!` prefix is a syntax error (only `(!x)` reduction form) | every inverting gate |
| 7 | `assign O = $signed(I) >>> S;` | no `$signed` | arithmetic shift right |
| 8 | one generated module per memory + `module main` | exactly one module per file | every sheet with RAM/ROM |
| 9 | positional instantiation `RM_mem RM_inst (RM, A, clk);` | named connections only, and the module must already exist as a project component | every memory instance |
| 10 | memory internals: `initial` blocks, `integer i`, `for` fill loops, unsized decimal preloads | none of these | every memory module |
| 11 | simulation flavour: testbench `initial` block, `#5` delays, `$display`/`$time` | out of scope for a synthesisable-subset compiler | every `ForSimulation` file |

Two things that used to be on this list are **not** gaps: the lexer takes standard Verilog
identifiers, so the `$` the emitter puts in generated names (`A$5` from subsheet flattening,
`ADD$o1` for a multi-output component) parses — only a *leading* `$` is rejected, so system tasks
still error; and no whitespace is required after `input`/`output`/`bit`/`wire`, so
`output bit[15:0] o;` parses.

Beyond parsing, two semantic mismatches surface next:

- The emitter always emits a `clk` port, but the input checker rejects unused inputs, forbids `clk`
  anywhere except `@(posedge clk)`, and requires it only when `always_ff` exists.
- The input compiler has no RAM/ROM inference (arrays become register banks; the RAM path in
  `SheetCreator.fs` is commented out), so memory components cannot survive a round trip as
  memories.

### What a golden cycle would take

The cheapest path is a new emitter mode (say `ForVerilogComponent`) targeting the input dialect,
plus one input-compiler change, rather than teaching the input compiler all of Verilog-2001.

Emitter side, all local to `Verilog.fs`:

- emit `input bit`/`output bit`/`bit` instead of bare directions, `reg` and `wire`;
- emit `always_ff @(posedge clk)` with the reset-value initialisers dropped (Issie registers reset
  to 0 implicitly in the input compiler too);
- split concat-LHS adder lines into two assigns via a temporary of width n+1;
- expand Mux4/Mux8 into if/else chains inside `always_comb`, or a temporary per level;
- emit `~(…)` (bitwise) instead of `!(…)` for inverting gates — equal-width operands make them
  equivalent here;
- emit ASR-by-constant without `$signed` (the sign-spread construction the input compiler itself
  uses), and suppress `clk` for purely combinational sheets.

Input-compiler side, only memories are left. The cheapest route is the *emitter* lowering them into
the dialect the compiler already handles — a RAM as an array with a clocked write, a ROM as
`always_comb case` over literal labels. That is behaviourally exact and needs no compiler change,
but it costs ~3-4 components per word (the compiler expands arrays to register banks with per-word
selection), so it only suits small memories: a 64K-word fixture like `3cpu`'s code memory would
explode into hundreds of thousands of components. RAM/ROM inference from arrays (the commented-out
path in `SheetCreator.fs`) — genuinely tricky — is only needed if emitted memories must come back
*as* memory components, with sparse simulation and usable sheets.

With the emitter mode in place, the golden test is straightforward under plain .NET + node: emit
each fixture sheet, run the emitted text through `parser.js` and the error checker, build a sheet
with `SheetCreator.createSheet`, simulate both sheets for N cycles and compare outputs — the same
pattern `Tests/Issie.Tests/GoldenModel.fs` uses today.

## Which route first

Route A is a day's work on machinery that already exists and turns 75 hand-run cases into a suite.
Route B is the only thing that can tell you the emitter is right, and reuses Route A's process
handling. Route C is the most work and needs no external tool, so it is the one that could run on
every push — but it changes the emitter, which Route B should be watching by then.

## Quick checks while working

`run_parser.mjs` answers "does this source parse?" in milliseconds — the quickest way to check any
candidate Verilog without opening the app, and how the gap table above was verified:

```bash
cd src/Renderer/VerilogComponent
printf 'module main(a,o); input bit a; output bit o; assign o = a; endmodule' > /tmp/t.sv
node run_parser.mjs /tmp/t.sv        # prints {"Ok": …} or {"Err": …}
```

`parser.js` also runs under node directly
(`require("./parser.js").parseFromFile(source)` from the same directory) if the AST is wanted
rather than the pass/fail.

Two things to point at when a generated sheet looks wrong. The grammar actions index into
positional token arrays, so an off-by-one there silently *discards* a construct rather than failing
— `#(parameter ...)` headers and constant array word-selects (`arr[2][0]`) were both lost this way.
And `checkVariablesUsed` tracks assignment per array *word* while a declaration's `Range` is its
*vector bits*: an array whose vector width differs from its word count (`bit [7:0] hist [3:0]`) is
the shape that catches confusion between the two. The intended rule is that every array word must
be both written and read.
