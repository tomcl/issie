# Can ISSIE's Verilog output be fed back into its Verilog input?

Answer, as of August 2026: **no — 0 of 48 tried files parse** — and the reasons are systematic,
not incidental. This page records the experiment, the exact gaps, and what a "golden cycle"
(design → emit Verilog → recompile as a Verilog component → simulate both → compare) would take.

## Why this matters

A working round trip would give the Verilog subsystem a free, self-checking test harness: every
fixture project becomes a test of *both* the emitter and the compiler, with the FastSimulation of
the original sheet as the reference for the recompiled one. Today the two directions speak
different dialects and were developed independently: the emitter
(`src/Renderer/VerilogComponent/Verilog.fs`) writes classic Verilog-2001 aimed at
yosys/Icarus, while the input compiler (`VerilogGrammar.ne` + `ErrorCheck*.fs` + `SheetCreator.fs`)
reads a SystemVerilog-flavoured subset (`bit`, `always_comb`/`always_ff`).

## The experiment

Emission runs under plain .NET (`Verilog.getVerilog` is pure F#; the `Issie.VerilogOutput` test
group already does this), so no app is needed:

1. Build the tests: `dotnet build Tests/Issie.Tests/Issie.Tests.fsproj -c Release`.
2. In an fsx referencing `Tests/Issie.Tests/bin/Release/net10.0/{Renderer,Issie.Tests,Expecto,Newtonsoft.Json}.dll`:
   for each sheet of each fixture project (`TestFixtures.loadProject`), run
   `Simulator.startCircuitSimulation` and `Verilog.getVerilog` in both `ForSynthesis` and
   `ForSimulation` modes, and write the results to files. The three fixture projects
   (`1fulladder`, `adder4`, `3cpu`) give 48 files covering gates, adders, muxes, comparators,
   shifts, registers, memories and custom-component hierarchy.
3. Parse each file with the input compiler's own parser:
   `require("src/Renderer/VerilogComponent/parser.js").parseFromFile(src)` under node.

Every file failed at the parse stage. Each gap below was then confirmed in isolation with a
minimal test case.

## The gaps, in blocking order

Emitted construct (left) vs what the input grammar accepts (right):

| # | Emitter writes | Input compiler accepts | Affects |
|---|---|---|---|
| 1 | `input clk;` / `input [7:0] X;` | `bit` keyword mandatory: `input bit [7:0] X;` | every file |
| 2 | `reg [7:0] RG = 8'h0;` declarations | no `reg` at all; `bit`/`wire`, no reset-value initialisers | every register, every simulation-mode input |
| 3 | `always @(posedge clk) X <= …;` | only `always_ff @(posedge clk)` (clock literally named `clk`) | every sequential sheet |
| 4 | `$` in identifiers (`A$5` subsheet flattening, `ADD$o1` multi-output, `$1` disambiguation) | `$` not in the identifier lexer | every hierarchical design, every adder |
| 5 | `assign { COUT,SUM } = A + B + CIN;` | concatenation not allowed on an LHS | every adder with carry-out |
| 6 | nested ternaries for `Mux4`/`Mux8` | a `?:` arm cannot contain another `?:` (nor can parens: `(a ? b : c)` is also rejected) | 4- and 8-way muxes |
| 7 | `assign O = !(A & B);` for Nand/Nor/Xnor | bare `!` prefix is a syntax error (only `(!x)` reduction form) | every inverting gate |
| 8 | `assign O = $signed(I) >>> S;` | no `$signed` | arithmetic shift right |
| 9 | one generated module per memory + `module main` | exactly one module per file | every sheet with RAM/ROM |
| 10 | positional instantiation `RM_mem RM_inst (RM, A, clk);` | named connections only, and the module must already exist as a project component | every memory instance |
| 11 | memory internals: `initial` blocks, `integer i`, `for` fill loops, unsized decimal preloads, `output[15:0]` with no space (the lexer requires one) | none of these | every memory module |
| 12 | simulation flavour: testbench `initial` block, `#5` delays, `$display`/`$time` | out of scope for a synthesisable-subset compiler | every `ForSimulation` file |

Confirmations by minimal edit: a purely combinational, non-hierarchical sheet
(`3cpu/nzgen`) parses after only adding `bit` to its port declarations (gap 1 — its unused `clk`
port would still fail the semantic "input not used" check); a hierarchical sheet
(`1fulladder/fulladd`) parses after adding `bit` and replacing `$` in identifiers (gaps 1+4).

Beyond parsing, three semantic-level mismatches would surface next:

- The emitter always emits a `clk` port, but the input checker rejects unused inputs, forbids
  `clk` anywhere except `@(posedge clk)`, and requires it only when `always_ff` exists.
- The input compiler has no RAM/ROM inference (arrays become register banks; the RAM path in
  `SheetCreator.fs` is commented out), so memory components cannot survive a round trip as
  memories.
- The input compiler silently compiles `~^`/`^~` (XNOR) as XOR (`SheetCreator.fs` maps
  `EBitwiseXnor` to plain `NbitsXor`), which would corrupt any design containing XNOR even once
  the syntax gaps were closed. (Found while auditing; it is an input-compiler bug regardless of
  round-tripping.)

## What a golden cycle would take

The cheapest path is a new emitter mode (say `ForVerilogComponent`) targeting the input dialect,
plus a few small input-compiler fixes, rather than teaching the input compiler all of
Verilog-2001:

Emitter side (all local to `Verilog.fs`):

- emit `input bit`/`output bit`/`bit` instead of bare directions, `reg` and `wire`;
- emit `always_ff @(posedge clk)` with the reset-value initialisers dropped (ISSIE registers
  reset to 0 implicitly in the input compiler too);
- avoid `$` in generated names (use `_` — the disambiguator already exists);
- split concat-LHS adder lines into two assigns via a temporary of width n+1;
- expand Mux4/Mux8 into if/else chains inside `always_comb`, or a temporary per level;
- emit `~(…)` (bitwise) instead of `!(…)` for inverting gates — equal-width operands make them
  equivalent here;
- emit ASR-by-constant without `$signed` (the sign-spread construction the input compiler itself
  uses), and suppress `clk` for purely combinational sheets.

Input-compiler side:

- memories: the cheapest route is the *emitter* lowering them into the dialect the compiler
  already handles — a RAM as an array with a clocked write, a ROM as `always_comb case` over
  literal labels (constants + muxes). That is behaviourally exact and needs no compiler change,
  but it costs ~3-4 components per word (the compiler expands arrays to register banks with
  per-word selection), so it only suits small memories: a 64K-word fixture like `3cpu`'s code
  memory would explode into hundreds of thousands of components. RAM/ROM inference from arrays
  (the commented-out path in `SheetCreator.fs`) — genuinely tricky — is only needed if emitted
  memories must come back *as* memory components, with sparse simulation and usable sheets;
- ~~the XNOR-compiled-as-XOR bug~~ — **fixed** (Aug 2026): `~^`/`^~` now convert to
  `BitwiseXnor` and synthesise as XOR + NbitsNot, pinned by a simulated truth-table test;
- ~~allow `$` in identifiers~~ and leading `_` — **fixed**: the lexer now takes standard
  Verilog identifiers (leading `$` stays rejected, so system tasks still error). Mandatory
  whitespace after `input`/`output`/`bit`/`wire` is gone too (`bit[3:0] x;` parses); note
  `output[15:0]` remains invalid only because `bit` is required in IO declarations.

With the emitter mode in place, a golden test is straightforward under plain .NET + node: emit
each fixture sheet, run the emitted text through `parser.js` and the error checker, build a sheet
with `SheetCreator.createSheet`, simulate both sheets for N cycles and compare outputs — the same
pattern `Tests/Issie.Tests/GoldenModel.fs` uses today.

## Test infrastructure (added Aug 2026)

`Tests/Issie.Tests/VerilogCompiler.fs` runs the *input* compiler end-to-end under plain .NET:
source → the real nearley parse (`node run_parser.mjs`, the same parse the editor runs) → AST
deserialised into `VerilogTypes.VerilogInput` → `ErrorCheck.getSemanticErrors` →
`SheetCreator.createSheet` → simulate and assert on outputs. New compiler work should come with
tests there; `run_parser.mjs` alone answers "does this source parse?" in milliseconds.

Building that pipeline flushed out latent bugs, fixed with it: `#(parameter ...)`
headers were silently discarded (two compounding wrong indices in the `module_new` grammar
action), constant array word-selects (`arr[2][0]`) stored the `]` token instead of the index
(three wrong indices in `ARRAY_SELECT`), `DrawHelpers.uuid` under .NET returned the same
string for every call (`Guid.NewGuid` missing its parentheses), which collapsed every generated
sheet to one component, and the `(~&x)`/`(~|x)` reductions crashed AST conversion
(`parseOperation` knew a `"!&"` spelling the lexer never produces and had no Nor case; note NOR
is the compare-to-zero itself, with no trailing inverter).

Known checker bug, not yet fixed: `checkVariablesUsed` enumerates an array's *vector bits*
(`decl.Range`) but assignments are tracked per *word*, so an array whose vector width differs
from its word count (e.g. `bit [7:0] hist [3:0]`) reports phantom unassigned words. The corpus's
`2d_array.sv` masks this because its width and word count are both 3. Also by design: every
array word must be both written and read.

## Reproducing

The probe scripts are trivial; the emission fsx and parse loop used for this analysis are
reproducible from the descriptions above. `parser.js` runs under node directly
(`require("./parser.js").parseFromFile(source)` from `src/Renderer/VerilogComponent`), which is
also the quickest way to check whether any candidate Verilog parses without opening the app.
