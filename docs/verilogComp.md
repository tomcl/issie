---
title: Verilog Components
category: Documentation
categoryindex: 1
index: 6
---

# ISSIE Verilog Components

### Introduction

An ISSIE component can be defined by Verilog source instead of a schematic. The component can then
be placed on any sheet from the Catalogue, like any custom component. Create one with
`Verilog` -> `New Verilog Component` in the Catalogue; edit an existing one from its Properties
pane (the component's name cannot change once created). The editor has syntax highlighting and
checks the code as you type: the Save button stays disabled until the code compiles, and many
errors come with a one-click fix.

The accepted language is a subset of **SystemVerilog**: combinational logic (`assign`,
`always_comb`) *and* synchronous logic (`always_ff @(posedge clk)`), plus parameters, arrays,
`if`/`case`/`for`, and instantiation of other project components. Behind the scenes the code is
compiled to an ordinary ISSIE sheet built from standard components, so a Verilog component
simulates exactly like the equivalent schematic.

This page documents what is implemented, exactly. Anything not listed here is not supported —
notably there are **no** `initial` blocks, no `always @(...)` (use `always_comb`/`always_ff`), no
`reg`/`logic` declarations (use `bit`/`wire`), no `casez`/`casex`, no functions, tasks or
`generate`, no `/`, `%` or `**` operators, no tri-state logic, no `$`-system tasks, no
`` ` ``-directives, only `//` comments (not `/* */`), and one module per file.

### Module declaration

Both old-style and new-style (ANSI) headers are supported. Port declarations **must include the
`bit` keyword** — `input [15:0] instr;` is rejected, `input bit [15:0] instr;` is required.

Old style — ports named in the header, declared in the body:

```verilog
module decoder(instr, carry, negative, jump, mux1_sel, mux2_sel);
input bit [15:0] instr;
input bit carry;
input bit negative;
output bit jump;
output bit mux1_sel;
output bit mux2_sel;
wire [3:0] opc;
assign opc = instr[15:12];
assign jump = opc[0] ? carry | negative : carry & negative | opc[1];
assign mux1_sel = (&opc) & carry;
assign mux2_sel = jump | instr[3];
endmodule
```

New style — ports declared in the header; this is also the only form that can take a parameter
list:

```verilog
module decoder2 #(parameter W = 16)(input bit [W-1:0] instr, input bit carry, output bit jump);
always_comb begin
    if (carry) jump = instr[0];
    else jump = instr[1];
end
endmodule
```

Rules that the checker enforces:

- Every port must be declared as `input` or `output`, exactly once, and every input must be used.
- Bus ranges must be of the form `[N-1:0]` (i.e. ending at 0, MSB first). Range bounds may be
  expressions of parameters.
- Every bit of every output must be assigned, on every path (no accidental latches), and no bit
  may be driven twice.

### Identifiers

Standard Verilog rules: a letter or underscore first, then letters, digits, `_` and `$`
(so `_state` and `count$next` are fine; a leading `$` is reserved for system tasks, which are
not supported).

### Declarations

- Internal signals: `wire x;`, `bit x;`, `wire [7:0] y;`, `bit [7:0] y;` — `wire` and `bit` are
  treated identically. Declare before use.
- Declaration with initial value uses `bit`: `bit [3:0] opc = instr[15:12];` (this is a
  continuous assignment, not a reset value).
- Arrays (memories): `bit [7:0] mem [15:0];` — at most 2 dimensions, not allowed as ports.
  Arrays compile to one register (or wire) per word, so they are for small structures like
  register files, not large RAMs.
- `parameter N = 4;` in the body, or `#(parameter N = 4, M = 2)` in a new-style header.
  `localparam` is not supported.

### Numbers

Numbers can be written in binary, hexadecimal or decimal form, sized or unsized:

| Form | Meaning |
| :---: | :--- |
| `16'h3fa5` | 16-bit hexadecimal |
| `4'b0101` | 4-bit binary |
| `16'd154` | 16-bit decimal |
| `42` | unsized decimal, treated as 32 bits |
| `4'd(N)` | parameter-valued constant (parameter `N`) |

No `x`/`z` values, no `_` separators, no octal, no signed literals. A sized number must fit its
stated width, and widths must be consistent: the checker verifies that the right-hand side of
every assignment fits the left-hand side.

### Operators

In descending order of precedence (operators in one row have equal precedence):

| Operator | Description |
| :---: | :--- |
| `[ ]` | bit select `x[3]`, part select `x[7:4]` (constant or variable index) |
| `( )` | grouping |
| `~`, `(&a)`, `(\|a)`, `(~&a)`, `(~\|a)`, `(!a)` | bitwise NOT; reduction AND/OR/NAND/NOR and logical NOT, which must be written **in parentheses** |
| `*` | multiply (result truncated to operand width) |
| `+`  `-` | add, subtract; operands must have equal width N, result is N bits |
| `<<`  `>>`  `>>>` | shift left / right / arithmetic right; shift amount is an unsigned constant or variable |
| `<`  `<=`  `>`  `>=` | comparison (unsigned), 1-bit result |
| `==`  `!=` | equality, 1-bit result |
| `&` | bitwise AND (equal widths) |
| `^`  `~^`  `^~` | bitwise XOR / XNOR (equal widths) |
| `\|` | bitwise OR (equal widths) |
| `&&` | logical AND (operands may differ in width; 0 is false) |
| `\|\|` | logical OR |
| `? :` | conditional — compiles to a multiplexer |
| `{ }` | concatenation, e.g. `{a[2:0], b[3:2], 2'b01}` — right-hand side only |

Notable restrictions:

- There is no division `/`, modulus `%`, power `**`, or unary minus.
- Logical NOT and the reductions exist only in parenthesised form: `(!a)`, `(&a)` — a bare `!a`
  is a syntax error.
- A conditional cannot nest inside another conditional's arms; use an intermediate signal or an
  `if`/`else` chain in an `always_comb` block instead.
- Concatenation is not allowed on the left-hand side of an assignment.

### Continuous assignment

```verilog
assign out = expression;
```

One assignment per statement. The left-hand side may be a whole signal, a bit (`out[3] = …`,
including a variable index) or a part select (`out[7:4] = …`); assigning bits or slices
separately is fine as long as every output bit ends up assigned exactly once.

### Procedural blocks

Two forms only:

```verilog
always_comb begin ... end                    // combinational: blocking assignments (=) only
always_ff @(posedge clk) begin ... end       // synchronous: non-blocking assignments (<=) only
```

Signals assigned in an `always_ff` become registers. The clock is special: it must be an input
port named exactly `clk`, of width 1, and it may not be used in any expression — it can only
appear in `@(posedge clk)`. Sheets built from a Verilog component share ISSIE's single global
clock, like every other clocked ISSIE component.

Inside a block you can use:

- `if (cond) ... else if (...) ... else ...` — compiles to multiplexers. In `always_comb` every
  variable assigned anywhere must be assigned on every path (else the checker reports a possible
  latch); in `always_ff` a missing branch means "hold value", as usual.
- `case (expr) ... endcase` — labels must be numeric literals of the same width as the
  expression, with no duplicates; multiple labels per item (`2'd0, 2'd1:`) and a `default` (last)
  are supported. `casez`/`casex` are not.
- `for (i = 0; i < N; i = i + 1) ...` — bounds must be compile-time constants; the loop is fully
  unrolled at compile time, and the unrolled size is capped (around 500 components) to keep the
  generated sheet manageable. The loop variable needs no declaration and creates no hardware.
- A variable read in `always_comb` must not be written later in the same block, and combinational
  dependency cycles across the design are rejected.

### Module instantiation

A Verilog component can instantiate other components of the current project (Verilog or
schematic), by name, with **named** port connections:

```verilog
adder4 add1 (.a(x), .b(y), .sum(s));
counter #(.N(8)) c1 (.en(enable), .q(count));
```

- Positional connections are not supported; every port must be connected, each exactly once.
- The instantiated component must already exist in the project.
- Parameter overrides (`#(.N(8))`) work on Verilog components that declare parameters: ISSIE
  creates and saves a specialised copy of the component's sheet with the overridden value.

### What the compiler generates

The Verilog is synthesised to a normal ISSIE sheet: expressions become gates, adders,
multiplexers, bus selections and merges; `always_ff` variables become registers; arrays become
banks of registers (there is currently no RAM/ROM inference); instantiations become the
corresponding custom components. You can open the generated sheet like any other and watch it in
the step simulator or wave simulator.
