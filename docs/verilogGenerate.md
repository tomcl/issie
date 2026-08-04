---
title: Verilog Output
category: Documentation
categoryindex: 1
index: 7
---

# Verilog output

ISSIE can write a design out as *synthesisable* or *simulatable* Verilog. The whole design —
the chosen sheet with all of its subsheets flattened — becomes a single Verilog module named
`main`, plus one small generated module per RAM/ROM component.

To write a design as Verilog, right-click the sheet's name in the sheet menu (the breadcrumb of
sheets at the top of the canvas) and choose **Write design as Verilog**. A dialog offers the two
flavours, and help on using each:

- **Synthesis Verilog** — inputs and `clk` are module ports; suitable for synthesis tools such
  as yosys, and used by ISSIE's own FPGA build flow (see [the ISSIE-Stick page](issiestick.html),
  and the **Build** tab, which drives yosys/nextpnr/icepack/iceprog directly).
- **Simulation Verilog** — inputs become internal registers initialised to the values they had
  in the ISSIE simulator, and a testbench `initial` block runs the clock and `$display`s the
  outputs each cycle; suitable for running under a Verilog simulator such as Icarus Verilog.

The file is written as `<sheet>.v` in the project directory.

### Shape of the output

The generated code is plain Verilog-2001: a non-ANSI module header, `wire`/`reg` declarations,
combinational logic as `assign` statements (with `?:` for multiplexers), sequential components as
one-line `always @(posedge clk)` statements, and each memory as its own generated module with an
`initial`-block preload, instantiated positionally. Identifiers are derived from component
labels, upper-cased, with `$`-separated suffixes distinguishing subsheet instances and multiple
outputs.

Current limitations:

- Buses wider than 64 bits are not supported (output fails with an error).
- One design per file; the module is always called `main`.
- The emitted Verilog is *not* accepted by ISSIE's own [Verilog component input](verilogComp.html),
  which reads a different (SystemVerilog-flavoured) subset — so a design exported from ISSIE
  cannot currently be re-imported as a Verilog component. The gap between the two is analysed in
  [docs/dev/verilogRoundTrip.md](https://github.com/tomcl/issie/blob/master/docs/dev/verilogRoundTrip.md).

The generating code (`src/Renderer/VerilogComponent/Verilog.fs`) is simple and easily changed;
it is exercised by the `Issie.VerilogOutput` test group. Please add issues if what currently
exists does not fit your requirements.
