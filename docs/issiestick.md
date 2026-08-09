---
title: ISSIE-Stick FPGA hardware
category: Documentation
categoryindex: 1
index: 10
---

# ISSIE-Stick and the FPGA build flow

**This page describes older work that still exists in ISSIE but is no longer maintained.** The
code is all there and the flow has worked; the hardware, the toolchain versions and the
instructions have not been kept up with the rest of ISSIE, so expect to do some work before it
runs. It is recorded here so that anyone picking it up knows what was built and where to look —
not as a feature to reach for. If you want Verilog for an FPGA today, use
[Verilog Output](verilogGenerate.html) and your own toolchain.

## What it is

The ISSIE-Stick is a USB plug-in board that lets an ISSIE design run on a real FPGA. Two revisions
exist, v0.1 (iCE40 HX4K, TQ144) and v1.0 (iCE40 HX8K, BG121); the Lattice IceStick (HX1K) is
supported by the same flow.

## What is still in the app

A **Build** tab drives the open-source iCE40 flow directly — yosys for synthesis, nextpnr-ice40
for place and route, icepack to make the bitstream, iceprog to program the board. It is hidden by
default and turned on from the View menu, which is a fair reflection of how finished it is: the
tools have to be installed and on the path yourself, and nothing checks for them until the build
runs. The pin constraint files for all three boards ship with ISSIE, under `static/hdl`.

Writing the Verilog itself is separate from any of this and is maintained: `Sheet` →
`Write design as Verilog`, choosing **Synthesis Verilog**. That output is ordinary synthesisable
Verilog and needs nothing from this page.

## Where the rest of it is

The board design and the toolchain instructions live in a separate repository,
[issie-synth](https://github.com/edstott/issie-synth), whose README is the real documentation for
building or obtaining the hardware.

## What updating this would mean

Someone reviving this should expect to: check the flow against current yosys and nextpnr releases,
say plainly which tool versions are known to work and how to install them on each platform, replace
the failure that occurs when a tool is missing with a message that says what to install, and write
the walkthrough from a design to a programmed board that this page has never had.
