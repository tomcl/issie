---
title: Home
category: Documentation
categoryindex: 1
index: 1
---

# Issie Documentation Home

See [Features](features.html) for what Issie does and why it is easy to use.

See [Getting Started](gettingStarted.html) for how to put Issie on your laptop or develop Issie.

See [User Tutorial](userGuide.html) for a useful introduction to Issie on one page which you can follow or read.

See [Schematic Editor Features](coolFeatures.html) for a reference of every editor operation.

See [Parameter System](parameterSystem.html) for a detailed explanation of symbolic parameters, expressions, constraints, and simulation integration.

<br>

## What is ISSIE?

* ISSIE is an **easy-to-use schematic editor and simulator** for hierarchical design of **digital logic circuits**. Run it and see the built-in demos for what it can do! ISSIE is targeted at 1st year university students, but would be useful teaching in schools and even for quick hardware design and test in an industrial or research environment. For the latter use case although we allow Verilog input and output these features need a bit more work to be as complete and nice as the rest of ISSIE.
* ISSIE was motivated because we found that industry-standard CAD systems were too complex and buggy to be learnt in labs, and when learnt still nasty to use. Other educational products were too limited. We wanted a system to teach hierarchical design and digital electronics visually that would scale to large designs.
   - We find that EEE-educated internal PhD candidates starting with schematic logic designs usually have a better understanding of what **digital hardware is** than those from other institutions starting with HDL.
* ISSIE has as a design principle that it should be easy to use by a novice, with all errors well highlighted. Error messages telling the user what the error is and how precisely to correct it, so a novice user can make the correction — and where the correction is unambiguous, Issie offers a button that makes it. Keeping to this principle has been challenging but worthwhile. We also wanted a complete system capable of real design work: good user interface is separate from large-scale features and performance and there is no reason they cannot both be implemented.
* ISSIE is developed **by undergraduate students and staff at Imperial College London**. The code is cross-platform and binaries are released for Windows and Silicon (Arm64) Mac and Linux platforms.
* ISSIE is implemented in about 75K lines of code (112K lines including comments and blanks) in the Functional-first language F#, the equivalent of some 200K lines in a typical OOP language, with a further 4K lines of automatic tests. ISSIE's implementation is (almost) pure functional programming without assignment. This makes the codebase very maintainable! ISSIE has **its own digital simulator**, which uses **write-once semantics** on mutable (JS typed) arrays to combine the robustness of functional programming with high performance.
* ISSIE uses the [FABLE F# to Javascript compiler](https://fable.io/), [Elmish MVU framework](https://elmish.github.io/elmish/), and [Electron](https://www.electronjs.org/). This tool chain creates seamless cross-platform applications with simple and highly productive coding.
* ISSIE can generate synthesisable Verilog output to drive FPGAs with your own toolchain. An integrated build flow for **ISSIE-Stick** hardware also exists, from an earlier project, but is [no longer maintained](issiestick.html).
* ISSIE can also accept Verilog source, using this to define and simulate equivalent schematic components.
* For acronym geeks, ISSIE stands for: *Interactive Schematic Simulator with Integrated Editor*.
  

<br>

## What is new

ISSIE has recently gained [sheet parameters](parameterSystem.html) with per-instance bindings and
constraints, reusable [component libraries](features.html) in the Catalogue, one in-app
keyboard and menu system with its shortcut help generated from the dispatch table, an in-app
project browser with drag-and-drop placement and a design hierarchy tree, a rewritten fast
simulator, and an automatic test suite covering the simulator, the parameter system and the
schematic editor. It runs on .NET 10, Fable 5, Elmish 4 and React 18.

Every release and what changed in it is on the
[releases page](https://github.com/tomcl/issie/releases).

## Future work

* **Automated Verilog testing against an external tool.** ISSIE's Verilog output is currently
  checked only against ISSIE's own simulator, so nothing establishes that a real Verilog tool
  agrees with it. An Icarus Verilog corpus runner exists but has to be driven by hand. See
  [Testing the Verilog subsystem](dev/verilogTesting.html) for what is covered today, and the three
  routes to closing the gap — automating the existing Icarus differential test of the input
  compiler, adding an Icarus check of the *emitter*, and making ISSIE's Verilog output readable
  again by its own Verilog input.
* Broader test coverage of the waveform simulator, the truth-table UI and wire routing, none of
  which the [test suite](https://github.com/tomcl/issie/blob/master/Tests/README.md) reaches today.
* Viewing a parameterised sheet *as a particular instance*, and parameterising memory sizes —
  see [Parameter System](parameterSystem.html).
* [Issue 506](https://github.com/tomcl/issie/issues/506) collects other possible changes.


<br>

## Acknowledgements

- **Marco Selvatici** for the 8K lines of base code written for his 3rd year BEng FYP at Imperial College London.
- **Edoardo Santi** for work improving ISSIE over Summer 2020 and creating the waveform simulator
- **High Level Programming 2020/21 cohort** for providing the base code of the new all-F# schematic editor AKA draw block
- **Jo Merrick** for work improving ISSIE for her 3rd year BEng FYP
- **High Level Programming 2021/22 cohort** for implementing a much enhanced schematic editor
- **All 2020/2021 1st year undergraduate students** of the EEE department, Imperial College London, for acting as excellent and unpaid beta-testers in their DECA module
- **Jason Zheng** for improving the waveform simulator for his 4th year MEng FYP
- **Aditya Deshpande** for creating the truth table simulation for his 4th year MEng FYP
- **Archontis Pantelopoulos** for creating the Verilog editor and compiler and improving ISSIE over Summer 2022
- **Petra Ratkai** and **Yujie Wang** for improving the Verilog compiler & ISSIE simulator respectively in 2022-23.
- **The High Level Programming 22/23 cohort** for adding many features to the schematic editor
- **Samuel Wang** for on-demand waveform creation and a software scrollbar to improve the waveform simulator over Summer 2024.
- **The High Level Programming 24/25 cohort** for new waveform selector and design sheet parameter mechanism
- **Dr Tom Clarke** (Imperial College London, EEE department) for running HLP and his continued work maintaining and improving ISSIE code throughout
<br><br>

