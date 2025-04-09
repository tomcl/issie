---
title: Home
category: Documentation
categoryindex: 1
index: 1
---

# Issie Documentation Home

See [Getting Started](gettingStarted.html) for how to put Issie on your laptop or develop Issie.

See [User Tutorial](userGuide.html) for a useful introduction to Issie on one page which you can follow or read.

<br>

## What is ISSIE?

* ISSIE is an **easy-to-use schematic editor and simulator** for hierarchical design of **digital logic circuits**. Run it and see the built-in demos for what it can do! ISSIE is targeted at 1st year university students, but would be useful teaching in schools and even for quick hardware design and test in an industrial or research environment. For the latter use case although we allow Verilog input and output these features need a bit more work to be as complete and nice as the rest of ISSIE.
* ISSIE was motivated because we found that industry-standard CAD systems were too complex and buggy to be learnt in labs, and when learnt still nasty to use. Other educational products were too limited. We wanted a system to teach hierarchical design and digital electronics visually that would scale to large designs.
   - We find that EEE-educated internal PhD candidates starting with schematic logic designs usually have a better understanding of what **digital hardware is** than those from other institutions starting with HDL.
* ISSIE has as a design principle that it should be easy to use by a novice, with all errors well highlighted. Error messages telling the user what the error is and how precisely to correct it, so a novice user can make the correction. Keeping to this principle has been challenging but worthwhile. We also wanted a complete system capable of real design work: good user interface is separate from large-scale features and performance and there is no reason they cannot both be implemented.
* ISSIE is developed **by undergraduate students and staff at Imperial College London**. The code is cross-platform and binaries are released for Windows and Silicon (Arm64) Mac and Linux platforms.
* ISSIE is implemented using nearly 50K lines in the Functional-first language F#, the equivalent of 150K lines in a typical OOP language. ISSIE's implementation is (almost) pure functional programming without assignment. This makes the codebase very maintainable! ISSIE has **its own digital simulator**, which uses **write-once semantics** on mutable (JS typed) arrays to combine the robustness of functional programming with high performance.
* ISSIE uses the [FABLE F# to Javascript compiler](https://fable.io/), [Elmish MVU framework](https://elmish.github.io/elmish/), and [Electron](https://www.electronjs.org/). This tool chain creates seamless cross-platform applications with simple and highly productive coding.
* ISSIE can generate Verilog output to drive FPGAs and has an integrated system to do this directly using **ISSIE-Stick** hardware.
* ISSIE can also accept Verilog source, using this to define and simulate equivalent schematic components.
* For acronym geeks, ISSIE stands for: *Interactive Schematic Simulator with Integrated Editor*.
  

<br>

## ISSIE Development Roadmap

#### 2024 
* Workaround unexpected & undocumented React 17 memory leak
* Better user interface to Waveform Simulator
* Longer simulation times keeping waveforms in memory
* Automatic whole-circuit wire segment separation for almost perfect auto-routing

#### 2025

* New Waveform Selector for waveform simulations (complete)
* Tooltips on waveform simulator waveforms (complete)
* Symbolic parameters to make component bit-widths etc adjustable (in progress)
* Improvements to Verilog parser and to-Issie synthesis (in progress)
* See [Issue 506](https://github.com/tomcl/issie/issues/506) for possible other changes.


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

