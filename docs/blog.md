# ISSIE Blog

## 4 July 2021

Issie came about when we were working out the coursework for a combined two-Term 1st year Digital Electronics and Computer Architecture University module. For
those interested it is [Elec40003](https://www.imperial.ac.uk/media/imperial-college/study/programme-specifications/eee/2021/ProgSpec-H600-BEng-Electrical-and-Electronic-Engineering-2020-21.pdf).
We wanted coursework for introductory Digital Electronics, that would scale to simple CPU design, with hierarchical block schematic based
design entry. We considered an HDL, and decided this was less intuitive and in any case the extra time learning an HDL could be better used. 
Schematics help in visualising signal flow and hierarchy - key concepts when
first learning digital Electronics. There is also a strong case for preferring block schematic representation for the
top-level structural levels of a design, even if the design is all written in an HDL.

It is now one year from when Issie was first created, and it has survived three terms of heavy use by 1st year undergraduates. The final working student designs,
implemented from scratch with no pre-built blocks,
were dual-core CPUs with cut-down ARM-like instruction set, UARTs, and IEEE 754 single precision FPUs. Designs were also output as verilog and
synthesised on FPGAs.

The [original design](https://github.com/MarcoSelvatici/DEflow) created by Marco Selvatici (3rd year EEE student) has stood quite well, with two major changes:

* **Simulation**. the original (very clever) recursive functional simulator proved to be incapable of correctly representing circuits with complex subsheets that
contain clocked and combinational logic. Also it was very slow. So we wrote, under pressure of student complex designs now working, a new simulator. 
This flattened the hierarchical design to a sea of Issie components and then determined a feasible fixed execution order for all components. It proved
robust and also muhc. muhc, faster than the original simulation. Both of these qualities were needed!
* **Drawing library**. Issie originally depended on Draw2D - a big and very sophisticated javascript block drawing library. The schematic editor used this and
added custom (Javascript) digital component symbols. However the wire auto-router in this library that we wanted to use proved to be buggy (it would crash) and it was not easy
to mend because the crash was an infinite loop through a lot of complex Draw2D repainting code. A completely new custom drawing library designed specifically for Issie
was written by 3rd year students in EEE, and integrated with Issie. This was debugged through Summer Term issue usage and is now muhc nicer than the original Draw2D
library. This rewrite has a lot of advantages - the application is now all F#, the draw library is written as a fully MVU (Elm-like) web-style application without internal state,
and the complete editor is now much faster than the old javascript one.

These changes and some other minor additions have pushed the code base from 8K lines F# to 17K lines. F# is a very concise language, so that is equivalent to at least double that in
most other high level languages.

Issie is run as an open source project but thus far all serious development has been internal to Imperial Collge London, from staff and students. We are now hoping that other people
will be interested!

