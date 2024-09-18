# What is ISSIE?

* ISSIE is **a very easy-to-use schematic editor and simulator** for hierarchical design of **digital logic circuits**. Run it and see the built-in demos for what it can do! ISSIE is targeted at 1st year university students, but would be useful teaching in schools and even for quick hardware design and test in an industrial or research environment. For the latter use case although we allow Verilog input and output these features need a bit more work.
* ISSIE was motivated because we found that industry-standard CAD systems were too complex and buggy to be learnt in labs, and when learnt still nasty to use. Other educational products were too limited. We wanted **a schematic-based system to teach hierarchical design and digital electronics visually that would scale to large designs**.
* ISSIE is an **open source project at Imperial College London developed over 5 years by students**. The code is cross-platform and binaries are released for Windows and Silicon (Arm64) Mac targets.
* ISSIE is implemented using nearly 46K lines of F#, the equivalent of 150K lines in a typical OOP language. ISSIE has **its own digital simulator**.
* ISSIE can generate Verilog output to drive FPGAs and has an integrated system to do this directly using the **ISSIE-Stick** hardware.
* For acronym geeks, ISSIE stands for: *Interactive Schematic Simulator with Integrated Editor*.


# Running ISSIE

Go to the [latest ISSIE release](https://github.com/tomcl/issie/releases/latest). Scroll down this page till at the bottom you find the `Assets` section - this has binaries for Windows and Macos PCs. Download the appropriate one and unzip it anywhere (or add the dmg file to applications under Macos by double-clicking). No installation is required - ISSIE runs from the unzipped files under windows if you double-click the top-level `ISSIE.exe` file with the blue ISSIE chip icon. 

# Help and User Guides

For more setup information see the  [Getting Started](https://github.com/tomcl/ISSIE#getting-started) section of the repo README.

### A Beginners Tutorial On ISSIE

Go through the  [User Guide](https://tomcl.github.io/issie/user-guide/).

### ISSIE Under The Hood for Developers

The ISSIE [wiki](https://github.com/tomcl/issie/wiki) has a large amount of information on the details of how ISSIE is designed, referencing a few student project reports with (a lot) of detail and background.

### ISSIE Code documentation (auto-generated)

F# XML documentation on the [ISSIE API](https://tomcl.github.io/issie/reference/index.html)


<br><br>

# Acknowledgements

- Marco Selvatici for the 8K lines of base code written for his 3rd year BEng FYP at Imperial College London.
- Edoardo Santi for work improving ISSIE over Summer 2020 and creating the waveform simulator
- High Level Programming 2020/21 cohort for providing the base code of the new schematic editor
- Jo Merrick for work improving ISSIE for her 3rd year BEng FYP
- High Level Programming 2021/22 cohort for implementing a much enhanced schematic editor
- All 2020/2021 1st year undergraduate students of the EEE department, Imperial College London, for acting as excellent and unpaid beta-testers in their DECA module!
- Jason Zheng for improving the waveform simulator for his 4th year MEng FYP
- Aditya Deshpande for creating the truth table for his 4th year MEng FYP
- Archontis Pantelopoulos for creating the Verilog Component and improving ISSIE over Summer 2022
- Petra Ratkai and Yujie Wang for improving the Verilog compiler & ISSIE simulator in 2022-23.
- Samuel Wang for on-demand waveform creation and a software scrollbar to improve the waveform simulator over Summer 2024.
- Dr Tom Clarke (Imperial College London, EEE department) for running HLP and his continued work maintaining and improving the 46K lines of ISSIE code throughout
<br><br>


# Contact

If you encounter any problems using or downloading the software, please see the [Gihub Issue](https://github.com/tomcl/issie/issues) page, or [create a new issue](https://github.com/tomcl/issie/issues/new) on the ISSIE GitHub repository. Any feedback and suggestions are also welcome - we keep feature request issues and usually manage to implement them!

if you want to use ISSIE in an educational context contact Dr. Tom Clarke, [Department of Electronic & Electrical Engineering, Imperial College London](https://www.imperial.ac.uk/electrical-engineering).

<br><br>