---
layout: home
title: "Home" 

carousels:
  - images:
    - [image: img/homePage/keyFeatures.gif, 
      description: "ISSIE has an extensive library of schematic components available in the 'Catalogue' menu. Components include low-level gates, flipflops, and multiplexers, as well as larger blocks: RAMs, ROMs, configurable n-bit registers, counters and adders. Viewer components are used to (optionally) view simulation waveforms of nodes on sub-sheets. Wire label components allow any number of nodes on one design sheet to be connected without visible wires. More complex functions can quickly be constructed as sub-sheets and then used as a 'custom component' (found under 'THIS PROJECT'). Custom components can have shape and I/O positions altered at any time via an intuitive and fast drag and drop GUI", 
        title: "Hierarcical Design with Schematic Components"]
    - [image: img/homePage/wireRouting.gif, 
        description: "ISSIE schematic component ports are connected using drag-and-drop: each connection represents a wire or bus. ISSIE has two methods of routing wires: <b>auto-routing</b> and <b>manual-routing</b>. 
        <br><br>
        Wires will all start out as auto-routed, which means that the wire’s path is created automatically by the program. This path will update when moving any connected components. ISSIE also allows for manual routing, where the user may manipulate segments of the wire as desired to make the circuit more readable. Much care has been put into a user interface for routing which <i>just works</i> quickly with no learning curve.", 
        title: "Wire Routing"]
    - [image: img/userGuide/features2.gif, 
        description: "The ISSIE canvas is fully customisable to allow the creation of readable and good-looking schematics. <b>Specifically:</b> <br><br> (a) Rotate, flip and Move all symbols <br> (b) Change and move around the symbols' labels <br> (c) Manually route wires as you like <br> (d) Auto-align elements <br> (e) Select the wire type you desire (radiussed, jump or modern wires)", 
        title: "Canvas Customization"]
    - [image: img/homePage/verilogComp.PNG, 
        description: "ISSIE allows users to create combinational components by defining their logic in Verilog. Such component can be used as a Custom Component in all designs.
        <br> <br>
        For more information see the <a href=\"/issie/verilog-comp/\">Verilog Component page</a>", 
        title: "Verilog Component"]
    - [image: img/homePage/stepSim.gif, 
        description: "Step Simulation allows the user to 'step' or cycle through each clock tick, and view the current design sheet's Output and Viewer component information. It also allows users to view how the state changes in stateful components such as RAM.", 
        title: "Step Simulation"]
    - [image: img/homePage/waveSim.gif, 
        description: "Waveform Simulation allows the user to see the values in each selected set of connected wires (net) over time as a waveform. The waveform simulator uses a drag-and-drop GUI to delete or reorder waveforms, and a separate project explorer window to add them. Hovering on a waveform name highlights its component and all connected busses on its design sheet. Any design sheet may be viewed or edited and the simulation refreshed to see changes immediately. The values in the waveform simulator can be viewed in various formats: binary, hexadecimal, unsigned decimal and signed decimal. The Waveform Simulator uses a draggable sidebar to partition screen space dynamically between waveforms and circuit.
        <br><br>

        Waveform Simulation also allows for the simulation and contents viewing of memory components such as RAM.", 
        title: "Waveform Simulation"]
    - [image: img/homePage/truthTable.png, 
        description: "ISSIE allows users to view the truth table for a selected circuit of combinational logic. This can be either the full truth table or a reduced one by denoting all Don't Cares with 'X's. 
        <br> <br>
        Furthermore, users can set any number of inputs as algebra. The resultant truth table will show outputs as a function of the inputs.", 
        title: "Truth Table"]
    - [image: img/homePage/verilogOutput.png, 
        description: "Users may convert their ISSIE schematic design into a Verilog file using the \"Write design as Verilog\" option found in the header bar of the application. This allows great flexibility as ISSIE designs may be used in more complex design tools and other programs that use Verilog; allowing ISSIE to be used as a top-level design that can be further developed if needed. Verilog output for simulation or synthesis is documented as part of the Verilog write process, this includes links to a <a href=\"http://bygone.clairexen.net/yosys/download.html\">YoSys</a> workflow for synthesis on FPGAs. Imperial College users can download a pre-installed VM for this workflow, the VHDL output is standalone and should work with other synthesis methods", 
        title: "Verilog Output"]
    - [image: img/homePage/memoryEditor.png, 
        description: "ISSIE allows users to directly edit the contents of Memory components, for more versatility and ease of use. Memory contents can also be exported and imported via .ram files", 
        title: "Memory Editor"]

---

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