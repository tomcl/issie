---
layout: home
title: "Home" 

carousels:
  - images:
    - [image: img/homePage/catalogue.png, 
      description: "ISSIE has an extensive and library of schematic components available in the 'Catalogue' menu. Components include low-level gates, flipflops, and multiplexers, as well as larger blocks: RAMs, ROMs, n-bit registers and adders. Viewer components are used to (optionally) view simulation waveforms of nodes on sub-sheets. Wire label components allow any number of nodes on one design sheet to be connected without visible wires. More complex functions can quickly be constructed as sub-sheets and then used as any other component (found under \"THIS PROJECT\")", 
        title: "Component Library"]
    - [image: img/homePage/wireRouting.gif, 
        description: "ISSIE schematic component ports are connected using drag-and-drop: each connection represents a wire or bus. ISSIE has two methods of routing wires: <b>auto-routing</b> and <b>manual-routing</b>. 
        <br><br>
        Wires will all start out as auto-routed, which means that the wire’s path is created automatically by the program. This path will update when moving any connected components. ISSIE also allows for manual routing, where the user may manipulate segments of the wire as desired to make the circuit more readable. Much care has been put into a user interface for routing which <i>just works</i> quickly with no learning curve.", 
        title: "Wire Routing"]
    - [image: img/homePage/UserGuideNew/features2.gif, 
        description: "The ISSIE canvas is fully customisable to allow the creation of readable and good-looking schematics. <b>Specifically:</b> <br><br> (a) Rotate, flip and Move all symbols <br> (b) Change and move around the symbols' labels <br> (c) Manually route wires as you like <br> (d) Auto-align elements <br> (e) Select the wire type you desire (radiussed, jump or modern wires)", 
        title: "Canvas Customization"]
    - [image: img/homePage/verilogComp.png, 
        description: "ISSIE allows users to create combinational components by defining their logic in Verilog. Such component can be used as a Custom Component in all designs.
        <br> <br>
        For more information see the <a href=\"/issie/verilog-comp/\">Verilog Component page</a>", 
        title: "Verilog Component"]
    - [image: img/homePage/stepSim.gif, 
        description: "Step Simulation allows the user to 'step' or cycle through each clock tick, and view the current design sheet's Output and Viewer component information. It also allows users to view how the state changes in stateful components such as RAM.", 
        title: "Step Simulation"]
    - [image: img/homePage/waveSim.gif, 
        description: "Waveform Simulation allows the user to see the values in each selected set of connected wires (net) over time as a waveform. The waveform simulator allows users to move waveforms up/down in the list to make the simulation more readable. Users may cycle through the simulator using the arrows to directly go to a specific clock cycle. Alternatively there is a scrollbar for the Waveform simulator that will automatically lengthen the simulation once the end is reached. The values in the waveform simulator can be viewed in various formats: binary, hexadecimal, unsigned decimal and signed decimal. The Waveform Simulator also features a draggable sidebar to partition screen space dynamically between waveforms and circuit.
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



# Running Issie

The download button on this page take you to the [Issie latest release page](https://github.com/tomcl/issie/releases). Scroll down the top release on this page till you get the the `Assets` section - this has binaries for windows and macos PCs. Download the appropriate one and unzip it anywhere. No installation is required - Issie runs from the unzipped files under windows if you double-click the top-level `Issie.exe` file with the blue Issie chip icon. For more information see [Getting Started](https://github.com/tomcl/ISSIE#getting-started) or read the [User Guide](https://tomcl.github.io/issie/user-guide/).

<br><br>

# Acknowledgements

- Marco Selvatici for the 8K lines of base code written for his 3rd year BEng FYP
- Edoardo Santi for work improving Issie over Summer 2020 and creating the waveform simulator
- High Level Programming 2020/21 cohort for providing the base code of the schematic editor
- Jo Merrick for work improving ISSIE for her 3rd year BEng FYP
- High Level Programming 2021/22 cohort for implementing a much enhanced schematic editor
- Dr Tom Clarke for running HLP and his continued work maintaining and improving ISSIE throughout
- All 2020/2021 1st year undergraduate students of the EEE department, Imperial College London, for acting as excellent and unpaid beta-testers in their DECA module!
- Jason Zheng for improving the waveform simulator for his 4th year MEng FYP
- Aditya Deshpande for creating the truth table for his 4th year MEng FYP
- Archontis Pantelopoulos for creating the Verilog Component and improving ISSIE over Summer 2022
<br><br>


# Contact

If you encounter any problems using or downloading the software, please see the [Gihub Issue](https://github.com/tomcl/issie/issues) page, or [create a new issue](https://github.com/tomcl/issie/issues/new) on the ISSIE GitHub repository. Any feedback and suggestions are also welcome!

<br><br>