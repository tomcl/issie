---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: default
title: "Home" 
---

<!-- 
{% include module.html 
    image_path="img/catalog.png" 
    title="Width Inference"
    description="Wires will automatically be created the correct width using a width inference algorithm. Wires connecting two ports of incompatible widths will be highlighted red and an error message shown, as will multiply driven wires. Additional sanity checks (e.g. for combinational loops, or unconnected ports) are made before simulation with errors highlighted visually" 
%}
-->



# What is ISSIE?

**I**nteractive **S**chematic **S**imulator and **I**ntegrated **E**ditor **(ISSIE)** is a very easy to use application for digital circuit design and simulation. It is targeted at students and hobbyists that want to get a grasp of Digital Electronics concepts in a simple and fun way. Issie is designed to be beginner-friendly and guide the users toward their goals via clear error messages and visual clues. 

The motivation for Issie was the observation that during digital lab work in EEE, previously, more time was spent learning to use (or cope with subtle bugs in) large commercial hardware design tools, than was spent learning digital design. We wanted a tool as powerful as those commercial packages which was more productive.

* Issie is developed by 3rd and 4th year EEE students at Imperial College [Imperial College London](https://www.imperial.ac.uk/).
* Issie has been well tested on hierarchical designs with up to 15,000 schematic components. We expect it to be reasonably performant on much larger designs as well. The simulation speed is approximately 10,000 component-clocks per ms (on an 11th gen Intel low power laptop). Thus a circuit with 1000 components would run at 10,000 clocks per second. Issie creates fully synchronous circuits with a single clock: logic with asynchronous loops is currently not supported.
* Issie designs can be exported to Verilog and synthesised on FPGAs with a simple workflow.
* Design entry in an Hardware Design Language is on the Roadmap to be supported soon.
* The application is mostly written in F#, a functional programming language taught by the EEE department in the module **High Level Programming**, which gets transpiled to JavaScript via the [Fable](https://www.fable.com) compiler and the [Elmish](https://elmish.github.io/) web framework. [Electron](https://www.electronjs.org) is then used to convert the developed web-app to a cross-platform application.

<br><br>

# Key Features

{% include module.html 
    image_path = "img/catalogue.gif" 
    title = "Component Library"
    description = "ISSIE has an extensive and library of schematic components available in the 'Catalogue' menu. Components include low-level gates, flipflops, and multiplexers, as well as larger blocks: RAMs, ROMs, n-bit registers and adders. The lack of HDL-based combinational logic is currently filled by special components: Bus Select (extracts a bit-field), Bus Compare (decodes a min-term of a bus). More complex functions can quickly be constructed as sub-sheets and then used as any other component.
    <br><br>
    Viewer components are used to (optionally) view simulation waveforms of nodes on sub-sheets. Wire label components allow any number of nodes on one design sheet to be connected without visible wires.
    <br><br> 
    Under 'THIS PROJECT' you can find all other design sheets of your project as custom components which can be added to your current sheet and replicate the sheet logic." 
%}

<br><br>

{% include m0odule.html 
    image_path="img/wireRouting.gif" 
    title="Wire Routing"
    description="ISSIE schematic component ports are connected using drag-and-drop: each connection represents a wire or bus. ISSIE has two methods of routing wires: **auto-routing** and **manual-routing**. 
    <br><br>
    Wires will all start out as auto-routed, which means that the wire’s path is created automatically by the program. This path will update when moving any connected components. ISSIE also allows for manual routing, where the user may manipulate segments of the wire as desired to make the circuit more readable. Much care has been put into a user interface for routing which *just works* quickly with no learning curve." 
%}

<br><br>

{% include module.html 
    image_path="img/canvasCustomisation.gif" 
    title="Canvas Customisation"
    description="The ISSIE canvas is fully customisable to allow the creation of readable and good-looking schematics. **Specifically:** <br><br> (a) Rotate, flip and Move all symbols <br> (b) Change and move around the symbols' labels <br> (c) Manually route wires as you like <br> (d) Auto-align elements <br> (e) Select the wire type you desire (radiussed, jump or modern wires)"
%}

<br><br>

{% include module.html 
    image_path="img/widthError.gif" 
    title="Width Inference"
    description="When components are joined wires will automatically form busses of the correct number of bits using a width inference algorithm. Wires connecting two ports of incompatible widths are be highlighted red and an error message shown, as are multiply driven wires. Additional sanity checks (e.g. for combinational loops, or unconnected ports) are made before simulation with errors highlighted visually. These errors are presented with clear messages so that correcting them is obvious and very quick." 
%}

<br><br>


{% include module.html 
    image_path="img/stepSimulation.gif" 
    
    title="Step Simulation"
    
    description="Step Simulation allows the user to 'step' or cycle through each clock tick, and view the current design sheet's Output and Viewer component information. It also allows users to view how the state changes in stateful components such as RAM." 
%}

<br><br>

{% include module.html 
    image_path="img/waveSimulation.gif" 
    title="Waveform Simulation"
    description="Waveform Simulation allows the user to see the values in each selected set of connected wires (net) over time as a waveform. The waveform simulator allows users to move waveforms up/down in the list to make the simulation more readable. Users may cycle through the simulator using the arrows to directly go to a specific clock cycle. Alternatively there is a scrollbar for the Waveform simulator that will automatically lengthen the simulation once the end is reached. The values in the waveform simulator can be viewed in various formats: binary, hexadecimal, unsigned decimal and signed decimal. The Waveform Simulator also features a draggable sidebar to partition screen space dynamically between waveforms and circuit.
    <br><br>

    Waveform Simulation also allows for the simulation and contents viewing of memory components such as RAM.
    " 
%}

<br><br>

{% include module.html 
    image_path="img/verilogOutput.png" 
    title="Verilog Output"
    description="Users may convert their ISSIE schematic design into a Verilog file using the 'Write design as Verilog' option found in the header bar of the application. This allows great flexibility as ISSIE designs may be used in more complex design tools and other programs that use Verilog; allowing ISSIE to be used as a top-level design that can be further developed if needed. Verilog output for simulation or synthesis is documented as part of the Verilog write process, this includes links to a [YoSys](http://bygone.clairexen.net/yosys/download.html) workflow for synthesis on FPGAs. Imperial College users can download a pre-installed VM for this workflow, the VHDL output is standalone and should work with other synthesis methods." 
%}

<br><br>

{% include module.html 
    image_path="img/memoryEditor.png" 
    title="Memory Editor"
    description="ISSIE allows users to directly edit the contents of Memory components, for more versatility and ease of use. Memory contents can also be exported and imported via .ram files.
    " 
%}


<br><br>

# Running Issie

The download button on this page take you to the [Issie latest release page](https://github.com/tomcl/issie/releases). Scroll down the top release on this page till you get the the `Assets` section - this has binaries for windows and macos PCs. Download the appropriate one and unzip it anywhere. No installation is required - Issie runs from the unzipped files under windows if you double-click the top-level `Issie.exe` file with the blue Issie chip icon. For more information see [Getting Started](https://github.com/tomcl/ISSIE#getting-started) or read the [User Guide](https://tomcl.github.io/issie/user-guide/).

# Acknowledgements

- Marco Selvatici for the 8K lines of base code written for his 3rd year BEng FYP
- Edoardo Santi for work improving Issie over Summer 2020 and creating the waveform simulator
- High Level Programming 2020/21 cohort for providing the base code of the schematic editor
- Jo Merrick for work improving ISSIE for her 3rd year BEng FYP
- High Level Programming 2021/22 cohort for implementing a much enhanced schematic editor
- Dr Tom Clarke for running HLP and his continued work maintaining and improving ISSIE throughout
- All 2020/2021 1st year undergraduate students of the EEE department, Imperial College London, for acting as excellent and unpaid beta-testers in their DECA module!

<br><br>




# Contact

If you encounter any problems using or downloading the software, please see the [Gihub Issue](https://github.com/tomcl/issie/issues) page, or [create a new issue](https://github.com/tomcl/issie/issues/new) on the ISSIE GitHub repository. Any feedback and suggestions are also welcome!