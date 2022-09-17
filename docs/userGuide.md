---
layout: page
title: "User Guide"
permalink: /user-guide/
---

## Contents
{:.no_toc}

- TOC
{:toc}

## Getting Started

### Downloading and Running ISSIE
{:.no_toc}

Start by clicking the [download](https://github.com/tomcl/issie/releases/latest) button on the top-right of your screen. This opens the page of the latest release of ISSIE on GitHub. At the bottom of the page, you can find the latest prebuilt binary for your platform (Windows or Macos). Issie will require in total about 200M of disk space.

- **Windows:** unzip \*.zip anywhere and double-click the top-level `Issie.exe` application in the unzipped files.
- **Macos:** Double click the dmg file  and run the application inside the folder, or drag and drop this to install.
    - The binaries are not signed. You will need to [perform a one-off security bypass](https://www.wikihow.com/Install-Software-from-Unsigned-Developers-on-a-Mac).
    

### Creating a New Project
{:.no_toc}

Once you open Issie you should see two options: `New Project` and `Open Project`. 
  
- Click `New Project`
- Navigate to the folder you want to save your project
- Enter the name of your project
- Click `Create Project`

This process creates a folder where your project will be stored and the first sheet of your project, called `main`. You can see this by clicking at the `Sheets` selection button.

### Your first design
{:.no_toc}

Let's start with a very simple schematic: a simple 2-input AND gate. 

Add the following components to your canvas from the `Catalogue` tab:
- `INPUT/OUTPUT` => `Input` => Name: 'A', Bits: 1
- `INPUT/OUTPUT` => `Input` => Name: 'B', Bits: 1
- `GATES` => `And` 
- `INPUT/OUTPUT` => `Output` => Name: 'OUT', Bits: 1

Now make the appropriate wiring to connect all the components by clicking on one port and dragging the wire to the port you want to connect it to. 
**Connect:**
- Input 'A' to the first input port of the AND gate
- Input 'B' to the second input port of the AND gate
- Output 'OUT' to the output port of the AND gate

Your design should look like this:

![](../img/userGuide/firstDesign.gif)


### Simulation
{:.no_toc}

Time to simulate our design and see how the output `OUT` changes as we change the two inputs.

Click the `Simulation` tab which is located on the top-right corner and then `Start Simulation`. Now you can change the value of the two inputs and see how the value of the output. Try all 4 combinations of inputs: 
- A=0, B=0  
- A=0, B=1  
- A=1, B=0  
- A=1, B=1 
   
and check that the output is correct based on the truth table of the AND gate.

![](../img/userGuide/firstDesignSim.gif)


**Well Done!** You just completed your first ISSIE design.  

## Exploiting the ISSIE Features

### A slightly more complex design
{:.no_toc}

Time to increase the complexity of our design and see how we can exploit the features of ISSIE to create clean and good-looking schematics.

- Add two more inputs named `C` and `D` each 1-bit.
- Add one OR gate and one 2-input MUX
- Delete the output `OUT`
  - Note: You can delete components and/or wires by selecting them and clicking the `delete` button on your keyboard
- Add a new 1-bit output `RESULT`   
- Make all necessary connections to achieve a diagram like the one bellow:

![](../img/userGuide/features1.png)

Again, **simulate the design** and check the output remains correct as you change the values of the 4 inputs

### Improving the looking of our design
{:.no_toc}

Clearly, this is a terrible and hard to understand design. **Let's improve it!** The ISSIE canvas is fully customisable to allow the creation of readable and good-looking schematics. Specifically, we can:
1. Rotate, Flip and Move around all symbols 
2. Change and Move around the symbols' labels
3. Manually route wires as you like
4. Auto-align elements 
5. Select the wire type we desire (radiussed, jump or modern wires)

You can view the shortcuts for all these modifications by clicking on the `edit` and `view` menus.

**Let's now look at our improved schematic:**

![](../img/userGuide/features2.gif)


### Summary
{:.no_toc}

- In the `Catalogue` Menu we can find an extensive and complete library of components (gates, flip-flops, RAMs, ROMs, n-bit registers)
- We can add any number of components in our sheet and name them as we like
- When clicking on a port, ISSIE shows us all the ports we can connect that port to. 
- Wires are initially automatically routed
- We can modify our schematic as we like to create a good-looking design.
- We can simulate our design and check how the outputs change as we change the inputs.


## Using Custom Components

### Your root schematic
{:.no_toc}

Time now to learn how to use or schematics as cstom components in other design sheets. Here is the idea: The very simple and theoretically useless design we created earlier can be used as a decoder of a 4-bit message to produce a true/false result. Therefore, we are going to create a schematic with an asynchronous-read 4-bit ROM and the schematic we created before as a custom symbol. 

Steps: 
1. Change the name of the current sheet from `main` to `decoder` (Sheets -> rename)
2. Add a new sheet and name it `main`
3. Add to the main sheet:
  - Asynchronous ROM (`MEMORIES` => `ROM (asynchronous)`). Select 4 bits addressor, 4 bits data and the `Enter data later` option
  - Your decoder (`THIS PROJECT` => `decoder`)
  - 1-bit output named 'RESULT' (`INPUT/OUTPUT` => `Output`) 
  - 4-bit input named 'Addressor' (`INPUT/OUTPUT` => `Input`) 
4. Using 3 `SplitWire` components (`BUSES` => `SplitWire`) separate the 4-bit ROM output to 4 1-bit wires. (see image below)
5. Make the appropriate connections to achieve the schematic below

![](../img/userGuide/custom.png)

### Improving the design sheet
{:.no_toc}

It's time to use another cool feature of Issie: Moving ports in custom components. Issie allows you to re-order and change the side of input and output ports of custom components by `CTRL` + `CLICKING ON THE PORT` you want to move.

Let's look how it works in the gif below:

![](../img/userGuide/custom2.gif)


### ROM Initialisation
{:.no_toc}

Currently our ROM is empty as we selected the option `Enter Data Later` before. Let's put some values in our ROM.

1. Select the ROM and click on the `Properties` tab
2. Click on `view/edit memory content`
3. Change the content of the 16 memory location available by assigning a random 4-bit number to each one
4. Click `done`

### Simulation
{:.no_toc}

Simulate your design! Change the value of the addressor input and see whether your decoder produces a true or false result for each number you assigned to the ROM. 

## Waveform Simulation

### Creating a closed loop design
{:.no_toc}

Let's now make our top-level design a closed-loop one using a custom addressor which will increment every clock cycle. Now, using the waveform simulator we will be able to view the output of our circuit for all memory locations. In order to create such designs easily, ISSIE offers a `Counter` component which starting from 0, it increments by one every clock cycle.

Add a `Counter` from the Catalogue (`FLIP FLOPS AND REGISTERS`). Now select the component and click on `Properties`. You can select to remove the `load` and `enable` ports and give them the default functionality (which is what we want in this case): enable=1; load=0;

Create a schematic like the one below:

![](../img/userGuide/waveform.png)

### Simulating your design
{:.no_toc}

As soon as you connect everything correctly, You can simulate your design. Click on `Simulations` and then `Wave Simulation`.

- Click the `Start Simulation` button
- Click `Select Waves`
- Select:
  - `AROM1.Dout[3:0]`
  - `REG1.Dout[3:0]`
  - `RESULT: DECODER1.RESULT`
- Click `Done`
- Change the data to either `hex` or `bin` to make them more readable
- Check that the waveform simulator output matches your previous (Step Simulation) results 

![](../img/userGuide/waveform1.png)


### Changing your design
{:.no_toc}

Now add an extra register between the counter and the ROM address (or make any other change you want) and check that the simulation has the expected output. You can see the changes in the waveform simulator simply by clicking the `refresh` button which will be enabled as soon as it detects a change in the schematic. 


![](../img/userGuide/waveform2.png)

## Truth Table

One of ISSIE's features is the ability to view the truth table for a combinational circuit. 

- Click on `Simulations` and then `Truth Table`
- Select the `DECODER` component
- Click on `Generate Truth Table` button on the 'Truth Table for selected logic' section
- Click on `Remove Redundancies`
- Thuth table should look like this:

![](../img/userGuide/truthTable1.png)


You can also select your inputs to be algebraic values to get an expression for each of your outputs.

- Click on `Back to full table`
- Click on `Algebra`
- Select the inputs you want to be algebraic values
- Truth table should now look like this:

![](../img/userGuide/truthTable2.png)

## Verilog Component

Last but not least, ISSIE allows you to create combinational custom components by defining the logic in Verilog. Click on `Verilog` -> `New Verilog Component` (Catalogue) and write the logic of your decoder in Verilog.

![](../img/userGuide/verilog1.png)

- Click `Save`
- Replace the previous Decoder with the new one (found under `Verilog` section in the Catalogue)
- Simulate again your design. Everything should be the same as before.

## Now what?

You now know how to use ISSIE to create & simulate digital designs. 

You can now create your designs (from simple circuits to fully functionable CPUs) and either simulate them or extract them as Verilog to use them with other tools.