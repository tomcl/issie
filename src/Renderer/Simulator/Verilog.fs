﻿module Verilog

open CommonTypes
open SimulatorTypes
open SynchronousUtils
open EEExtensions
open FastRun
open Helpers
open NumberHelpers

type VMode = ForSynthesis | ForSimulation


/// take FullName and convert it into a verilog compatible form
/// this is not 1-1, so outputs may not be unique, that is OK
let verilogNameConvert (maxChars:int) (s: string) =
    let maxIdentifierLength = 50

    let baseName =
        EEExtensions.String.split [| '(' |] s
        |> Array.toList
        |> function
        | h :: _ -> h
        | [] -> "x"
        |> Seq.map (function | ch when System.Char.IsLetterOrDigit ch || ch = '_' -> string ch | _ -> "")
        |> Seq.truncate maxChars
        |> String.concat ""


    let extraLength = baseName.Length - maxIdentifierLength

    if extraLength > 0 then
        baseName[extraLength..]
        |> Seq.map string
        |> string
    else
        baseName

/// simple way to assign to each component and component output a unique verilog compatible name.
/// outputs will become reg or wire signals in the Verilog
let writeVerilogNames (fs: FastSimulation) =
    let getShortPath (path: ComponentId list) : string =
        path
        |> List.map (fun (ComponentId cid) -> cid)
        |> (function | (t) -> t)
        |> List.map (verilogNameConvert 1)
        |> String.concat ""

    /// generate from a component a maybe non-unique name made from its Label and abbreviated path
    let getBaseVerilogName fc =
        let sc = fc.SimComponent
        let fakeName s = $"%s{s}{String.substringLength 0 2 (match sc.Id with | ComponentId s -> s)}"
        let cLabel =
            match sc.Label , sc.Type with
            | ComponentLabel "", SplitWire _ -> fakeName "Split"
            | ComponentLabel "", MergeWires ->  fakeName "Merge"
            | ComponentLabel "", _ -> fakeName "Other"
            | ComponentLabel lab,_ -> lab.ToUpper()

        match fc.fId with
        | (_,[]) -> verilogNameConvert 20 cLabel
        | (_,path) -> verilogNameConvert 20 cLabel + "$" + getShortPath path
 
    // keep array of components and base names in well defined order
    let namesWithFC = 
        fs.FComps
        |> Map.toArray
        |> Array.sortBy (fun (fid,_) -> fid)
        |> Array.map (fun (fid, fc) ->
            getBaseVerilogName fc, fc)
    /// if the set of names is not distinct add suffixes as needed to make it so
    /// recursive to deal with unusual case where adding a suffix causes another clash
    let rec disambiguate names: (string * FastComponent) array =
        if Array.length (Array.distinctBy fst names) = names.Length
        then
            names
        else
            names
            |> Array.groupBy fst
            |> Array.collect (fun (name, groupA) -> 
                match groupA.Length with
                | 1 -> groupA // if length 1 => unique and no suffix needed
                | _ -> Array.mapi (fun i (vName,fc) -> $"{vName}${i}",fc) groupA)
            |> disambiguate

    // write verilog names
    disambiguate namesWithFC
    |> Array.iter (fun (name, fc) -> 
        fc.VerilogComponentName <- name
        fc.VerilogOutputName
        |> Array.iteri
            (fun portNum _ ->
                let suffix = 
                    if fc.VerilogOutputName.Length = 1 then 
                        "" 
                    else 
                        $"$o{portNum}"
                let outName = $"{fc.VerilogComponentName}{suffix}"
                fc.VerilogOutputName[portNum] <- outName))

 
        

let makeAsyncRomModule (moduleName: string) (mem: Memory1) =
    let aMax = mem.AddressWidth - 1
    let dMax = mem.WordWidth - 1
    let numWords = 1 <<< mem.AddressWidth

    let romInits =
        mem.Data
        |> Map.toArray
        |> Array.map (fun (a, d) -> sprintf $"rom[%d{a}] = %d{d};")
        |> String.concat "\n"

    sprintf
        $"""

    module %s{moduleName}(q, a);
    output[%d{dMax}:0] q;
    input [%d{aMax}:0] a;
    reg [%d{dMax}:0] rom [%d{numWords - 1}:0];

    assign q = rom[a];
    integer i;
    initial
    begin
        for (i=0; i < {numWords}; i=i+1)
        begin
            rom[i] = 0;
        end
    
        %s{romInits}
    end
    endmodule
     """

let makeRomModule (moduleName: string) (mem: Memory1) =
    let aMax = mem.AddressWidth - 1
    let dMax = mem.WordWidth - 1
    let numWords = 1 <<< mem.AddressWidth

    let romInits =
        mem.Data
        |> Map.toArray
        |> Array.map (fun (a, d) -> sprintf $"rom[%d{a}] = %d{d};")
        |> String.concat "\n"

    sprintf
        $"""

    module %s{moduleName}(q, a, clk);
    output reg [%d{dMax}:0] q;
    input clk;
    input [%d{aMax}:0] a;
    reg [%d{dMax}:0] rom [%d{numWords - 1}:0];
    always @(posedge clk) q <= rom[a];
    integer i;
    initial
    begin
        for (i=0; i < {numWords}; i=i+1)
        begin
            rom[i] = 0;
        end
    
        %s{romInits}
    end
    endmodule
     """

let makeRamModule (moduleName: string) (mem: Memory1) =
    let aMax = mem.AddressWidth - 1
    let dMax = mem.WordWidth - 1
    let numWords = 1u <<< mem.AddressWidth

    let ramInits =
        mem.Data
        |> Map.toArray
        |> (Array.map (fun (a, d) -> sprintf $"ram[%d{a}] = %d{d};"))
        |> String.concat "\n"

    sprintf
        $"""

    module %s{moduleName}(q, a, d, we, clk);
    output reg [%d{dMax}:0] q;
    input [%d{dMax}:0] d;
    input [%d{aMax}:0] a;
    input we, clk;
    reg [%d{dMax}:0] ram [%d{numWords - 1u}:0];
     always @(posedge clk) begin
         if (we)
             ram[a] <= d;
         q <= ram[a];
     end

    integer i;
    initial
    begin
        for (i=0; i < {numWords}; i=i+1)
        begin
            ram[i] = 0;
        end

        %s{ramInits}
    end
    endmodule

    """

let makeAsyncRamModule (moduleName: string) (mem: Memory1) =
    let aMax = mem.AddressWidth - 1
    let dMax = mem.WordWidth - 1
    let numWords = 1u <<< mem.AddressWidth

    let ramInits =
        mem.Data
        |> Map.toArray
        |> (Array.map (fun (a, d) -> sprintf $"ram[%d{a}] = %d{d};"))
        |> String.concat "\n"

    sprintf
        $"""

    module %s{moduleName}(q, a, d, we, clk);
    output reg [%d{dMax}:0];
    output q [%d{dMax}:0];
    input [%d{dMax}:0] d;
    input [%d{aMax}:0] a;
    input we, clk;
    reg [%d{dMax}:0] ram [%d{numWords - 1u}:0];
     always @(posedge clk) begin
         if (we)
             ram[a] <= d;
     end
    q <= ram[a];


    integer i;
    initial
    begin
        for (i=0; i < {numWords}; i=i+1)
        begin
            ram[i] = 0;
        end

        %s{ramInits}
    end
    endmodule

    """

/// get all the RAM and ROM modules used
/// NB at the moment each instance is made a separately named module, for simplicity
let getInstantiatedModules (fs: FastSimulation) =
    fs.FComps
    |> Map.toArray
    |> Array.collect
        (fun (fid, fc) ->
            let name = fc.VerilogComponentName

            match fc.FType with
            | RAM1 mem -> [| makeRamModule name mem |]
            | AsyncRAM1 mem -> [| makeAsyncRamModule name mem |]
            | ROM1 mem -> [| makeRomModule name mem |]
            | AsyncROM1 mem -> [| makeAsyncRomModule name mem |]
            | _ -> [||])

let removeHybridComps (fa: FastComponent array) =
    Array.filter (fun fc -> not (isHybridComponent fc.FType)) fa

let activeComps (fs: FastSimulation) =
    [ fs.FClockedComps; removeHybridComps fs.FOrderedComps ]
    |> Array.concat

let makeAccessPathIndex (fs: FastSimulation) =
    let apArr =
        Array.append
            [| [] |]
            (activeComps fs
             |> Array.map (fun fc -> fc.AccessPath))

    apArr
    |> Array.distinct
    |> Array.sortBy (fun ap -> List.length ap)
    |> Array.indexed
    |> Array.map (fun (index, ap) -> ap, index)
    |> Map.ofArray




/// generate an instance of a module named block
let getInstanceOf (block: string) (instanceName: string) (ports: string array) =
    let portNames = ports |> String.concat ","
    sprintf $"%s{block} %s{instanceName} (%s{portNames});\n"

/// implement binary operator for two-input gate
let getVerilogBinaryOp cType op1 op2 =
    let bin opS = sprintf "%s %s %s" op1 opS op2
    let not exp = sprintf "!(%s)" exp

    match cType with
    | And -> bin "&&"
    | Or -> bin "||"
    | Nand -> not <| bin "&&"
    | Nor -> not <| bin "||"
    | Xor -> sprintf "((%s && !%s) || (!%s) && %s)" op1 op2 op1 op2
    | Xnor -> sprintf "!((%s && !%s) || (!%s) && %s)" op1 op2 op1 op2
    | _ -> failwithf "operator %A not defined" cType

/// get valid Verilog constant for bus of given width (may be 1)
let makeBits w (c: uint64) = 
    let c = c &&& ((1UL <<< w) - 1UL)
    sprintf $"%d{w}'h%x{c}"

/// get output port name
let getVPortOut (fc: FastComponent) (OutputPortNumber opn) = fc.VerilogOutputName[opn]


/// Get string corresponding to output port name with its width prepended as a Verilog
/// slice.
/// All output ports are internal wire or reg definitions.
let getVPortOutWithSlice (fc: FastComponent) (opn: OutputPortNumber) =
    let name = getVPortOut fc opn
    let (OutputPortNumber n) = opn
    let width = Option.get fc.OutputWidth[n]

    match width with
    | 1 -> $"%s{name}"
    | _ -> $" [%d{width - 1}:0] {name}"

/// Get string corresponding to name of signal that drives component input port
let getVPortInput (fs: FastSimulation) (fc: FastComponent) (InputPortNumber ipn) : string =
    let labBase = fc.FullName

    match fc.InputDrivers[ipn] with
    | Some (fid, opn) -> getVPortOut fs.FComps[fid] opn
    | None -> failwithf "Can't find input driver for %A port %d" fc.FullName ipn


/// Create fixed width verilog zero.
/// NB it seems this is not strictly needed, integer 0 works!
let getZeros width =
    match width with
    | 1 -> "1'b0"
    | _ -> $"{width}'h0"

/// what verilog declaration should the output signal have?
let fastOutputDefinition (vType:VMode) (fc: FastComponent) (opn: OutputPortNumber) =
    let (OutputPortNumber n) = opn
    let name = fc.VerilogOutputName[n]
    let vDef = getVPortOutWithSlice fc opn

    match fc.FType, fc.AccessPath with
    | Output n, [] -> $"output {vDef};\n"
    | DFF, _
    | DFFE, _ -> $"reg {vDef} = 1'b0;\n"
    | Input _, _ -> failwithf "Legacy Input component types should never occur"
    | Input1 (n, _), [] ->
        match vType with 
        | ForSynthesis -> $"input {vDef};\n"
        | ForSimulation -> $"reg {vDef} = {getZeros n};\n"
    | Register n, _
    | RegisterE n, _ -> $"reg {vDef} = {getZeros n};\n"
    | _ -> $"wire {vDef};\n"

/// Translates from a component to its Verilog description
let getVerilogComponent (fs: FastSimulation) (fc: FastComponent) =
    let ins i = getVPortInput fs fc (InputPortNumber i)
    let outs i = getVPortOut fc (OutputPortNumber i)
    let name = fc.VerilogComponentName
    let idNum =
        name
        |> String.split [|'_'|]
        |> Array.last
        

    let outW i =
        match fc.OutputWidth[i] with
        | Some n when n > 64 -> failwithf "Sorry - Verilog output does not yet work for busses > 64 bit. Output failed"
        | Some n -> n
        | None -> failwithf "Can't find output width for output port %d of %A\n" i fc.FullName

    let inW i =
        let (fid, OutputPortNumber opn) =
            match fc.InputDrivers[i] with
            | Some x -> x
            | None -> failwithf "Can't find input driver for port %d of %s" i fc.FullName

        fs.FComps[fid].OutputWidth[opn]
        |> function
        | Some n -> n
        | None -> failwithf "Can't find output width for output port %d of %A\n" opn fs.FComps[fid]
    
    let demuxOutput (outputPort: string) (selectPort: string) (w:int) = 
        if outputPort = selectPort
        then ins 0
        else makeBits w (uint64 0)

    match fc.FType with
    | Viewer _ -> ""
    | Input1 _ when fc.AccessPath = [] 
        -> failwithf "What? cannot call getVerilogComponent to find code for global Input"
    | Output _
    | Viewer _
    | IOLabel _
    | Input1 _ -> sprintf $"assign %s{outs 0} = %s{ins 0};\n"

    | Not -> sprintf "assign %s = ! %s;\n" (outs 0) (ins 0)
    | And
    | Or
    | Xor
    | Nand
    | Nor
    | Xnor
    | Xor -> sprintf "assign %s = %s;\n" (outs 0) (getVerilogBinaryOp fc.FType (ins 0) (ins 1))
    | DFFE
    | RegisterE _ -> $"always @(posedge clk) %s{outs 0} <= %s{ins 1} ? %s{ins 0} : %s{outs 0};\n"
    | DFF
    | Register _ -> $"always @(posedge clk) %s{outs 0} <= %s{ins 0};\n"
    | Constant1 (w, c,_) 
    | Constant (w, c)
        -> $"assign %s{outs 0} = %s{makeBits w (uint64 c)};\n"
    | Decode4 ->
        let w = outW 1

        $"assign %s{outs 0} = (%s{ins 0} == 2'b00) ? %s{ins 1} : {makeBits w (uint64 0)};\n"
        + $"assign %s{outs 1} = (%s{ins 0} == 2'b01) ? %s{ins 1} : {makeBits w (uint64 0)};\n"
        + $"assign %s{outs 2} = (%s{ins 0} == 2'b10) ? %s{ins 1} : {makeBits w (uint64 0)};\n"
        + $"assign %s{outs 3} = (%s{ins 0} == 2'b11) ? %s{ins 1} : {makeBits w (uint64 0)};\n"
    | Demux2 ->
        let w = outW 0

        $"assign %s{outs 0} = %s{ins 1} ? {makeBits w (uint64 0)} : %s{ins 0};\n"
        + $"assign %s{outs 1} = %s{ins 1} ? %s{ins 0} : {makeBits w (uint64 0)};\n"
    | Demux4 ->
        let w = outW 0
        
        $"assign %s{outs 0} = %s{demuxOutput (outs 0) (ins 1) w};\n"
        + $"assign %s{outs 1} = %s{demuxOutput (outs 1) (ins 1) w};\n"
        + $"assign %s{outs 2} = %s{demuxOutput (outs 2) (ins 1) w};\n"
        + $"assign %s{outs 3} = %s{demuxOutput (outs 3) (ins 1) w};\n"
    | Demux8 ->
        let w = outW 0
        
        $"assign %s{outs 0} = %s{demuxOutput (outs 0) (ins 1) w};\n"
        + $"assign %s{outs 1} = %s{demuxOutput (outs 1) (ins 1) w};\n"
        + $"assign %s{outs 2} = %s{demuxOutput (outs 2) (ins 1) w};\n"
        + $"assign %s{outs 3} = %s{demuxOutput (outs 3) (ins 1) w};\n"
        + $"assign %s{outs 4} = %s{demuxOutput (outs 4) (ins 1) w};\n"
        + $"assign %s{outs 5} = %s{demuxOutput (outs 5) (ins 1) w};\n"
        + $"assign %s{outs 6} = %s{demuxOutput (outs 6) (ins 1) w};\n"
        + $"assign %s{outs 7} = %s{demuxOutput (outs 7) (ins 1) w};\n"
    | NbitsAdder n ->
        let cin = ins 0
        let a = ins 1
        let b = ins 2
        let sum = outs 0
        let cout = outs 1
        $"assign {{%s{cout},%s{sum} }} = %s{a} + %s{b} + %s{cin};\n"
    | NbitsXor n ->
        let a = ins 0
        let b = ins 1
        let xor = outs 0
        $"assign {xor} = {a} ^ {b};\n"
    | Mux2 -> $"assign %s{outs 0} = %s{ins 2} ? %s{ins 1} : %s{ins 0};\n"
    | Mux4 -> 
        let outputBit = 
            match ins 4 with
            | "0" -> ins 0 
            | "1" -> ins 1 
            | "2" -> ins 2
            | "3" -> ins 3
            | _ -> failwithf "Cannot happen"

        $"assign %s{outs 0} = %s{outputBit};\n"
    | Mux8 -> 
        let outputBit = 
            match ins 8 with
            | "0" -> ins 0 
            | "1" -> ins 1 
            | "2" -> ins 2
            | "3" -> ins 3
            | "4" -> ins 4 
            | "5" -> ins 5 
            | "6" -> ins 6
            | "7" -> ins 7
            | _ -> failwithf "Cannot happen"

        $"assign %s{outs 0} = %s{outputBit};\n"
    | BusSelection (outW, lsb) ->
        let sel = sprintf "[%d:%d]" (outW + lsb - 1) lsb
        $"assign {outs 0} = {ins 0}{sel};\n"
    | BusCompare (w, c) -> $"assign %s{outs 0} = %s{ins 0} == %s{makeBits w (uint64 (uint32 c))};\n"
    | MergeWires -> $"assign {outs 0} = {{ {ins 0},{ins 1} }};\n"  
    | SplitWire _ ->
        let lsbBits = outW 0
        let msbBits = outW 1

        $"assign %s{outs 0} = %s{ins 0}[%d{lsbBits - 1}:0];\n"
        + $"assign %s{outs 1} = %s{ins 0}[%d{msbBits + lsbBits - 1}:%d{msbBits}];\n"
    | AsyncROM1 mem -> sprintf $"%s{name} I{idNum} (%s{outs 0}, %s{ins 0});\n"
    | ROM1 mem -> $"%s{name} I{idNum} (%s{outs 0}, %s{ins 0}, clk);\n"
    | RAM1 mem | AsyncRAM1 mem -> $"%s{name} I{idNum} (%s{outs 0}, %s{ins 0}, %s{ins 1}, %s{ins 2}, clk);\n"
    | Custom _ -> failwithf "What? custom components cannot exist in fast Simulation data structure"
    | AsyncROM _ | RAM _ | ROM _ -> 
        failwithf $"Invalid legacy component type '{fc.FType}'"
    | Input _ -> failwithf "Legacy Input component types should never occur"


/// return the header of the main verilog module with hardware inputs and outputs in header.
let getMainHeader (vType:VMode) (fs: FastSimulation) =
    Array.append
        fs.FGlobalInputComps
        (Array.filter (fun fc -> isOutput fc.FType && fc.AccessPath = []) fs.FOrderedComps)
    |> Array.collect
        (fun fc -> // NB - inputs are assigned zero and not included in module header
            match fc.FType, fc.AccessPath with
            | Output _, [] -> // NB - inputs are assigned zero in synthesis and not included in module header
                [| fc.VerilogOutputName[0] |]
            | Input1 _, [] when vType = ForSynthesis -> [| fc.VerilogOutputName[0] |]
            | _ -> [||])
    |> Array.append (match vType with | ForSynthesis -> [|"clk"|] | ForSimulation -> [||])
    |> String.concat ",\n\t"
    |> (fun header -> 
            let clock = match vType with ForSimulation -> "" | ForSynthesis -> "input clk;"
            $"module main (\n\t{header});\n{clock}")
    |> fun s -> [| s |]

/// return the wire and reg definitions needed to make the verilog design work.
let getMainSignalDefinitions (vType: VMode) (fs: FastSimulation) =
    fs.FComps
    |> mapValues
    |> Array.filter (fun fc -> fc.Active)
    |> Array.collect
        (fun fc ->
            fc.Outputs
            |> Array.mapi (fun i _ -> fastOutputDefinition vType fc (OutputPortNumber i)))
    |> Array.sort
    |> Array.append (match vType with | ForSimulation -> [| "reg clk;\n" |] | ForSynthesis -> [||])
    

/// get the module definitions (one per RAM instance) that define RAMs used
/// TODO: make output more compact by using multiple instances of one module where possible.
/// NB. Initial statement is used to initialise RAM as per simulation: should work with Quartus.
/// NB - there is some inconsistency between this definition and current simulation, which will output
/// ram[0] contents in clock 0 on q. the simulation is incompatible with FPGA tools and should change so
/// that initial ram output is always 0.
let extractRamDefinitions (fs: FastSimulation) =
    fs.FOrderedComps
    |> Array.collect (
        (fun fc ->
            match fc.FType with
            | ROM1 mem
            | RAM1 mem
            | AsyncRAM1 mem
            | AsyncROM1 mem -> [| fc.VerilogComponentName, fc.FType |]
            | _ -> [||])
    )

/// get the verilog statements output from each component
let getMainHardware (fs: FastSimulation) =
    let hardware =
        [| fs.FClockedComps; fs.FOrderedComps |]
        |> Array.concat

    Array.map (getVerilogComponent fs) hardware

/// make a simple testbench which displays module outputs for the first 30 clock cycles
let getInitialSimulationBlock (vType:VMode) (fs: FastSimulation) =

    let inDefs =
        fs.FGlobalInputComps
        |> Array.map
            (fun fc ->
                let width = Option.get fc.OutputWidth[0]
                let sigName = fc.VerilogOutputName[0]
                $"assign {sigName} = {makeBits width 0uL};")
        |> String.concat "\n"

    let outNames, (outFormat, outVars) =
        fs.FComps
        |> Map.toArray
        |> Array.filter
            (function
            | _, { AccessPath = []; FType = Output _ } -> true
            | _ -> false)
        |> Array.map
            (fun (_, fc) ->
                let sigName = fc.VerilogOutputName[0]

                let hexWidth =
                    let w = Option.get fc.OutputWidth[0]
                    if w <= 0 then failwithf $"Unexpected width ({w})in verilog output for {fc.FullName}"
                    (w - 1) / 4 + 1

                let (ComponentLabel heading) = fc.SimComponent.Label
                let heading = fc.VerilogComponentName
                let padding = max 0 (hexWidth - heading.Length)
                let heading = (String.replicate padding " ") + heading
                heading, (max hexWidth heading.Length, $"{sigName}"))
        |> Array.unzip
        |> (fun (a, b) -> a, Array.unzip b)

    let outNames = String.concat " " outNames

    let outFormat =
        outFormat
        |> Array.map (fun width -> "%" + $"{width}h")
        |> String.concat " "

    let outVars = String.concat "," outVars
    match vType with
    | ForSynthesis -> [||]
    | ForSimulation ->
        [| $"""
            initial
                    begin
                    {inDefs}
                    clk = 1'b0;
                    #10
                    $display("{outNames}");
                    while ($time < 300)
                    begin
                        $display("{outFormat}",{outVars});
                        #5 clk = ~clk;
                        #5 clk = ~clk;
                    end
                    end
        """ |]


/// Outputs a string which contains a single verilog file with the hardware in verilog form.
/// The top-level simulation moudle is called main - other modules may be included for RAM & ROM
/// this can be called any time after after buildFastSimulation has created the initial FastSimulation
/// data structure.
/// To simulate this you would need to set up clk as a clock input, and provide stimulus for other inputs if
/// there are any.
let getVerilog (vType: VMode) (fs: FastSimulation) =
    // make sure we have Ok names to use for output
    writeVerilogNames fs

    [| getInstantiatedModules fs
       getMainHeader vType fs
       getMainSignalDefinitions vType fs
       getMainHardware fs
       getInitialSimulationBlock vType fs
       [| "endmodule\n" |] |]
    |> Array.map (String.concat "")
    |> String.concat "\n"
   
