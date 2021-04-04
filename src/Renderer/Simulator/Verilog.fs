module Verilog

open CommonTypes
open SimulatorTypes
open SynchronousUtils
open EEExtensions
open Fast
open Helpers
open NumberHelpers


/// take FullName and convert it into a verilog compatible form
let verilogNameConvert (s: string) =
    let maxIdentifierLength = 50

    let baseName =
        EEExtensions.String.split [| '(' |] s
        |> Array.toList
        |> function
        | h :: _ -> h
        | [] -> "v"
        |> String.replace "." "_"
        |> String.replace " " "_"

    let extraLength = baseName.Length - maxIdentifierLength

    if extraLength > 0 then
        baseName.[extraLength..]
        |> Seq.map string
        |> string
    else
        baseName

/// simple way to assign to each component and component output a unique verilog compatible name.
/// outputs will become reg or wire signals in the Verilog
let writeVerilogNames (fs: FastSimulation) =
    fs.FComps
    |> Map.toArray
    |> Array.iteri
        (fun i (fid, fc) ->
            let oName =
                match fc.SimComponent.Label with
                | ComponentLabel lab -> lab

            let vName = verilogNameConvert oName

            let vName =
                if vName = "" then
                    "v" + vName
                else
                    vName

            let name = $"{vName}_{i}"
            fc.VerilogComponentName <- name

            fc.VerilogOutputName
            |> Array.iteri
                (fun portNum _ ->
                    let outName = $"{name}_out{portNum}"
                    fc.VerilogOutputName.[portNum] <- outName))





/// write a guaranteed globally unique name for each Verilog output.
/// NB - not used now, delete it?
let addUniqueVerilogNames (fs: FastSimulation) =
    /// convert an index into an equivalent alphabetic suffix
    let numToChars (root: char) n =
        $"%d{n}"
        |> Seq.map (fun ch -> int ch + int root - int '0' |> char |> string)
        |> String.concat ""

    /// these are the components we need to give disambuated outputs
    let activeComps =
        fs.FComps
        |> mapValues
        |> Array.filter (fun fc -> fc.Active)

    /// Make sure all names are unique by appending to names disambiguating characters
    /// don't allow "" as a name, so similarly append to that.
    let rec disambiguate (root: char) (nameL: ('a * string) array) =
        let groups =
            nameL |> Array.groupBy (fun (fc, vName) -> vName)

        match groups with
        | _ when groups.Length = nameL.Length -> nameL
        | groups ->
            groups
            |> Array.collect
                (fun (vName, fcA) ->
                    fcA
                    |> Array.mapi
                        (fun i (fc, vName) ->
                            match i with
                            | 0
                            | 0 -> fc, vName
                            | i -> fc, vName + numToChars root i))
            // recursively disambiguate in case we have created a new name clash
            |> disambiguate root

    let uniqueNames =
        activeComps
        |> Array.map (fun fc -> fc, verilogNameConvert fc.FullName)
        |> Array.map
            (function
            | (fc, "") -> fc, "NULL"
            | x -> x)
        |> disambiguate 'a'
        |> Array.collect
            (fun (fc, vName) ->
                [| 0 .. fc.Outputs.Length - 1 |]
                |> Array.map
                    (fun i ->
                        match fc.FType, fc.AccessPath with
                        | Output _, []
                        | Input _, [] -> ((fc, OutputPortNumber i), $"{vName}")
                        | _ -> ((fc, OutputPortNumber i), $"{vName}_out{i}")))
        |> disambiguate 'q'

    uniqueNames
    |> Array.iter (fun ((fc, OutputPortNumber n), name) -> fc.VerilogOutputName.[n] <- name)


let makeAsyncRomModule (moduleName: string) (mem: Memory) =
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
    wire [%d{dMax}:0] rom [%d{numWords - 1}:0];
    assign q <= rom[a]
    initial
    begin
        %s{romInits}
    end
     """

let makeRomModule (moduleName: string) (mem: Memory) =
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
    output reg [%d{dMax}:0] q = 0;
    input [%d{aMax}:0] a;
    reg [%d{dMax}:0] rom [%d{numWords - 1}:0];
    always @(posedge clk) q <= rom[a];
    integer i;
    initial
    begin
        for (i=0; i < {numWords}; i=i+1)
        begin
            ram[i] = 0;
        end
    begin
        %s{romInits}
    end
     """

let makeRamModule (moduleName: string) (mem: Memory) =
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
    output reg [%d{dMax}:0] q = 0;
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
/// get all the RAM and ROM modules used
/// NB at the moment each instance is made a separately named module, for simplicity
let getInstantiatedModules (fs: FastSimulation) =
    fs.FComps
    |> Map.toArray
    |> Array.collect
        (fun (fid, fc) ->
            let name = fc.VerilogComponentName

            match fc.FType with
            | RAM mem -> [| makeRamModule name mem |]
            | ROM mem -> [| makeRomModule name mem |]
            | AsyncROM mem -> [| makeAsyncRomModule name mem |]
            | _ -> [||])


let activeComps (fs: FastSimulation) =
    [ fs.FClockedComps; fs.FOrderedComps ]
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
let makeBits w (c: uint64) = sprintf $"%d{w}'h%x{c}"

/// get output port name
let getVPortOut (fc: FastComponent) (OutputPortNumber opn) = fc.VerilogOutputName.[opn]


/// Get string corresponding to output port name with its width prepended as a Verilog
/// slice.
/// All output ports are internal wire or reg definitions.
let getVPortOutWithSlice (fc: FastComponent) (opn: OutputPortNumber) =
    let name = getVPortOut fc opn
    let (OutputPortNumber n) = opn
    let width = Option.get fc.OutputWidth.[n]

    match width with
    | 1 -> $"%s{name}"
    | _ -> $" [%d{width - 1}:0] {name}"

/// Get string corresponding to name of signal that drives component input port
let getVPortInput (fs: FastSimulation) (fc: FastComponent) (InputPortNumber ipn) : string =
    let labBase = fc.FullName

    match fc.InputDrivers.[ipn] with
    | Some (fid, opn) -> getVPortOut fs.FComps.[fid] opn
    | None -> failwithf "Can't find input driver for %A port %d" fc.FullName ipn


/// Create fixed width verilog zero.
/// NB it seems this is not strictly needed, integer 0 works!
let getZeros width =
    match width with
    | 1 -> "1'b0"
    | _ -> $"{width}'h0"

/// what verilog declaration should the output signal have?
let fastOutputDefinition (fc: FastComponent) (opn: OutputPortNumber) =
    let (OutputPortNumber n) = opn
    let name = fc.VerilogOutputName.[n]
    let vDef = getVPortOutWithSlice fc opn

    match fc.FType, fc.AccessPath with
    | Output n, [] -> $"output {vDef};\n"
    | DFF, _
    | DFFE, _ -> $"reg {vDef} = 1'b0;\n"
    | Input n, []
    | Register n, _
    | RegisterE n, _ -> $"reg {vDef} = {getZeros n};\n"
    | Input n, _ -> $"wire {vDef};\n"
    | _ -> $"wire {vDef};\n"

/// Translates from a component to its Verilog description
let getVerilogComponent (fs: FastSimulation) (fc: FastComponent) =
    let ins i = getVPortInput fs fc (InputPortNumber i)
    let outs i = getVPortOut fc (OutputPortNumber i)
    let name = fc.VerilogComponentName

    let outW i =
        match fc.OutputWidth.[i] with
        | Some n -> n
        | None -> failwithf "Can't find output width for output port %d of %A\n" i fc.FullName

    let inW i =
        let (fid, OutputPortNumber opn) =
            match fc.InputDrivers.[i] with
            | Some x -> x
            | None -> failwithf "Can't find input driver for port %d of %s" i fc.FullName

        fs.FComps.[fid].OutputWidth.[opn]
        |> function
        | Some n -> n
        | None -> failwithf "Can't find output width for output port %d of %A\n" opn fs.FComps.[fid]

    match fc.FType, fc.AccessPath with
    | Input _, [] -> failwithf "What? cannot call getVerilogComponent to find code for global Input"
    | Output _, _
    | IOLabel _, _
    | Input _, _ -> sprintf $"assign %s{outs 0} = %s{ins 0};\n"
    | _ ->
        match fc.FType with
        | Not -> sprintf "assign %s = ! %s;\n" (outs 0) (ins 0)
        | And
        | Or
        | Xor
        | Nand
        | Nor
        | Xor -> sprintf "assign %s = %s;\n" (outs 0) (getVerilogBinaryOp fc.FType (ins 0) (ins 1))
        | DFFE
        | RegisterE _ -> $"always @(posedge clk) %s{outs 0} <= %s{ins 1} ? %s{ins 0} : %s{outs 0};\n"
        | DFF
        | Register _ -> $"always @(posedge clk) %s{outs 0} <= %s{ins 0};\n"
        | Constant (w, c) -> $"assign %s{outs 0} = %s{makeBits w (uint64 (uint32 c))};\n"
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
        | NbitsAdder n ->
            let cin = ins 0
            let a = ins 1
            let b = ins 2
            let sum = outs 0
            let cout = outs 1

            sprintf
                "%s"
                ("assign {"
                 + $"%s{cout},%s{sum}"
                 + "} = "
                 + $"%s{a} + %s{b} + %s{cin};\n")
        | NbitsXor n ->
            let a = ins 0
            let b = ins 1
            let xor = outs 0
            $"assign {xor} = {a} ^ {b};\n"
        | Mux2 -> sprintf $"assign %s{outs 0} = %s{ins 2} ? %s{ins 1} : %s{ins 0};\n"
        | BusSelection (outW, lsb) ->
            let sel = sprintf "[%d:%d]" (outW + lsb - 1) lsb
            sprintf $"assign {outs 0} = {ins 0}{sel};\n"
        | BusCompare (w, c) -> sprintf $"assign %s{outs 0} = %s{ins 0} == %s{makeBits w (uint64 (uint32 c))};\n"
        | MergeWires -> sprintf "assign %s = {%s,%s};\n" (outs 0) (ins 0) (ins 1)
        | SplitWire _ ->
            let lsbBits = outW 0
            let msbBits = outW 1

            sprintf $"assign %s{outs 0} = %s{ins 0}[%d{lsbBits - 1}:0];\n"
            + sprintf $"assign %s{outs 1} = %s{ins 0}[%d{msbBits + lsbBits - 1}:%d{msbBits}];\n"
        | AsyncROM mem -> sprintf $"%s{name} I1 (%s{outs 0}, %s{ins 0});\n"
        | ROM mem -> sprintf $"%s{name} I1 (%s{outs 0}, %s{ins 0}, clk);\n"
        | RAM mem -> sprintf $"%s{name} I1 (%s{outs 0}, %s{ins 0}, %s{ins 1}, %s{ins 2}, clk);\n"
        | Custom _ -> failwithf "What? custom components cannot exist in fast Simulation data structure"
        | _ -> failwithf "What? impossible!: fc.FType =%A" fc.FType

/// return the header of the main verilog module with hardware inputs and outputs in header.
let getMainHeader (fs: FastSimulation) =
    Array.append
        fs.FGlobalInputComps
        (Array.filter (fun fc -> isOutput fc.FType && fc.AccessPath = []) fs.FOrderedComps)
    |> Array.collect
        (fun fc -> // NB - inputs are assigned zero and not included in module header
            match fc.FType, fc.AccessPath with
            | Output _, [] -> // NB - inputs are assigned zero and not included in module header
                [| fc.VerilogOutputName.[0] |]
            | _ -> [||])
    |> String.concat ",\n\t"
    |> sprintf "module main (\n\t%s);"
    |> fun s -> [| s |]

/// return the wire and reg definitions needed to make the verilog design work.
let getMainSignalDefinitions (fs: FastSimulation) =
    fs.FComps
    |> mapValues
    |> Array.filter (fun fc -> fc.Active)
    |> Array.collect
        (fun fc ->
            fc.Outputs
            |> Array.mapi (fun i _ -> fastOutputDefinition fc (OutputPortNumber i)))
    |> Array.sort
    |> Array.append [| "reg clk;\n" |]

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
            | ROM mem
            | RAM mem
            | AsyncROM mem -> [| verilogNameConvert fc.FullName, fc.FType |]
            | _ -> [||])
    )

/// get the verilog statements output from each component
let getMainHardware (fs: FastSimulation) =
    let hardware =
        [| fs.FClockedComps; fs.FOrderedComps |]
        |> Array.concat

    Array.map (getVerilogComponent fs) hardware

/// make a simple testbench which displays module outputs for the first 30 clock cycles
let getInitialSimulationBlock (fs: FastSimulation) =

    let inDefs =
        fs.FGlobalInputComps
        |> Array.map
            (fun fc ->
                let width = Option.get fc.OutputWidth.[0]
                let sigName = fc.VerilogOutputName.[0]
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
                let sigName = fc.VerilogOutputName.[0]

                let hexWidth =
                    (Option.get fc.OutputWidth.[0] - 1) / 4 + 1

                let (ComponentLabel heading) = fc.SimComponent.Label
                let heading = verilogNameConvert heading
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

    [| $"""
        initial
                begin
                {inDefs}
                clk = 1'b0;
                $display("{outNames}");
                while ($time < 300)
                begin
                    $display("{outFormat}",{outVars});
                    #5 clk = !clk;
                    #5 clk = !clk;
                end
                end
    """ |]


/// Outputs a string which contains a single verilog file with the hardware in verilog form.
/// The top-level simulation moudle is called main - other modules may be included for RAM & ROM
/// this can be called any time after after buildFastSimulation has created the initial FastSimulation
/// data structure.
/// To simulate this you would need to set up clk as a clock input, and provide stimulus for other inputs if
/// there are any.
let getVerilog (fs: FastSimulation) =
    // make sure we have Ok names to use for output
    writeVerilogNames fs

    [| getInstantiatedModules fs
       getMainHeader fs
       getMainSignalDefinitions fs
       getMainHardware fs
       getInitialSimulationBlock fs
       [| "endmodule\n" |] |]
    |> Array.map (String.concat "")
    |> String.concat "\n"
