module Verilog

open CommonTypes
open SimGraphTypes
open SimTypes
open SynchronousUtils
open EEExtensions
open FastBuild
open FastRun
open Helpers
open NumberHelpers

type VMode = ForSynthesis | ForSimulation
type CompilationProfile = Release | Debug

/// longest identifier this module will emit. Verilog itself allows more, but short names keep
/// the output readable and stay within the limits of some synthesis tools.
let private maxIdentifierLength = 50

/// Truncate to maxIdentifierLength, keeping the start of the name.
/// Keeping the start rather than the end means the result still cannot begin with a digit or '$'
/// when the input could not, so it stays a legal Verilog identifier. Truncation may make two names
/// equal; disambiguate (below) runs afterwards and separates them again.
let private capLength (s: string) =
    if s.Length > maxIdentifierLength then s[.. maxIdentifierLength - 1] else s

/// take FullName and convert it into a verilog compatible form
/// this is not 1-1, so outputs may not be unique, that is OK
let verilogNameConvert (maxChars:int) (s: string) =
    EEExtensions.String.split [| '(' |] s
    |> Array.toList
    |> function
    | h :: _ -> h
    | [] -> "x"
    |> Seq.map (function | ch when System.Char.IsLetterOrDigit ch || ch = '_' -> string ch | _ -> "")
    |> Seq.truncate maxChars
    |> String.concat ""
    |> capLength

/// simple way to assign to each component and component output a unique verilog compatible name.
/// outputs will become reg or wire signals in the Verilog
let writeVerilogNames (fs: FastSimulation) =
    let getShortPath (path: ComponentId list) : string =
        path
        |> List.map (fun (ComponentId cid) -> string cid)
        |> (function | (t) -> t)
        |> List.map (verilogNameConvert 1)
        |> String.concat ""

    /// generate from a component a maybe non-unique name made from its Label and abbreviated path
    let getBaseVerilogName fc =
        let sc = fc.SimComponent
        let fakeName s = $"%s{s}{match sc.Id with | ComponentId n -> n}"
        let cLabel =
            match sc.Label , sc.Type with
            | ComponentLabel "", SplitWire _ -> fakeName "Split"
            | ComponentLabel "", MergeWires ->  fakeName "Merge"
            | ComponentLabel "", _ -> fakeName "Other"
            | ComponentLabel lab,_ -> lab.ToUpper()

        match fc.fId with
        | (_,[]) -> verilogNameConvert 20 cLabel
        // the cap has to be applied again here: verilogNameConvert caps each part, but a deep
        // sheet hierarchy can make label + path long even when both parts are short enough
        | (_,path) -> capLength (verilogNameConvert 20 cLabel + "$" + getShortPath path)

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

 
        

/// Name of the module generated for a memory component. Distinct from the component's own name,
/// which is already in use as the net driven by the instance: a module and a net sharing an
/// identifier is legal in some tools and rejected by others.
let memModuleName (fc: FastComponent) = $"{fc.VerilogComponentName}_mem"

/// Name of the single instance of that module. Derived from the component name, which
/// writeVerilogNames has already made unique, so instances cannot collide either.
let memInstanceName (fc: FastComponent) = $"{fc.VerilogComponentName}_inst"

let makeAsyncRomModule (moduleName: string) (mem: Memory1) =
    let aMax = mem.AddressWidth - 1
    let dMax = mem.WordWidth - 1
    let numWords = 1 <<< mem.AddressWidth

    let romInits =
        mem.Data
        |> Map.toArray
        |> Array.map (fun (a, d) -> sprintf $"rom[%A{a}] = %A{d};")
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
        |> Array.map (fun (a, d) -> sprintf $"rom[%A{a}] = %A{d};")
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
        |> (Array.map (fun (a, d) -> sprintf $"ram[%A{a}] = %A{d};"))
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
        |> (Array.map (fun (a, d) -> sprintf $"ram[%A{a}] = %A{d};"))
        |> String.concat "\n"

    sprintf
        $"""

    module %s{moduleName}(q, a, d, we, clk);
    output [%d{dMax}:0] q;
    input [%d{dMax}:0] d;
    input [%d{aMax}:0] a;
    input we, clk;
    reg [%d{dMax}:0] ram [%d{numWords - 1u}:0];
     always @(posedge clk) begin
         if (we)
             ram[a] <= d;
     end
    // asynchronous read: q follows the addressed word without waiting for a clock edge
    assign q = ram[a];

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
let getInstantiatedModules (profile: CompilationProfile) (fs: FastSimulation) =
    fs.FComps
    |> Map.toArray
    |> Array.collect
        (fun (fid, fc) ->
            let name = memModuleName fc

            match fc.FType with
            | RAM1 mem -> [| makeRamModule name mem |]
            | AsyncRAM1 mem -> [| makeAsyncRamModule name mem |]
            | ROM1 mem -> [| makeRomModule name mem |]
            | AsyncROM1 mem -> [| makeAsyncRomModule name mem |]
            | _ -> [||])
    // only the debug controller instantiates a uart, and including a file that is not there
    // stops the output compiling at all
    |> Array.append (
        match profile with
        | Debug -> [| "`include \"cores/osdvu/uart.v\"" |]
        | Release -> [||])

let removeHybridComps (fa: FastComponent array) =
    Array.filter (fun fc -> not (isHybridComponent fc.FType)) fa

let activeComps (fs: FastSimulation) =
    [ fs.FClockedComps; removeHybridComps fs.FOrderedComps ]
    |> Array.concat

/// The bitwise operator a gate combines its inputs with, before any inversion.
/// Gate inputs are always one bit wide, so the bitwise operators are the right ones.
let getVerilogGateOp gateType =
    match gateType with
    | And | Nand -> "&"
    | Or | Nor -> "|"
    | Xor | Xnor -> "^"

/// Implement an n-input gate.
/// The inputs are combined with the un-negated operator and the result inverted once, which is
/// what an n-input NAND/NOR/XNOR gate means, and what getNInpBinaryGateReducer in EvalReference does.
/// Folding the negated operator pairwise instead would give different logic for n > 2.
let getVerilogNInputBinaryOp cType portConversionFn =
    match cType with
    | GateN (gateType, n) ->
        let terms =
            List.init n portConversionFn
            |> String.concat $" {getVerilogGateOp gateType} "
        if isNegated gateType then $"!({terms})" else terms
    | _ -> failwithf "operator %A not defined" cType

/// get valid Verilog constant for bus of given width (may be 1)
/// NB the digits must be hex, since they follow a 'h prefix: %A or %d would print the value in
/// decimal and Verilog would then read those digits as hex, giving a different (and too wide)
/// number. printf "%x" does not work on bigints, hence hexBignum.
let makeBits w (c: bigint) =
    let c = c &&& ((1I <<< w) - 1I)
    sprintf $"%d{w}'h%s{(hexBignum c)[1..]}"

/// get output port name
let getVPortOut (fc: FastComponent) (OutputPortNumber opn) = fc.VerilogOutputName[opn]


/// Get string corresponding to output port name with its width prepended as a Verilog
/// slice.
/// All output ports are internal wire or reg definitions.
let getVPortOutWithSlice (fc: FastComponent) (opn: OutputPortNumber) =
    let name = getVPortOut fc opn
    let (OutputPortNumber n) = opn
    let width = fc.OutputWidth n

    match width with
    | 1 -> $"%s{name}"
    | _ -> $" [%d{width - 1}:0] {name}"

/// Get string corresponding to name of signal that drives component input port
let getVPortInput (fs: FastSimulation) (fc: FastComponent) (InputPortNumber ipn) : string =
    match fc.InputDrivers[ipn] with
    | Some (index, opn) -> getVPortOut (fs.ComponentAt index) opn
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
    | RegisterE n, _ 
    | Counter n, _ 
    | CounterNoEnable n, _
    | CounterNoLoad n, _
    | CounterNoEnableLoad n, _ -> $"reg {vDef} = {getZeros n};\n"
    | _ -> $"wire {vDef};\n"

/// Translates from a component to its Verilog description
let getVerilogComponent (fs: FastSimulation) (fc: FastComponent) =
    let ins i = getVPortInput fs fc (InputPortNumber i)
    let outs i = getVPortOut fc (OutputPortNumber i)
    let name = fc.VerilogComponentName

    let outW i =
        match fc.OutputWidth i with
        | n when n > 64 -> failwithf "Sorry - Verilog output does not yet work for busses > 64 bit. Output failed"
        | n -> n

    let inW i =
        let (index, OutputPortNumber opn) =
            match fc.InputDrivers[i] with
            | Some x -> x
            | None -> failwithf "Can't find input driver for port %d of %s" i fc.FullName

        (fs.ComponentAt index).OutputWidth opn

    
    let demuxOutput (outputPort: string) (selectPort: string) (w:int) = 
        if outputPort = selectPort
        then ins 0
        else makeBits w 0I

    match fc.FType with
    | Input1 _ when fc.AccessPath = [] 
        -> failwithf "What? cannot call getVerilogComponent to find code for global Input"
    | Viewer _
    | Output _
    | IOLabel
    | Input1 _ -> sprintf $"assign %s{outs 0} = %s{ins 0};\n"

    | NotConnected -> ""

    | Not -> sprintf "assign %s = ! %s;\n" (outs 0) (ins 0)
    | GateN (gateType, n) -> sprintf "assign %s = %s;\n" (outs 0) (getVerilogNInputBinaryOp fc.FType ins)
    | DFFE
    | RegisterE _ -> $"always @(posedge clk) %s{outs 0} <= %s{ins 1} ? %s{ins 0} : %s{outs 0};\n"
    | Counter _ -> $"always @(posedge clk) %s{outs 0} <= %s{ins 2} ? (%s{ins 1} ? %s{ins 0} : (%s{outs 0}+1'b1)) : %s{outs 0};\n"
    | CounterNoEnable _ -> $"always @(posedge clk) %s{outs 0} <= %s{ins 1} ? %s{ins 0} : (%s{outs 0}+1'b1) ;\n"
    | CounterNoLoad _ -> $"always @(posedge clk) %s{outs 0} <= %s{ins 0} ? (%s{outs 0}+1'b1) : %s{outs 0};\n"
    | CounterNoEnableLoad _ -> $"always @(posedge clk) %s{outs 0} <= (%s{outs 0}+1'b1) ;\n"
    | DFF
    | Register _ -> $"always @(posedge clk) %s{outs 0} <= %s{ins 0};\n"
    | Constant1 (w, c,_) 
    | Constant (w, c)
        -> $"assign %s{outs 0} = %s{makeBits w c};\n"
    | Decode4 ->
        let w = outW 1

        $"assign %s{outs 0} = (%s{ins 0} == 2'b00) ? %s{ins 1} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 1} = (%s{ins 0} == 2'b01) ? %s{ins 1} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 2} = (%s{ins 0} == 2'b10) ? %s{ins 1} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 3} = (%s{ins 0} == 2'b11) ? %s{ins 1} : {makeBits w (bigint 0)};\n"
    | Demux2 ->
        let w = outW 0

        $"assign %s{outs 0} = %s{ins 1} ? {makeBits w (bigint 0)} : %s{ins 0};\n"
        + $"assign %s{outs 1} = %s{ins 1} ? %s{ins 0} : {makeBits w (bigint 0)};\n"
    | Demux4 ->
        let w = outW 0

        $"assign %s{outs 0} = (%s{ins 1} == 2'b00) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 1} = (%s{ins 1} == 2'b01) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 2} = (%s{ins 1} == 2'b10) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 3} = (%s{ins 1} == 2'b11) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
    | Demux8 ->
        let w = outW 0
        
        $"assign %s{outs 0} = (%s{ins 1} == 3'b000) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 1} = (%s{ins 1} == 3'b001) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 2} = (%s{ins 1} == 3'b010) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 3} = (%s{ins 1} == 3'b011) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 4} = (%s{ins 1} == 3'b100) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 5} = (%s{ins 1} == 3'b101) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 6} = (%s{ins 1} == 3'b110) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
        + $"assign %s{outs 7} = (%s{ins 1} == 3'b111) ? %s{ins 0} : {makeBits w (bigint 0)};\n"
    | NbitsAdder n ->
        let cin = ins 0
        let a = ins 1
        let b = ins 2
        let sum = outs 0
        let cout = outs 1
        $"assign {{%s{cout},%s{sum} }} = %s{a} + %s{b} + %s{cin};\n"
    | NbitsAdderNoCin n ->
        let a = ins 0
        let b = ins 1
        let sum = outs 0
        let cout = outs 1
        $"assign {{%s{cout},%s{sum} }} = %s{a} + %s{b} ;\n"
    | NbitsAdderNoCout n ->
        let cin = ins 0
        let a = ins 1
        let b = ins 2
        let sum = outs 0
        $"assign %s{sum} = %s{a} + %s{b} + %s{cin};\n"
    | NbitsAdderNoCinCout n ->
        let a = ins 0
        let b = ins 1
        let sum = outs 0
        $"assign %s{sum} = %s{a} + %s{b} ;\n"
    
    | NbitsXor(n,op) ->
        let a = ins 0
        let b = ins 1
        let xor = outs 0
        match op with
        | None -> $"assign {xor} = {a} ^ {b};\n"
        // no slice needed: assigning to the n-bit output truncates the product to n bits, which is
        // what the simulation does. A bit-select on a parenthesised expression is not legal Verilog.
        | Some Multiply -> $"assign {xor} = {a} * {b};\n"
    | NbitsAnd n ->
        let a = ins 0
        let b = ins 1
        let andOut = outs 0
        $"assign {andOut} = {a} & {b};\n"
    | NbitsOr n ->
        let a = ins 0
        let b = ins 1
        let orOut = outs 0
        $"assign {orOut} = {a} | {b};\n"
    | NbitsNot n ->
        let a = ins 0
        let not = outs 0
        $"assign {not} = ~{a};\n"
    | NbitSpreader n ->
        let a = ins 0
        let out = outs 0
        let result1 =
            ("",[1..n])||>List.fold (fun s v -> s+"1") 
        // $"assign {out} = {a} << {n});\n"
        $"assign {out} = {a} ? {n}'b{result1} : {n}'b0;\n"
    | Mux2 -> $"assign %s{outs 0} = %s{ins 2} ? %s{ins 1} : %s{ins 0};\n"
    | Mux4 -> $"assign %s{outs 0} = %s{ins 4}[1] ? (%s{ins 4}[0] ? %s{ins 3} : %s{ins 2}) : (%s{ins 4}[0] ? %s{ins 1} : %s{ins 0})  ;\n"
        
    | Mux8 -> 
        $"assign %s{outs 0} = %s{ins 8}[2] ? (%s{ins 8}[1] ? (%s{ins 8}[0] ? %s{ins 7} : %s{ins 6}) : (%s{ins 8}[0] ? %s{ins 5} : %s{ins 4})) : (%s{ins 8}[1] ? (%s{ins 8}[0] ? %s{ins 3} : %s{ins 2}) : (%s{ins 8}[0] ? %s{ins 1} : %s{ins 0}))  ;\n"
    | BusSelection (outW, lsb) ->
        let sel = sprintf "[%d:%d]" (outW + lsb - 1) lsb
        $"assign {outs 0} = {ins 0}{sel};\n"
    | BusCompare (w, c) -> $"assign %s{outs 0} = %s{ins 0} == %s{makeBits w c};\n"
    | BusCompare1 (w, c, _) -> $"assign %s{outs 0} = %s{ins 0} == %s{makeBits w c};\n"
    | MergeWires -> $"assign {outs 0} = {{ {ins 1},{ins 0} }};\n" 
    | MergeN n ->  
        let mergedInputs = 
            [| for i in n - 1 .. -1 .. 0 ->
                if i = 0 then
                    $"{ins i}"
                else
                    $"{ins i},"
            |]
            |> String.concat ""
        $"assign {outs 0} = {{ {mergedInputs} }};\n" 
    | SplitWire _ ->
        let lsbBits = outW 0
        let msbBits = outW 1

        $"assign %s{outs 0} = %s{ins 0}[%d{lsbBits - 1}:0];\n"
        + $"assign %s{outs 1} = %s{ins 0}[%d{msbBits + lsbBits - 1}:%d{lsbBits}];\n"
    | SplitN (n, outputWidths, lsBits) -> 
        List.map3 (
            fun index width lsb -> 
                let msb = width+lsb-1
                $"assign %s{outs index} = %s{ins 0}[%d{msb}:%d{lsb}];\n"
        ) [0..n-1] outputWidths lsBits
        |> List.fold (fun accstr outstr -> accstr+outstr) ""
    | AsyncROM1 mem -> $"{memModuleName fc} {memInstanceName fc} (%s{outs 0}, %s{ins 0});\n"
    | ROM1 mem -> $"{memModuleName fc} {memInstanceName fc} (%s{outs 0}, %s{ins 0}, clk);\n"
    | RAM1 mem | AsyncRAM1 mem ->
        $"{memModuleName fc} {memInstanceName fc} (%s{outs 0}, %s{ins 0}, %s{ins 1}, %s{ins 2}, clk);\n"
    | Custom _ -> failwithf "What? custom components cannot exist in fast Simulation data structure"
    | Input _
    | AsyncROM _ | RAM _ | ROM _ ->
        failwithf $"Invalid legacy component type '{fc.FType}'"
    | Shift (n,m,tp) ->
        let input = ins 0
        let shifter = ins 1
        let output = outs 0
        match tp with
        |LSL -> $"assign %s{output} = %s{input} << %s{shifter};\n"
        |LSR -> $"assign %s{output} = %s{input} >> %s{shifter};\n"
        // >>> only shifts arithmetically when its left operand is signed, and every signal here is
        // an unsigned wire or reg: without $signed it would be an ordinary logical shift
        |ASR -> $"assign %s{output} = $signed(%s{input}) >>> %s{shifter};\n"
/// return the header of the main verilog module with hardware inputs and outputs in header.
let getMainHeader (vType:VMode) (profile: CompilationProfile) (fs: FastSimulation) =
    Array.append
        fs.FGlobalInputComps
        (Array.filter (fun fc -> isOutput fc.FType && fc.AccessPath = []) fs.FOrderedComps)
    |> Array.collect
        (fun fc -> // NB - inputs are assigned zero and not included in module header
            match fc.FType, fc.AccessPath with
            | Output _, [] -> // NB - inputs are assigned zero in synthesis and not included in module header
                [| fc.VerilogOutputName[0] |]
            | Input1 _, [] |Input _, [] when vType = ForSynthesis -> [| fc.VerilogOutputName[0] |]
            | _ -> [||])
    |> Array.append (
        match vType with
        | ForSynthesis -> match profile with | Release -> [|"clk"|] | Debug -> [|"debug_clk"; "RS232_Rx_TTL"; "RS232_Tx_TTL"|]
        | ForSimulation -> [||])
    |> String.concat ",\n\t"
    |> (fun header -> 
            let clock =
                match (vType, profile) with
                | (ForSimulation, _) -> ""
                | (ForSynthesis, Release) -> "input clk;"
                | (ForSynthesis, Debug) -> "input debug_clk;\ninput RS232_Rx_TTL;\noutput RS232_Tx_TTL;"
            // a sheet with no inputs or outputs would otherwise give "module main (\n\t);"
            let ports = if header = "" then "" else $" (\n\t{header})"
            $"module main{ports};\n{clock}")
    |> fun s -> [| s |]

/// return the wire and reg definitions needed to make the verilog design work.
let getMainSignalDefinitions (vType: VMode) (profile: CompilationProfile) (fs: FastSimulation) =
    fs.FComps
    |> mapValues
    |> Array.filter (fun fc -> fc.Active)
    |> Array.collect
        (fun fc ->
            fc.Outputs
            |> Array.mapi (fun i _ -> fastOutputDefinition vType fc (OutputPortNumber i)))
    |> Array.sort
    |> Array.append (match (vType, profile) with
                     | (ForSimulation, _) -> [| "reg clk;\n" |]
                     | (ForSynthesis, Release) -> [||]
                     | (ForSynthesis, Debug) -> [| "wire clk;\n" |])

/// get the verilog statements output from each component
/// NB a hybrid component (AsyncRAM1) is in both FClockedComps and FOrderedComps, since it has both
/// clocked and combinational behaviour. Its Verilog describes the whole component, so it must be
/// written once: activeComps drops the hybrid copy from the combinational half.
let getMainHardware (fs: FastSimulation) =
    activeComps fs
    |> Array.map (getVerilogComponent fs)

/// make a simple testbench which displays module outputs for the first 30 clock cycles
let getInitialSimulationBlock (vType:VMode) (fs: FastSimulation) =
    
    // a plain procedural assignment, not "assign": inputs are regs, so a procedural continuous
    // assignment would pin them to zero for the whole run and leave nothing for the user to drive
    let inDefs =
        fs.FGlobalInputComps
        |> Array.map
            (fun fc ->
                let width = fc.OutputWidth 0
                let sigName = fc.VerilogOutputName[0]
                $"{sigName} = {makeBits width 0I};")
        |> String.concat "\n                    "

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
                    let w = fc.OutputWidth 0
                    if w <= 0 then failwithf $"Unexpected width ({w})in verilog output for {fc.FullName}"
                    (w - 1) / 4 + 1

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


let getDebugController (profile: CompilationProfile) (fs: FastSimulation) =
    
    let padWithZeros (a: string array) =
        (Array.toList a, List.replicate 8 "1'b0")
        ||> List.append
        |> List.take 8
        |> List.toArray

    let comps =
        fs.FOrderedComps
        |> Array.filter (fun fc -> match fc.FType with | Viewer _ -> true | _ -> false)
        |> Array.map (fun fc -> getVPortOut fc (OutputPortNumber 0), fc.OutputWidth 0)
        |> Array.collect (fun (name, width) -> [0 .. width - 1] |> List.toArray |> Array.map (fun i -> $"{name}[{i}]"))
        //|> Array.map (fun (name, index) -> $"{name}[{index}]")
    
    
    
    let comps =
        comps
        |> Array.chunkBySize 8
        |> Array.map padWithZeros
        |> Array.mapi (fun i s -> 
            let i32 = (int32 i)
            let hexString = i32.ToString("x2");
            $"    \"{hexString}\": tx_byte <= {{ {s} }};")
    
    let comps = 
        Array.append comps [|"    default: tx_byte <= 8'hFF;"|]
        |> String.concat "\n"

    // TODO: Add RS232_Rx_TTL and RS232_Tx_TTL to the IO header
    match profile with
    | Release -> [||]
    | Debug ->
        [| $"""
wire RS232_Rx_TTL;
wire RS232_Tx_TTL;
wire reset = 0;
reg transmit = 0;
reg [7:0] tx_byte = 0;
wire received;
wire [7:0] rx_byte;
wire is_receiving;
wire is_transmitting;
wire recv_error;
reg [3:0] num_received = 0;
reg [31:0] received_bytes = 0;
uart #(.baud_rate(9600), .sys_clk_freq(12000000))
uart0(
    .clk(debug_clk),
    .rst(reset),
    .rx(RS232_Rx_TTL),
    .tx(RS232_Tx_TTL),
    .transmit(transmit),
    .tx_byte(tx_byte),
    .received(received),
    .rx_byte(rx_byte),
    .is_receiving(is_receiving),
    .is_transmitting(is_transmitting),
    .recv_error(recv_error),
);
reg single_step = 0;
reg is_running = 0;
reg clk_is_active = 0;
assign clk = debug_clk & clk_is_active;
always @ (negedge debug_clk) begin
    clk_is_active <= 0;
    if (single_step)
        clk_is_active <= 1;
    if (is_running)
        clk_is_active <= 1;
end
always @ (posedge debug_clk) begin
    transmit <= 0;
    single_step <= 0;
    if (received) begin
        num_received <= num_received + 1;
        received_bytes <= {{ received_bytes [23:0], rx_byte }};
    end
    if (num_received == 4'd1) begin
        if (received_bytes[7:0] == 8'h53/*S*/) begin
            num_received <= 4'h0;
            single_step <= 1;
        end
    if (received_bytes[7:0] == 8'h43/*C*/) begin
            num_received <= 4'h0;
            is_running <= 1;
        end
        if (received_bytes[7:0] == 8'h50/*P*/) begin
            num_received <= 4'h0;
            is_running <= 0;
        end
    end else if (num_received == 4'd3) begin
        if (received_bytes[23:16] == 8'h52/*R*/) begin // Read value of internal registers/wires
            num_received <= 4'h0;
            transmit <= 1;
            case (received_bytes[15:0])
            {comps}
            endcase
        end
    end
end
""" |]

/// Outputs a string which contains a single verilog file with the hardware in verilog form.
/// The top-level simulation moudle is called main - other modules may be included for RAM & ROM
/// this can be called any time after after buildFastSimulation has created the initial FastSimulation
/// data structure.
/// To simulate this you would need to set up clk as a clock input, and provide stimulus for other inputs if
/// there are any.
let getVerilog (vType: VMode) (fs: FastSimulation) (profile: CompilationProfile) =
    // make sure we have Ok names to use for output
    writeVerilogNames fs

    [| getInstantiatedModules profile fs
       getMainHeader vType profile fs
       getMainSignalDefinitions vType profile fs
       getMainHardware fs
       getInitialSimulationBlock vType fs
       getDebugController profile fs
       [| "endmodule\n" |] |]
    |> Array.map (String.concat "")
    |> String.concat "\n"
   
