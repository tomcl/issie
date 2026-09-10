// Build the projects the user tutorial screenshots are taken from, as data, so that a
// screenshot can be retaken against a later Issie without anyone redrawing a schematic.
//
//     dotnet fsi scripts/doc-shot-projects.fsx     then: node scripts/doc-shots.js all
//
// Two projects, because the tutorial shows the same top sheet before and after it is made
// clocked, and both are called "main" on screen:
//
//   tutorial          decoder + main, combinational   -> custom.png
//   tutorialClocked   decoder + main, counter-driven  -> waveform.png, waveform1/2.png
//
// The decoder is the function the tutorial's own truth-table screenshot documents:
// D = 0 -> RESULT = A.B + C,  D = 1 -> RESULT = A.B.  That comes out of
//   G1   = A AND B
//   MUX1 = D ? G1 : C
//   G2   = G1 OR MUX1
// since with D = 1 the OR sees A.B twice.

// The Tests output directory, because it is the one place every dependency of Renderer.dll sits
// side by side. src/Renderer/bin holds only Renderer and Shared, and a dependency that cannot be
// loaded surfaces as a type initializer throwing inside Symbol rather than as a load error.
// Build it first if it is not there: dotnet build Tests/Issie.Tests/Issie.Tests.fsproj
#I @"../Tests/Issie.Tests/bin/Debug/net10.0"
#r "Shared.dll"
#r "Renderer.dll"

open System.IO
open CommonTypes
open SheetDescription
open SheetDescription.Operators

let repo = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))

// Inside the repository, because Issie refuses to open a project outside the directories it may
// use, and the system temp directory is not one of them. tmp/ is gitignored.
let outRoot = Path.Combine(repo, "tmp", "docShots")

/// The four-bit ROM the tutorial asks the reader to fill in with "a random 4-bit number"
/// per location. Fixed here so that the screenshots are reproducible.
let romData =
    [ 0, 5; 1, 3; 2, 12; 3, 3; 4, 9; 5, 6; 6, 3; 7, 15
      8, 3; 9, 10; 10, 7; 11, 3; 12, 1; 13, 11; 14, 3; 15, 8 ]
    |> List.map (fun (a, d) -> bigint a, bigint d)
    |> Map.ofList

let rom: Memory1 =
    { Init = FromData; AddressWidth = 4; WordWidth = 4; Data = romData; Comments = None }

/// Declaration order fixes the sheet's port order, so A B C D here is A B C D on every
/// instance of the custom component.
let decoder =
    describeSheet "decoder" [
        comp "A" (Input1(1, None))
        comp "B" (Input1(1, None))
        comp "C" (Input1(1, None))
        comp "D" (Input1(1, None))
        comp "G1" (GateN(And, 2))
        comp "MUX1" Mux2
        comp "G2" (GateN(Or, 2))
        comp "RESULT" (Output 1)
    ] [
        "A" ==> "G1/0"
        "B" ==> "G1/1"
        "C" ==> "MUX1/0"
        "G1" ==> "MUX1/1"
        "D" ==> "MUX1/SEL"
        "G1" ==> "G2/0"
        "MUX1" ==> "G2/1"
        "G2" ==> "RESULT"
    ]

let decoderInstance: ComponentType =
    Custom {
        Name = "decoder"
        InputLabels = [ "A", 1; "B", 1; "C", 1; "D", 1 ]
        OutputLabels = [ "RESULT", 1 ]
        Form = Some User
        ParameterBindings = None
        Description = None
    }

/// The ROM's four output bits, peeled off one at a time by three SplitWires, exactly as the
/// tutorial asks for. SplitWire n takes the n least significant bits to its first output.
let splitChain = [
    comp "SW1" (SplitWire 3)
    comp "SW2" (SplitWire 2)
    comp "SW3" (SplitWire 1)
]

let splitConns = [
    "AROM1/DOUT" ==> "SW1"
    "SW1/0" ==> "SW2"
    "SW2/0" ==> "SW3"
    // SW3 gives bit 0 then bit 1; SW2's second output is bit 2; SW1's second is bit 3.
    "SW3/0" ==> "DECODER1/A"
    "SW3/1" ==> "DECODER1/B"
    "SW2/1" ==> "DECODER1/C"
    "SW1/1" ==> "DECODER1/D"
]

// The split chain is declared between the ROM and the decoder so that the layout's bisection
// puts it there too: declared after the decoder, SW3 was placed to its right and its two bits
// had to travel back over the top of it.
let mainCombinational =
    describeSheet "main" ([
        comp "ADDR" (Input1(4, None))
        comp "AROM1" (AsyncROM1 rom)
    ] @ splitChain @ [
        comp "DECODER1" decoderInstance
        comp "RESULT" (Output 1)
    ]) ([
        "ADDR" ==> "AROM1/ADDR"
        "DECODER1/RESULT" ==> "RESULT"
    ] @ splitConns)

let mainClocked =
    describeSheet "main" ([
        // The tutorial has the reader remove both the load and the enable port in
        // Properties, which is this variant. CounterNoLoad keeps an enable input, and
        // leaving it unconnected is a simulation error.
        //
        // The counter stands where the input did in the combinational sheet. It is not an
        // Input, so it is not placed in the left column the way one would be, and the layout
        // has only its single edge to AROM1 to go on - declaring it first is what keeps that
        // edge pointing rightwards rather than folding the sheet back on itself.
        comp "CNT1" (CounterNoEnableLoad 4)
        comp "AROM1" (AsyncROM1 rom)
    ] @ splitChain @ [
        comp "DECODER1" decoderInstance
        comp "RESULT" (Output 1)
    ]) ([
        "CNT1" ==> "AROM1/ADDR"
        "DECODER1/RESULT" ==> "RESULT"
    ] @ splitConns)

/// The clocked sheet with a register between the counter and the ROM - the change the tutorial
/// asks the reader to make while the waveform simulation is open, so that Refresh has something
/// to show.
let mainClockedWithRegister =
    describeSheet "main" ([
        comp "CNT1" (CounterNoEnableLoad 4)
        comp "REG1" (Register 4)
        comp "AROM1" (AsyncROM1 rom)
    ] @ splitChain @ [
        comp "DECODER1" decoderInstance
        comp "RESULT" (Output 1)
    ]) ([
        "CNT1" ==> "REG1"
        "REG1" ==> "AROM1/ADDR"
        "DECODER1/RESULT" ==> "RESULT"
    ] @ splitConns)

// ---------------------------------------------------------------------------------------------
// The features page: a design with a program memory, which is what its Catalogue and memory
// editor pictures are of. The memory is initialised from a .ram file so that the editor has
// comments to show against the locations - which is the point of that picture.
// ---------------------------------------------------------------------------------------------

let programRam = """0 3a // load the count into the accumulator
1 1b // add the constant below
2 07 // constant: 7
3 4c // store the accumulator
4 22 // jump if zero
5 00 // (target, filled in by the assembler)
6 5f // halt
7 00
"""

let program: Memory1 =
    { Init = FromFile "program"; AddressWidth = 3; WordWidth = 8; Data = Map.empty; Comments = None }

let showcase =
    describeSheet "sequencer" [
        comp "PC" (CounterNoEnableLoad 3)
        comp "PROG" (AsyncROM1 program)
        comp "SPLIT" (SplitN(2, [ 4; 4 ], [ 0; 4 ]))
        comp "IR" (Register 4)
        comp "OPCODE" (Output 4)
        comp "OPERAND" (Output 4)
    ] [
        "PC" ==> "PROG/ADDR"
        "PROG/DOUT" ==> "SPLIT"
        "SPLIT/0" ==> "IR"
        "IR" ==> "OPERAND"
        "SPLIT/1" ==> "OPCODE"
    ]

let report name result =
    match result with
    | Ok () -> printfn "wrote %s" name
    | Error (msg: string) -> printfn "FAILED %s: %s" name msg; exit 1

/// Write a project only if it is not already there, unless --force is given.
///
/// These are laid out by hand after they are generated - the bisection layout is good enough for
/// a test and not for a picture - so regenerating over the top of one would throw that away. The
/// generator records what the design IS; the file on disk records what it LOOKS like.
let force = fsi.CommandLineArgs |> Array.contains "--force"

let writeProject name sheets extraFiles =
    let dir = Path.Combine(outRoot, name)
    if Directory.Exists dir && not force then
        printfn "kept   %s (already laid out - pass --force to overwrite)" name
    else
        SheetLayout.saveProject dir sheets |> report name
        for (file: string, contents: string) in extraFiles do
            File.WriteAllText(Path.Combine(dir, file), contents)
            printfn "       + %s" file

writeProject "tutorial" [ decoder; mainCombinational ] []
writeProject "tutorialClocked" [ decoder; mainClocked ] []
writeProject "tutorialClockedReg" [ decoder; mainClockedWithRegister ] []
writeProject "showcase" [ showcase ] [ "program.ram", programRam ]
