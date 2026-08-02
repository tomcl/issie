module SheetDescription

(*
    SheetDescription.fs

    A design sheet described as data: a list of components and a list of logical connections,
    with no geometry at all.

    This is deliberately plain: it names components, gives their types, and says which port
    connects to which. It has no dependency on the draw block, on Fable, or on anything that
    cannot run under .NET, so a description can be built anywhere - a test, a generator, a
    translator from another format - and handed to SheetLayout to become a sheet that opens in
    Issie and can be read.

    Positions and wire routes are SheetLayout's business. Nothing here knows what a symbol looks
    like or where it goes.
*)

open CommonTypes
open ParameterTypes

/// One port of one component, written "COMP/PORT".
///
/// PORT is matched against the port names Issie gives that component type, ignoring case and
/// surrounding space, and falls back to being read as a port index. The fallback is not a
/// convenience: many component types - gates, inputs, outputs, wire labels, constants, bus
/// selects, splitters - have no port names at all, so an index is the only way to refer to them.
///
/// PORT may be left off entirely ("A" rather than "A/0") when the component has exactly one port
/// in the direction wanted, which covers Inputs, Outputs and most single-output components.
type PortRef = {
    Comp: string
    /// "" means "the only port in this direction"
    Port: string
}

/// A connection from an output port to an input port. Which end is which is decided by position:
/// From is always the driver.
type ConnSpec = { From: PortRef; To: PortRef }

/// A component to place. `Name` identifies it within the description and, unless `Label` says
/// otherwise, is also the label drawn on the sheet.
type CompSpec = {
    Name: string
    Type: ComponentType
    Label: string option
}

/// A parameter the sheet declares: a name, the value it takes when nothing binds it, and what it
/// means. The description is compulsory here as it is everywhere else - it is what an instance of
/// this sheet shows the user when asking them for a value.
type ParamSpec = {
    Name: string
    Default: int
    Description: string
}

/// One integer of one component driven by a parameter expression. The expression is written as
/// text and read by the same parser the properties pane uses, so "W", "W*2" and "W-1" all mean
/// here exactly what they mean when typed into a properties box.
type SlotSpec = {
    Comp: string
    Slot: CompSlotName
    Expression: string
}

/// A whole sheet, before it has any geometry.
type SheetDescription = {
    Name: string
    Comps: CompSpec list
    Conns: ConnSpec list
    Params: ParamSpec list
    Slots: SlotSpec list
}

/// Read "COMP/PORT", or "COMP" for the only port in whichever direction it is used.
/// A port name may itself contain a '/', so only the FIRST separator counts.
let parsePortRef (reference: string) : PortRef =
    match reference.IndexOf '/' with
    | -1 -> { Comp = reference.Trim(); Port = "" }
    | i -> { Comp = reference.Substring(0, i).Trim(); Port = reference.Substring(i + 1).Trim() }

/// A component whose label is its name.
let comp (name: string) (compType: ComponentType) : CompSpec =
    { Name = name; Type = compType; Label = None }

/// A component labelled differently from the name used to refer to it here.
let compLabelled (name: string) (label: string) (compType: ComponentType) : CompSpec =
    { Name = name; Type = compType; Label = Some label }

/// A connection, driver first. The same as the ==> operator below, and always safe to use.
let connect (fromPort: string) (toPort: string) : ConnSpec =
    { From = parsePortRef fromPort; To = parsePortRef toPort }

/// The order components are given in fixes the order of the sheet's own inputs and outputs, so
/// declare Input and Output components in the order the sheet's ports should appear.
let describeSheet (name: string) (comps: CompSpec list) (conns: ConnSpec list) : SheetDescription =
    { Name = name; Comps = comps; Conns = conns; Params = []; Slots = [] }

/// Declare a parameter on the sheet. Pipeline style:
///     describeSheet "adder" comps conns
///     |> withParam "W" 4 "width of the data bus in bits"
///     |> withSlot "ADD" Buswidth "W"
let withParam (name: string) (defaultValue: int) (description: string) (sheet: SheetDescription) =
    { sheet with Params = sheet.Params @ [ { Name = name; Default = defaultValue; Description = description } ] }

/// Drive one integer of one component from a parameter expression, written as it would be typed
/// into the properties pane.
let withSlot (compName: string) (slot: CompSlotName) (expression: string) (sheet: SheetDescription) =
    { sheet with Slots = sheet.Slots @ [ { Comp = compName; Slot = slot; Expression = expression } ] }

/// NOT opened by `open SheetDescription` - it has to be asked for.
///
/// `==>` is already Fable.Core.JsInterop's object-literal operator (used in FilesIO, MainView and
/// TruthTableView) and FsCheck's implication operator. Opening this module alongside either of
/// those shadows one with the other, silently, by order of the open statements. So open it only in
/// a file that uses neither - which is all of the test files. `connect` above is the same function
/// and carries no such hazard.
module Operators =
    let (==>) (fromPort: string) (toPort: string) : ConnSpec = connect fromPort toPort
