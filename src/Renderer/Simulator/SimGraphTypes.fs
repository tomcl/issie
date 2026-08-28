(*
    Types.fs

    This module collects a series of types used in the simulator logic.
*)

module rec SimGraphTypes

open Fable.Core
open CommonTypes
open Fable.Core.JsInterop

/// Binary data used in simulation
type Bit =
    | Zero
    | One

/// Fixed width bus data used in simulation
/// TODO: refactor as bigint for efficiency
/// The list is little-endian: the LSB is at index 0, and the MSB is at index N-1,
/// where N is the length of the list.
type WireData = Bit list

/// State (possibly none) remembered by component
/// from previous clock cycle. Combinational components
/// have no state.
type SimulationComponentState =
    | NoState // For all stateless components.
    | DffState of uint32
    | RegisterState of FastData
    /// A read/write memory's contents. Not a `Memory1`: that would mean building a whole `Map`
    /// for every clock step, which is the cost `RamStore` exists to remove. The store is mutable
    /// and shared by every step, so the step it is being read at has to be supplied alongside -
    /// see [docs/dev/ramRepresentation.md].
    | RamState of RamStore.Ram


/// Like Component but with additional dynamic info used by simulator
/// Clocked components have state data.
/// All components have optional data on inputs that propagates
/// During evaluation of combinational logic
/// Components require all inputs to have data before they can
/// generate output data
/// Note that reducer is a function that generates the outputs
/// TODO: make this equatable data?
type SimulationComponent =
    { Id: ComponentId
      Type: ComponentType
      Label: ComponentLabel
      /// What each output port drives: entry k is the ports and components connected to output
      /// port k, and a port driving nothing - an unconnected IOLabel - is an empty list rather
      /// than a missing key.
      ///
      /// An ARRAY and not a Map, on the rule that an array is right exactly when it is written
      /// once: this is built complete when the component is made and never updated, and the key is
      /// a port number, dense from 0. It used to be one small F# Map allocated per component per
      /// INSTANCE of its sheet - the expansion's worth of them on a design that multiplies out -
      /// each iterated twice by the flatten. OutputWidths beside it is already an array indexed
      /// the same way.
      Outputs: (ComponentId * InputPortNumber) list array
      DrivenInputWidths: ((ComponentId * InputPortNumber) * int) list
      InputWidths: Map<InputPortNumber, int>
      OutputWidths: int array
      // this is MUTABLE and used only during clock tick change propagation
      // location n = true => the output (of a synchronous component) has been
      // propagated in propagateStateChanges. Location n corresponds to
      // OutputPortNumber n.
      // not used except for synchronous components and custom components
      CustomSimulationGraph: SimulationGraph option
      // State for synchronous stateful components, like flip flops and memories.
      // The state should only be changed when clock ticks are fed. Other changes
      // will be ignored.
     }

/// Every component of one sheet instance, by its design-time id.
///
/// Keyed by the bare int INSIDE a ComponentId, and reached only through the SimGraph module below,
/// which takes and returns the wrapped type. F#'s Map takes its comparer from
/// LanguagePrimitives.FastGenericComparer, which has hard-coded fast paths for genuine primitives
/// and a boxing fallback for everything else - so an int key is 2.4x faster than a reference
/// wrapper and a struct wrapper is slower than either, boxing BOTH operands on every comparison.
/// Unwrapping at this one boundary buys the primitive path and keeps the types at every call site,
/// which is what lets ComponentId be [<Struct>] without this map being the thing that pays.
and SimulationGraph = Map<int, SimulationComponent>

/// The one way in and out of a SimulationGraph, in ComponentIds.
///
/// The key-carrying functions WRAP on the way out, which is free under Fable ([<Erase>]) and free
/// under .NET only while ComponentId is a struct. So where a caller does not need the key there is
/// a value-only form beside each - `values`, `iterValues`, `foldValues`, `mapValues` - and the hot
/// iterations use those. Nothing outside this module names the int.
[<RequireQualifiedAccess>]
module SimGraph =
    let empty: SimulationGraph = Map.empty

    let tryFind (ComponentId c) (g: SimulationGraph) = Map.tryFind c g
    let find (ComponentId c) (g: SimulationGraph) = Map.find c g
    let containsKey (ComponentId c) (g: SimulationGraph) = Map.containsKey c g
    let add (ComponentId c) (comp: SimulationComponent) (g: SimulationGraph) : SimulationGraph = Map.add c comp g
    let count (g: SimulationGraph) = g.Count
    let isEmpty (g: SimulationGraph) = Map.isEmpty g

    let ofList (pairs: (ComponentId * SimulationComponent) list) : SimulationGraph =
        pairs |> List.map (fun (ComponentId c, comp) -> c, comp) |> Map.ofList

    let toList (g: SimulationGraph) : (ComponentId * SimulationComponent) list =
        g |> Map.toList |> List.map (fun (c, comp) -> ComponentId c, comp)

    let iter (f: ComponentId -> SimulationComponent -> unit) (g: SimulationGraph) =
        g |> Map.iter (fun c comp -> f (ComponentId c) comp)

    let map (f: ComponentId -> SimulationComponent -> SimulationComponent) (g: SimulationGraph) : SimulationGraph =
        g |> Map.map (fun c comp -> f (ComponentId c) comp)

    let filter (f: ComponentId -> SimulationComponent -> bool) (g: SimulationGraph) : SimulationGraph =
        g |> Map.filter (fun c comp -> f (ComponentId c) comp)

    let fold (f: 'S -> ComponentId -> SimulationComponent -> 'S) (state: 'S) (g: SimulationGraph) : 'S =
        (state, g) ||> Map.fold (fun s c comp -> f s (ComponentId c) comp)

    // value-only forms: no key, so nothing to wrap
    let values (g: SimulationGraph) = Map.values g
    let iterValues (f: SimulationComponent -> unit) (g: SimulationGraph) = g |> Map.iter (fun _ comp -> f comp)

    let mapValues (f: SimulationComponent -> SimulationComponent) (g: SimulationGraph) : SimulationGraph =
        g |> Map.map (fun _ comp -> f comp)

    let filterValues (f: SimulationComponent -> bool) (g: SimulationGraph) : SimulationGraph =
        g |> Map.filter (fun _ comp -> f comp)

    let foldValues (f: 'S -> SimulationComponent -> 'S) (state: 'S) (g: SimulationGraph) : 'S =
        (state, g) ||> Map.fold (fun s _ comp -> f s comp)

/// For every IO node, keep track of its Id, Label and wire width.
/// - Id: to feed values into the simulationGraph.
/// - Label: to display a nice form to the user.
/// - Width: to feed the right values into the simulation.
type SimulationIO = ComponentId * ComponentLabel * int



type SimulationErrorType =
    | PortNumMissing of PortType
    | WrongPortType of PortType * Port
    | ConnTypeHasNum of PortType * int
    | LabelConnect
    | LabelDuplicate of string * string
    | WidthMismatch of WidthInferError
    | InferConnWidths of string
    | BadName of string
    | MissingSheet of string
    | InPortMismatch of string * string * string
    | OutPortMismatch of string * string * string
    | InputConnError of int * Port * PortRmInfo
    | OutputConnError of int * Port * PortRmInfo
    | LabelConnError of int
    | CycleDetected of string
    | AlgInpNotAllowed of string
    | DependencyNotFound of string
    | WrongSelection of string
    | UnnecessaryNC
    | InternalError of exn
    | GenericSimError of string

/// - Documents an error found while simulating.
/// - Should never happen
type SimulationError =
    { ErrType: SimulationErrorType
      InDependency: string option
      ComponentsAffected: ComponentId list
      ConnectionsAffected: ConnectionId list }

type PortRmInfo =
    | Unremovable
    | Removable of ComponentType // specify original type and type after port removal
/// "input"/"output" as a user would write it. Used instead of %A so that a message cannot change
/// shape if PortType ever gains a field, and so that it reads as prose rather than as F#.
let private portTypeName (pType: PortType) =
    match pType with
    | PortType.Input -> "input"
    | PortType.Output -> "output"

/// Said after the port-consistency errors below. None of them can be caused by anything the user
/// did in the editor, so telling them to correct it - as every other error here does - would send
/// them looking for something that is not there.
let private damagedSheetAdvice =
    "\n\nThis is not something that can be caused by editing a schematic: the sheet file is \
     probably damaged. Try the most recent snapshot in the project's 'backup' folder, and please \
     report it (Info -> Bug Reports)."

let errMsg (errType: SimulationErrorType) =
    match errType with
    | PortNumMissing correctType ->
        sprintf "This component has an %s port with no port number.%s"
            (portTypeName correctType) damagedSheetAdvice
    | WrongPortType (correctType, port) ->
        // Option.get here would throw inside the code that renders an error, which is the worst
        // place to throw: the user would lose the error as well as having it.
        let which =
            match port.PortNumber with
            | Some n -> sprintf "Port %d of this component" n
            | None -> "A port of this component"
        sprintf "%s should be an %s port but is recorded as an %s port.%s"
            which (portTypeName correctType) (portTypeName port.PortType) damagedSheetAdvice
    | ConnTypeHasNum (correctType, portNum) ->
        sprintf "This component has an %s port carrying a port number (%d) that it should not have.%s"
            (portTypeName correctType) portNum damagedSheetAdvice
    | LabelConnect ->
        sprintf
            "You can't connect two Net Labels with a wire. Delete the connecting wire. If you want to join two net labels \
                     you need only give them the same name - then they will form a single net."
    | LabelDuplicate (ioType, compLabel) ->
        sprintf "Two %s components cannot have the same label: %s." ioType compLabel
    | WidthMismatch err -> err.Msg
    | InferConnWidths msg -> msg
    | BadName msg -> msg 
    | MissingSheet compName ->
        sprintf "Can't find a design sheet named %s for the custom component of this name" compName
    | InPortMismatch (compName, instIns, compIns) ->
        sprintf
            "This component is an instance of sheet '%s', but its inputs no longer match that \
             sheet's - the sheet has been edited since the instance was placed.\n\n\
             Instance inputs: %s\nSheet inputs: %s\n\n\
             Delete this component and place it again from the Catalogue to bring it up to date."
            compName
            instIns
            compIns
    | OutPortMismatch (compName, instOuts, compOuts) ->
        sprintf
            "This component is an instance of sheet '%s', but its outputs no longer match that \
             sheet's - the sheet has been edited since the instance was placed.\n\n\
             Instance outputs: %s\nSheet outputs: %s\n\n\
             Delete this component and place it again from the Catalogue to bring it up to date."
            compName
            instOuts
            compOuts
    | InputConnError (count, _, rmInfo) ->
        if count = 0 then
            match rmInfo with
            | Removable _ -> "Every component input port must be connected: but no connection was found"
            | Unremovable -> "Every component input port must be connected: but no connection was found \
                                Please connect this input port to the output of another component or an input component."
        else
            sprintf
                "A component input port must have precisely one driving component, but %d \
                        were found. If you want to merge wires together use a MergeWires component, not direct connection."
                count
    | OutputConnError (count, _, _) ->
        if count = 0 then
            "A component output port must have at least one connection. If the component output \
                is meant to be disconnected you can add a \"Not Connected\" component to stop this error"
        else
            // Not currently reachable: checkPortsAreConnectedProperly raises this only for a count
            // of 0. It is written out anyway so that widening that check cannot leave the user
            // reading a bare number, which is what stood here before.
            sprintf
                "A component output port has %d connections, which is more than this check allows. \
                 An output may drive any number of inputs, so if you are seeing this please report \
                 it (Info -> Bug Reports)."
                count
    | LabelConnError count ->
        if count = 0 then
            "A set of labelled wires must be driven (on the input of one of the labels): but no such driver was found"
        else
            sprintf
                "A set of labelled wires must have precisely one driving component, but %d \
                were found. \
                If you are driving two labels from the same component delete one of them: \
                a set of labels with the same name are all connected together and only one \
                label in each same-name set must be driven."
                count

    | CycleDetected msg -> msg
    | AlgInpNotAllowed msg -> msg
    | DependencyNotFound depName ->
        sprintf
            "Could not resolve dependency: \"%s\". Make sure a dependency with such name exists in the current project."
            depName
    | WrongSelection msg -> msg
    | UnnecessaryNC -> "Unnecessary 'Not Connected' components at adder COUTs"
    | InternalError e ->
        // The user cannot act on a stack trace, but they are the only person who can report it.
        // Frame it as a request rather than dumping it and leaving them to guess.
        sprintf
            "Issie's simulator has hit an internal problem. This is a fault in Issie, not in your \
             design, and we would like to know about it: please report it with the text below and \
             your project (Info -> Bug Reports).\n\n%s\n\n%s"
            e.Message
            e.StackTrace
    | GenericSimError msg -> msg

/// Wrapper for Javascript (Diagram) component. Why here?

[<Erase>]
type JSComponent = JSComponent of obj

/// Wrapper for Javascript (Diagram) connection. Why here?

[<Erase>]
type JSConnection = JSConnection of obj

/// State retrieves directly from Diagram has Javascript objects
type JSCanvasState = JSComponent list * JSConnection list

//----------------------------------------------------------------------------------------------//
//--------------------------------Fast Digital Bus Data Type------------------------------------//
//----------------------------------------------------------------------------------------------//
// data is stored differently according to its buswidth.
// We use all three options for efficiency
// Bit is more efficient than word for known boolean ops but it can be normalised to Word
// to make implementation of multiple bit components (that may carry one bit) simpler.
// BigWord is needed for > 32 bits, and much less efficient for < 32 bits.

type FastBits =
    | Word of dat: uint32
    | BigWord of dat: bigint

type FastData =
    { Dat: FastBits
      Width: int }

    member inline this.GetBigInt = // always possible
        match this.Dat with
        | Word n -> bigint n
        | BigWord n -> n

    /// return Some uint32 representing data if possible else None
    member inline this.GetUint32 = // not possible if too large
        match this.Dat with
        | Word n -> Some n
        | BigWord n when this.Width <= 32 -> Some(uint32 n)
        | _ -> None

    /// can fail - for fast access to word data
    member inline this.GetQUint32 =
        match this.Dat with
        | Word n -> n
        | BigWord n when this.Width <= 32 -> uint32 n
        | _ -> failwithf $"GetQint32 Can't turn Alg into a uint32"

    /// if given width <= 32 it will generate Word form FastData, otherwise BigWord.
    /// The value is masked into the width, since every stored value must be within its bus
    /// width. NB this used to store a positive unmasked (MakeFastData 2 4I gave 4) and to
    /// reduce a negative with %, which leaves a negative remainder rather than the two's
    /// complement pattern.
    static member inline MakeFastData (width: int) (data: bigint) =
        let masked = data &&& ((1I <<< width) - 1I)

        if width <= 32 then
            { Dat = Word(uint32 masked); Width = width }
        else
            { Dat = BigWord masked; Width = width }

//-------------------------------------------------------------------------------------//
//-----------------------------TT Algebra Types----------------------------------------//
//-------------------------------------------------------------------------------------//

// Types used for Algebraic Truth Tables caluclated in the Fast Simulation
// Defined here instead of in TruthTableTypes.fs because they are used in the FastSimulation

// Binary Algebraic Operators
type BinaryOp =
    | AddOp // A + B (mathematical addition)
    | SubOp // A - B (mathematical subtraction)
    | BitAndOp // A & B (bitwise AND)
    | BitOrOp // A | B (bitwise OR)
    | BitXorOp // A XOR B (bitwise XOR)
//| AppendOp // B::A (B becomes MSB, A becomes LSB)

// Unary Algebraic Operators
type UnaryOp =
    | NegOp // -A (mathematical negation, bitwise two's complement)
    | NotOp // bit inversion (bitwise XOR with -1)
    | BitRangeOp of Lower: int * Upper: int // A[upper:lower] (subset of bits of A)
    | CarryOfOp

// Comparison between expression and constant
type ComparisonOp = | Equals

// Type for algebraic expressions in Issie.
//
// ALGEBRA SEMANTICS. Every expression denotes an UNSIGNED value truncated to its bit-width
// (getAlgExpWidth): AddOp, SubOp and NegOp are modular, so NotOp e = -1 - e exactly, and
// NegOp produces the two's complement bit pattern - there is no signed interpretation
// anywhere. The one exception is CarryOfOp, which reads the carry OUT of its addition
// operand, i.e. bit w of the pre-truncation sum. At width 1, A XOR B = A + B and -A = A
// (mod 2). Doubling has two equivalent forms - arithmetic (A + A) and structural
// (A[w-2:0] :: 0, see doubleExp) - which simplify in different contexts.
// AppendExp lists are MSB-first: the head holds the most significant bits.
type FastAlgExp =
    | SingleTerm of SimulationIO
    | DataLiteral of FastData
    | UnaryExp of Op: UnaryOp * Exp: FastAlgExp
    | BinaryExp of Exp1: FastAlgExp * Op: BinaryOp * Exp2: FastAlgExp
    | ComparisonExp of Exp: FastAlgExp * Op: ComparisonOp * bigint
    | AppendExp of FastAlgExp list

/// Calculates and returns the expected width of an Algebraic Expression
let rec getAlgExpWidth (exp: FastAlgExp) =
    match exp with
    | SingleTerm(_, _, w) -> w
    | DataLiteral d -> d.Width
    | UnaryExp(BitRangeOp(l, u), _) -> u - l + 1
    | UnaryExp(CarryOfOp, _) -> 1
    // Assuming all other unary operators do not change width of expression
    | UnaryExp(_, exp) -> getAlgExpWidth exp
    // Assuming all other binary operators do not change width of expression
    // Return the greatest width
    | BinaryExp(exp1, _, exp2) ->
        let w1 = getAlgExpWidth exp1
        let w2 = getAlgExpWidth exp2
        if w1 > w2 then w1 else w2
    | ComparisonExp _ -> 1
    | AppendExp exps ->
        if exps.IsEmpty then
            failwithf "what? List in AppendExp is empty"
        else
            (0, exps)
            ||> List.fold (fun w exp -> w + getAlgExpWidth exp)


let bigIntMaskA =
    [| 0..128 |]
    |> Array.map (fun width -> (1I <<< width) - 1I)

let bigIntBitMaskA =
    [| 0..128 |]
    |> Array.map (fun width -> (1I <<< width))

/// all bits with numbers < width = 1
let bigIntMask width =
    if width <= 128 then
        bigIntMaskA[width]
    else
        (1I <<< width) - 1I

/// single bit 1 (2 ** pos)
let bigIntBitMask pos =
    if pos <= 128 then
        bigIntBitMaskA[pos]
    else
        (1I <<< pos)

/// A DataLiteral holding the given non-negative value, respecting the representation
/// invariant that Word is used iff width <= 32
let valueLiteral (width: int) (value: bigint) =
    if width <= 32 then
        DataLiteral { Dat = Word(uint32 value); Width = width }
    else
        DataLiteral { Dat = BigWord value; Width = width }

let zeroLiteral (width: int) = valueLiteral width 0I

let allOnesLiteral (width: int) = valueLiteral width (bigIntMask width)

/// The structural form of 2*exp truncated to width: exp's bits shifted up one, LSB 0.
/// Degenerates to the constant 0 at width 1 (2A = 0 mod 2). Doubling also has the
/// arithmetic form A + A; the two simplify in different contexts - the structural form
/// merges with bit ranges and appends, the arithmetic form cancels against negated terms.
let doubleExp (width: int) (exp: FastAlgExp) =
    if width = 1 then
        zeroLiteral 1
    else
        AppendExp [ UnaryExp(BitRangeOp(0, width - 2), exp); zeroLiteral 1 ]

let rec flattenNestedArithmetic exp =
    /// Multiplies an expression by -1: Positive <-> Negative
    let multiplyByMinusOne exp =
        match exp with
        | UnaryExp(NegOp, e) -> e
        | e -> UnaryExp(NegOp, e)

    match exp with
    | BinaryExp(left, AddOp, right) ->
        (flattenNestedArithmetic left)
        @ (flattenNestedArithmetic right)
    | BinaryExp(left, SubOp, right) ->
        let rhs =
            flattenNestedArithmetic right
            |> List.map multiplyByMinusOne
        (flattenNestedArithmetic left) @ rhs
    | UnaryExp(NotOp, e) ->
        let w = getAlgExpWidth e
        let minusOne = UnaryExp(NegOp, DataLiteral { Dat = Word 1u; Width = w })
        flattenNestedArithmetic
        <| BinaryExp(minusOne, SubOp, e)

    | _ -> [ exp ]

let assembleArithmetic width expLst =
    let rec assemble stateExp currentExp =
        match currentExp with
        | UnaryExp(NegOp, e) -> BinaryExp(stateExp, SubOp, e)
        | _ -> BinaryExp(stateExp, AddOp, currentExp)

    match expLst with
    | [] -> DataLiteral { Dat = Word 0u; Width = width }
    | [ exp ] -> exp
    | [ UnaryExp(NegOp, expN); exp ]
    | [ exp; UnaryExp(NegOp, expN) ] -> BinaryExp(exp, SubOp, expN)
    | [ exp1; exp2 ] -> BinaryExp(exp1, AddOp, exp2)
    | UnaryExp(NegOp, expN) :: exp :: tl
    | exp :: UnaryExp(NegOp, expN) :: tl ->
        (BinaryExp(exp, SubOp, expN), tl)
        ||> List.fold assemble
    | exp1 :: exp2 :: tl ->
        (BinaryExp(exp1, AddOp, exp2), tl)
        ||> List.fold assemble

let tryBitwiseOperation (expressions: FastAlgExp list) =
    match expressions with
    | [] -> failwithf "what? Expressions List should never be empty"
    | (BinaryExp(_, AddOp, _)) :: _
    | (BinaryExp(_, SubOp, _)) :: _ -> None
    | (BinaryExp(UnaryExp(BitRangeOp(_, _), left), bop, UnaryExp(BitRangeOp(_, _), right))) :: _ ->
        let widthL, widthR = getAlgExpWidth left, getAlgExpWidth right

        if widthL <> widthR || List.length expressions <> widthL then
            None
        else
            // AppendExp lists are MSB-first, so element i must be exactly bit (width-1-i)
            // of both operands: a permuted bit order is a different value and must not
            // collapse to the plain bus operation
            let allBitsInOrder =
                expressions
                |> List.mapi (fun i exp -> (widthL - 1 - i), exp)
                |> List.forall (fun (bitIndex, exp) ->
                    match exp with
                    | BinaryExp(UnaryExp(BitRangeOp(ll, lu), l), op, UnaryExp(BitRangeOp(rl, ru), r)) ->
                        ll = lu
                        && ll = rl
                        && rl = ru
                        && ll = bitIndex
                        && l = left
                        && r = right
                        && op = bop
                    | _ -> false)

            if allBitsInOrder then
                BinaryExp(left, bop, right) |> Some
            else
                None
    | _ -> None

/// Check the Bit Ranges for two expressions, and check if they can be merged.
/// The first range must be the MORE significant part, since AppendExp lists are MSB-first:
// A[5:3] then A[2:1] -> A[5:1]
// A[2:1] then A[5:3] -> None (a swapped-halves value, not a contiguous slice)
// A[5:4] then A[2:1] -> None
// A[5:3] then B[2:1] -> None
let tryMergeBitRanges (l1, u1, exp1) (l2, u2, exp2) =
    if exp1 = exp2 && l1 = u2 + 1 then
        UnaryExp(BitRangeOp(l2, u1), exp1) |> Some
    else
        None

let foldAppends (expressions: FastAlgExp list) =
    ([], expressions)
    ||> List.fold (fun acc exp ->
        match acc, exp with
        | [], e -> exp :: acc
        | (UnaryExp(BitRangeOp(l1, u1), exp0)) :: tl, UnaryExp(BitRangeOp(l2, u2), exp1) ->
            match tryMergeBitRanges (l1, u1, exp0) (l2, u2, exp1) with
            | Some newExp -> newExp :: tl
            | None -> exp :: acc
        | _, _ -> exp :: acc)
    |> List.rev

/// Converts an Algebraic Expression to a string for pretty printing
/// This function is now used for debugging purposes, if Katex doesn't work as expected, we can use this function again
/// by changing the name to expToKatex. -- 13/1/2025
// let expToString exp =
//     let rec expToString' (exp: FastAlgExp) =
//         match exp with
//         | SingleTerm(_, label, _) -> string label
//         | DataLiteral { Dat = Word w; Width = _ } -> string w
//         | DataLiteral { Dat = BigWord w; Width = _ } -> string w
//         | UnaryExp(NegOp, exp) ->
//             let expStr = expToString' exp
//             $"(-{expStr})"
//         | UnaryExp(NotOp, exp) ->
//             let expStr = expToString' exp
//             $"(~{expStr})"
//         | UnaryExp(BitRangeOp(low, up), exp) ->
//             let expStr = expToString' exp

//             if low = up then // Replace A[x:x] with A[x]
//                 $"{expStr}[{up}]"
//             else if getAlgExpWidth exp = (up - low + 1) then
//                 // Replace A[w-1:0] with A when A has width w
//                 expStr
//             else
//                 $"{expStr}[{up}:{low}]"
//         | UnaryExp(CarryOfOp, exp) ->
//             let expStr = expToString' exp
//             $"carry({expStr})"
//         | BinaryExp(exp1, AddOp, exp2) ->
//             // let expStr1 = expToString' exp1
//             // let expStr2 = expToString' exp2
//             // $"({expStr1}+{expStr2})"
//             $"({arithmeticToString exp})"
//         | BinaryExp(exp1, SubOp, exp2) ->
//             // let expStr1 = expToString' exp1
//             // let expStr2 = expToString' exp2
//             // $"({expStr1}-{expStr2})"
//             $"({arithmeticToString exp})"
//         | BinaryExp(exp1, BitAndOp, exp2) ->
//             let expStr1 = expToString' exp1
//             let expStr2 = expToString' exp2
//             $"({expStr1}&{expStr2})"
//         | BinaryExp(exp1, BitOrOp, exp2) ->
//             let expStr1 = expToString' exp1
//             let expStr2 = expToString' exp2
//             $"({expStr1}|{expStr2})"
//         | BinaryExp(exp1, BitXorOp, exp2) ->
//             let expStr1 = expToString' exp1
//             let expStr2 = expToString' exp2
//             $"({expStr1}⊕{expStr2})"
//         | ComparisonExp(exp, Equals, x) ->
//             let expStr = expToString' exp
//             $"({expStr} == {string x})"
//         | AppendExp exps ->
//             exps
//             |> List.map expToString'
//             |> String.concat "::"
//             |> (fun s -> $"({s})")

//     and arithmeticToString exp =
//         exp
//         |> flattenNestedArithmetic
//         |> List.mapi (fun i expr ->
//             match i, expr with
//             | 0, e -> expToString' e
//             | _, UnaryExp(NegOp, e) -> $"- {expToString' e}"
//             | _, e -> $"+ {expToString' e}")
//         |> String.concat " "

//     let expS = expToString' exp
//     // Remove the parentheses from the outermost expression
//     if expS.StartsWith "(" && expS.EndsWith ")" then
//         expS[1 .. (expS.Length - 2)]
//     else
//         expS

let rec expToKatex (exp: FastAlgExp) : string =
    let rec expToKatex' exp =
        match exp with
        | SingleTerm (_, ComponentLabel label, _) ->
            // Variable
            label

        | DataLiteral { Dat = Word w; Width = _ } ->
            // Num to string
            string w

        | DataLiteral { Dat = BigWord w; Width = _ } ->
            string w

        | UnaryExp (NegOp, e) ->
            // -
            sprintf "(-%s)" (expToKatex' e)

        | UnaryExp (NotOp, e) ->
            // not, ~, use \overline
            sprintf "\\overline{%s}" (expToKatex' e)

        | UnaryExp (BitRangeOp (low, up), e) ->
            // A[u:l] to A_{[u:l]}
            let baseStr = expToKatex' e
            if low = up then
                sprintf "%s_{[%d]}" baseStr up
            else
                sprintf "%s_{[%d:%d]}" baseStr up low

        | UnaryExp (CarryOfOp, e) ->
            sprintf "\\mathrm{carry}\\bigl(%s\\bigr)" (expToKatex' e)

        | BinaryExp (_, AddOp, _)
        | BinaryExp (_, SubOp, _) when getAlgExpWidth exp = 1 ->
            // at width 1 addition and subtraction are both XOR (and negation is the
            // identity), so print the chain with the XOR symbol
            exp
            |> flattenNestedArithmetic
            |> List.map (function
                | UnaryExp(NegOp, e) -> expToKatex' e
                | e -> expToKatex' e)
            |> String.concat " \\oplus "
            |> sprintf "\\bigl(%s\\bigr)"

        | BinaryExp (_, AddOp, _)
        | BinaryExp (_, SubOp, _) ->
            // flatten the whole Add/Sub chain so a - b - c renders without nested brackets
            sprintf "\\bigl(%s\\bigr)" (arithmeticToKatex exp)

        | BinaryExp (e1, BitAndOp, e2) ->
            // AND, \cdot
            sprintf "%s \\cdot %s" (expToKatex' e1) (expToKatex' e2)

        | BinaryExp (e1, BitOrOp, e2) ->
            // OR, +
            sprintf "%s + %s" (expToKatex' e1) (expToKatex' e2)

        | BinaryExp (e1, BitXorOp, e2) ->
            // XOR, \oplus
            sprintf "%s \\oplus %s" (expToKatex' e1) (expToKatex' e2)

        | ComparisonExp (e, Equals, x) ->
            // =
            sprintf "\\bigl(%s = %s\\bigr)" (expToKatex' e) (string x)

        | AppendExp exps ->
            exps
            |> List.map expToKatex'
            |> String.concat "\\Vert "
            |> sprintf "\\bigl(%s\\bigr)"

    and arithmeticToKatex exp =
        // change it to LaTeX
        exp
        |> flattenNestedArithmetic
        |> List.mapi (fun i expr ->
            match i, expr with
            | 0, e -> expToKatex' e
            | _, UnaryExp(NegOp, e) -> sprintf "- %s" (expToKatex' e)
            | _, e -> sprintf "+ %s" (expToKatex' e))
        |> String.concat " "

    let katexStr = expToKatex' exp

    // delete the outermost brackets, added only by the cases matched here (a prefix/suffix
    // check alone would corrupt e.g. \bigl(a+b\bigr) \cdot \bigl(c+d\bigr))
    match exp with
    | BinaryExp(_, AddOp, _)
    | BinaryExp(_, SubOp, _)
    | ComparisonExp _
    | AppendExp _ -> katexStr["\\bigl(".Length .. katexStr.Length - "\\bigr)".Length - 1]
    | UnaryExp(NegOp, _) -> katexStr[1 .. katexStr.Length - 2]
    | _ -> katexStr


/// Recursively evaluates an expression to reduce it to its simplest form
let rec evalExp exp =
    match exp with
    | SingleTerm _ -> exp
    | DataLiteral _ -> exp
    | UnaryExp(NotOp, exp) ->
        match evalExp exp with
        | UnaryExp(NotOp, inner) -> // Catch double inversion ~(~(A))
            evalExp inner
        | _ ->
            let evaluated = evalExp exp
            UnaryExp(NotOp, evaluated)
    | UnaryExp(NegOp, UnaryExp(NegOp, exp)) -> // Catch double negation -(-(A))
        match evalExp exp with
        | UnaryExp(NegOp, inner) -> evalExp inner
        | _ ->
            let evaluated = evalExp exp
            UnaryExp(NegOp, evaluated)
    | UnaryExp(op, exp) ->
        let evaluated = evalExp exp
        UnaryExp(op, evaluated)
    | BinaryExp(exp1, BitAndOp, exp2) ->
        let left = evalExp exp1
        let right = evalExp exp2

        match left, right with
        // Annulment: AND with 0 is always 0
        | _, DataLiteral { Dat = Word 0u; Width = w }
        | DataLiteral { Dat = Word 0u; Width = w }, _ -> DataLiteral { Dat = Word 0u; Width = w }
        | _, DataLiteral { Dat = BigWord z; Width = w }
        | DataLiteral { Dat = BigWord z; Width = w }, _ when z = 0I -> zeroLiteral w
        // Identity: AND with all-ones is always the other operand
        | exp, DataLiteral { Dat = Word n; Width = w }
        | DataLiteral { Dat = Word n; Width = w }, exp ->
            if n = uint32 (bigIntMask w) then
                exp
            else
                BinaryExp(left, BitAndOp, right)
        | exp, DataLiteral { Dat = BigWord n; Width = w }
        | DataLiteral { Dat = BigWord n; Width = w }, exp ->
            if n = bigIntMask w then
                exp
            else
                BinaryExp(left, BitAndOp, right)
        // Complement: A AND (NOT A) = 0, whichever side the NOT is on
        // (guarded so a non-complement NOT falls through to the later rules)
        | e1, UnaryExp(NotOp, e2)
        | UnaryExp(NotOp, e2), e1 when e1 = e2 -> zeroLiteral (getAlgExpWidth e1)
        // (A OR B) AND (A OR C) = A OR (B AND C)
        | BinaryExp(e1, BitOrOp, e2), BinaryExp(e3, BitOrOp, e4) ->
            if e1 = e3 then
                BinaryExp(e1, BitOrOp, BinaryExp(e2, BitAndOp, e4))
            else if e1 = e4 then
                BinaryExp(e1, BitOrOp, BinaryExp(e2, BitAndOp, e3))
            else if e2 = e3 then
                BinaryExp(e2, BitOrOp, BinaryExp(e1, BitAndOp, e4))
            else if e2 = e4 then
                BinaryExp(e2, BitOrOp, BinaryExp(e1, BitAndOp, e3))
            else
                BinaryExp(left, BitAndOp, right)
        | l, r ->
            // Idempotent: A AND A = A
            if l = r then
                l
            else
                BinaryExp(l, BitAndOp, r)
    | BinaryExp(exp1, BitOrOp, exp2) ->
        let left = evalExp exp1
        let right = evalExp exp2

        match left, right with
        // Identity: OR with 0 is always the other operand
        | exp, DataLiteral { Dat = Word 0u; Width = _ }
        | DataLiteral { Dat = Word 0u; Width = _ }, exp -> exp
        | exp, DataLiteral { Dat = BigWord z; Width = _ }
        | DataLiteral { Dat = BigWord z; Width = _ }, exp when z = 0I -> exp
        // Annulment: OR with all-ones is always all-ones
        | _, DataLiteral { Dat = Word n; Width = w }
        | DataLiteral { Dat = Word n; Width = w }, _ ->
            if n = uint32 (bigIntMask w) then
                DataLiteral { Dat = Word n; Width = w }
            else
                // rebuilt as an OR: this used to say BitAndOp, silently turning the
                // expression into an AND
                BinaryExp(left, BitOrOp, right)
        | _, DataLiteral { Dat = BigWord n; Width = w }
        | DataLiteral { Dat = BigWord n; Width = w }, _ ->
            if n = bigIntMask w then
                allOnesLiteral w
            else
                BinaryExp(left, BitOrOp, right)
        // Complement: A OR (NOT A) = all-ones, whichever side the NOT is on
        // (guarded so a non-complement NOT falls through to the later rules)
        | e1, UnaryExp(NotOp, e2)
        | UnaryExp(NotOp, e2), e1 when e1 = e2 -> allOnesLiteral (getAlgExpWidth e1)
        // Check for Carry from Full Adder
        // All combinations of: CIN&(A+B)|(A&B)
        | BinaryExp(c1, BitAndOp, BinaryExp(a1, AddOp, b1)), BinaryExp(a2, BitAndOp, b2)
        | BinaryExp(a2, BitAndOp, b2), BinaryExp(c1, BitAndOp, BinaryExp(a1, AddOp, b1))
        | BinaryExp(BinaryExp(a1, AddOp, b1), BitAndOp, c1), BinaryExp(a2, BitAndOp, b2)
        | BinaryExp(a2, BitAndOp, b2), BinaryExp(BinaryExp(a1, AddOp, b1), BitAndOp, c1) ->
            let a1Eval, a2Eval, b1Eval, b2Eval, c1Eval =
                evalExp a1, evalExp a2, evalExp b1, evalExp b2, evalExp c1

            if
                (a1Eval = a2Eval && b1Eval = b2Eval)
                || (a1Eval = b2Eval && a2Eval = b1Eval)
            then
                let addition = BinaryExp(c1Eval, AddOp, BinaryExp(a1Eval, AddOp, b1Eval))
                UnaryExp(CarryOfOp, addition)
            else
                BinaryExp(left, BitOrOp, right)
        | e1, BinaryExp(e2, BitAndOp, e3)
        | BinaryExp(e2, BitAndOp, e3), e1 ->
            // A OR (A AND B) = A
            if e1 = e2 || e1 = e3 then
                e1
            // A OR ((NOT A) AND B) = A OR B
            else if e1 = UnaryExp(NotOp, e2) then
                BinaryExp(e1, BitOrOp, e3)
            else if e1 = UnaryExp(NotOp, e3) then
                BinaryExp(e1, BitOrOp, e2)
            else
                BinaryExp(left, BitOrOp, right)

        | l, r ->
            // Idempotent: A OR A = A
            if l = r then
                l
            else
                BinaryExp(l, BitOrOp, r)
    | BinaryExp(exp1, BitXorOp, exp2) ->
        let left = evalExp exp1
        let right = evalExp exp2

        match left, right with
        // XOR with 0 is always the other operand
        | exp, DataLiteral { Dat = Word 0u; Width = _ }
        | DataLiteral { Dat = Word 0u; Width = _ }, exp -> exp
        | exp, DataLiteral { Dat = BigWord z; Width = _ }
        | DataLiteral { Dat = BigWord z; Width = _ }, exp when z = 0I -> exp
        // XOR with all-ones is always the inverse of the other operand
        | exp, DataLiteral { Dat = Word n; Width = w }
        | DataLiteral { Dat = Word n; Width = w }, exp ->
            if n = uint32 (bigIntMask w) then
                UnaryExp(NotOp, exp)
            else
                // XOR with a general constant is NOT an addition (carries differ):
                // keep it as an XOR
                BinaryExp(left, BitXorOp, right)
        | exp, DataLiteral { Dat = BigWord n; Width = w }
        | DataLiteral { Dat = BigWord n; Width = w }, exp ->
            if n = bigIntMask w then
                UnaryExp(NotOp, exp)
            else
                BinaryExp(left, BitXorOp, right)
        | l, r ->
            if l = r then
                // A XOR A = 0 at any width
                zeroLiteral (getAlgExpWidth l)
            elif getAlgExpWidth l = 1 && getAlgExpWidth r = 1 then
                // at width 1 XOR is exactly addition mod 2 (arithmetic is truncated to
                // width), which reduceArithmetic can fold and which lets the full-adder
                // carry rule in the OR branch recognise gate-built adders
                reduceArithmetic (BinaryExp(l, AddOp, r))
            else
                BinaryExp(l, BitXorOp, r)
    | BinaryExp(_, AddOp, _)
    | BinaryExp(_, SubOp, _) -> reduceArithmetic exp
    | ComparisonExp(exp, Equals, x) ->
        let evaluated = evalExp exp
        ComparisonExp(evaluated, Equals, x)
    | AppendExp exps ->
        let evaluated = List.map evalExp exps

        evaluated
        |> tryBitwiseOperation
        |> function
            | Some e -> e
            | None -> evaluated |> foldAppends |> AppendExp

and reduceArithmetic expression =
    let increment x = x + 1
    let decrement x = x - 1

    let updateExpCount exp (trackMap: Map<FastAlgExp, int>) action =
        match Map.tryFind exp trackMap with
        | Some count ->
            let newCount = action count
            Map.add exp newCount trackMap
        | None ->
            let newCount = action 0
            Map.add exp newCount trackMap

    let width = getAlgExpWidth expression
    let flatLst =
        flattenNestedArithmetic expression
        |> List.map evalExp

    let numVal, expCounts =
        ((0I, Map.empty<FastAlgExp, int>), flatLst)
        ||> List.fold (fun (numTrack, expTrack) expr ->
            match expr with
            | DataLiteral { Dat = Word w; Width = _ } -> (numTrack + bigint w), expTrack
            | UnaryExp(NegOp, DataLiteral { Dat = Word w; Width = _ }) -> (numTrack - bigint w), expTrack
            | UnaryExp(NegOp, e) ->
                let newExpTrack = updateExpCount e expTrack decrement
                numTrack, newExpTrack
            | _ ->
                let newExpTrack = updateExpCount expr expTrack increment
                numTrack, newExpTrack)

    // at width 1 arithmetic is mod 2, where -A = A and 2A = 0 (see doubleExp), so every
    // multiplicity reduces mod 2: this is what cancels A XOR A
    let expCounts =
        if width = 1 then
            expCounts |> Map.map (fun _ count -> ((count % 2) + 2) % 2)
        else
            expCounts

    // the numeric total truncated to width; sign is kept (except at width 1, where +1 is
    // preferred to -1) so that A - 3 renders as a subtraction rather than a large constant
    let constVal =
        let m = 1I <<< width
        if width = 1 then
            ((numVal % m) + m) % m
        else
            numVal % m

    let numDataExp =
        if constVal >= 0I then
            valueLiteral width constVal
        else
            UnaryExp(NegOp, valueLiteral width (-constVal))

    let expressionsToAssemble =
        expCounts
        |> Map.toList
        |> List.collect (fun (exp, count) ->
            if count = 0 then
                []
            else if count > 0 then
                [ for i in 1..count -> exp ]
            else
                [ for i in 1 .. (abs count) -> UnaryExp(NegOp, exp) ])
        |> fun l ->
            // append the literal only when it is non-zero after truncation, else a
            // constant total of 2^width would emit a spurious "- 0" term
            if constVal = 0I then
                l
            else
                l @ [ numDataExp ]

    assembleArithmetic width expressionsToAssemble

/// Raised when an Algebraic case is found in FastSim which has not been implemented,
/// or does not make sense to implement.
exception AlgebraNotImplemented of SimulationError

// Types that can be passed to and retrieved from the Fast Simulation
type FSInterface =
    | IData of FastData
    | IAlg of FastAlgExp



let fastBit (n: uint32) =
#if ASSERTS
    Helpers.assertThat (n < 2u) (sprintf "Can't convert %d to a single bit FastData" n)
#endif
    { Dat = Word n; Width = 1 }

let rec bitsToInt (lst: Bit list) =
    match lst with
    | [] -> 0u
    | x :: rest ->
        (if x = Zero then 0u else 1u)
        + (bitsToInt rest) * 2u

let rec bitsToBig (lst: Bit list) =
    match lst with
    | [] -> 0I
    | x :: rest ->
        (if x = Zero then 0I else 1I)
        + ((bitsToBig rest) <<< 1)

/// convert Wiredata to FastData equivalent
let rec wireToFast (wd: WireData) =
    let n = wd.Length

    let dat =
        if n <= 32 then
            Word(bitsToInt wd)
        else
            BigWord(bitsToBig wd)

    { Dat = dat; Width = n }

/// convert FastData to WireData equivalent
let rec fastToWire (f: FastData) =
    match f.Dat with
    | Word x ->
        [ 0 .. f.Width - 1 ]
        |> List.map (fun n ->
            if (x &&& (1u <<< n)) = 0u then
                Zero
            else
                One)
    | BigWord x ->
        [ 0 .. f.Width - 1 ]
        |> List.map (fun n ->
            if (x &&& bigIntBitMask n) = 0I then
                Zero
            else
                One)

let fastDataZero = { Dat = Word 0u; Width = 1 }
let fastDataOne = { Dat = Word 1u; Width = 1 }

let rec b2s (b: bigint) =
    let lsw = b &&& ((1I <<< 32) - 1I)
    let hex = $"%08x{uint32 lsw}"
    let msws = b >>> 32
    if msws <> 0I then
        b2s msws + hex
    else
        hex

/// Extract bit field (msb:lsb) from f. Bits are numbered little-endian from 0.
/// Note that for a single bit result the un-normalised version is used, so it will
/// be compatible with fast implementation of boolean logic.
let getBits (msb: int) (lsb: int) (f: FastData) =
    let outW = msb - lsb + 1
    let outWMask32 =
        if outW = 32 then
            0xFFFFFFFFu
        else
            ((1u <<< outW) - 1u)
#if ASSERTS
    Helpers.assertThat
        (msb <= f.Width - 1 && lsb <= msb && lsb >= 0)
        (sprintf "Bits selected out of range (%d:%d) from %A" msb lsb f)
#endif
    match f.Dat with
    | Word x ->
        let bits = (x >>> lsb) &&& outWMask32
        { Dat = Word bits; Width = outW }
    | BigWord x ->
        let mask = bigIntMask outW
        let bits = (x >>> lsb) &&& mask
        let dat =
            if outW <= 32 then
                if bits < 0I || bits >= (1I <<< 32) then
                    Log.warn $"""bits out of range for the width: bits = {bits.ToString("X")} mask = {mask} msb,lsb = ({msb},{lsb})"""

                Word((uint32 bits) &&& outWMask32)
            else
                BigWord(bits &&& bigIntMask outW)

        { Dat = dat; Width = outW }

let getBitsFromUInt32 (msb: int) (lsb: int) (x: uint32) =
    let outW = msb - lsb + 1
    let outWMask32 =
        if outW = 32 then
            0xFFFFFFFFu
        else
            ((1u <<< outW) - 1u)
// #if ASSERTS
//     Helpers.assertThat
//         (msb <= f.Width - 1 && lsb <= msb && lsb >= 0)
//         (sprintf "Bits selected out of range (%d:%d) from %A" msb lsb f)
// #endif
    let bits = (x >>> lsb) &&& outWMask32
    bits

let getBitsFromBigInt (msb: int) (lsb: int) (x: bigint) =
    let outW = msb - lsb + 1
// #if ASSERTS
//     Helpers.assertThat
//         (msb <= f.Width - 1 && lsb <= msb && lsb >= 0)
//         (sprintf "Bits selected out of range (%d:%d) from %A" msb lsb f)
// #endif
    let mask = bigIntMask outW
    let bits = (x >>> lsb) &&& mask
    bits &&& bigIntMask outW

let getBitsFromBigIntToUInt32 (msb: int) (lsb: int) (x: bigint) =
    let outW = msb - lsb + 1
    let outWMask32 =
        if outW = 32 then
            0xFFFFFFFFu
        else
            ((1u <<< outW) - 1u)
// #if ASSERTS
//     Helpers.assertThat
//         (msb <= f.Width - 1 && lsb <= msb && lsb >= 0)
//         (sprintf "Bits selected out of range (%d:%d) from %A" msb lsb f)
// #endif
    let mask = bigIntMask outW
    let bits = (x >>> lsb) &&& mask
    if bits < 0I || bits >= (1I <<< 32) then
        Log.warn $"""bits out of range for the width: bits = {bits.ToString("X")} mask = {mask} msb,lsb = ({msb},{lsb})"""

    (uint32 bits) &&& outWMask32

