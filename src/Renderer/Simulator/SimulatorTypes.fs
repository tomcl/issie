(*
    Types.fs

    This module collects a series of types used in the simulator logic.
*)

module rec SimulatorTypes

open Fable.Core
open CommonTypes

/// document current status of a simulation as used by waveform simulator
type SimulationRunStatus =
    | SimEmpty // simulation has been created but not yet setup from a circuit
    | SimOutOfDate // one of more of the sheets being simulated has changed after the simulation was setup
    | SimValidSameSheet
    /// simulation has run and is currently uptodate. The current sheet is being simulated
    | SimValidDifferentSheet // The simulation is uptodate, but a differnt sheet from the current one is being simulated
    | SimNoProject // there is no open project - this should not normally happen.

/// Binary data used in simulation
type Bit =
    | Zero
    | One

/// Fixed width bus data used in simulation
/// TODO: refactor as int64 or bigint for efficiency
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
    | RamState of Memory1

/// Message used to feed forward evaluation. Clock
/// tick => state changes to that in next cycle
type IsClockTick =
    | No
    | Yes of SimulationComponentState // Pass the state only for clock ticks.

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
      // Mapping from each input port number to its value (it will be set
      // during the simulation process).
      // TODO: maybe using a list would improve performance?
      Inputs: Map<InputPortNumber, WireData>
      // Mapping from each output port number to all of the ports and
      // Components connected to that port.
      Outputs: Map<OutputPortNumber, (ComponentId * InputPortNumber) list>
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
      State: SimulationComponentState }

/// Map every ComponentId to its SimulationComponent.
and SimulationGraph = Map<ComponentId, SimulationComponent>

/// This drives the generation of component outputs
/// it is processed by the Reducer function.
and ReducerInput =
    { Inputs: Map<InputPortNumber, WireData>
      CustomSimulationGraph: SimulationGraph option
      IsClockTick: IsClockTick }

/// When all inputs are available the reducer function will generate
/// these outputs. For custom components the SimulationGraph contains
/// embedded state.
and ReducerOutput =
    { Outputs: Map<OutputPortNumber, WireData> option
      NewCustomSimulationGraph: SimulationGraph option
      NewState: SimulationComponentState } // Will be saved only after clock ticks.

/// contains info needed to propagate wire value changes through a simulation.
and OutputChange = { CComp: SimulationComponent; COutputs: Map<OutputPortNumber, WireData> }

/// For every IO node, keep track of its Id, Label and wire width.
/// - Id: to feed values into the simulationGraph.
/// - Label: to display a nice form to the user.
/// - Width: to feed the right values into the simulation.
type SimulationIO = ComponentId * ComponentLabel * int

/// - Top level data tracking a simulation
type SimulationData =
    { FastSim: FastSimulation
      Graph: SimulationGraph
      // For each input/output, keep its Id and Label to easily access it.
      Inputs: SimulationIO list
      Outputs: SimulationIO list
      // Whether the graph contains synchronous logic.
      IsSynchronous: bool
      // The base that should be used to display numbers in the simulation.
      NumberBase: NumberBase
      // Keep track of the number of clock ticks of the simulation.
      ClockTickNumber: int }

let graph_ = Optics.Lens.create (fun a -> a.Graph) (fun s a -> {a with Graph = s})
let fastSim_ = Optics.Lens.create (fun a -> a.FastSim) (fun s a -> {a with FastSim = s})
let numberBase_ = Optics.Lens.create (fun a -> a.NumberBase) (fun s a -> {a with NumberBase = s})
let clockTickNumber_ = Optics.Lens.create (fun a -> a.ClockTickNumber) (fun s a -> {a with ClockTickNumber = s})

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
let errMsg (errType: SimulationErrorType) =
    match errType with
    | PortNumMissing correctType ->
        sprintf "%A port appears to have no port number" correctType
    | WrongPortType (correctType, port) ->
        sprintf "%A port %d appears to be an %A port" correctType (Option.get port.PortNumber) port.PortType 
    | ConnTypeHasNum (correctType, portNum) ->
        sprintf "%A port appears to have a port number: %d" correctType portNum
    | LabelConnect ->
        sprintf
            "You can't connect two Wire Labels with a wire. Delete the connecting wire. If you want to join two bus labels \
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
            "Sheet %s is used as a custom component. Instance In ports: %A are different from Component In ports: %A."
            compName
            instIns
            compIns
    | OutPortMismatch (compName, instOuts, compOuts) ->
        sprintf
            "Sheet %s is used as a custom component. Instance Out ports: %A are different from Component Out ports: %A."
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
            sprintf "%d" count
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
        sprintf "\nInternal ERROR in Issie fast simulation: %A\n\n%A\n" e.Message e.StackTrace
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
    static member inline MakeFastData (width: int) (data: bigint) =
        match width with
        | w when w <= 32 && data >= bigint 0 -> { Dat = Word(uint32 data); Width = w }
        | w when w <= 32 && data < bigint 0 ->
            let data = data % (bigint 2 ** w)
            { Dat = Word(uint32 data); Width = w }
        | w -> { Dat = BigWord data; Width = w }

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

// Type for algebraic expressions in Issie
type FastAlgExp =
    | SingleTerm of SimulationIO
    | DataLiteral of FastData
    | UnaryExp of Op: UnaryOp * Exp: FastAlgExp
    | BinaryExp of Exp1: FastAlgExp * Op: BinaryOp * Exp2: FastAlgExp
    | ComparisonExp of Exp: FastAlgExp * Op: ComparisonOp * uint32
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

// /// Converts multiple AppendOps which would usually be nested inside one another
// /// to a list of expressions for easier evaluation and clearer printing
// let rec flattenNestedAppends exp =
//     match exp with
//     | BinaryExp (left,AppendOp,right) ->
//         (flattenNestedAppends left) @ (flattenNestedAppends right)
//     | _ -> [exp]

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
    | (BinaryExp(_, AddOp, _)) :: tl
    | (BinaryExp(_, SubOp, _)) :: tl -> None
    | (BinaryExp(UnaryExp(BitRangeOp(_, _), left), bop, UnaryExp(BitRangeOp(_, _), right))) :: tl ->
        let rec checkList exps state remBits =
            match (exps: FastAlgExp list), state with
            | [], s -> s, remBits
            | hd :: tl, false -> checkList tl false remBits
            | (BinaryExp(UnaryExp(BitRangeOp(ll, lu), l), op, UnaryExp(BitRangeOp(rl, ru), r))) :: tl, true ->
                if
                    ll = lu
                    && ll = rl
                    && rl = ru
                    && l = left
                    && r = right
                    && op = bop
                then
                    let newRemBits = List.except [ ll ] remBits
                    checkList tl true newRemBits
                else
                    checkList tl false remBits
            | _ :: tl, s -> checkList tl false remBits

        let widthL, widthR = getAlgExpWidth left, getAlgExpWidth right

        if widthL <> widthR then
            None
        else
            let remBits = [ 0 .. (widthL - 1) ]

            match checkList expressions true remBits with
            | true, [] -> BinaryExp(left, bop, right) |> Some
            | _, _ -> None
    | _ -> None

/// Check the Bit Ranges for two expressions, and check if they can be merged.
/// If they can, return the merged expression, otherwise return None.
// A[5:3] and A[2:1] -> A[5:1]
// A[5:4] and A[2:1] -> None
// A[5:3] and B[2:1] -> None
let tryMergeBitRanges (l1, u1, exp1) (l2, u2, exp2) =
    let lHigh, lLow = if l1 > l2 then l1, l2 else l2, l1
    let uHigh, uLow = if u1 > u2 then u1, u2 else u2, u1

    if exp1 = exp2 && lHigh = uLow + 1 then
        UnaryExp(BitRangeOp(lLow, uHigh), exp1) |> Some
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
let expToString exp =
    let rec expToString' (exp: FastAlgExp) =
        match exp with
        | SingleTerm(_, label, _) -> string label
        | DataLiteral { Dat = Word w; Width = _ } -> string w
        | DataLiteral { Dat = BigWord w; Width = _ } -> string w
        | UnaryExp(NegOp, exp) ->
            let expStr = expToString' exp
            $"(-{expStr})"
        | UnaryExp(NotOp, exp) ->
            let expStr = expToString' exp
            $"(~{expStr})"
        | UnaryExp(BitRangeOp(low, up), exp) ->
            let expStr = expToString' exp

            if low = up then // Replace A[x:x] with A[x]
                $"{expStr}[{up}]"
            else if getAlgExpWidth exp = (up - low + 1) then
                // Replace A[w-1:0] with A when A has width w
                expStr
            else
                $"{expStr}[{up}:{low}]"
        | UnaryExp(CarryOfOp, exp) ->
            let expStr = expToString' exp
            $"carry({expStr})"
        | BinaryExp(exp1, AddOp, exp2) ->
            // let expStr1 = expToString' exp1
            // let expStr2 = expToString' exp2
            // $"({expStr1}+{expStr2})"
            $"({arithmeticToString exp})"
        | BinaryExp(exp1, SubOp, exp2) ->
            // let expStr1 = expToString' exp1
            // let expStr2 = expToString' exp2
            // $"({expStr1}-{expStr2})"
            $"({arithmeticToString exp})"
        | BinaryExp(exp1, BitAndOp, exp2) ->
            let expStr1 = expToString' exp1
            let expStr2 = expToString' exp2
            $"({expStr1}&{expStr2})"
        | BinaryExp(exp1, BitOrOp, exp2) ->
            let expStr1 = expToString' exp1
            let expStr2 = expToString' exp2
            $"({expStr1}|{expStr2})"
        | BinaryExp(exp1, BitXorOp, exp2) ->
            let expStr1 = expToString' exp1
            let expStr2 = expToString' exp2
            $"({expStr1}⊕{expStr2})"
        | ComparisonExp(exp, Equals, x) ->
            let expStr = expToString' exp
            $"({expStr} == {string x})"
        | AppendExp exps ->
            exps
            |> List.map expToString'
            |> String.concat "::"
            |> (fun s -> $"({s})")

    and arithmeticToString exp =
        exp
        |> flattenNestedArithmetic
        |> List.mapi (fun i expr ->
            match i, expr with
            | 0, e -> expToString' e
            | _, UnaryExp(NegOp, e) -> $"- {expToString' e}"
            | _, e -> $"+ {expToString' e}")
        |> String.concat " "

    let expS = expToString' exp
    // Remove the parentheses from the outermost expression
    if expS.StartsWith "(" && expS.EndsWith ")" then
        expS[1 .. (expS.Length - 2)]
    else
        expS

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
        | exp, DataLiteral { Dat = Word 0u; Width = w }
        | DataLiteral { Dat = Word 0u; Width = w }, exp -> DataLiteral { Dat = Word 0u; Width = w }
        // Identity: AND with 1 is always the other operand
        | exp, DataLiteral { Dat = Word n; Width = w }
        | DataLiteral { Dat = Word n; Width = w }, exp ->
            let one = uint32 <| (2.0 ** w) - 1.0
            if n = one then
                exp
            else
                BinaryExp(left, BitAndOp, right)
        // Complement: A AND (NOT A) = 0
        | e1, UnaryExp(NotOp, e2) ->
            if e1 = e2 then
                let w = getAlgExpWidth e1
                DataLiteral { Dat = Word 0u; Width = w }
            else
                BinaryExp(left, BitAndOp, right)
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
        printfn "Or Case Matched"
        let left = evalExp exp1
        let right = evalExp exp2

        match left, right with
        // Identity: OR with 0 is always the other operand
        | exp, DataLiteral { Dat = Word 0u; Width = _ }
        | DataLiteral { Dat = Word 0u; Width = _ }, exp -> exp
        // Annulment: OR with 1 is always 1
        | exp, DataLiteral { Dat = Word n; Width = w }
        | DataLiteral { Dat = Word n; Width = w }, exp ->
            let one = uint32 <| (2.0 ** w) - 1.0

            if n = one then
                DataLiteral { Dat = Word one; Width = w }
            else
                BinaryExp(left, BitAndOp, right)
        // Complement: A OR (NOT A) = 1
        | e1, UnaryExp(NotOp, e2) ->
            if e1 = e2 then
                let w = getAlgExpWidth e1
                DataLiteral { Dat = Word 1u; Width = w }
            else
                BinaryExp(left, BitOrOp, right)
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
        // XOR with 1 is always the inverse of the other operand
        | exp, DataLiteral { Dat = Word n; Width = w }
        | DataLiteral { Dat = Word n; Width = w }, exp ->
            let one = uint32 <| (2.0 ** w) - 1.0

            if n = one then
                UnaryExp(NotOp, exp)
            else
                reduceArithmetic (BinaryExp(exp, AddOp, DataLiteral { Dat = Word n; Width = w }))
        | l, r ->
            if getAlgExpWidth l = 1 && getAlgExpWidth r = 1 then
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
        ((0, Map.empty<FastAlgExp, int>), flatLst)
        ||> List.fold (fun (numTrack, expTrack) expr ->
            match expr with
            | DataLiteral { Dat = Word w; Width = _ } -> (numTrack + (int w)), expTrack
            | UnaryExp(NegOp, DataLiteral { Dat = Word w; Width = _ }) -> (numTrack - (int w)), expTrack
            | UnaryExp(NegOp, e) ->
                let newExpTrack = updateExpCount e expTrack decrement
                numTrack, newExpTrack
            | _ ->
                let newExpTrack = updateExpCount expr expTrack increment
                numTrack, newExpTrack)

    let numDataExp =
        numVal % (int (2. ** width))
        |> fun n ->
            if n > 0 then
                DataLiteral { Dat = Word(uint32 n); Width = width }
            else
                UnaryExp(NegOp, DataLiteral { Dat = Word(uint32 <| abs n); Width = width })

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
            if numVal = 0 then
                l
            else
                l @ [ numDataExp ]

    printfn "To Assemble: %A" (List.map expToString expressionsToAssemble)

    assembleArithmetic width expressionsToAssemble

/// Raised when an Algebraic case is found in FastSim which has not been implemented,
/// or does not make sense to implement.
exception AlgebraNotImplemented of SimulationError

// Types that can be passed to and retrieved from the Fast Simulation
type FSInterface =
    | IData of FastData
    | IAlg of FastAlgExp

//------------------------------------------------------------------------------//
//-------------------EXPERIMENTAL - new data structure to replace WireData------//
//------------------------------------------------------------------------------//

type BitInt = uint32 array

let getBIBit (bits: BitInt) (pos: int) : uint32 = bits[pos / 32] >>> (pos % 32)

/// get a field of bits width 'width' offset by 'offset', return the field with 0 offset
let inline getUpperField (x: uint32) (width: int) (offset: int) : uint32 =
    (x >>> offset) &&& ((1u <<< width) - 1u)

/// get the lower 'width' bits, return then offset by 'offset' bits
let inline getLowerField (x: uint32) (width: int) (offset: int) : uint32 =
    (x &&& ((1u <<< width) - 1u)) <<< offset

let getBIBitsInt (bits: BitInt) (msb: int) (lsb: int) : uint32 =
    let width = msb - lsb + 1

    if width < 32 then
        let lowerWord = bits[lsb / 32]
        let offset = lsb % 32
        let lowerChunk = (lowerWord >>> offset)

        if offset + width <= 32 then
            // output from only one word
            lowerChunk
        else
            // one word output from two words of source
            let upperChunk = getLowerField bits[lsb / 32 + 1] (width - offset - 32) offset
            lowerChunk ||| upperChunk
    else
        failwithf "Cannot extract bits {msb}..{lsb} as a single 32 bit word"

let getBIBits (bits: BitInt) (msb: int) (lsb: int) : BitInt =
    let lsw = lsb / 32
    let outWidth = msb - lsb + 1
    let msw = msb / 32
    let offset = lsb % 32

    if offset = 0 then
        bits[msw..lsw]
    else
        let outWords = outWidth / 32 + 1

        Array.init outWords (fun n ->
            match n with
            | n when n + lsw = msw -> getLowerField bits[msw] (outWidth - offset % 32) offset
            | n ->
                getLowerField bits[n + lsw + 1] (32 - offset) offset
                ||| getUpperField bits[n + lsw] offset offset)

let floatCarryBit = Seq.init 32 (fun _ -> 2.) |> Seq.reduce (*)

let addBIBits (bits1: BitInt) (bits2: BitInt) (cin: uint32) : BitInt * uint32 =
    let mutable tempCarry = if cin = 1u then floatCarryBit else 0.

    let outs =
        Array.init bits1.Length (fun n ->
            tempCarry <-
                float bits1[n]
                + float bits2[n]
                + (if tempCarry >= floatCarryBit then
                       1.
                   else
                       0.)

            uint32 tempCarry)

    outs,
    (if tempCarry >= floatCarryBit then
         1u
     else
         0u)

let binopBIBits (op: uint32 -> uint32 -> uint32) (bits1: BitInt) (bits2: BitInt) : BitInt =
    Array.init bits1.Length (fun n -> op bits1[n] bits2[n])

/// invert bits1: assuming that width is the bit width of bits1
/// MS bits not used by bits1 are not inverted.
let invertBIBits (bits: BitInt) (width: int) =
    let msw = width / 32

    Array.init bits.Length (fun n ->
        let x = bits[n]

        if n = msw then
            x &&& ((1u <<< width % 32) - 1u)
        else
            x ^^^ 0xFFFFFFFFu)

/// append bits2 on MSB side of bits1
let appendBIBits ((bits1: BitInt, width1: int)) ((bits2: BitInt, width2: int)) =
    let outWidth = width1 + width2
    let outMSW = outWidth / 32
    let offset = width1 % 32
    let msw1 = width1 / 32

    if offset = 0 then
        // we can do straight array append
        Array.append bits1 bits2
    elif outMSW = width1 / 32 then
        // the added bits can be put in the existing MSW of width1
        let out = Array.copy bits1
        out[outMSW] <-
            out[outMSW]
            ||| getLowerField bits2[0] width2 offset
        out
    else
        Array.init (outMSW + 1) (fun n ->
            match n with
            | _ when n = outMSW ->
                getLowerField bits2[n - msw1] (32 - offset) offset
                ||| getUpperField bits2[n - msw1 + 1] (offset + outWidth - 32) offset

            | _ when n = width1 / 32 ->
                getLowerField bits1[n - width1 % 32] (32 - offset) offset
                ||| getUpperField bits2[n - width1 / 32 + 1] offset offset
            | _ when n >= width1 / 32 ->
                getLowerField bits2[n - width1 / 32] (32 - offset) offset
                ||| getUpperField bits2[n - width1 / 32 + 1] offset offset
            | _ -> bits1[n])

let bigIntMaskA =
    [| 0..128 |]
    |> Array.map (fun width -> (1I <<< width) - 1I)

let bigIntBitMaskA =
    [| 0..128 |]
    |> Array.map (fun width -> (bigint 1 <<< width))

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
    | [] -> bigint 0
    | x :: rest ->
        (if x = Zero then bigint 0 else bigint 1)
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
            if (x &&& bigIntBitMask n) = bigint 0 then
                Zero
            else
                One)

let fastDataZero = { Dat = Word 0u; Width = 1 }
let fastDataOne = { Dat = Word 1u; Width = 1 }

let rec b2s (b: bigint) =
    let lsw = b &&& ((bigint 1 <<< 32) - bigint 1)
    let hex = $"%08x{uint32 lsw}"
    let msws = b >>> 32
    if msws <> bigint 0 then
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
        //printfn $"lsb={lsb},msb={msb},outW={outW}, mask={b2s mask}, x={b2s x},x/lsb = {b2s(x >>> lsb)} bits={b2s bits}, bits=%x{uint32 bits}"
        let dat =
            if outW <= 32 then
                if bits < 0I || bits >= (1I <<< 32) then
                    printf $"""HELP! weird bits = {bits.ToString("X")} mask = {mask} msb,lsb = ({msb},{lsb})"""

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
    //printfn $"lsb={lsb},msb={msb},outW={outW}, mask={b2s mask}, x={b2s x},x/lsb = {b2s(x >>> lsb)} bits={b2s bits}, bits=%x{uint32 bits}"
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
    //printfn $"lsb={lsb},msb={msb},outW={outW}, mask={b2s mask}, x={b2s x},x/lsb = {b2s(x >>> lsb)} bits={b2s bits}, bits=%x{uint32 bits}"
    if bits < 0I || bits >= (1I <<< 32) then
        printf $"""HELP! weird bits = {bits.ToString("X")} mask = {mask} msb,lsb = ({msb},{lsb})"""

    (uint32 bits) &&& outWMask32

//---------------------------------------------------------------------------------------//
//--------------------------------Fast Simulation Data Structure-------------------------//
//---------------------------------------------------------------------------------------//

// type FComponentId = ComponentId * ComponentId list moved to CommonTypes

type FData =
    | Data of FastData
    | Alg of FastAlgExp

    member this.Width =
        match this with
        | Data d -> d.Width
        | Alg exp -> getAlgExpWidth exp

    member this.fdToString =
        match this with
        | Data { Dat = Word w; Width = _ } -> string w
        | Data { Dat = BigWord w; Width = _ } -> w.ToString()
        | Alg exp -> expToString exp

    member this.toExp =
        match this with
        | Alg exp -> exp
        | Data fd -> DataLiteral fd

    member this.toFastData =
        match this with
        | Data fd -> fd
        | _ -> failwithf "Expected data, found Alg FData"

    member this.toInt64 =
        match this with
        | Data { Width = w } when w > 64 -> failwithf "Help! Can't convert numbers wider than 64 bits to int64."
        | Data { Dat = Word w } -> int64 (uint64 w)
        | Data { Dat = BigWord w } -> int64 (uint64 w)

        | _ -> failwithf "Can't convert Alg style FData to int64"

/// Wrapper to allow arrays to be resized for longer simulations while keeping the links between inputs
/// and outputs
type StepArray<'T> = { Step: 'T array; Index: int }

// [<Struct>] // TODO - check whether fable optimzed Struct

/// This type represents an array of time steps of simulation data.
/// In any simulation, for a given IOArray, only one of the three 'Step' arrays will be used.
/// For (very strong) efficiency reasons this cannot be implemented as a disjoint union:
/// the code that reads and writes IOArray array elements will access the appropriate array.
/// Truthtable simulations use FDataStep everywhere.
/// Normal simulations use UInt32step or BigIntStep according to the size of the relevant bus.
type IOArray =
    { FDataStep: FData array
      UInt32Step: uint32 array
      BigIntStep: bigint array
      Width: int
      Index: int }

/// Used for efficiency reasons.
/// For a given normal simulation these arrays show whether the corresponding
/// component input or output is a bigint or a unint32 type bus, and therefore
/// show IOArray array is used for the data.
type BigIntState =
    { InputIsBigInt: bool array // NOTE - whether each input uses BigInt or UInt32
      OutputIsBigInt: bool array }

/// FastComponent represents a physical component in a simulation. Because sheets can be
/// instantiated in multiple places a given sheet component can have multiple FastComponents
/// in the simulation.
/// Arrays on FastComponent are filled up with simulation data per clock step as a clocked
/// simulation progresses.
type FastComponent =
    {
      /// contains component path to root of simulation - unique
      fId: FComponentId
      /// allows access to the underlying component
      cId: ComponentId
      /// convenience access to the Type of the underlying component
      FType: ComponentType
      /// Used only by clocked components, contains an array of the component state in
      /// every clock cycle. Filled as simulation progresses.
      State: StepArray<SimulationComponentState> option
      mutable Active: bool
      /// Most components have all bus inputs and outputs the same width. This gives the
      /// default array field to use - BigIntStep or UInt32Step - in IOArray.
      mutable UseBigInt: bool
      /// components that may have variable inputs and output widths use this instead of UseBigInt to
      /// determine the correct array.
      mutable BigIntState: BigIntState option // This is only used for components that have variable input/output widths
      /// Input data - this an array of fxed links to the relevant driver output data arrays
      InputLinks: IOArray array
      /// info on where the drivers are for each input
      InputDrivers: (FComponentId * OutputPortNumber) option array
      /// the output data for this component (this gets linked to all the conmponents driven
      Outputs: IOArray array
      /// the legacy SimulationConmponent from which this FastComponent is generated.
      SimComponent: SimulationComponent
      /// Path from thsi component to root of simulation, if it is in a subsheet.
      AccessPath: ComponentId list
      /// for human use: long name of component
      FullName: string
      /// label of component
      FLabel: string
      SheetName: string list
      // these fields are used only to determine component ordering for correct evaluation
      mutable Touched: bool // legacy field
      mutable DrivenComponents: FastComponent list
      mutable NumMissingInputValues: int
      // these fields are used only by the Verilog output code
      mutable VerilogOutputName: string array
      mutable VerilogComponentName: string }
    /// Number of component inputs
    member inline this.InputWidth(n) = this.InputLinks[n].Width
    /// Number of component outputs
    member inline this.OutputWidth(n) = this.Outputs[n].Width
    /// Get the uint32 data array for a given input
    member inline this.GetInputUInt32 (epoch) (InputPortNumber n) = this.InputLinks[n].UInt32Step[epoch]
    /// Get the BigInt data array for a given input
    member inline this.GetInputBigInt (epoch) (InputPortNumber n) = this.InputLinks[n].BigIntStep[epoch]
    /// Get the FData array for a given input
    member inline this.GetInputFData (epoch) (InputPortNumber n) = this.InputLinks[n].FDataStep[epoch]
    /// for debugging - get a short usually unique truncation of the fId
    member this.ShortId =
        let (ComponentId sid, ap) = this.fId
        (EEExtensions.String.substringLength 0 5 sid)
    /// write data to the Unint32Step output array for the given time step (epoch) and output (n)
    member inline this.PutOutputUInt32 (epoch) (OutputPortNumber n) dat =
        this.Outputs[n].UInt32Step[ epoch ] <- dat
    /// write data to the BigIntStep output array for the given time step (epoch) and output (n)
    member inline this.PutOutputBigInt (epoch) (OutputPortNumber n) dat =
        this.Outputs[n].BigIntStep[ epoch ] <- dat
    /// write data to the FData output array for the given time step (epoch) and output (n)
    member inline this.PutOutputFData (epoch) (OutputPortNumber n) dat =
        this.Outputs[n].FDataStep[ epoch ] <- dat
    member inline this.Id = this.SimComponent.Id
    member inline this.SubSheet = this.SheetName[0 .. this.SheetName.Length - 2]

/// Convenience array used so that waveform simulation can access
/// component outputs (drivers) without a Map lookup
type Driver =
    {
      /// Index of this driver in the array or drivers
      Index: int
      /// Bus width of the driven bus
      DriverWidth: int
      /// Simulation data for the driven bus
      DriverData: IOArray }

/// Type used to tie component ports to simulation data
/// for advanved wavefor simulation features.
type SheetPort = {
    Sheet: string;
    PortOnComp: Port
    } // must include port number (which ports on connections do not)

// The fast simulation components are similar to the issie components they are based on but with addition of arrays
// for direct lookup of inputs and fast access of outputs. The input arrays contain pointers to the output arrays the
// inputs are connected to, the InputPortNumber integer indexes this.
// In addition outputs are contained in a big array indexed by epoch (simulation time). This allows results for multiple
// steps to begin built efficiently and also allows clocked outputs for the next cycle to be constructed without overwriting
// previous outputs needed for that construction.
//
// For reasons of efficiency Issie's list-style WireData type is optimised by using integers as bit arrays.
//
// For ease of implementation Input and Output components are given a single output (input) port not present on issie.
// this allows sub-sheet I/Os to be linked as normal in the constructed graph via their respective Input and Output connections.
//
// Although keeping input and output connections in the new graph is slightly less efficient it makes things simpler because there is a
// 1-1 connection between components (except for custom components which are removed by the gathering process).
// Note that custom component info is still kept because each component in the graph has a path - the list of custom component ids
// between its graph and root. Following issie this does not include a custom component for the sheet being simulated, which is viewed as
// root. Since custom components have been removed this no longer complicates the simulation.
type FastSimulation =
    {
        // last step number (starting from 0) which is simulated.
        mutable ClockTick: int
        // Maximum size of simulation arrays - after which they form a circular buffer
        MaxArraySize: int
        // top-level inputs to the simulation
        FGlobalInputComps: FastComponent array
        // constants
        FConstantComps: FastComponent array
        // clocked components
        FClockedComps: FastComponent array
        // Components that will be reduced in order allowing sequential reduction to implement simulation
        FOrderedComps: FastComponent array
        // which is the active component for each set of labels?
        mutable FIOActive: Map<ComponentLabel * ComponentId list, FastComponent>
        // list of deferred links driven from inactive IOlabls - at end of linkage the
        // corresponding active IOLabel can be substituted as driver an dthe link made
        mutable FIOLinks: ((FastComponent * InputPortNumber) * FastComponent) list
        // Fast components: this array is longer than FOrderedComps because it contains
        // IOlabel components that are redundant in the simulation
        FComps: Map<FComponentId, FastComponent>
        FCustomComps: Map<FComponentId, FastComponent>
        // Fast components: this map is longer than FComps because it contains
        // Custom components not used by Fast simulation but needed in Waveform simulation
        WaveComps: Map<FComponentId, FastComponent>
        FSComps: Map<FComponentId, SimulationComponent * ComponentId list>
        // look up from output port of custom component to the relevant Output component
        FCustomOutputCompLookup: Map<(ComponentId * ComponentId list) * OutputPortNumber, FComponentId>
        // GatherData from which this simulation was made
        G: GatherData
        // Total number of step arrays (= drivers)
        NumStepArrays: int
        // Each driver represents one output with its data
        Drivers: Driver option array
        // Each wave index represents one component port with associated driver and data
        WaveIndex: WaveIndexT array
        /// Connections on simulated sheets indexed by directly connected port. Each connection appears twice.
        ConnectionsByPort: Map<SheetPort, Connection list>
        /// This contains all components in the sheets of the simulation indexed by ComponentId.
        /// Subsheet components are indexed only once.
        /// Contrast this with Fast Components - which have the design expanded out with
        /// one per instance: the different versions correspond to distinct access paths.
        ComponentsById: Map<string, Map<ComponentId, Component>>
        /// Circuit simulated (sheet and all dependencies)
        SimulatedCanvasState: LoadedComponent list
        /// The root sheet being simulated
        SimulatedTopSheet: string
    }

/// GatherTemp is the output type used to accumulate lists of data links when recursively exploring SimulationGraph
/// as first step in flattening it.
/// Each list of pairs is converted into a map at the end in the final GatherData structure
/// The cost of creating maps makes it important to use lists here as the intermediate structures
and GatherTemp =
    {
      // Links Custom Component Id and input port number to corresponding Input
      // Component Id (of an Input component which is not top-level)
      CustomInputCompLinksT: ((FComponentId * InputPortNumber) * FComponentId) list
      // Links (non-top-level) Output component Id to corresponding Custom Component Id & output port number
      CustomOutputCompLinksT: (FComponentId * (FComponentId * OutputPortNumber)) list
      // Shortcut to find the label of a component; notice that the access path is not needed here because
      // Labels of the graph inside a custom component are identical for different instances of the component
      Labels: (ComponentId * string) list
      // This indexes the SimulationGraph components from Id and access path. Note that the same simulation
      // component Id can appear with different access paths if a sheet is instantiated more than once.
      // Each entry corresponds to a single FastComponent.
      // Note that Custom components are not included in this list.
      AllCompsT: ((ComponentId * ComponentId list) * (SimulationComponent * ComponentId list)) list } // links to component and its path in the graph

and GatherData =
    {
      // Existing Issie data structure representing circuit for simulation - generated by runCanvasStateChecksAndBuildGraph
      Simulation: SimulationGraph
      // Maps Custom Component Id and input port number to corresponding Input
      // Component Id (of an Input component which is not top-level)
      CustomInputCompLinks: Map<FComponentId * InputPortNumber, FComponentId>
      // Maps (non-top-level) Output component Id to corresponding Custom Component Id & output port number
      CustomOutputCompLinks: Map<FComponentId, FComponentId * OutputPortNumber>
      // Maps custom component output to corresponding output FastComponent.
      // Inverse of CustomOutputCompLinks
      CustomOutputLookup: Map<FComponentId * OutputPortNumber, FComponentId>
      // Shortcut to find the label of a component; notice that the access path is not needed here because
      // Labels of the graph inside a custom component are identical for different instances of the component
      Labels: Map<ComponentId, string>
      // This indexes the SimulationGraph components from Id and access path. Note that the same simulation
      // component Id can appear with different access paths if a sheet is instantiated more than once.
      // Each entry corresponds to a single FastComponent.
      // Note that Custom components are not included in this list.
      AllComps: Map<ComponentId * ComponentId list, SimulationComponent * ComponentId list> } // maps to component and its path in the graph

    member this.getFullName(cid, ap) =
        List.map
            (fun cid ->
                match Map.tryFind cid this.Labels with
                | Some lab -> lab
                | None -> "*")
            (ap @ [ cid ])
        |> String.concat "."

    member this.getSheetName(cid, ap) =
        List.map
            (fun cid ->
                match Map.tryFind cid this.Labels with
                | Some lab -> lab.ToUpper()
                | None -> "*")
            (ap @ [ cid ])

//-------------------------------------------------------------------------------------//
//-------------------Helper functions for simulation types-----------------------------//
//-------------------------------------------------------------------------------------//

let sprintSimComponent (sComp: SimulationComponent) =
    sprintf "'%A': %20s" sComp.Label (sComp.Type.ToString() |> Helpers.sprintInitial 20)

let shortPSComp (comp: SimulationComponent) =
    let lab =
        match comp.Label with
        | ComponentLabel lab' -> lab'

    match comp.Type with
    | Custom sc -> sprintf "%s:Custom.(%s.%A->%A)" lab sc.Name sc.InputLabels sc.OutputLabels
    | _ -> sprintf "%s:%A.{%A}" lab comp.Type comp.State

let printSimGraph (sg: SimulationGraph) =
    printfn
        "%s"
        (String.concat
            "\n"
            (sg
             |> Map.toList
             |> List.map (fun (ComponentId id, comp) -> sprintSimComponent comp + id)))

let tryGetCompLabel (compId: ComponentId) (sg: SimulationGraph) =
    Map.tryPick (fun k v -> if k = compId then Some v else None) sg
    |> Option.map (fun comp -> comp.Label)
    |> Option.map (fun (ComponentLabel s) -> s)
    |> Option.defaultValue "'Not in SimGraph'"

let extractLabel (label: ComponentLabel) =
    let (ComponentLabel name) = label
    name

//-------------------------------------------------------------------------------------//
//-------------------Helper functions for WaveformSim----------------------------------//
//-------------------------------------------------------------------------------------//

// NB - all the NetGroup functions assume a working netlist in which NO NET IS UNDRIVEN
// Every Net must be driven by exactly one componnet output port (NLSource).
// IOLabels are nout counted as drivers themselves; every group of same label IOlabels
// and all of their output nets
// makes a netgroup which must be driven by just one NLSource (connected to one of the IOLabel inputs).
// every net is therefore part of one netgroup which is either a single net, or a group of nets associated
// with a set of IOLabel connectors having a given common label.

let mapKeys (map: Map<'a, 'b>) = Map.keys map |> Array.ofSeq
let mapValues (map: Map<'a, 'b>) = Map.values map |> Array.ofSeq
let mapItems (map: Map<'a, 'b>) = Map.toArray map
