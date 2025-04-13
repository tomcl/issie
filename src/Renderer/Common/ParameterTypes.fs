module ParameterTypes

//------------------------------------------------------------------------------------------------//
//----Types for Parameters defined on sheets and bound to values by custom component instances----//
//------------------------------------------------------------------------------------------------//

/// Probably needs to be bigint eventually to deal with the value of an N bit constant for n > 32.
// There should be no problem doing that but to get started let us use int and move to bigint later when needed.
type ParamInt = int

/// A named parameter in a custom component type
/// For MVP this is ok but maybe names need to be qualified by the
/// design sheet they are in to make functions support parameter inheritance.
type ParamName = ParamName of string

/// An arithmetic expression containing symbolic parameters
/// For MVP this could be limited to PInt and PParameter only.
/// However, it would be useful to have a more general type definition so that
/// functions that manipulate constraints, parameters, etc can be written in a more general way.
/// The actual parameter value is customisable so that the same code can be used for int parameters (normal)
/// and BigInt parameters (needed for constant values in N bit components).
/// For MVP set 'PINT = int
/// TODO: refactor this to use an enumeration DU for operators to reduce cases.
type ParamExpression =
    | PInt of ParamInt
    | PParameter of ParamName
    | PAdd of ParamExpression * ParamExpression
    | PSubtract of ParamExpression * ParamExpression
    | PMultiply of ParamExpression * ParamExpression
    | PDivide of ParamExpression * ParamExpression
    | PRemainder of ParamExpression * ParamExpression

type ParamError = string

/// For MVP could allow only PInt case constraints
/// The Errors are human-readable explanations of why violating the constraint is not allowed.
/// They should if possible be component-specific "constant MyConstName is 3 bit width so not allowed to be less than -4".
type ParamConstraint =
    | MinVal of ParamExpression * ParamError
    | MaxVal of ParamExpression * ParamError

/// A string marking a specific integer value in a case of ComponentType.
/// The values here are arbitrary and ComponentType-case specific and all
/// that matters is that each value is unique within the case.
type CompSlotName =
    | Buswidth
    | NGateInputs
    | IO of Label: string
    | CustomCompParam of ParamName: string // TODO-RYAN: Should this have type ParamName??
    | SheetParam of ParamName: ParamName    // Used to update default sheet parameters

/// A slot in a component instance that can be bound to a parameter expression
/// CompId should be a ComponentId but then we would need these types to be defined after CommonTypes.
/// That is not possible, because we will wnat to modify CommonTypes types to use these!
/// eventually these types can be folded into CommonTypes, and that could if need be be made recursive so
/// solving the problem.
/// In practice this is OK because ParamSlot is strongly typed and we will not be likely to confused CompID with any
/// other string.
type ParamSlot = {CompId: string; CompSlot: CompSlotName}

/// A parameter expression and its corresponding constraints
type ConstrainedExpr = {
    Expression: ParamExpression
    Constraints: ParamConstraint list
}

/// Data for a new parameterised slot being created
type NewParamCompSpec = {
    CompSlot: CompSlotName
    Expression: ParamExpression
    Constraints: ParamConstraint list
    Value: ParamInt
}

/// The Elmish Model state used to manage input boxes that can be used to define parameter expressions.
/// Part of Model.PopupDialogData.DialogState.
type ParamBoxDialogState = Map<CompSlotName, Result<NewParamCompSpec, ParamError>>

/// Map from name to expression for each parameter
type ParamBindings = Map<ParamName, ParamExpression>

/// This field should store all the Component's slot information where slots are bound to parameters.
type ComponentSlotExpr = Map<ParamSlot, ConstrainedExpr>

/// The state used per design sheet to define integer slots
/// that have values defined with parameter expressions
/// LoadedComponent.LCParameterSlots
/// (also used in SheetInfo - to save / load files - but the LoadedComponent field is the only one used by HLP Teams)
type ParameterDefs = {
    DefaultBindings: ParamBindings
    ParamSlots: ComponentSlotExpr
}

/// Lenses for ParamDefs
let defaultBindings_ = Optics.Lens.create (fun s -> s.DefaultBindings) (fun v s -> {s with DefaultBindings = v})
let paramSlots_ = Optics.Lens.create (fun s -> s.ParamSlots) (fun v s -> {s with ParamSlots = v})



