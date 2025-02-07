module Hlp25Types

//------------------------------------------------------------------------------------------------
//----------------------------- Sample Types for HLP25 -------------------------------------------
//----------------------------- use these to get started -----------------------------------------
//-------------------- Modify them as you see fit for your implementation-------------------------
//------------------------------------------------------------------------------------------------

open System

/// Probably needs to be bigint eventually to deal with the value of an N bit constant for n > 32.
// There should be no porblem doing that but to gte started let us use int and move to bigint later when needed.
type ParamInt = int

/// A named parameter in a custom component type
/// For MVP this is ok but maybe names need to be qualified by the
/// design sheet they are in to make functions support parameter inheritance.
type ParamName = ParamName of string


/// An arithmetic expression containing symbolic parameters
/// For MVP this could be limited to PInt and PParameter only.
/// However, it would be useful to have a more general type definition so that
/// functions that manipulate constraints, parameters, etc can be written in a more general way.
type ParamExpression =
    | PInt of ParamInt
    | PParameter of ParamName
    | PAdd of ParamExpression * ParamExpression
    | Psubtract of ParamExpression * ParamExpression

type ParamError = ParamError of string

/// For MVP could allow only PInt case constraints
/// The Errors are human-readable explanations of why violating the constraint is not allowed.
/// They should if possible be component-specific "constant MyConstName is 3 bit width so not allowed to be less than -4".
type PConstraint = | Max of ParamExpression * ParamError | Min of ParamExpression * ParamError



/// A named parameter given a specific value in a custom component instance
/// For MVP this could be limited to PInt case only if parameters are not inherited.
type ParamBinding = {Name: ParamName; PValue: ParamExpression}

/// A string marking a specific integer value in a case of ComponentType.
/// The values here are arbitrary and ComponentType-case specific and all that matters is that each value is unique
/// within the case.
/// Should use a D.U. or a one-case D.U. wrapper for this type?
/// Leave that choice for later.
type CompSlotName = string


/// A slot in a component instance that can be bound to a parameter expression
/// CompId should be a ComponentId but then we would need these types to be defined after CommonTypes.
/// That is not possible, because we will wnat to modify CommonTypes types to use these!
/// eventually these types can be folded into CommonTypes, and that could if need be be made recursive so
/// solving the problem.
/// In practice this is OK because ParamSlot is strongly types and we will not be likely to confused CompID with any
/// other string.
type ParamSlot = {CompId: string; CompSlot: CompSlotName; }

/// A binding of a parameter slot to a parameter value.
/// every custom component instantiated on a sheet has a set of these bindings.
type SlotBinding = {ParamSlot: ParamSlot; Value: ParamInt; Constraints: PConstraint list}

/// returns the value of a parameter expression given a set of parameter bindings.
/// The simplified value will be either a constant or a linear combination of a constant and a parameter.
let evaluateParamExpression (paramBindings: ParamBinding list) (paramExpr: ParamExpression) : ParamExpression =
    failwithf "Not implemented yet"



/// Evaluates a list of constraints got from slots against a set of parameter bindings to
/// check what values of param are allowed.
let evaluateConstraints (paramBindings: ParamBinding list) (slots: SlotBinding list) (param: ParamName) : Result<Unit, PConstraint list> =
    failwithf "Not implemented yet"
