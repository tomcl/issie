module Hlp25Types

//------------------------------------------------------------------------------------------------
//----------------------------- Sample Types for HLP25 -------------------------------------------
//----------------------------- use these to get started -----------------------------------------
//-------------------- Modify them as you see fit for your implementation-------------------------
//------------------------------------------------------------------------------------------------

open System

//------------------------------------------------------------------------------------------------//
//--------------Types for Part A: Custom Components and Parameters--------------------------------//
//------------------------------------------------------------------------------------------------//

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
/// The actual parameter value is customisable so that the same code can be used for int parameters (normal)
/// and BigInt parameters (needed for constant values in N bit components).
/// For MVP set 'PINT = int
type ParamExpression<'PINT> =
    | PInt of 'PINT
    | PParameter of ParamName
    | PAdd of ParamExpression<'PINT> * ParamExpression<'PINT>
    | Psubtract of ParamExpression<'PINT> * ParamExpression<'PINT>

type ParamError = ParamError of string

/// For MVP could allow only PInt case constraints
/// The Errors are human-readable explanations of why violating the constraint is not allowed.
/// They should if possible be component-specific "constant MyConstName is 3 bit width so not allowed to be less than -4".
type PConstraint<'PINT> = | Max of ParamExpression<'PINT> * ParamError | Min of ParamExpression<'PINT> * ParamError



/// A named parameter given a specific value in a custom component instance
/// For MVP this could be limited to PInt case only if parameters are not inherited.
type ParamBinding<'PINT> = {Name: ParamName; PValue: ParamExpression<'PINT>}

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
/// In practice this is OK because ParamSlot is strongly typed and we will not be likely to confused CompID with any
/// other string.
type ParamSlot = {CompId: string; CompSlot: CompSlotName; }

/// A parameter slot specification with a current integer value and a set of constraints.
/// The slot is a named slot in a component instance that can be bound to a parameter expression.
/// Every custom component instantiated on a sheet has a set of these bindings.
type PSlotSpec<'PINT> = {ParamSlot: ParamSlot; Value: 'PINT; Constraints: PConstraint<'PINT> list}

//------------------------------------------------------------------------------------------------//
//------------------------Types for Part B: Waveform Selector-------------------------------------//
//------------------------------------------------------------------------------------------------//

/// The data carroed by a node in a tree that represents the structure of the waveforms in the Waveform Selector
/// This specifies the content of the node, which can be a component group, component, port etc
type WTNode = unit // replace with your content: a record, Map, etc

/// a tree that represents the structure of the waveforms in the Waveform Selector
/// this is a skeleton type definition and should be changed as required in Part B
type WaveTreeNode = { // replace if necessary with your own definiiton of a tree node
    WTNode: WTNode;
    HiddenNodes: WaveTreeNode list}

/// a tree that represents the structure of the waveforms in the Waveform Selector
type WaveDisplayTree = WaveTreeNode list
//-------------------------------------------------------------------------------------------------//
//------------------------------Interface Types for HLP25------------------------------------------//
//---------------------Replace the dummy placeholder by your content-------------------------------//
//-------------------------------------------------------------------------------------------------//

/// For Part A: The state used to manage input boxes
/// Model.PopupDialogData.DialogState.
type Hlp25DialogState = unit // replace with your content: a record, Map, etc

/// For Part A: the state used per instance of a custom component
/// to bind the parameters of the Sheet defining the component
/// To integers (or ParamExpressions).
/// CustomComponentType.ParameterBindings
type Hlp25CustomComponentState = unit // replace with your content: a Record, Map, etc

/// For Part A: the state used per design sheet to define integer slots
/// That have values defined with parameter expressions
/// LoadedComponent.LCParameterSlots
/// (also used in SheetInfo - to save / load files - but the LoadedComponent field is the only one used by HLP Teams)
type Hlp25SheetInfo = unit // replace with your content: a Record, Map, etc

/// For Part B: The additional state used to manage wave selector search boxes
/// WaveSimModel.Hlp25State
type Hlp25WSModelState = unit // replace with your content: a Record, Map, etc


