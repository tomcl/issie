module Hlp25Code
open Hlp25Types
open EEExtensions
open VerilogTypes
open Fulma
open Fable.React
open Fable.React.Props

open JSHelpers
open NumberHelpers
open ModelType
open CommonTypes
open MemoryEditorView
open PopupHelpers
open UIPopups
open Notifications
open Sheet.SheetInterface
open DrawModelType
open FilesIO
open CatalogueView
open TopMenuView
open MenuHelpers

//------------------------------------------------------------------------------------------------
//----------------------------- Sample Code for HLP25 --------------------------------------------
//----------------------------- use these to get started -----------------------------------------
//-------------------- Modify the signatures as you see fit for your implementation---------------
//------------------------------------------------------------------------------------------------

/// Return a Lens that can be used to read or update the value of a component slot integer in the component.
/// The value is contained in the ComponentType part of a Component record.
/// The Component record will be found in various places, depending on the context.
/// For Properties changes, the Component record will be in the Model under SelectedComponent.
/// For changes in a newly created component the component is created by CatalogueView.createComponent.
/// A partial implementation of this function would be OK for MVP.
/// NB - the Lens cannot be part of the slot record because the Lens type can change depending on 'PINT.
/// Maybe this will be fixed by using a D.U. for the slot type: however for MVP
/// we can simplify things by dealing only with int parameters.
let makeCompUpdateLens (slot: PSlotSpec<'PINT>) : Optics.Lens<Component, 'PINT> =
    failwithf "Not implemented yet"

/// returns the value of a parameter expression given a set of parameter bindings.
/// The simplified value will be either a constant or a linear combination of a constant and a parameter.
/// NB here 'PINT is not a polymorphic type but a type parameter that will be instantiated to int or bigint.
let evaluateParamExpression (paramBindings: ParamBinding<'PINT> list) (paramExpr: ParamExpression<'PINT>) : ParamExpression<'PINT> =
    failwithf "Not implemented yet"

/// Evaluates a list of constraints got from slots against a set of parameter bindings to
/// check what values of param are allowed.
/// NB here 'PINT is not a polymorphic type but a type parameter that will be instantiated to int or bigint.
let evaluateConstraints (paramBindings: ParamBinding<'PINT> list) (slots: PSlotSpec<'PINT> list) (param: ParamName) : Result<Unit, PConstraint<'PINT> list> =
    failwithf "Not implemented yet"

/// UI component creates an input box to enter an integer or parameter expression in a component slot.
/// The input box should be validated against the constraints of the slot.
let slotInputBox (destination: Optics.Lens<Model,ParamExpression<'a>>) (slot: PSlotSpec<'a>) : ReactElement =
// This function implements a GUI input box that can be used ina  apopup or as part of teh Properties RH pane.
// The function gets rerun whenever a GUI event happens, recreating the box.
// Persistent changes must therefore be saved in the Issie Model - NOT in the box internal state.
// Therefore text in the input box (persistent) is stored in a PopupDialogState Map keyed by the ParamSlot.
// Whenever that text is valid, the parsed ParamExpression or integer is updated in the Model using the destination Lens.
// Thus any number of input boxes can be operational at the same time.
// The input box should be one of:
//     1. a number input box with a button that appears when there are available parameters
//         to open a GUI parameter expression builder.
//     2. an expression input box that parses the expression and shows an error if it is not valid.
    failwithf "Not implemented yet"


