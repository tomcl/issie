module SmartRotate
open Elmish
open Fable.React.Props
open CommonTypes
open Fable.React
open DrawModelType
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open Symbol
open Optics
open Operators

(*
    HLP23: this is a placeholder module for some work that can be done in the individual or team phase
    but has not been given a "starter" by me. however it is pretty easy to get started on it.

    This code would be HIGHLY USEFUL (especially the "scaling" option).

    Currently if multiple symbols are selected and rotated, each symbol will rotate, but the positions
    of teh symbols will stay fixed. The desired function for the entire block of symbols to rotate,
    applying 2-D rotation 90 degree rotation and flipping to the symbol positions about the centre of 
    the block of selected symbols.

    This operation has "simple" and "better" implementations, like all the initial tasks:
    1. Rotate all symbols and wires exact - do not allow custom components
    2. Rotate all symbols, allow custom components, CC wires will change due to CC shape changes,
      and must be partial autorouted.
    3. Allow scaling as well as rotation (autoroute wires since components will not scale and therefore
      exact wire shape will change). This operation can include Custom components since all wires are
      autorouted anyway.
    Driver test code can easily be adapted from existing Smart module Test menu items. Menu commands 
    which operate on selected symbols - the tests will be more or less how this operation is actually used).

    One key UI challenge for SmartRotate is that when a block of symbols is rotated it may overlap other
    symbols. To allow valid placement it should be possible to move the block on the sheet until a place
    to drop it is found, using an interface identical to the "copy" and "paste" interface - which works 
    fine with multiple symbols (try it). It should be possible to use that exact same interface by placing
    the rotated blokc into the copy buffer (not sure - maybe the copy buffer will need to be modified a bit).

    This could be ignored initially writing code, but muts be addressed somehow for teh operation to be usable.

    Those interested can ask me for details.
*)

