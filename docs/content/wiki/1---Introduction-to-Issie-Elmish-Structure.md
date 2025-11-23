Issie has > 60 modules and is implemented as a single component Elmish Model-View-Update application. This is the *simplest* type of Elmish application where all state is contained in a single Model type and there are no separate react components with their own internal state. Although Elmish uses a react DOM (react has a virtual DOM that makes updates more efficient) developers can usually ignore react and write UI functions as though the DOM was the equivalent HTML.

## Module splitting

Modules larger than about 600 lines are less convenient to edit - and larger than 1000 lines should be split. Typically modules are split in standard ways into multiple files which are independently compiled:

1. NameHelpers (helper functions specific to module)
2. Name (main code)

or, for the modules that implement their own Elmish messages:

1. Name  (module code)
2. NameUpdate (functions needed to implement messages, and the message match function)

Splitting modules needs to respect F# compile order: function calls can only reference previous modules in the compile order.
Within a source file, normally, all references are backward. If absolutely necessary, all functions can be allowed to be mutually recursive using `module rec`. This is bad practice and not currently used in Issie. A contiguous set of functions can be made mutually recursive using `and` instead of `let` in function definition (similarly types).

## Elmish Tricks

Surprisingly, even with a large application, this works well. The Issie message processing loop is at the end of `Renderer\Renderer.fs`. In order to make execution fast enough a few tricks are used:

### Optimising mouse move messages

Draw Block mouse move messages (the only thing that has very high repetition rate) are dealt with specially - grouping them together into a smaller number of larger moves if the app does not have enough time to process them.

### Memoisation (caching) to prevent unnecessary code execution

Normally you do not need to understand how this works - it is used in a few places in the code and transparent - so you can just ignore it. If you need to add or change what gets memoized you will need to understand this section.

Memoisation is used in various places: DrawBlock component and wire view, main Issie display, simulation, so that when the inputs to a potentially long computation remain the same, the computation does not have to be redone. For examples see:

* https://github.com/tomcl/issie/blob/master/src/Renderer/DrawBlock/SymbolView.fs :: `renderSymbol` (use `FunctionComponent.Of` to memoize symbol display)
* https://github.com/tomcl/issie/blob/master/src/Renderer/UI/SimulationView.fs :: `prepareSimulationMemoized` (uses custom mutable data store to prevent simulation being redone when circuit has not changed. The simulation code is complex, see documentation elsewhere)

Note the difference: any function that returns part of the react DOM (what you see on the screen, which returned by the the Elmish View function) can be memoized with built-in Elmish (and React) `FunctionComponent.Of`. Functions that do not return react DOM can be cached in whatever way is wished, using a memoize function such as `Common.Helpers.memoizeBy`, or with some custom equivalent code.

## Elmish Model and Messages

The Elmish model (type Model) is structured using nested models within a single record (the top level `model`). Most of the code is top-level with all messages and model global. The exception is the Draw Block which comprises Sheet, BusWire, and Symbol modules, each with its own nested model. Messages can be defined for each separate component of the model (top-level, `Sheet`, `BusWire`, `Symbol`).

```
Renderer (top-level)
  ---> Sheet
         ---> BusWire
                ---> Symbol
```

The nested models, and their types, are as follows:

| Model data | Model Type | Used by|
|------------|------------|--------|
| `model` | `ModelType.Model` | All of Issie except draw block |
| `model.Sheet` | `DrawModelType.SheetT.Model` | Sheet/SheetUpdateHelpers/SheetUpdate modules|
| `model.Sheet.Wire` | `DrawModelType.BusWireT.Model` | BusWire/BusWireUpdateHelpers/BusWireUpdate  modules |
| `model.Sheet.Wire.Symbol` | `DrawModelType.SymbolT.Model` | Symbol/SymbolView/SymbolUpdate <br> SymbolUpdatePortHelpers/SymbolReplaceHelpers  modules |


Normally each of these components dispatches messages at the same level so this nesting is not obvious, and `model` in each module references the expected model. However, it is possible for any module to send messages to any other module (types permitting). It can be confusing to get the types, or the selectors, or dispatch functions, right for each model when going outside the "current" model. There are lots of examples of this working which can be copied. Typically if code needs to dispatch a message from another model (e.g. top-level code needs to dispatch a Sheet message) a specialised `sheetDispatch` function is written and used. These functions may be defined at the top of a source file or locally:

```
let sheetDispatch sMsg = dispatch (Sheet sMsg)
```

A more complex example of this can be found in popupView where it is necessary to run the Buswire `BusWidths` message.

```
        let sheetDispatch sMsg = dispatch (Sheet sMsg)

        (* other code *)

        let busWireDispatch bMsg = sheetDispatch (DrawModelType.SheetT.Msg.Wire bMsg)
        busWireDispatch DrawModelType.BusWireT.Msg.BusWidths
```

It would be simpler, and possible, to make all of the Model type global - defined in a single file - and used by all. However it would then be very large. The current structure provides significant type protection and simplification when working at each level, at the cost of communication between levels being rather clunky. 

TODO - consider rationalising types and interfaces as below

* Combining `DrawModelType` types with `ModelType` types in a single module, and putting this at the start of the compile order.
* Writing a consistent set of interface functions to wrap access to messages and model components outside of current model
* Putting these functions in a module which can be opened by any other module.
 
This is quite a big job to do well and should not be attempted by the faint-hearted!


