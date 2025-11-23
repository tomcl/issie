Historically Issie did not use deeply nested record types so there was little motivation to use lenses. As it got more complex the need for a better way to do nested record update properly became apparent, and this has now been implemented with an optics library `Common/Optics` taken with small modifications from `https://github.com/xyncro/aether`.

For an (easy) read that motivates Optics see [this blog post](https://medium.com/@heytherewill/functional-programming-optics-in-net-7e1998bfb47e). There are many other more precise descriptions.

Currently minimal use is made of Optics, see `Symbol.selectSymbols` for an example of this - using standard naming `name_` is the name of a lens to access record field `Name`. The `Lens` definitions are in `DrawModeType` - after the relevant record types.

Expect much more use in the future.

The following introduction is an expanded Tutorial on basic Lens use and some notes on Issie conventions which should be followed.

## Issie's Introduction to Optics

Optics are an alternative (functional) way to access and update fields of records. F# has built-in update syntax which becomes very unpleasant when updating nested records. Optics are about the same as F# built-in for single-level field access, and much clearer for deeply nested fields: they also work well for pipelined operations.

The key to using optics properly is to make the **Lenses** that combine field get and (immutable) update functions with 100% standard names (derived from the field names).

Here is an example:

```fsharp
open Optics // top-level modules containing Optics library. Should be opened whenever lenses are used.


type Appearance = {
   Color: Color
   Opacity: float
}

let color_ = Lens.create (fun a -> a.Color) (fun c a -> {a with Color = c}
let opacity_ = Lens.create (fun a -> a.Opacity) (fun o a -> { a with Opacity = o}

type Thing = {
   Appearance: Appearance
   Pos: float
}

let appearance_ = Lens.create (fun t -> t.Appearance) (fun a t -> {t with Appearance = a}
let pos_ = Lens.create (fun t -> t.Pos) (fun p t -> { t with Pos = t}
```

There are three operations on fields of a record we can now do the standard F# way, or using optics:


```fsharp
open Optics
let thing: Thing = ...
let newPos = 2.4
let changeP (pos:float) = pos + 1.

// getting the value of a field
let thingPos = Thing.Pos // standard way
let pos = Optic.get pos_ thing // using Optics

// setting the value of a field
let thing' = {thing with Pos = newPos} // standard way
let thing' = Optic.set pos_ newPos thing // using Optics

// mapping a function over  a field 
let thing' = {thing with Pos = changeP thing.Pos} // standard way
let thing' = Optic.map pos_ changeP thing // using Optics
```

From which we see that for single field access Optics are slightly simpler for `map`, about the same for `update`, and more noisy for `get`.

[Why use optics?](#why-use-optics) Because they can be combined simply to implement nested record operations!

### Operators - to use or not to use?

The *noise* coming from the Optic functions can be reduced by using operators:

```fsharp
open Optics
open Operators // for the operators
Optic.get = ^.
Optic.set = ^=
Optic.map = ^%

// getting the value of a field
let pos =  pos_ ^. thing // using Optics

// setting the value of a field
let thing' = (pos_ ^= newPos) thing // using Optics

// mapping a function over  a field 

let thing' = (pos_ ^% changeP) thing // using Optics
```

If Optics are used everywhere and all maintainers trained this is possibly a good idea, the operators 
are easy to remember. Against this, for those not familiar with them, they are completely opaque.

Alternatively, the `Optic` submodule can be opened:

```fsharp
open Optics
open Optic // for get,set,map without qualifier

// getting the value of a field
let pos =  get pos_ thing // using Optics

// setting the value of a field
let thing' = set pos_ newPos thing // using Optics

// mapping a function over  a field 

let thing' = map pos_ changeP thing // using Optics
```

My preference is **not** to use the operators (except for `>->` which is [used to combine Optics](#why-use-optics)), nor to open `Optic`, as shown in the example below. This makes the use of Optics very transparent at the expense of some repetitive noise.

### Issie example use of optics

Lenses defined in `DrawBlock/DrawModelType.fs`:

These definitions are completely standard with names derived from the relevant field - and could be auto-generated from the record types.

```fsharp
    // Lenses for fields of Sheet.Model
    let scrollingLastMousePos_ = 
        Lens.create (fun m -> m.ScrollingLastMousePos) (fun w m -> {m with ScrollingLastMousePos = w})
    let lastMousePos_ = 
        Lens.create (fun m -> m.LastMousePos) (fun w m -> {m with LastMousePos = w})
    let screenScrollPos_ = 
        Lens.create (fun m -> m.ScreenScrollPos) (fun w m -> {m with ScreenScrollPos = w})
    let lastMousePosForSnap_ = 
        Lens.create (fun m -> m.LastMousePosForSnap) (fun w m -> {m with LastMousePosForSnap = w})
    let canvasSize_ = 
        Lens.create (fun m -> m.CanvasSize) (fun w m -> {m with CanvasSize = w})

    // Lens for field of Sheet.XYPosMov. Definition local to Sheet not to be confused with Symbol pos_
    let pos_ = Lens.create (fun m -> m.Pos) (fun w m -> {m with Pos = w})
```

Code using these lenses from `DrawBlock/sheet.fs`:

```fsharp
/// Check that canvas is large enough to have space all round the visible area.
/// If not, then change model by moving circuit on canvas and/or extending canvas.
/// Keep components in same visible position during this process.
/// returns new model with all positions updated if need be.
let ensureCanvasExtendsBeyondScreen model : Model =
    let boxParas = Constants.boxParameters
    let edge = getScreenEdgeCoords model
    let box = 
        symbolWireBBUnion model
        |> addBoxMargin boxParas.CanvasExtensionFraction  boxParas.BoxMin
    let quant = boxParas.CanvasExtensionFraction * min box.H box.W       
    let newSize =
        [box.H;box.W]
        |> List.map (fun x -> x + 4.*quant)
        |> List.max
        |> max model.CanvasSize
    let bottomRight = box.TopLeft + {X=box.W;Y=box.H}
    let size = model.CanvasSize
    let xIsOk = box.TopLeft.X > 0. && bottomRight.X < size
    let yIsOk = box.TopLeft.Y > 0. &&  bottomRight.Y < size
    if xIsOk && yIsOk then
        model
    else
        let circuitMove = 
            box
            |> (fun bb -> 
                let centre = bb.Centre()
                {
                    X = if xIsOk then 0. else newSize/2.- centre.X
                    Y = if yIsOk then 0. else newSize/2. - centre.Y
                })

        //printfn $"scroll move = {newSize}:({circuitMove.X},{circuitMove.Y})"
        match canvasDiv, model.ScreenScrollPos + circuitMove*model.Zoom with
        | Some el, pos ->
            el.scrollLeft <- pos.X
            el.scrollTop <- pos.Y
        | None,_-> ()
        let posDelta :(XYPos -> XYPos) = ((+) circuitMove)
        let posScreenDelta :(XYPos -> XYPos) = ((+) (circuitMove*model.Zoom))
        model 
        |> moveCircuit circuitMove
        |> Optic.map screenScrollPos_ posDelta 
        |> Optic.set canvasSize_ newSize
        |> Optic.map screenScrollPos_ posScreenDelta
        |> Optic.map lastMousePos_ posDelta
        |> Optic.map lastMousePosForSnap_ posDelta
        |> Optic.map (scrollingLastMousePos_ >-> pos_) posDelta // note two lenses combined to 
                                                                // map over field of subrecord
```

## Why use Optics?

Optics start to make a lot of sense when operations on nested records are needed, or when operations are pipelined:

```fsharp
open Optics
open Operators // for >->

let thing' = 
   thingGenerator()
   |> (fun thing -> 
        {thing with Appearance = {thing.Appearance with Color = changeC thing.Appearance.Color}}) // standard way

let thing' = 
   thingGenerator()
   |> Optic.map (appearance_ >-> color_) changeC
```

This example is the one Optics work best for , `get` and `set` are not so unpleasant done the standard way, but still less noisy using Optics when in a pipeline as here.



## Advanced Optics

* Having mastered lenses for field access, note that lenses for looking up elements in Maps are also available. See `DrawModelType` for examples.
* Note also prisms - which access fields of record type options. Don't confuse this with fields which are option types in records: Lenses work fine for that!

We define specific *can in theory fail* `Map` lookup lenses for access to individual values of the Model maps which are accessed via ID keys and therefore *should never fail*. In this case we still use a `failwithf` to explicitly tag a failure with a `"What?..."` error message - but expect this to be impossible to happen. See the example of `symbolOf_` in `DrawModelType`. We use, as a convention, a singular version of the map name followed by suffix `Of` to indicate `Map` lookup.

## Strict Lens Naming Conventions in Issie

### Standard Optics names

The names of lenses are normally defined right after the type definition they refer to. See `DrawModelType.fs` for examples.

* Lens for field `MyField`: `myField_`
* Lens for subfield `FieldB` of field `FieldA`: 
* `fieldB_fieldA_` is equivalent to `(fieldA_ >-> fieldB_)`

In draw block the `Symbol` and `BusWire` `Model` fields are accessed by field names from any block (different accessors in each case).

Thus the lens to access `Symbols` field of `SymbolT.Model` from the current module `Model` record is defined as 

* `symbols_` (in Symbol)
* `wire >-> symbols` (in BusWire)
* `(wire_ >-> symbol_ >-> symbols_)` (in Sheet)

This is a very special case where there is no ambiguity.

* Lens for Map field element lookup. `thingOf_ tId` is a lens which accesses element `tId: IdT` of Map field `Things: Map<IdT,ThingT>.
    * This assumes the map is complete so that lookup can never fail. The Lens definition should include a suitable informative `failwithf` making it 
      better on failure than a failing Map lookup. Even so it muts only be used when there is a strong proof that failure is impossible.