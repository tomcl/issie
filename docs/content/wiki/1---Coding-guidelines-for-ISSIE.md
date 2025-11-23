The aspects of code quality listed here are the easy ones for you to address. Anyone can practice them. If you put effort into it you will find it easier to write and read working code, and other people will find it much easier to work with your code. As will you, in one year's time.

All of the advice on this page, if followed, will help you write good, as easy as possible to understand, code for ISSIE in F#. If your code remains obscure after following this you probably have algorithmic difficulties.

These things noted here are mostly though not all common to OOP and FP - they are just a bit more obvious in FP which if well coded is very simple and easy to read.

You will find that most of the code in Issie is fairly good quality, some of it pretty bad. 50% of Issie code could be improved.

# Executive Summary

1. [Types](#Types). Getting types right affects everything else. Use D.U.s, options to model data domain precisely. Use records, or anonymous records, to group data used together. Records (or anonymous version) are nearly always better than tuples if  meaningful field names are possible.
    * All types, including record fields and D.U. cases, can be nested. Nested records and D.U.s must be defined using multiple definitions
    * FSharp coding guidelines view recursive custom types as a *code smell* - you may need it but if possible try to avoid it. The same is true of recursion: often library functions can replace recursive function definitions, although there are use cases where recursion is needed. Issie code has very little recursion.
       * Recursive type definitions (with a D.U. case to break any infinite recursion) are allowed. No rec keyword is needed in type definition.
       * Use `and` keyword instead of `type` or use a `rec` style module to allow mutual recursion in types where `and` does not work.

2. [Names](#Names). Give thought to meaningful names, so that code documents a program as far as possible. Make name length proportionate to value: long names cost the reader in noise and difficulty reading expressions. If a name adds useful info use it, use pipes, anonymous functions where a name adds nothing.  Use `'` or explicit redefinition of same name for similar versions of a name. Use names to document and break up long expressions are functions. Good naming is an art - somewhat subjective - but very important.
3. [Library functions](#when-to-use-library-functions)
   * Use them - don't reinvent wheel
   * Take care to use the simplest of `map`,`fold`, tail recursion to solve problems that would need a loop in imperative languages
3. [Functions](#Functions). 
   * Choose functions boundaries carefully so that functions do a well-defined understandable thing, and can have a useful name. 
   * Make Currying work: order arguments so that pipelines work on collections, normally the collection parameter is last; Put `static` configuration parameters first so that configured versions of functions can be easily used. 
   * Beware too many functions - if a subfunction is called only once and does not divide up computation in a useful way it should be incorporated in the calling function
   * Beware too few functions. Functions modularise code at large and small scale - dividing up expressions using functions that make sense improves code.
      * Use tiny subfunctions or local values to simplify repetitive expressions - a *very local* definition for this is low cost because the definition is read with the expression(s) it is used in.
      * Use helpers - write them carefully with names and XML doc that aids use - think about where they go so they can be re-used.
   * Avoid replicated code. Think about how to turn replicated code into a function called multiple times, or a function mapped over a collection. Function parameters can encapsulate the bits that change between different copies of code.
3. [Pipelines](#Pipelines)
    * Use pipelines a lot, they are usually more readable that local definitions
    * Long pipelines can be difficult to debug because there is not visibility of intermediate values, you can insert `|> (fun p -> printfn $"{p}"; p)` anywhere to get this.
    * Where pipelines are long and intermediate steps have names that help to chunk the transformation into understandable segments don't be afraid to
break up long pipelines with local definitions.
4. [Layout](#Layout). 
    * Use **vertical alignment** where that organises items as well as to break long lines. 
      * `match` cases can be aligned after the `->`
      * pipelines can be aligned
      * function parameters in definition can be aligned (with types)
      * function arguments in call aligned (when they are long expressions)
    * Use IDE auto-format with care, it can sometimes add too much vertical alignment. Try it per function and undo with `Ctrl-Z` as needed. Use meaningful grouping of code into related sections with headers.
5. [Documentation](#Documentation). 
    * Use short XML comments (`///` above definition) on all top-level functions, types, most field names, etc
    * Add documentation at top of each module to explain module purpose
    * Otherwise good code needs little documentation. Document strategies, ways types are used, things that are tricky, how types map data domain where this is not obvious (it should be as obvious as possible). Remember that a program where you need to read documentation to discover meaning requires you then also to read code to check correctness. Much better if the code is also the documentation!
6. [Miscellaneous](#Miscellaneous).
    * Never insert configuration/style values as literal constants. Define a constant value with a suitably explanatory name at the top of the relevant implementation file. Best practice is to use a submodule `Constants` for all such definitions to keep them tidy. In a few cases it may be appropriate to use static members of a type for such definitions.
    * Use the Helper functions, especially those in `Common.EEExtensions` and `Common.Helpers`.
    * Use *Optics* whenever two-level field access is needed, and optionally for single-level: use them for common Map access as well. See the Optics [page](https://github.com/tomcl/issie/wiki/Optics:-Lenses-and-prisms)
    * Use pattern matching _fully_. Patterns are safe and can replace indexing in lists, they make everything more readable and many things simpler. Patterns with guards (`when`) can replace `if ... then ... elif ...`. Consider using partial active patterns.

7. [Performance](#Performance). 
    * Performance is complex and differs between .Net and Issie which uses Fable/Javascript. 
    * For Immutable update use Maps. For better performance prefer large arrays over large lists except when construction is incremental. 
    * Try to use `List.init`, `Array.init` instead of incremental construction.
    * Tail-recursion is as fast as `for` or `while` loops
    * `Array.iter` is slightly faster than `for` loops
    * Full recursion is slower than loops.
    * Issie performance can be instrumented with the functions in `Common/timeHelpers.fs`. These allow you to see which messages cause long lags, and then to monitor performance of any part of the code to see where the lag comes from. The performance constraint is that the view + update functions should in all normal cases be < 20ms or so if possible to prevent noticeable lag. In practice fast performance is mostly needed for the Draw Block operations where fluid response to mouse movement (< 10ms) is important.


# Types


## Mutable types

* Never use mutable data except locally when there are clear performance advantages and performance matters. 
* Mutable data should be used in less than 5% of all code, when clearly needed, and may never be used.
* The Elmish Model must never be mutable. This means that F# Maps (implemented as binary trees) are the best type to use for large collections of data that must have individual items updated by messages.
* Arrays have mutable elements, however you can normally use them as immutable data by never updating elements. It is very rare in Issie to use arrays as mutable data, but common to use arrays because they are faster than lists (by a factor of 2 or more) for data that does not change

## Lists vs Arrays vs Maps vs Set

* Use List
   * For convenience (it is normally easier than other datatypes).
   * Where head / tail extraction or matching is used
   * Where lists are constructed incrementally using `::`
* Use Array
   * Where non-sequential indexed access is needed
   * Where performance is important and sequential access is guaranteed and updates are infrequent. 
   * Where operations are done using whole collection library functions. The code is identical for `List` or `Array`, but arrays are faster.
   * Arrays have immutable update time (create a whole new array) comparable with lists, but sequential access time twice as fast and non-sequential indexed access time very much faster.
* Use Map
   * For any large collection where single item immutable updates are required. These are O(log n) versus O(n) for List or Array.
   * For any large collection where lookup (e.g. access from a unique SHA key) is required. This is O(log n) versus O(n) for List or Array.
* Use Set
   * When Set difference (-) is needed or where set operations and nothing else is needed. In practice Set is not very useful, because Map operations are more flexible. Set, like Map, is O(n) - in fact Set implementation uses Map. Note that Set union can also by implemented on lists or arrays (O(n)) by `append` followed by `List.distinct`.

In terms of ease of use, where performance is irrelevant, default to `List`, change to `Array` if collections are large and library functions are used (this is no effort since List and Array are interchangeable). Use Maps for collections needing lookup, or large collections needing update.

## Records vs Tuples

* Records are much preferred over tuples except in simple cases for for general purpose tuple-input or output functions (like `List.split`). If record field names make code easier to read, records should be used
* The type overhead of many little record types used only locally can be avoided by using [anonymous records](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/anonymous-records) which do not require type definitions. These are great for documenting what the different things returned from are given to a function are.
* Tuples have a use locally, where what the parts of a tuple are is best found by looking at local context rather than a name
* Tuples are also useful when a function is called many times and the tuple data is *obvious* - the extra noise from record field names in the calls can make code less readable

## Discriminated Unions

These are a key datatype and tend to be under-used.

* Use them instead of true/false if this makes code more readable: in many cases it is not worth the overhead, but sometimes, when it is not obvious what true and false represent in a match statement, it can be very helpful.
* Use them whenever you have an enumeration.
* Use them whenever you have alternate data cases and `Option` or `Result` do not nicely fit.
* Use D.U. tags to name different items of data in a D.U. case tuple. That is not as good as a record, because you cannot match on it, but it can make construction much better documented and adds no overhead. Compared with this turning a case into a record to get filed names would be very unpleasant!

```fsharp
type Shape =
    | Rectangle of width : float * length : float
    | Circle of radius : float
    | Prism of width : float * float * height : float

let rect = Rectangle(length = 1.3, width = 10.0)
let circ = Circle (1.0)
let prism = Prism(5., 2.0, height = 3.0)
```

## Gotchas

* A D.U. type containing a functions in **any** of its cases not comparable, and can't be used in a Set or a as a Map key.
* Where a common field of data occurs in all D.U. cases, think about refactoring the D.U. as a record containing a smaller D.U. and the common field. Otherwise accessing the common D.U. field cannot be done except with a match statement, which is extremely annoying. An *access function* can help solve this but is extra overhead.
* If you just want to package data a record is better than a D.U. since its fields can be read or written more easily.

# Names

In F# names matter. If you name everything well, and use local named expressions to split up expressions into chunks where the names make sense, code is very readable. Conversely if you do this wrong the same code can be 10X more difficult to read. Your main aim (in this module, and as a programmer) is to write concise readable code. The two tend to go together, because when you eliminate noise what is left is easier to read.

## Long or Short

In FP names tend to be a bit shorter than in OOP. That is because FP relies more on expressions to do computation, and an expression with long names becomes unreadable. You typically can combine more things together in an FP expression, and short names allow the structure of the expression - how the elements are combined, to be seen easily.

Of course names help document a program, and for this purpose cryptic names are bad, and short often means cryptic.

Equally, less obvious, long names can be bad.

```fsharp
let segmentIntersectionCoordinatesWithAllOtherSegments 
   (wModel : Model) 
   (segmentIn : Segment) 
      : list<ConnectionId * SegmentId * ConnectionId * SegmentId * XYPos>
```

Here is a long function name that will make any expression it is part of difficult to read. As an exercise, how would you shorten this name without removing any of its information? In addition `segmentIn` is a bad name. It is medium length, but contains no useful information. `segment` is obvious from the type. `In` means nothing given we know this is the function input segment. See later, the long result tuple is also bad, you could fix this with a record or more likely an anonymous record. You might also structure the tuple as two 2-tuples, depending on how the parts of it relate to each other.

Another example where long names are confusing:

```fsharp
let rec listOfSegmentIntersectionsHelperFunction 
    (wModel : Model) 
    (listOfSegIntersection : list<ConnectionId * SegmentId * ConnectionId * SegmentId * XYPos>) : Model =
   
```

This is the inner tail recursive function of an outer function, so `Helper` is not needed (maybe a `'`). Whatever it is `Function` is gratuitous since it is clearly a function. `listOf` is also too long. The type shows this. If you want to emphasise this, and think pluralising the name does not do this, the best way is an L after the name. So we get: `segmentIntersectionL'` but probably `segmentIntersections` would be fine.

Finally here is an interesting example where long names are just very difficult to read in expressions in spite of being very clear individually. The problem is they are too long, and to work out which name we have it is necessary to read it all and put it together. Note the vertically written function arguments of `Map.containsKey` - they are too long to write horizontally!

```fsharp
            let listOfVerticalSegmentsInOtherWiresToBeResetted =
                ([], listOfHorizontalSegmentsOfCurrentWire)
                ||> List.fold
                    (
                        fun state horizontalSegmentOfCurrentWire ->
                            if (Map.containsKey 
                                   (horizontalSegmentOfCurrentWire) 
                                   (wModel.FromHorizontalToVerticalSegmentIntersections)) then
                                state @ wModel.FromHorizontalToVerticalSegmentIntersections.[horizontalSegmentOfCurrentWire]
                            else
                                state
                    )
```



* Use single letter, or two letter, names for function parameters where the meaning is entirely defined by the local context and the data item cannot easily be identified by a name. The classic case here is an array index, although good FP tends to use list library operations more and need array indices less.
* Use longer names for functions which are defined globally and used occasionally. Where a function is used a lot a shorter name becomes more appropriate.
* Avoid very long names, and multiple similar long names.
* Think about names. Do not add noise just to make the name seem like English. Shorter is always better if the meaning is as clear.
* Name functions by what they do or what their result is, not by how they do it. think as someone who has not implemented the function would do. Does the name help you to know how to use the function?
* It can be useful to use some standard notation to distinguish between items and lists of items in very local parameters where you are not giving real names: `a` vs `aL` or `as` (list). There is no very standard notation here, but you can make your own.
* Use `'` to distinguish different versions of the same name. The tail-recursive inner function of a function `myFunc` might be called `myFunc'`. The gotcha here is that if the two names have the same type they can be mixed up when writing code. It is too easy to write the unquoted version when you mean the quoted one. That is a reason for redefining a local name when you want to perform some initial minor transformation on an input that does NOT alter its type. Of course redefining the same name can be unclear as well, so all depends on the context.
* Names can be too short. A typical case would be a function or value defined a long way away from where it is used where a good name means it is obvious what it is or does, a shorter name is ambiguous. If the function is used locally many times in one place a local short alias can be defined for it. But, if it is used globally many times it is probably something that needs to be remembered and should be shortened.

## When to add a name (and type)

* If you find yourself staring at an expression not sure what it means, or unable to work out why it won't type check, divide it into sub-expressions each with a suitable name that documents the operation. Where the type is not obvious ad a type.
* Add a name (and type) for a folder function in List.fold if that helps it to be more readable.
* Add a name breaking a long pipeline into steps.
* Use the match parameter in a match expression to name the corresponding matched expression, when this is not obvious and the expression is not a single identifier.
* Add explicit types to the parameters of functions where this helps documentation. Use the *vertical form* of the function header to make this more readable.
* Make function parameter names help to document the function where possible

## Unused Names (in pattern matches)

```fsharp
match x with
| h :: _ -> 
```


A wild card `_` can always be used. If you want to document the unused name (useful) but want to ensure it is not used, you can use `_myName` which is semantically the same as `_` but helps readability - sometimes.

# Functions

The general rule in FP is that if you see much similar-looking code you probably need a subfunction to implement the common bits with parameters for those bits that vary. Functions tend to be under-used by imperative programmers, and they make your life a lot easier. However functions can also be over-used. The typical case you get in much-modified code is where every time a new thing is added an extra function is layered on top of existing functions to add the new functionality. This leads to very unreadable code where you need to go through 4 or 5 function definitions to find out what a function actually does. The layers perform some adaptation which does not make sense except in the context of the original.

## When to use library functions

* In common with OOP programming a lot of work can be avoided, and readability added, by using library functions. Typically, these are `List` functions, with exactly the same set for `Array`, making `List` -> `Array` transformation in data structures easy. `Map` shares some of these functions but is naturally different: items have keys as well as values. Other functions such as regular expressions for `String` types are important for text manipulation. One important to remember function is `String.concat (sep:string) (collectionToConcatenate: string seq)` which concatenates lists of strings with a separator. This will work on Lists and Arrays (the Seq type fits all types that imply a sequence) and is very useful for debugging. Don't confuse it with `List.concat` which concatenates lists without a separator.
* For example, many quite complex problems can be solved using `List.group` or `List.groupBy`. `List.zip` and `List.unzip` make other list library functions easier to use. `List.mapi` is very useful in addition to `List.map`. `List.collect` can implement `map` and `filter` as well as more general things.
* As with much in F# coding, library functions are also useful in OOP languages, and you will find them there as well. It is just that in F# they are more obviously useful. After using them in F# you will find yourself using them more in other languages.
* To answer this question: use library functions as much as possible.


### Using Fold versus Map versus tail recursion.

On `List, `Array` or `Map` datatypes:

* You can use the corresponding `fold` operation to perform sequential updates (iteration).
  1. If possible, use `map`, or `sumBy` - which are much simpler - not fold
  2. Use `fold` instead of tail recursion if there is natural sequential update one for each item in a collection
  3. Use tail recursion for more general iteration whether the number of iterations is not defined by a list etc
* In the above list of methods use the simplest (highest in list) construct that works naturally.
* As always break this rule in exceptional cases, with justification. The reason for the rule is to make programs more simple and readable. If it does not do this, you can break it.

## Where to put sub-functions

* A function used only in one place should theoretically be a subfunction of the function in which it is used. Two problems:
  1. That stops it being independently tested
  2. if it is large the function it is used in becomes difficult to read with header a long way away from body.
* Sub-functions work best:
  * When they are short
  * When they use parameters of the outer function and so reduce the number of passed parameters in function calls
* Global functions work best:
  * When used in more than one place
  * When the function is large
  * When the function result is clear from its name

## When not to use a subfunction if you can help it

* When it calls another *global* function immediately, and does not do much else. (Contrast this with tail recursion subfunctions which are fine).
* When the operation of the function it calls, and the operation of the outer function, cannot be obviously distinguished by different names

In these cases you maybe want to replace the inner function call by code inside the outer function - which now incorporates the inner one. Sometimes a function is used when a pipeline would be better.  Sometimes the inner function is implemented as an anonymous function used in a pipeline in the outer function, etc.

## How to use function parameters

* Group together associated items of data into records (or [anonymous records](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/anonymous-records)) and pass them as a single parameter when they are commonly all processed together.
* Try not to have multiple parameters of the same type in complex functions: it is easy to get them mixed up
* Name parameters well unless the parameter meaning is obvious from the name and signature of the function. In that case much shorter names are fine. Avoid overly long names for function parameters: they are usually not necessary.

## How to return data from a function

* Use an [anonymous record](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/anonymous-records) instead of a tuple to return multiple items of data, unless it is very obvious what is the return value
* Use a real record type where the return type from a function has some meaning of use elsewhere in the code

## When to use an anonymous function

* When the function definition is very simple, and reading it is all the documentation you need.
* When it is in a pipeline or some other F# construct and there is no useful way to name the thing it does.

## When to use tuple parameters `let f (a,b) =` instead of curried parameters `let f a b =`

Almost never. It is appropriate when a and b are always used together, e.g. x and y coordinates, but in that case you would probably use a record.

## How to choose order of function parameters in a curried function

* Where the function is used transforming something, the thing transformed should be the final parameter so that it can be partially applied in a pipeline without an adapter anonymous function to swap arguments 
   * `|> (fun lst -> wrongWayRoundFunction lst a b)`.
   * `|> rightWayRoundFunction a b` NB the third argument `lst` is needed, the function is Curried and when supplied with `a` and `b` returns a function that fits the pipeline.

## When to use recursion

In F# recursion is viewed as a policy of last resort. It is usually not needed, and can be replaced by library functions, folds, etc where the recursive part of the code is implemented internally (as iteration) in the library function. That is far clearer than writing a recursive function. Recursion wins when the problem is truly recursive, and may be needed when the function implemented can't be expressed as a fold or scan operation over a list. In Issie there are very few recursive functions. 

* Tail recursion is equivalent to iteration. Think of it as a different syntax for the same thing. In F# tail recursion is preferable to iteration but there is not much in it.
  * Non-FP programmers will often say that iteration is clearer than tail recursion. What you know is always easier! However, tail recursion is semantically more restricted and therefore easier to analyse than iteration. The various parts of the loop, initialisation, how things change, are bound together in a tail-recursive function slightly better than an iteration where initialisation can happen anywhere before the loop.
* Full recursion is more general than iteration and sometimes better (though slower) at solving complex problems. Full recursion that is linear and therefore deeply nested when applied to long lists should be avoided. F# has performance issues with more than 100K nested non-tail function calls because of the way its garbage collector works. It will also run out of stack. In practice this use case is not something you need to worry about - it is very rare.
* Don't be afraid to use recursion, but look for other methods, especially `List.fold` and `List.scan`, first.

# Pipelines

* Use pipelines a lot, they are usually more readable that local definitions
* Pipelines have one hidden advantage. By putting the data before e.g. `List.map` an anonymous function can be used without the need for explicit typing of its parameter:
    * `myType |> List.map (fun mt -> ...) // the type of `mt` is inferred from `myType`, it would not be without the pipeline.`
* Long pipelines can be difficult to debug because there is not visibility of intermediate values, you can insert `|> (fun p -> printfn $"{p}"; p)` anywhere to get this.
* Where pipelines are long and intermediate steps have names that help to chunk the transformation into understandable segments don't be afraid to
break up long pipelines with local definitions.
* Horizontal pipelines are useful if very short: `a |> Some`, `lst |> List.map (fun n -> n+1)`. Mostly, pipelines should be vertical.
* Use reverse pipe operator for bracket reduction sparingly and consistently. In Issie it is useful (and should be consistently used) with the `dispatch` function:
    * `dispatch (some expression)` => `dispatch <| some expression`
    * Never use `<|` with `|>` - the associativity is too confusing and usually wrong.
* Use `>>` to compose functions where this is readable, typically a pipeline with many functions applied to a list all using `List.map` would be simplified this way. 

```fsharp
a
|> List.map f1
|> List.map f2
|< List.map f3

=>

a
|> List.map (
    f1 >>
    f2 >>
    f3)
```

If `f1`, `f2` are short then `List.map (f1 >> f2)` works as well

# Layout

* Extra lines have:
   * a cost in making less of the program visible in one window.
   * a benefit where vertical alignment conveys sematic information.
* Good practice:
   * start match cases on new lines so that the code after the `->` aligns vertically. This makes it much easier to read the `match`.
   * similarly with if-then-else, align then and else parts vertically.
   * consider using vertical alignment of parameters/arguments in function definitions/calls. This makes things much more readable and is especially helpful for functions that need types as well as names to better document parameters.
   * use vertical alignment for record field
* In all cases when constructs are very short it may be better to put them on one line.

# Documentation

## XML comments

* In F# comments written above a function or value definition with 3 slashes are called [XML comments](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/xml-documentation). 
* They appear in the help popups in an IDE when you hover over the function name. 
* You should provide short (normally 1 - 3 lines) comments for every global function in your program and subfunctions large enough to need them, unless the function usage is completely clear from its name and parameter names. If you choose names well this will often be the case.
* Adding XML comments throughout a program is the best way to document it. If you can't summarise a function in 2-3 lines you know more documentation, or refactoring with more functions, is required.
* XML comment gotcha. Lines in comments get removed in the XML. Don't rely on them, make sure you terminate sentences with '.'.

## When are comments needed?

Other than XML comments on the definitions of non-obvious functions, I find comments are only needed to explain how complex data structures are used, or when the algorithm is *tricky*, or when some local element of mutability has been added, or to provide an introduction at top level. Good programs should be mostly self-documenting.

# Side-effects

Avoiding side-effects must be a priority. Use lists and list functions, but not (unless you have to) list indices, `List.Head`, `List.Tail`.

* Never leave `match` expressions incomplete.
* Prefer `match` or `Option.defaultValue` rather than `Option.get`, which will fail if the option is `None`.
* Prefer `List.tryItem` etc, rather than `List.item` or equivalently `lst.[i]`. Indexing is very compact and convenient, but it can easily go wrong. Errors from out of range indices, or other *impure* code that can fail if given the wrong data are always more difficult to debug than if you had caught the failing case with a match statement and a documented `failwithf` that prints some data and explains the problem.
* In some cases map indexing can be used without trouble, e.g. when the map is known to contain all the values that exist of a given type. This is quite common where a map is used as a collection to look up records from SHA ids. You must protect the map key type by wrapping it in a one-element D.U. (e.g. `ConnectionId`, `ComponentId` in `common/commontypes`. This simple and harmless in terms of performance step catches a lot of errors in the type system and is absolutely critical to safer coding with maps.

# Miscellaneous

## Literal constants

Literal constants that adjust styles, configuration, etc should be easily changeable and properly highlighted. This is standard programming good practice and just as important in Issie! It can be untidy (and difficult to find config values) to have isolated constant definitions, so best practice is:
    
* Define a constant value with a suitably explanatory name at the top of the relevant implementation file. 
* Put all such values in a submodule `Config`. See `Common.Helpers.JsonHelpers` for submodule example. The `[<AutOpen>]` is optional.
* In some cases it may be appropriate to use static members of a type for such definitions, but this can make it more difficult to adjust them
* The Config submodule can be opened are even AutoOpened if constants are used a lot, however using fully qualified names is helpful to highlight that this is a potentially adjustable parameter.

## General-purpose Helper functions

* `Helpers` contains some functions you often need when using maps. 
* `EEExtensions` contains additions to various of the standard libraries
* These should be updated, conflicts with functions added in later versions of F# removed, and all put into `EEExtensions`

## Optics

Use the Optics library in `Common/Optics.fs` for lenses and prisms to make record, map access and update easier. Read the Optics Wiki [page](https://github.com/tomcl/issie/wiki/Optics:-Lenses-and-prisms) for an introduction, and for Issie conventions. It is **very important** to follow naming conventions precisely when using optics, so that any specific optic function name can be determined from context.



# Performance

Issie needs performance from both the view & update functions. The simulation performance scales as n: the simulator is optimised and has no significant map lookups. Performance of the draw block normally scales between log n and n where n is the number of components. Map lookups for the (small number) of moving components and wires scale as log n. The react render properties (`WireRenderProps` or `RenderSymbolProps`) generation - which feeds the single symbol or wire view functions and must all be evaluated every view, scales as n, so it must be very fast. the actual vie functions for symbols and wires can be much slower since typically only a few of them have props that change and therefore need to be re-evaluated.

Overall Issie performance is very complex. There is built-in instrumentation which can be switched on to print out real-time individual or average performance data for any segment of code: see `common/timehelpers.fs` and the update function for some examples of its use. Note also that having dev tools open noticeably slows the system, so shut them when making performance measurements. Careful instrumentation is required to understand the real performance bottlenecks.

The overall requirement is that most operations should not have noticeable lag and that move operations on the DrawBlock should seem fluid. In order to get much faster DrawBlock updates than Issie UI updates the DrawBlock mouse events are processed in an inner loop that does not re-evaluate the main Issie view function (this can be quite slow when the wave simulator is open).










  


