## How to interface F# to Javascript functions

F# is strongly typed. Javascript is not. FABLE (which translates the F# you write to Javascript) provides multiple ways to interface your code to JS libraries - in this case the JS code that implements electron file interface etc functions. The **recommended way** is to use an F# interface.

The details (there are a lot of details) can be found [here](https://tpetricek.github.io/Fable/docs/interacting.html). Your probably don't want to read that.

The electron API consists of a very large number of untyped methods which implement API functions. It comes with an auto-generated typescript type definition file (`.td`) for easy interface to typescript. The nice feature is that this file can be automatically transformed into an F# interface file which gives static F# types for all functions. FABLE is sufficiently compatible with JS that this works really well. `.src/Common/ElectronAPI.fs` is the published electron API translated automatically to F# and then adjusted manually till it works (the manual adjustment for this very complex interface is not very nice, but not much needs to be adjusted).

The F# interface file contains where necessary multiple definitions (with different numbers of paramemeters) for many of the interface functions:

```fsharp
    type [<AllowNullLiteral>] WebRequest =
        /// <summary>
        /// The <c>listener</c> will be called with <c>listener(details)</c> when a server initiated
        /// redirect is about to occur.
        /// </summary>
        abstract onBeforeRedirect: filter: Filter * listener: (OnBeforeRedirectListenerDetails -> unit) option -> unit
        /// <summary>
        /// The <c>listener</c> will be called with <c>listener(details)</c> when a server initiated
        /// redirect is about to occur.
        /// </summary>
        abstract onBeforeRedirect: listener: (OnBeforeRedirectListenerDetails -> unit) option -> unit
```

F# allows this!

Because JS functions are dynamically typed many will accept inputs of different types. The Interface file deals with this by using standard 2 and 3 case D.U.s (`U2<'A,'B>, U3<'A,'B, 'C>` etc) which can be used to offer different inputs:


```fsharp
abstract pageSize: U2<string, Size> option with get, set
```

defines the value `pageSize` to have two possible types: `string option` and `Size option`. These are constructed from F# using the standard selectors `Case1, Case2` etc. So to set `pageSize` to a string parameter from F# you use:

```fsharp
pageSize.set (Some (Case1 "10 inches"))
```

A more detailed explanation can be found [here](https://tpetricek.github.io/Fable/docs/interacting.html#Erase-attribute).




You can search the API interface file for precise names and definitions of all the API functions - and also check the API documentation - but the code interface is often clearer. Note that many API-specific types are defined which provide options use value-only D.U.s to provide these options (which are in JS juts specific strings) in a type-safe way. For example the array of zero or more OpenDialogOptions can be filled with values from this type defined in the ElectronAPI interface:

```fsharp
    type [<StringEnum>] [<RequireQualifiedAccess>] OpenDialogOptionsPropertiesArray =
        | OpenFile
        | OpenDirectory
        | MultiSelections
        | ShowHiddenFiles
        | CreateDirectory
        | PromptToCreate
        | NoResolveAliases
        | TreatPackageAsDirectory
        | DontAddToRecent
```

This is great to use - in the IDE you can see the correct option values for any command by looking at the type definition or asking for autocomplete on `OpenDialogOptionsPropertiesArray.` . So much better than the JS case where you need to read the documentation.

## Using ElectronAPI

See lots of examples in Issie UI, particularly `FilesIO.fs` which contains the low-level file interface all of which uses the Electron API functions.

To get the API methods and types you need to open `ElectronAPI` module.

## Summary

* Look at `Common/ElectronAPI.fs` for details, after you have found the API operations you want in the Electron documentation
* See `Interface/FilesIO.fs` for lots of example use cases.
* Opening module `ElectronAPI` gives you typed access to the functions. The API is your friend - if it type checks it will work. 
* JS functions with multiple types allowed for inputs or outputs can be handled through special "erased union" type parametrised F# D.U.s `U2,U3,etc`, you will see the `Case1`, `Case2` etc constructors everywhere. You can work out the actual types from the IDE or (with effort) from `ElectronAPI` definitions.
* One caveat. The current API is from an older version of electron. A few features will not be accessible from it and then must be accessed manually via [dynamic types](https://tpetricek.github.io/Fable/docs/interacting.html#Dynamic-programming).
* You will sometimes see `unbox` or `!!` in code. This is the unbox operator or function which allows you to manually break the F# type system an send any type to any function. It is sometimes used to interface low-level javascript.

