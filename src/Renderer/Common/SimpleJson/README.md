# Vendored Fable.SimpleJson 3.24.0

Source of the Fable.SimpleJson nuget package (MIT, (c) Zaid Ajaj,
https://github.com/Zaid-Ajaj/Fable.SimpleJson), vendored so it can be patched for Fable 5.

Patch (TypeInfo.Converter.fs): the ListType and OptionType active patterns are matched BEFORE
UnionType. Fable 4's reflection reported `FSharpType.IsUnion = false` for F# lists and options,
so upstream's order (union first) worked by accident. Fable 5's reflection is .NET-faithful -
lists and options ARE unions - which sent every list and option down the union path and broke
both serialization ("Cannot find case undefined") and parsing of every .dgm file.

Remove this directory and restore the nuget package if upstream ever releases a Fable 5
compatible version. Parser.fs still needs the Fable.Parsimmon nuget package.
