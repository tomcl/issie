namespace Elmish


[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Option =

    let tuple a b =
        match (a,b) with
        | Some a, Some b -> Some (a,b)
        | _ -> None

    let ofFunc f arg =
        try
            Some (f arg)
        with _ ->
            None