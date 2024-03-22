namespace Elmish.HMR

module Bundler =

    type T =
        | Vite
        | WebpackESM
        | WebpackCJS_and_Parcel

    let current =
        // This code is "special" to not call it ugly
        // But we need to protect against ReferenceError exception
        // when accessing non existing variables
        // By doing this detection only once here, we can avoid propagating
        // this "special" code everywhere else

        let mutable result = None

        try
            if HMR.Webpack.active then
                result <- Some WebpackESM
        with
            | _ ->
                ()

        if result.IsNone then
            try
                if HMR.Parcel.active then
                    result <- Some WebpackCJS_and_Parcel
            with
                | _ ->
                    ()

        if result.IsNone then
            try
                if HMR.Vite.active then
                    result <- Some Vite
            with
                | _ ->
                    ()

        result
