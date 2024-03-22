namespace Elmish.HMR

open Fable.Core.JsInterop
open Browser
open Elmish

[<RequireQualifiedAccess>]
module Program =

    type Msg<'msg> =
        | UserMsg of 'msg
        | Stop

    type Model<'model> =
        | Inactive
        | Active of 'model


    module Internal =

        type Platform =
            | Browser
            | ReactNative

        let platform =
            match window?navigator?product with
            | "ReactNative" -> ReactNative
            | _ -> Browser

        let tryRestoreState (hmrState : Model<_> ref) (data : obj) =
            match platform with
            | ReactNative ->
                let savedHmrState = window?react_native_elmish_hmr_state
                if not (isNull (box savedHmrState)) then
                    hmrState.Value <- savedHmrState

            | Browser ->
                if not (isNull data) && not (isNull data?hmrState) then
                    hmrState.Value <- data?hmrState

        let saveState (data : obj) (hmrState : Model<_>) =
            match platform with
            | ReactNative ->
                window?react_native_elmish_hmr_state <- hmrState
            | Browser ->
                data?hmrState <- hmrState


    let inline private updateElmish_HMR_Count () =
        window?Elmish_HMR_Count <-
            if isNull window?Elmish_HMR_Count then
                0
            else
                window?Elmish_HMR_Count + 1

    /// Start the dispatch loop with `'arg` for the init() function.
    let inline runWith (arg: 'arg) (program: Program<'arg, 'model, 'msg, 'view>) =
#if !DEBUG
        Program.runWith arg program
#else
        let hmrState : Model<'model> ref = ref (unbox null)

        match Bundler.current with
        | Some current ->
            updateElmish_HMR_Count ()

            let hmrDataObject =
                match current with
                | Bundler.Vite ->
                    HMR.Vite.hot.accept()
                    HMR.Vite.hot.data

                | Bundler.WebpackESM ->
                    HMR.Webpack.hot.accept()
                    HMR.Webpack.hot.data

                | Bundler.WebpackCJS_and_Parcel ->
                    HMR.Parcel.hot.accept()
                    HMR.Parcel.hot.data

            Internal.tryRestoreState hmrState hmrDataObject

        | None ->
            ()

        let map (model, cmd) =
            model, cmd |> Cmd.map UserMsg

        let mapUpdate update (msg : Msg<'msg>) (model : Model<'model>) =
            match msg with
            | UserMsg msg ->
                match model with
                | Inactive -> map(model, Cmd.none)
                | Active userModel ->
                    let newModel, cmd = update msg userModel
                    let newModel, cmd = map(Active newModel, cmd)
                    hmrState.Value <- newModel
                    newModel, cmd

            | Stop ->
                map(Inactive, Cmd.none)

        let createModel (model, cmd) =
            Active model, cmd

        let mapInit init =
            if isNull (box hmrState.Value) then
                init >> map >> createModel
            else
                (fun _ -> hmrState.Value, Cmd.none)

        let mapSetState setState (model : Model<'model>) dispatch =
            match model with
            | Inactive -> ()
            | Active userModel ->
                setState userModel (UserMsg >> dispatch)

        let hmrSubscription =
            let handler dispatch =
                match Bundler.current with
                | Some current ->
                    match current with
                    | Bundler.Vite ->
                        HMR.Vite.hot.dispose(fun data ->
                            Internal.saveState data hmrState.Value

                            dispatch Stop
                        )

                    | Bundler.WebpackESM ->
                        HMR.Webpack.hot.dispose(fun data ->
                            Internal.saveState data hmrState.Value

                            dispatch Stop
                        )

                    | Bundler.WebpackCJS_and_Parcel ->
                        HMR.Parcel.hot.dispose(fun data ->
                            Internal.saveState data hmrState.Value

                            dispatch Stop
                        )

                | None ->
                    ()

            [ handler ]

        let mapSubscribe subscribe model =
            match model with
            | Inactive -> Cmd.none
            | Active userModel ->
                Cmd.batch [ subscribe userModel |> Cmd.map UserMsg
                            hmrSubscription ]

        let mapView view =
            // This function will never be executed because we are using a local reference to access `program.view`.
            // For example,
            // ```fs
            // let withReactUnoptimized placeholderId (program: Program<_,_,_,_>) =
            //     let setState model dispatch =
            //         Fable.Import.ReactDom.render(
            //             lazyView2With (fun x y -> obj.ReferenceEquals(x,y)) program.view model dispatch,
            //                                                                  ^-- Here program is coming from the function parameters and not
            //                                                                      from the last program composition used to run the applicaiton
            //             document.getElementById(placeholderId)
            //         )
            //
            //     { program with setState = setState }
            // ```*)
            fun model dispatch ->
                match model with
                | Inactive ->
                    """
Your are using HMR and this Elmish application has been marked as inactive.

You should not see this message
                    """
                    |> failwith
                | Active userModel ->
                    view userModel (UserMsg >> dispatch)

        program
        |> Program.map mapInit mapUpdate mapView mapSetState mapSubscribe
        |> Program.runWith arg
#endif

    /// Start the dispatch loop with `unit` for the init() function.
    let inline run (program: Program<unit, 'model, 'msg, 'view>) =
#if !DEBUG
        Program.run program
#else
        runWith () program
#endif

    (*
        Shadow: Fable.Elmish.Navigation
    *)

    let inline toNavigable
        (parser : Navigation.Parser<'a>)
        (urlUpdate : 'a->'model->('model * Cmd<'msg>))
        (program : Program<'a,'model,'msg,'view>) =
#if !DEBUG
        Navigation.Program.toNavigable parser urlUpdate program
#else
        let onLocationChange (dispatch : Dispatch<_ Navigation.Navigable>) =
            match Bundler.current with
            | Some current ->
                match current with
                | Bundler.Vite ->
                    HMR.Vite.hot.dispose(fun _ ->
                        Navigation.Program.Internal.unsubscribe ()
                    )

                | Bundler.WebpackESM ->
                    HMR.Webpack.hot.dispose(fun _ ->
                        Navigation.Program.Internal.unsubscribe ()
                    )

                | Bundler.WebpackCJS_and_Parcel ->
                    HMR.Parcel.hot.dispose(fun _ ->
                        Navigation.Program.Internal.unsubscribe ()
                    )

                Navigation.Program.Internal.subscribe dispatch

            | None ->
                Navigation.Program.Internal.subscribe dispatch

        Navigation.Program.Internal.toNavigableWith parser urlUpdate program onLocationChange
#endif

    (*
        Shadow: Fable.Elmish.React
    *)

    let inline withReactBatched placeholderId (program: Program<_,_,_,_>) =
#if !DEBUG
        Elmish.React.Program.withReactBatched placeholderId program
#else
        Elmish.React.Program.Internal.withReactBatchedUsing lazyView2With placeholderId program
#endif

    let inline withReactSynchronous placeholderId (program: Program<_,_,_,_>) =
#if !DEBUG
        Elmish.React.Program.withReactSynchronous placeholderId program
#else
        Elmish.React.Program.Internal.withReactSynchronousUsing lazyView2With placeholderId program
#endif

    let inline withReactHydrate placeholderId (program: Program<_,_,_,_>) =
#if !DEBUG
        Elmish.React.Program.withReactHydrate placeholderId program
#else
        Elmish.React.Program.Internal.withReactHydrateUsing lazyView2With placeholderId program
#endif

    [<System.Obsolete("Use withReactBatched")>]
    let inline withReact placeholderId (program: Program<_,_,_,_>) =
#if !DEBUG
        Elmish.React.Program.withReactBatched placeholderId program
#else
        Elmish.React.Program.Internal.withReactBatchedUsing lazyView2With placeholderId program
#endif

    [<System.Obsolete("Use withReactSynchronous")>]
    let inline withReactUnoptimized placeholderId (program: Program<_,_,_,_>) =
#if !DEBUG
        Elmish.React.Program.withReactSynchronous placeholderId program
#else
        Elmish.React.Program.Internal.withReactSynchronousUsing lazyView2With placeholderId program
#endif
