module App.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser
open Global
open Types

let pageParser: Parser<Page->Page,Page> =
    oneOf [
        map About (s "about")
        map Counter (s "counter")
        map Home (s "home")
        map Draw (s "draw")
    ]

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHash model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init result =
    let (counter, counterCmd) = Counter.State.init()
    let (home, homeCmd) = Home.State.init()
    let (draw, drawCmd) = Draw.State.init()
    let (model, cmd) =
        urlUpdate result
          { CurrentPage = Home
            Counter = counter
            Home = home
            Draw = draw }

    model, Cmd.batch [ cmd
                       Cmd.map CounterMsg counterCmd
                       Cmd.map HomeMsg homeCmd
                       Cmd.map DrawMsg drawCmd ]

let update msg model =
    match msg with
    | CounterMsg msg ->
        let (counter, counterCmd) = Counter.State.update msg model.Counter
        { model with Counter = counter }, Cmd.map CounterMsg counterCmd
    | HomeMsg msg ->
        let (home, homeCmd) = Home.State.update msg model.Home
        { model with Home = home }, Cmd.map HomeMsg homeCmd
    | DrawMsg msg ->
        let (draw, drawCmd) = Draw.State.update msg model.Draw
        { model with Draw = draw }, Cmd.map DrawMsg drawCmd
