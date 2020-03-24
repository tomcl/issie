module App.Types

open Global

type Msg =
    | CounterMsg of Counter.Types.Msg
    | HomeMsg of Home.Types.Msg
    | DrawMsg of Draw.Types.Msg

type Model =
    { CurrentPage: Page
      Counter: Counter.Types.Model
      Home: Home.Types.Model 
      Draw: Draw.Types.Model }
