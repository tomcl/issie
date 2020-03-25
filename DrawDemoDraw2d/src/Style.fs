module MyStyle

open Fable.Core
open Fable.React
open Fable.React.Props

open Elmish
open Elmish.React

let bodyStyle =
    Style [
        //MarginLeft 20
    ]

let canvasStyle =
    Style [
        Width "1000px";
        Height "500px";
    ]

let titleStyle = 
    Style [
        Padding 15
        VerticalAlign "middle"
        BackgroundColor "lightgreen"
    ]