module Shapes

open DiagramTypes
open JSHelpers

type Shape = {
    Vertices : (int * int) list
    Ports : (PortType * PortLocation) list
    Label : string
}

let mux2 = {
    Vertices = [
        0, 0
        30, 12
        30, 38
        0, 50
    ]
    Ports = [
        Input, Left
        Input, Left
        Input, Bottom
        Output, Right
    ]
    Label = "mux2"
}

let box = {
    Vertices = [
        0, 0
        50, 0
        50, 50
        0, 50
    ]
    Ports = [
        Input, Left
        Output, Right
    ]
    Label = "box"
}
