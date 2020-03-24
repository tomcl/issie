module Types

// JS objects.
type SVG     = | SVG of obj
type Point   = | Point of obj
type Line    = | Line of obj
type Square  = | Square of Point

type DiagramComponent =
    | Pt of Point
    | Ln of Line
    | Sq of Square
