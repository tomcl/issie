module Global

type Page =
    | Home
    | Counter
    | About
    | Draw

let toHash page =
    match page with
    | About -> "#about"
    | Counter -> "#counter"
    | Home -> "#home"
    | Draw -> "#draw"
