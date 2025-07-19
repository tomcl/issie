module SimpleSample

// Simple math functions
let add x y = x + y

let multiply x y = x * y

let square x = multiply x x

// Function that calls other functions
let calculate x y =
    let sum = add x y
    let product = multiply x y
    let squareSum = square sum
    squareSum + product

// Recursive function
let rec factorial n =
    if n <= 1 then 1
    else multiply n (factorial (n - 1))

// Higher-order function
let applyTwice f x = f (f x)

// Main entry point
let main argv =
    printfn "Simple F# Sample"
    
    let result1 = calculate 3 4
    printfn "Calculate 3 4 = %d" result1
    
    let result2 = factorial 5
    printfn "Factorial 5 = %d" result2
    
    let result3 = applyTwice (add 5) 10
    printfn "ApplyTwice (add 5) 10 = %d" result3
    
    0