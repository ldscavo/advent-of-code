open System.IO
open System.Text.RegularExpressions

let parseInput (s: string) =
    s |> Regex("\s").Split |> Array.map int
  
let diff (a: int array) =
    (Array.max a) - (Array.min a)

let divisible (a: int array) =
    let isDivisible (x, y) =
        if x % y = 0 && x <> y then Some (x / y) else None

    (a, a) ||> Array.allPairs
    |> Array.map isDivisible
    |> Array.choose id
    |> Array.head

let getResult f =
    File.ReadAllLines "input.txt"
    |> Array.map parseInput
    |> Array.map f
    |> Array.fold (+) 0

// Part 1
getResult diff |> printfn "The answer to part 1 is: %i"

// Part 2
getResult divisible |> printfn "The answer to part 2 is: %i"