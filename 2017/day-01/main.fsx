open System
open System.IO

let parseInput (f: list<int> -> list<int*int>) (input: string) =
    input.ToCharArray ()
    |> Array.map (string >> Int32.Parse)
    |> Array.toList
    |> f
    |> Seq.fold (fun (total: int) (x, y) -> if x = y then total + x else total) 0

let pair (l: int list) =
    l @ [(l |> List.head)]
    |> List.pairwise

let halfPair (l: int list) =
    let jump = (l |> List.length) / 2
    let list = l @ l
    l |> List.mapi (fun i n -> (n, list[i + jump]))

// Part 1
File.ReadAllText "input.txt"
|> parseInput pair
|> printfn "The solution for part 1 is: %i"

// Part 2
File.ReadAllText "input.txt"
|> parseInput halfPair
|> printfn "The solution for part 2 is: %i"