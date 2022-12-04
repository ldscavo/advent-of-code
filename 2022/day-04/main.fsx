open System.IO

let toRange (s: string) =
    let split = s.Split "-"
    [(split[0] |> int) .. (split[1] |> int)] |> Set.ofList

let contains (set1, set2) =
    Set.isSubset set1 set2 || Set.isSubset set2 set1

let overlaps (set1, set2) =
    Set.intersect set1 set2 |> Set.isEmpty |> not

let input =
    File.ReadAllLines "input.txt"
    |> Array.map (fun s -> s.Split ',')
    |> Array.map (fun s -> (s[0] |> toRange, s[1] |> toRange))

// Part 1
input
|> Array.filter contains
|> Array.length
|> printfn "The number of overlapping assingments is: %i"

// Part 2
input
|> Array.filter overlaps
|> Array.length
|> printfn "The number of semi-overlapping assingments is: %i"