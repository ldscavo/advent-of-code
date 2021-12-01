open System.IO

let depths =
    File.ReadLines "input.txt"
    |> Seq.map int

// Part 1
depths
|> Seq.pairwise
|> Seq.map (fun (d1, d2) -> d2 > d1)
|> Seq.filter id
|> Seq.length
|> printfn "There are %i increasing measurements"

// Part 2
depths
|> Seq.windowed 3
|> Seq.map Array.sum
|> Seq.pairwise
|> Seq.map (fun (sum1, sum2) -> sum2 > sum1)
|> Seq.filter id
|> Seq.length
|> printfn "There are %i increasing measurements"