open System.IO


let input =
    File.ReadAllLines "input.txt"
    |> Array.map (fun s -> s.Split " " |> Array.map int)

let boundCheck = Array.exists (fun i -> abs i > 3) >> not
let staticCheck = Array.contains 0 >> not
let levelCheck arr =
    let absTotal = arr |> Array.map abs |> Array.sum
    if absTotal = (arr |> Array.sum) || absTotal = (arr |> Array.sum |> (*) -1) then true else false

input
|> Array.map (Array.pairwise >> Array.map (fun (x, y) -> x - y))
|> Array.filter boundCheck
|> Array.filter staticCheck
|> Array.filter levelCheck
|> Array.length
|> (printfn "Part 1: %i")
