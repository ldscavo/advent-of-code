open System.IO

let split (s: string) =
    let half = (String.length s) / 2
    [s.Substring(0, half); s.Substring(half, half)]

let getDupes (inputs: seq<string>) =
    inputs
    |> Seq.map Set.ofSeq
    |> Set.intersectMany

let input =
    File.ReadAllLines "input.txt"

let priorities =
    let lst = (seq {'a'..'z'} |> Seq.toList) @ (seq {'A' .. 'Z'} |> Seq.toList)
    lst |> List.fold (fun map l -> Map.add l ((List.findIndex (fun x -> x = l) lst) + 1) map) Map.empty

// Part 1
input
|> Array.map split
|> Array.map getDupes
|> Array.map (Set.toList >> List.head)
|> Array.map (fun x -> priorities[x])
|> Array.sum
|> printfn "The sum of the priorities is: %i"

// Part 2
input
|> Array.chunkBySize 3
|> Array.map getDupes
|> Array.map (Set.toList >> List.head)
|> Array.map (fun x -> priorities[x])
|> Array.sum
|> printfn "The sum of the priorities is: %i"