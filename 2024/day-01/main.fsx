open System.IO

let (left, right) =
    File.ReadAllLines "input.txt"
    |> Array.map (fun s -> s.Split "   ")
    |> Array.fold (fun (l, r) x -> ((int x[0]) :: l), ((int x[1]) :: r)) ([], [])
    |> (fun (l, r) -> (List.sort l, List.sort r))

let distance l r = l - r |> abs

// Part 1
left
|> List.map2 distance right
|> List.sum
|> printfn "The total for part 1 is: %i"

let score l = right |> List.filter (fun r -> r = l) |> List.length |> (*) l

// Part 2
left
|> List.map score
|> List.sum
|> printfn "The total for part 2 is: %A"
