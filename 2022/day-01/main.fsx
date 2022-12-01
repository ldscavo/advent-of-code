open System.IO

let parseInput fileName =
    File.ReadAllText fileName
    |> fun i -> i.Split("\r\n\r\n" )
    |> Array.map (fun i -> i.Split "\r\n" |> Array.map int)


// Part 1
parseInput "input.txt"
|> Array.map Array.sum
|> Array.max
|> printfn "The highest calorie count is: %i"

// Part 2
parseInput "input.txt"
|> Array.map Array.sum
|> Array.sortDescending
|> Array.take 3
|> Array.sum
|> printfn "The sum of the three highest calorie counts is: %i"