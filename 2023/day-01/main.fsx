open System.IO; 

let toNumber (input: string) = 
    input
        .Replace("one", "one1one")
        .Replace("two", "two2two")
        .Replace("three", "three3three")
        .Replace("four", "four4four")
        .Replace("five", "five5five")
        .Replace("six", "six6six")
        .Replace("seven", "seven7seven")
        .Replace("eight", "eight8eight")
        .Replace("nine", "nine9nine")

let getCalibrationValues (arr: char[]) =
    let first = arr |> Array.head
    let second = 
        if arr |> Array.length > 1 then (arr |> Array.last) else first

    [| first; second |]

// Part 1
File.ReadAllLines "input.txt"
|> Array.map Array.ofSeq
|> Array.map (Array.filter System.Char.IsNumber)
|> Array.map getCalibrationValues
|> Array.map (System.String >> int)
|> Array.sum
|> printfn "Part 1: %i"

// Part 2
File.ReadAllLines "input.txt"
|> Array.map toNumber
|> Array.map Array.ofSeq
|> Array.map (Array.filter System.Char.IsNumber)
|> Array.map getCalibrationValues
|> Array.map (System.String >> int)
|> Array.sum
|> printfn "Part 2: %A"