open System.IO

let input =
    File.ReadAllLines "input.txt"
    |> Array.map Seq.toList
    |> Array.toList

let nthBit (input: char list list) (n: int) =
    input |> List.map (fun inp -> inp[n])

let greater (bits: char list) =
    bits |> List.countBy id |> List.maxBy snd |> fst

let lesser (bits: char list) =
    bits |> List.countBy id |> List.minBy snd |> fst

let toInt (arr: char list) =
    arr
    |> Array.ofList
    |> System.String.Concat
    |> fun s -> System.Convert.ToInt32(s, 2)

let getValue (f: list<char> -> char) input =
    input
    |> List.transpose
    |> List.map f
    |> toInt

let gamma = getValue greater
let epsilon = getValue lesser

// Part 1
(input |> gamma) * (input |> epsilon)
|> (printfn "The power consumption is: %i")