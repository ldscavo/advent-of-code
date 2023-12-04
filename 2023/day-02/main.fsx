open System.IO
open System.Text.RegularExpressions

let red = Regex("([0-9]+) red")
let blue = Regex("([0-9]+) blue")
let green = Regex("([0-9]+) green")

let maxRed = 12
let maxGreen = 13
let maxBlue = 14

let topColor (color: Regex) (s: string) =
    color.Matches s
    |> Seq.map (fun m -> m.Groups[1].Value |> int)
    |> Seq.max

let isPossible (s: string) =
    s |> topColor red <= maxRed &&
    s |> topColor green <= maxGreen &&
    s |> topColor blue <= maxBlue

let getPower (s: string) =
    (s |> topColor red) *
    (s |> topColor green) *
    (s |> topColor blue)

let toGame (line: string) =
    let m = Regex("Game ([0-9]{1,3}): (.+)").Match(line).Groups
    (m[1].Value |> int, m[2].Value)

// Part 1
File.ReadAllLines "input.txt"
|> Array.map toGame
|> Array.filter (snd >> isPossible)
|> Array.map fst
|> Array.fold (+) 0
|> printfn "Part 1: %i"

// Part 2
File.ReadAllLines "input.txt"
|> Array.map toGame
|> Array.map (fun (x, y) -> (x, y |> getPower))
|> Array.map snd
|> Array.fold (+) 0
|> printfn "Part 2: %A"
