open System.IO

type Hand =
    | Rock
    | Paper
    | Scissors

let handScore = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let matchScore  = function
    | Rock, Paper -> 6
    | Rock, Scissors -> 0
    | Paper, Scissors -> 6
    | Paper, Rock -> 0
    | Scissors, Rock -> 6
    | Scissors, Paper -> 0
    | _, _ -> 3 // draw

let score round = 
    (matchScore round) + (snd round |> handScore)

let scoreStr round = 
    $"{(matchScore round)} + {(snd round |> handScore)}"


let toHand = function
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> failwith "invalid input"

let input = 
    File.ReadAllLines "input.txt"
    |> Array.map (fun i -> i.Split " ")

// Part 1
input
|> Array.map (fun arr -> (arr[0] |> toHand, arr[1] |> toHand))
|> Array.map score
|> Array.sum
|> printfn "The final score is: %i"

// Part 2
let win = function
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock

let lose = function
    | Rock -> Scissors
    | Paper -> Rock
    | Scissors -> Paper

let smartHand = function
    | "X" -> lose
    | "Y" -> id
    | "Z" -> win
    | _ -> failwith "invalid input"

let part2Round (arr: string[]) =
    let opponent = arr[0] |> toHand
    let self = opponent |> (smartHand arr[1])
    (opponent, self)

input
|> Array.map part2Round
|> Array.map score
|> Array.sum
|> printfn "The final score is: %A"