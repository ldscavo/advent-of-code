open System.IO

let split l =
    let i = l |> List.length |> (fun x -> x / 2)
    List.splitAt i l

let upper l = 
    l |> split |> snd

let lower l =
    l |> split |> fst

let rowStep = function
    | 'F' -> lower
    | 'B' -> upper
    | _ -> id

let colStep = function
    | 'L' -> lower
    | 'R' -> upper
    | _ -> id

let rec rowOrCol seats stepper steps =
    let currentStep =
        List.head steps
        |> stepper
    let seatOptions = (currentStep seats)

    match List.length seatOptions with
    | 1 -> seatOptions.[0]
    | _ -> rowOrCol seatOptions stepper (List.tail steps)

let row = rowOrCol [0..127] rowStep
let col = rowOrCol [0..7] colStep

let seatId input =
    let (rowNum, colNum) = (row input, col input)
    (rowNum * 8) + colNum

// Part 1
File.ReadAllLines "input.txt"
|> Array.map (Seq.toList >> seatId)
|> Array.max
|> printfn "The highest seatId is: %A"