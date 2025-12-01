open System.IO

let toDial (x: string) =
    let steps = x[1..] |> int
    match x[0] with
    | 'R' -> steps
    | 'L' -> steps * -1
    | _ -> failwith "Invalid direction"

let clicks =
    File.ReadAllLines "input.txt"
    |> Array.map toDial 

let calc state x = (100 + state + x) % 100

clicks
|> Array.scan calc 50
|> Array.filter ((=) 0)
|> Array.length
|> printfn "%A"
