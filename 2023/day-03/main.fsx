open System.IO

let input =
    File.ReadAllLines "example.txt"

input
|> Array.iter (printfn "%A")
