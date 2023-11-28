open System.IO

let getMarker file charCount =
    File.ReadAllText file
    |> Seq.toArray
    |> Array.windowed charCount
    |> Array.mapi (fun i x -> 
        let distinctCount = x |> Array.distinct |> Array.length
        if distinctCount < charCount then 0 else i)
    |> Array.filter (fun x -> x <> 0)
    |> Array.head
    |> ((+) charCount)
    |> printfn "first marker after character %i"

// Part 1
getMarker "input.txt" 4

// Part 2
getMarker "input.txt" 14