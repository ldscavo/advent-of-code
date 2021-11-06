open System.IO
open System.Text.RegularExpressions

// Part 1
File.ReadAllText "input.txt"
|> fun s -> s.Split "\r\n\r\n"
|> Array.sumBy
    ( fun s -> Regex("[^a-zA-z]").Replace(s, "")
      >> Seq.toList
      >> List.distinct
      >> List.length )
|> (printfn "The sum of the counts is: %i")

// Part 2
File.ReadAllText "input.txt"
|> (fun s -> s.Split "\r\n\r\n")
|> Array.map (fun s ->
    s.Split "\r\n" |> Array.map (Seq.toList >> Set.ofList)
    |> (fun ss -> ss |> Array.fold Set.intersect (Array.head ss)))
|> Array.sumBy Set.count
|> printfn "The sum of the unique answers is: %i"