open System.IO
open System.Text.RegularExpressions

type Step = { Count: int; From: string; To: string }

type CrateMover =
    | CrateMover9000
    | CrateMover9001

let split input =
    let index = input |> Array.findIndex (fun i -> i = "")
    input |> Array.splitAt index

let toMap (map: Map<string, string[]>) (arr: string[]) =
    let (key, contents) = arr |> Array.splitAt 1
    map |> Map.add (key |> Array.head) (contents |> Array.rev |> Array.filter (fun s -> s <> ""))

let parseStackInput =    
    Array.map (fun i -> Regex("[\[\]]").Replace(i, ""))
    >> Array.map (Regex("\s{1,4}").Split)
    >> Array.collect Array.indexed 
    >> Array.groupBy fst 
    >> Array.map (snd >> Array.map snd)
    >> Array.map Array.rev
    >> Array.fold toMap Map.empty

let parseProcedureInput (input: string[]) =
    let regex = Regex("move ([0-9]+) from ([0-9]+) to ([0-9]+)")
    input
    |> Array.filter (fun i -> i <> "")
    |> Array.map (fun i ->
        let groups = regex.Match(i).Groups
        { Count = (groups[1].Value |> int)
          From = groups[2].Value
          To = groups[3].Value })

let parseBoth (rawStacks, rawSteps) =
    (parseStackInput rawStacks, parseProcedureInput rawSteps)

let makeMove (mover: CrateMover) (stacks: Map<string, string[]>) step =
    let fromStack =
        stacks[step.From]
        |> Array.removeManyAt 0 step.Count

    let toStack =
        match mover with
        | CrateMover9000 -> stacks[step.To] |> Array.append (stacks[step.From] |> Array.take step.Count |> Array.rev)
        | CrateMover9001 -> stacks[step.To] |> Array.append (stacks[step.From] |> Array.take step.Count)
    
    let newStack =
        stacks
        |> Map.add step.From fromStack
        |> Map.add step.To toStack

    //printfn "%A" newStack
    newStack

let (stacks, steps) =
     File.ReadAllLines "input.txt"
     |> split
     |> parseBoth

// Part 1
steps
|> Array.fold (makeMove CrateMover9000) stacks
|> Map.values
|> Seq.map Array.head
|> System.String.Concat
|> printfn "Part 1: %A"

// Part 1
steps
|> Array.fold (makeMove CrateMover9001) stacks
|> Map.values
|> Seq.map Array.head
|> System.String.Concat
|> printfn "Part 2: %A"