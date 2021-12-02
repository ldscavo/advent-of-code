open System.IO

type Location =
    { Depth: int
      Position: int
      Aim: int }

let parseInputLine (line: string) =
    line.Split(' ')
    |> fun arr -> (arr[0], (arr[1] |> int))

let updateLocation (loc: Location) (heading, distance) =
    match heading with
    | "forward" -> { loc with Position = loc.Position + distance }
    | "down" -> { loc with Depth = loc.Depth + distance }
    | "up" -> { loc with Depth = loc.Depth - distance }
    | _ -> loc

let updateLocationMkII (loc: Location) (heading, distance) =
    match heading with
    | "forward" ->
        { loc with
            Position = loc.Position + distance
            Depth = loc.Depth + (loc.Aim * distance) }
    | "down" -> { loc with Aim = loc.Aim + distance }
    | "up" -> { loc with Aim = loc.Aim - distance }
    | _ -> loc

let getResult location =
    location.Depth * location.Position

let initialLocation =
    { Depth = 0; Position = 0; Aim = 0 }

let input =
    File.ReadAllLines "input.txt"
    |> Array.map parseInputLine

// Part 1
input
|> Array.fold updateLocation initialLocation
|> getResult
|> printfn "The result for part 1 is: %i"

// Part 2
input
|> Array.fold updateLocationMkII initialLocation
|> getResult
|> printfn "The result for part 2 is: %i"