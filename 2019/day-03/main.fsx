open System.IO

type Coordinate = int * int

type Direction = Up | Down | Left | Right // I'm gonna move my feet tonight
type Step = Direction * int

let toDirection = function
| 'U' -> Up
| 'D' -> Down
| 'L' -> Left
| 'R' -> Right
| _ -> failwith "Invalid direction"

let toSteps (line: string): seq<Step> =
  line.Split ','
  |> Seq.map (fun s -> (toDirection s.[0], int s.[1..]))

let expandSteps (step: Step) =
  List.fold (fun acc s -> (fst step) :: acc) [] [1..(snd step)]

let coordinateDelta = function
| Up    -> ( 0, +1)
| Down  -> ( 0, -1)
| Left  -> (-1,  0)
| Right -> (+1,  0)

let addCoordinate coord1 coord2 =
  (fst coord1 + fst coord2, snd coord1 + snd coord2)

let nextStep (current: Coordinate list) step =
  (addCoordinate current.[0] (coordinateDelta step)) :: current

let readCoordinatesFromFile filename =
  File.ReadLines filename
  |> Seq.map (toSteps >> Seq.collect expandSteps)

let paths = Seq.toList <| readCoordinatesFromFile "input.txt"

let manhattanDistance (coords: Coordinate) =
  abs (fst coords) + abs (snd coords)

let firstPathSteps =
  paths.[0] |> Seq.fold nextStep [(0, 0)] |> Set.ofList

let secondPathSteps =
  paths.[1] |> Seq.fold nextStep [(0, 0)] |> Set.ofList

let sharedSteps =
  Set.intersect firstPathSteps secondPathSteps

// Part 1
sharedSteps
|> Set.map manhattanDistance
|> Set.filter (fun m -> m > 0)
|> Set.minElement
|> printfn "%A"