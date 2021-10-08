open System.IO

type Orbit = string * string

let toOrbit (line: array<string>): Orbit =
  (line.[0], line.[1])

let rec getOrbitCount count (orbits: seq<Orbit>) (orbit: Orbit) =
  Seq.tryFind (fun o -> (fst orbit) = (snd o)) orbits
  |> function
    | Some orb -> getOrbitCount (count + 1) orbits orb
    | None -> count

let orbits =
  File.ReadAllLines "input.txt"
  |> Seq.map ((fun line -> line.Split ')') >> toOrbit)

// Part 1
orbits
|> Seq.sumBy (getOrbitCount 1 orbits)
|> printfn "%A"

let rec getOrbitSteps (steps: Set<string>) (orbits: seq<Orbit>) (orbit: Orbit) =
  Seq.tryFind (fun o -> (fst orbit) = (snd o)) orbits
  |> function
    | Some orb -> getOrbitSteps (Set.add (fst orb) steps) orbits orb
    | None -> steps

let getSteps name =
  Seq.find (fun o -> (snd o) = name) orbits
  |> (fun orb -> getOrbitSteps (Set.ofList [(fst orb)]) orbits orb)

let difference set1 set2 =
  let intersection = Set.intersect set1 set2
  Set.union (Set.difference set1 intersection) (Set.difference set2 intersection)

//Part 2
(getSteps "YOU", getSteps "SAN")
||> difference
|> Set.count
|> printfn "%A"