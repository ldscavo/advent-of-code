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