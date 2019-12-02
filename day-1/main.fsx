open System.IO

let moduleFuel mass = (mass / 3) - 2

let rec fuelFuel mass =
  let fuel = moduleFuel mass
  match fuel > 0 with
    | true -> fuel + (fuelFuel fuel)
    | false -> 0

let modules = File.ReadAllLines "input.txt" |> Seq.map int

// Part 1
modules
  |> Seq.sumBy moduleFuel
  |> printfn "The total fuel requirement is: %i"

// Part 2
modules
  |> Seq.sumBy fuelFuel
  |> printfn "The total (for real this time!) fuel requirement is: %i"