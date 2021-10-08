open System.IO
open System.Text.RegularExpressions

let fields =
    ["ecl"; "pid"; "eyr"; "hcl"; "byr"; "iyr"; "ecl"; "hgt"]

let toInt (str: string) =
    match System.Int32.TryParse str with
    | true,int -> int
    | _ -> 0

let isBetween (l, h) (n: int) =
    l <= n && n <= h

let isValidHeightField field =
    let matches = Regex("(\d+)(in|cm)").Match field
    let (height, unit) = ((matches.Groups.[1].Value |> toInt), matches.Groups.[2].Value)

    match unit with
    | "in" -> 59 <= height && height <= 76
    | "cm" -> 150 <= height && height <= 193
    | _ -> false

let checkValue (field: string) =
    let bork = field.Split ':' |> fun x -> (x.[0], x.[1])
    match bork with
    | "ecl", v ->
        ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> List.contains v
    | "pid", v ->
        Regex("^\d{9}$").IsMatch v
    | "eyr", v ->
        v |> int |> isBetween (2020, 2030)
    | "byr", v ->
        v |> int |> isBetween (1920, 2002)
    | "iyr", v ->
        v |> int |> isBetween (2010, 2020)
    | "hcl", v ->
        Regex("^#[0-9a-zA-Z]{6}$").IsMatch v
    | "hgt", v ->
        isValidHeightField v
    | _, _ -> false

let isValid check (passport: string[]) =
    fields
    |> List.fold (fun state field ->
        passport
        |> Array.exists (fun p -> p.Contains field && check p)
        |> (&&) state) true

let getInput fileName =
    File.ReadAllText fileName
    |> fun s -> s.Split "\r\n\r\n"
    |> Array.map (Regex("\s").Split >> Array.filter ((<>) ""))

// Part 1
getInput "input.txt"
|> Array.filter (isValid (fun _ -> true)) // Don't filter on anything
|> Array.length
|> printfn "There are %i valid passports in this list"

// Part 2
getInput "input.txt"
|> Array.filter (isValid checkValue)
|> Array.length
|> printfn "There are %i valid passports in this list"