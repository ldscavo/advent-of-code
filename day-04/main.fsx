type Comparer = bool -> (char * char) -> bool

let range = [356261..846303]

let isSixDigits input =
  input |> String.length = 6

let compareTwoDigits (compare: Comparer) (start: bool) (input: string) =
  Seq.toList input
    |> List.pairwise
    |> List.fold compare start

let areEqual: Comparer =
  fun x (y, z) -> x || (y = z)

let areIncreasing: Comparer =
  fun x (y, z) -> x && ((int y) <= (int z))

let hasTwoAdjacentDigits = compareTwoDigits areEqual false
let doDigitsNeverDecrease = compareTwoDigits areIncreasing true

let validPasswords =
  range
    |> List.map string
    |> List.filter isSixDigits
    |> List.filter hasTwoAdjacentDigits
    |> List.filter doDigitsNeverDecrease

printfn "The total count of valid passwords is: %i" <| List.length validPasswords