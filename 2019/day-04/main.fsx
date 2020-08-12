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

printfn "The Part 1 valid password count is: %i" <| List.length validPasswords

let addOrAcc (m: Map<char, int>) (i: char) =
  match m.ContainsKey i with
  | true -> m.Add(i, (m.[i]+1))
  | false -> m.Add(i, 1)

let hasOnlyTwoAjacentDigits (input: string) =
  Seq.toList input
  |> List.fold addOrAcc Map.empty
  |> Map.filter (fun m i -> i = 2)
  |> Map.isEmpty
  |> not    

let validPasswordsMkII =
  validPasswords
  |> List.filter hasOnlyTwoAjacentDigits

printfn "The Part 2 valid password count is: %A" <| List.length validPasswordsMkII