open System.IO

let mutable opCodes =
  File.ReadAllLines "input.txt"
    |> Seq.collect (fun line -> line.Split ',')
    |> Seq.map int
    |> Seq.toList

let runOperation = function
  | 1 -> (+)
  | 2 -> (*)
  | _ -> failwith "Invalid opcode"

let mutable index = 0

// Forgive me, for I have sinned
while opCodes.[index] <> 99 do
  let op1index = opCodes.[index+1]
  let op2index = opCodes.[index+2]
  let targetIndex = opCodes.[index+3]
  
  let newValue = (runOperation opCodes.[index]) opCodes.[op1index] opCodes.[op2index]

  opCodes <- opCodes |> List.mapi (fun i v -> if i = targetIndex then newValue else v)

  index <- index + 4

printfn "%i" opCodes.[0]