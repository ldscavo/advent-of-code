open System.IO

type Opcode  = Add | Multiply | Save | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals
type Mode = Position | Immediate
type ModedOpcode = { Opcode: Opcode; Modes: Mode * Mode * Mode }
type Program = int list
type Operation = ModedOpcode -> Program -> int -> Program
type Instruction = ModedOpcode -> Operation

let fst (x, _, _) = x
let snd (_, x, _) = x
let trd (_, _, x) = x

let input = 8 // Ship's AC unit id

let getOpcode = function
  | 1 -> Add
  | 2 -> Multiply
  | 3 -> Save
  | 4 -> Output
  | 5 -> JumpIfTrue
  | 6 -> JumpIfFalse
  | 7 -> LessThan
  | 8 -> Equals
  | _ -> failwith "Invalid opcode"

let getMode = function
  | 0 -> Position
  | 1 -> Immediate
  | _   -> failwith "Invalid mode"

let getModedOpCode (input: int) =
  //printfn "%A" input
  let op   = getOpcode (input % 100)
  let fstM = getMode ((input / 100) % 10)
  let sndM = getMode ((input / 1000) % 10)
  let trdM = getMode ((input / 10000) % 10)

  { Opcode = op; Modes = (fstM, sndM, trdM) }

let fstVal (mOp: ModedOpcode) (pr: Program) ind =
  match (fst mOp.Modes) with
    | Position -> pr.[pr.[ind+1]]
    | Immediate -> pr.[ind+1]

let sndVal (mOp: ModedOpcode) (pr: Program) ind =
  match (snd mOp.Modes) with
    | Position -> pr.[pr.[ind+2]]
    | Immediate -> pr.[ind+2]

let trdVal (mOp: ModedOpcode) (pr: Program) ind =
  match (trd mOp.Modes) with
    | Position -> pr.[pr.[ind+3]]
    | Immediate -> pr.[ind+3]    

let add: Operation =
  fun mop pr ind ->
    let newValue = (fstVal mop pr ind) + (sndVal mop pr ind)
    List.mapi (fun i v -> if i = pr.[ind+3] then newValue else v) pr

let mult: Operation =
  fun mop pr ind ->
    let newValue = (fstVal mop pr ind) * (sndVal mop pr ind)
    List.mapi (fun i v -> if i = pr.[ind+3] then newValue else v) pr

let save: Operation =
  fun mop pr ind ->
    List.mapi (fun i v -> if i = pr.[ind + 1] then input else v) pr

let output: Operation =
  fun mop pr ind ->
    printfn "%i" (fstVal mop pr ind)
    pr

let ifTrue: Operation =
  fun mop pr ind ->
    let fstParam = fstVal mop pr ind
    let sndParam = sndVal mop pr ind
    printfn "%A: %A %A -> %A" ind fstParam sndParam pr.[sndParam]
    match fstParam > 0 with
      | true -> List.mapi (fun i v -> if i = ind then pr.[sndParam] else v) pr
      | false -> pr

let ifFalse: Operation =
  fun mop pr ind ->
    let fstParam = fstVal mop pr ind
    let sndParam = sndVal mop pr ind
    //printfn "%A: %A %A" ind fstParam sndParam
    match fstParam = 0 with
      | true -> List.mapi (fun i v -> if i = ind then pr.[sndParam] else v) pr
      | false -> pr

let lessThan: Operation =
  fun mop pr ind ->
    let fstParam = fstVal mop pr ind
    let sndParam = sndVal mop pr ind
    let trdParam = pr.[ind+3]

    match fstParam < sndParam with
      | true -> List.mapi (fun i v -> if i = trdParam then 1 else v) pr
      | false -> List.mapi (fun i v -> if i = trdParam then 0 else v) pr

let equals: Operation =
  fun mop pr ind ->
    let fstParam = fstVal mop pr ind
    let sndParam = sndVal mop pr ind
    let trdParam = pr.[ind+3]
    //printfn "%A" mop  
    printfn "equals %A: %A %A -> %A" ind fstParam sndParam trdParam
    match fstParam = sndParam with
      | true -> List.mapi (fun i v -> if i = trdParam then 1 else v) pr
      | false -> List.mapi (fun i v -> if i = trdParam then 0 else v) pr

let instr: Instruction = 
  fun mop ->
    match mop.Opcode with
      | Add         -> add
      | Multiply    -> mult
      | Save        -> save
      | Output      -> output
      | JumpIfTrue  -> ifTrue
      | JumpIfFalse -> ifFalse
      | LessThan    -> lessThan
      | Equals      -> equals

let nextIndex op i c =
  match op with
    | Add  | Multiply          -> i + 4
    | Save | Output            -> i + 2
    | LessThan | Equals        -> i + 4
    | JumpIfTrue | JumpIfFalse -> if c then i else i + 3

let mutable program =
  File.ReadAllLines "test-input.txt"
    |> Seq.collect (fun line -> line.Split ',')
    |> Seq.map int
    |> Seq.toList

let mutable index = 0

// Forgive me, for I have sinned
while program.[index] <> 99 do
  printfn "%A" program.[index]
  let mop = getModedOpCode program.[index]

  let newProgram = (instr mop) mop program index
  //printfn "%A" newProgram
  let wasChanged = program <> newProgram

  program <- newProgram
  index <- nextIndex mop.Opcode index wasChanged