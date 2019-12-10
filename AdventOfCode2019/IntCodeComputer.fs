module IntCodeComputer

type Computer = {
    Memory: int64[]
    Pointer: int;
    RelativeBase: int;
}
type Parameter =
    | PositionMode of int64
    | ImmediateMode of int64
    | RelativeMode of int64

type BinaryOperation = {
    Left: Parameter
    Right: Parameter
    Output: Parameter
}

type UnaryOperation = {
    Parameter: Parameter;
}

type JumpOperation = {
    Parameter: Parameter;
    Jump: Parameter;
}

type OpCode =
    | Multiply of BinaryOperation
    | Add of BinaryOperation
    | Input of UnaryOperation
    | Output of UnaryOperation
    | JumpTrue of JumpOperation
    | JumpFalse of JumpOperation
    | LessThan of BinaryOperation
    | Equals of BinaryOperation
    | RelativeBaseOffset of UnaryOperation
    | Halt

let write computer (output: Parameter, value) =
    match output with
    | PositionMode address
    | ImmediateMode address -> Array.set computer.Memory (int address) value
    | RelativeMode address -> Array.set computer.Memory (computer.RelativeBase + int address) value

let advance computer count =
    { computer with Pointer = computer.Pointer + count }

let read computer parameter =
    match parameter with
        | PositionMode value -> computer.Memory.[value |> int]
        | ImmediateMode value -> value
        | RelativeMode value -> computer.Memory.[computer.RelativeBase + int value ]

let applyBinaryOperation (computer: Computer) (op: BinaryOperation) mathOperation =
    let reader = read computer
    let left = reader op.Left
    let right = reader op.Right

    write computer (op.Output, mathOperation left right)

    advance computer 4

let multiply (computer: Computer) (op: BinaryOperation) =
    applyBinaryOperation computer op (*)

let add (computer: Computer) (op: BinaryOperation) =
    applyBinaryOperation computer op (+)

let input (inputter: unit -> int64) (computer: Computer) (op: UnaryOperation) =
    let result = inputter ()
    write computer (op.Parameter, result)
    advance computer 2

let output (outputter: int64 -> unit) (computer: Computer) (op: UnaryOperation) =
    let value = read computer op.Parameter
    outputter value
    advance computer 2

let jump computer address =
    { computer with Pointer = address}

let jumpOp (computer: Computer) (op: JumpOperation) comparer =
    let value = read computer op.Parameter
    let address = read computer op.Jump |> int
    if comparer value then (jump computer address) else (advance computer 3)


let private True value = value <> 0L
let private False value = value = 0L

let private jumpTrue (computer: Computer) (op: JumpOperation) =
    jumpOp computer op True

let private jumpFalse (computer: Computer) (op: JumpOperation) =
    jumpOp computer op False


let private LessThanComparison a b = a < b
let private EqualComparison a b = a = b

let comparison (computer: Computer) (op: BinaryOperation) comparer =
    let reader = read computer
    let left = reader op.Left
    let right = reader op.Right

    let result = if (comparer left right) then 1L else 0L
    write computer (op.Output, result)
    advance computer 4

let lessThan (computer: Computer) (op: BinaryOperation) =
    comparison computer op LessThanComparison

let equalTo (computer: Computer) (op: BinaryOperation) =
    comparison computer op EqualComparison

let updateRelativeBase (computer: Computer) (op: UnaryOperation) =
    let offset = read computer op.Parameter |> int
    let computer' = { computer with RelativeBase = computer.RelativeBase + offset; }
    advance computer' 2


let parseMode instruction (parameter: int) =
    let strippedOpCode = instruction / (100 * int (10.0 ** float parameter));
    let digit = strippedOpCode % 10
    match digit with
        | 0 -> PositionMode
        | 1 -> ImmediateMode
        | 2 -> RelativeMode
        | _ -> sprintf "Unrecognized parameter mode digit %d" digit |> failwith

let parseBinaryOperation computer =
    let instruction = computer.Memory.[computer.Pointer] |> int;
    let leftParam = computer.Memory.[computer.Pointer + 1];
    let rightParam = computer.Memory.[computer.Pointer + 2];
    let outputParam = computer.Memory.[computer.Pointer + 3];

    let leftMode = parseMode instruction 0
    let rightMode = parseMode instruction 1
    let outputMode = parseMode instruction 2

    { 
        Left = leftMode leftParam
        Right = rightMode rightParam
        Output = outputMode outputParam
    }

let parseUnaryOperation computer =
    let instruction = computer.Memory.[computer.Pointer] |> int;
    let param = computer.Memory.[computer.Pointer + 1];

    let mode = parseMode instruction 0

    {
        Parameter = mode param
    }

let parseJumpOperation computer =
    let instruction = computer.Memory.[computer.Pointer] |> int;
    let parameter = computer.Memory.[computer.Pointer + 1];
    let address = computer.Memory.[computer.Pointer + 2];
    let parameterMode = parseMode instruction 0
    let addressMode = parseMode instruction 1

    {
        Parameter = parameterMode parameter
        Jump = addressMode address
    }

let parseInstruction computer =
    let instruction = (int computer.Memory.[computer.Pointer]) % 100;
    match instruction with
        | 1 -> Add (parseBinaryOperation computer)
        | 2 -> Multiply (parseBinaryOperation computer)
        | 3 -> Input (parseUnaryOperation computer)
        | 4 -> Output (parseUnaryOperation computer)
        | 5 -> JumpTrue (parseJumpOperation computer)
        | 6 -> JumpFalse (parseJumpOperation computer)
        | 7 -> LessThan (parseBinaryOperation computer)
        | 8 -> Equals (parseBinaryOperation computer)
        | 9 -> RelativeBaseOffset (parseUnaryOperation computer)
        | 99 -> Halt
        | _ -> sprintf "Unrecognized instruction: '%d'" instruction |> failwith

let consoleInputter () = 
    printfn "Input required:"
    System.Console.ReadLine() |> int64

let consoleOutputter (n: int64) = printfn "%d" n

let sequenceInputter inputs =
    let mutable l = inputs
    let inputter' () =
        let temp = List.head l
        l <- List.tail l
        temp

    inputter'


let loadProgram (instructionString: string) =
    let memory = Array.zeroCreate<int64> (10*1024)

    instructionString.Split(",")
    |> Seq.map int64
    |> Seq.iteri (fun i value -> memory.[i] <- value)


    { Pointer = 0; Memory = memory; RelativeBase = 0 }


let loadProgramFromFile file =
    loadProgram (System.IO.File.ReadAllText(file))

type TickResult =
    | Continue of Computer
    | Abort

type IoTickResult =
    | Continue of Computer
    | OutputPause of Computer
    | InputPause of Computer
    | Abort

type ComputerState =
    | PausedForInput of Computer
    | PausedForOutput of Computer
    | Aborted

let advanceToIo computer =
    let tick computer =
       let instruction = parseInstruction computer
       match instruction with
           | Multiply op -> Continue (multiply computer op)
           | Add op -> Continue (add computer op)
           | Input op -> InputPause computer
           | Output op -> OutputPause computer
           | JumpTrue op -> Continue (jumpTrue computer op)
           | JumpFalse op -> Continue (jumpFalse computer op)
           | LessThan op -> Continue (lessThan computer op)
           | Equals op -> Continue (equalTo computer op)
           | RelativeBaseOffset op -> Continue (updateRelativeBase computer op)
           | Halt -> Abort

    let rec recurse computer =
       let result = tick computer
       match result with
           | Continue c -> recurse c
           | OutputPause c -> PausedForOutput c
           | InputPause c -> PausedForInput c
           | Abort -> Aborted

    recurse computer

let provideInput value computer =
    let processInput = input (fun () -> value)

    let instruction = parseInstruction computer
    match instruction with
        | Input op -> processInput computer op
        | _ -> failwith "called provideInput when not on an input instruction"

let retrieveOutput computer =
    let processOutput (op: UnaryOperation) =
        let value = read computer op.Parameter
        (advance computer 2, value)

    let instruction = parseInstruction computer
    match instruction with
        | Output op -> processOutput op
        | _ -> failwith "called retrieveOutput when not on an output instruction"

    

let run computer inputter outputter =
    let input = input inputter
    let output = output outputter

    let tick computer =
        let instruction = parseInstruction computer
        match instruction with
            | Multiply op -> TickResult.Continue (multiply computer op)
            | Add op -> TickResult.Continue (add computer op)
            | Input op -> TickResult.Continue (input computer op)
            | Output op -> TickResult.Continue (output computer op)
            | JumpTrue op -> TickResult.Continue (jumpTrue computer op)
            | JumpFalse op -> TickResult.Continue (jumpFalse computer op)
            | LessThan op -> TickResult.Continue (lessThan computer op)
            | Equals op -> TickResult.Continue (equalTo computer op)
            | RelativeBaseOffset op -> TickResult.Continue (updateRelativeBase computer op)
            | Halt -> TickResult.Abort

    let rec recurse computer =
        let result = tick computer
        match result with
            | TickResult.Continue c -> recurse c
            | TickResult.Abort -> computer

    recurse computer


