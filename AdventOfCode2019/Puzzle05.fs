module Puzzle05

type Computer = {
    Memory: int[]
    Pointer: int;
}


let write computer (address, value) =
    Array.set computer.Memory address value

type ParameterMode = 
    | PositionMode
    | ImmediateMode

type BinaryOperation = {
    LeftMode: ParameterMode;
    LeftParameter: int;
    RightMode: ParameterMode;
    RightParamter: int;
    ResultParameter: int;
}

type UnaryOperation = {
    Mode: ParameterMode;
    Parameter: int;
}

type JumpOperation = {
    Mode: ParameterMode;
    Parameter: int;
    JumpAddressMode: ParameterMode
    JumpAddress: int;
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
    | Halt

let advance computer count =
    { computer with Pointer = computer.Pointer + count }

let read computer (mode: ParameterMode, parameter: int) =
    match mode with
        | PositionMode -> computer.Memory.[parameter]
        | ImmediateMode -> parameter

let applyBinaryOperation (computer: Computer) (op: BinaryOperation) mathOperation =
    let reader = read computer
    let left = reader (op.LeftMode, op.LeftParameter)
    let right = reader (op.RightMode, op.RightParamter)

    write computer (op.ResultParameter, mathOperation left right)

    advance computer 4

let multiply (computer: Computer) (op: BinaryOperation) =
    applyBinaryOperation computer op (*)

let add (computer: Computer) (op: BinaryOperation) =
    applyBinaryOperation computer op (+)

let input (inputter: unit -> int) (computer: Computer) (op: UnaryOperation) =
    let result = inputter ()
    write computer (op.Parameter, result)
    advance computer 2

let output (outputter: int -> unit) (computer: Computer) (op: UnaryOperation) =
    let value = read computer (op.Mode, op.Parameter)
    outputter value
    advance computer 2

let jump computer address =
    { computer with Pointer = address}

let jumpOp (computer: Computer) (op: JumpOperation) comparer =
    let value = read computer (op.Mode, op.Parameter)
    let address = read computer (op.JumpAddressMode, op.JumpAddress)
    if comparer value then (jump computer address) else (advance computer 3)


let private True value = value <> 0
let private False value = value = 0

let private jumpTrue (computer: Computer) (op: JumpOperation) =
    jumpOp computer op True

let private jumpFalse (computer: Computer) (op: JumpOperation) =
    jumpOp computer op False


let private LessThanComparison a b = a < b
let private EqualComparison a b = a = b

let comparison (computer: Computer) (op: BinaryOperation) comparer =
    let reader = read computer
    let left = reader (op.LeftMode, op.LeftParameter)
    let right = reader (op.RightMode, op.RightParamter)

    let result = if (comparer left right) then 1 else 0
    write computer (op.ResultParameter, result)
    advance computer 4

let lessThan (computer: Computer) (op: BinaryOperation) =
    comparison computer op LessThanComparison

let equalTo (computer: Computer) (op: BinaryOperation) =
    comparison computer op EqualComparison

let parseMode instruction (parameter: int) =
    let strippedOpCode = instruction / (100 * int (10.0 ** float parameter));
    let digit = strippedOpCode % 10
    match digit with
        | 0 -> PositionMode
        | 1 -> ImmediateMode
        | _ -> sprintf "Unrecognized parameter mode digit %d" digit |> failwith

let parseBinaryOperation computer =
    let instruction = computer.Memory.[computer.Pointer];
    let leftParam = computer.Memory.[computer.Pointer + 1];
    let rightParam = computer.Memory.[computer.Pointer + 2];
    let outputParam = computer.Memory.[computer.Pointer + 3];

    let leftMode = parseMode instruction 0
    let rightMode = parseMode instruction 1

    { 
        LeftMode = leftMode;
        LeftParameter = leftParam;
        RightMode = rightMode;
        RightParamter = rightParam;
        ResultParameter = outputParam
    }

let parseUnaryOperation computer =
    let instruction = computer.Memory.[computer.Pointer];
    let param = computer.Memory.[computer.Pointer + 1];

    let mode = parseMode instruction 0

    {
        Mode = mode;
        Parameter = param
    }

let parseJumpOperation computer =
    let instruction = computer.Memory.[computer.Pointer];
    let parameter = computer.Memory.[computer.Pointer + 1];
    let address = computer.Memory.[computer.Pointer + 2];
    let parameterMode = parseMode instruction 0
    let addressMode = parseMode instruction 1

    {
        Parameter = parameter;
        Mode = parameterMode;
        JumpAddress = address;
        JumpAddressMode = addressMode;
    }

let parseInstruction computer =
    let instruction = computer.Memory.[computer.Pointer] % 100;
    match instruction with
        | 1 -> Add (parseBinaryOperation computer)
        | 2 -> Multiply (parseBinaryOperation computer)
        | 3 -> Input (parseUnaryOperation computer)
        | 4 -> Output (parseUnaryOperation computer)
        | 5 -> JumpTrue (parseJumpOperation computer)
        | 6 -> JumpFalse (parseJumpOperation computer)
        | 7 -> LessThan (parseBinaryOperation computer)
        | 8 -> Equals (parseBinaryOperation computer)
        | 99 -> Halt
        | _ -> sprintf "Unrecognized instruction: '%d'" instruction |> failwith

let consoleInputter () = 
    printfn "Input required:"
    System.Console.ReadLine() |> int

let consoleOutputter n = printfn "%d" n


let loadProgram (instructionString: string) =
    let memory = 
        instructionString.Split(",")
        |> Seq.map int
        |> Seq.toArray

    { Pointer = 0; Memory = memory }


let loadProgramFromFile file =
    loadProgram (System.IO.File.ReadAllText(file))

type TickResult =
    | Continue of Computer
    | Abort


let run computer inputter outputter =
    let input = input inputter
    let output = output outputter

    let tick computer =
        let instruction = parseInstruction computer
        match instruction with
            | Multiply op -> Continue (multiply computer op)
            | Add op -> Continue (add computer op)
            | Input op -> Continue (input computer op)
            | Output op -> Continue (output computer op)
            | JumpTrue op -> Continue (jumpTrue computer op)
            | JumpFalse op -> Continue (jumpFalse computer op)
            | LessThan op -> Continue (lessThan computer op)
            | Equals op -> Continue (equalTo computer op)
            | Halt -> Abort

    let rec recurse computer =
        let result = tick computer
        match result with
            | Continue c -> recurse c
            | Abort -> computer

    recurse computer


