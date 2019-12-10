module Puzzle07
open IntCodeComputer


// http://www.fssnip.net/4u/title/Very-Fast-Permutations
let rec insertions x = function
    | [] -> [[x]]
    | (y::ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | [] -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))




let outputSignal program phaseSettings =
    let run input phase =
        let computer = IntCodeComputer.loadProgram program
        let inputter = IntCodeComputer.sequenceInputter [ phase; input; ]
        let mutable result = 0L
        let outputter r =
            result <- r

        IntCodeComputer.run computer inputter outputter |> ignore
        result

    let STARTING_INPUT = 0L
    phaseSettings
    |> List.fold run STARTING_INPUT
    

let largestOutputSignal program =
    let execute = outputSignal program
    let possiblePhaseSettings = permutations [0L; 1L; 2L; 3L; 4L;]

    possiblePhaseSettings
    |> Seq.map execute
    |> Seq.max

type AmpLoop = {
    a: Computer;
    b: Computer;
    c: Computer;
    d: Computer;
    e: Computer;
}

let createAmpLoop program =
    {
        a = loadProgram program;
        b = loadProgram program;
        c = loadProgram program;
        d = loadProgram program;
        e = loadProgram program;
    }

let private extractComputer = function
    | PausedForInput computer -> computer
    | PausedForOutput computer -> computer
    | Aborted -> failwith "Couldnt extract"

let initalizeComputer computer phase =
    let state = IntCodeComputer.advanceToIo computer
    match state with 
        | PausedForInput computer ->
            IntCodeComputer.provideInput phase computer |> IntCodeComputer.advanceToIo |> extractComputer
        | _ -> failwith "initializeComputer did not find a input"

let initializeComputers phaseSettings amps =
    match phaseSettings with
        | (a::b::c::d::e::_) ->
            { amps with
                a = (initalizeComputer amps.a a);
                b = initalizeComputer amps.b b;
                c = initalizeComputer amps.c c;
                d = initalizeComputer amps.d d;
                e = initalizeComputer amps.e e;
            }
            
        | _ -> failwith "bad phase settings"


let runCycle (amp: IntCodeComputer.Computer) input =
    amp 
    |> IntCodeComputer.advanceToIo
    |> extractComputer
    |> IntCodeComputer.provideInput input
    |> IntCodeComputer.advanceToIo
    |> extractComputer
    |> IntCodeComputer.retrieveOutput


type RunLoopResult =
    | Continue of AmpLoop * int64
    | Abort of int64

let runLoop amps aInput =
    let (a, aOutput) = runCycle amps.a aInput
    let (b, bOutput) = runCycle amps.b aOutput
    let (c, cOutput) = runCycle amps.c bOutput
    let (d, dOutput) = runCycle amps.d cOutput
    let (e, eOutput) = runCycle amps.e dOutput

    match IntCodeComputer.advanceToIo e with
        | IntCodeComputer.PausedForOutput e' 
        | IntCodeComputer.PausedForInput e' -> Continue ({ a = a; b = b; c = c; d = d; e = e'; }, eOutput)
        | IntCodeComputer.Aborted -> Abort eOutput

let feedbackSignal program phaseSettings =
    let amps = createAmpLoop program |> initializeComputers phaseSettings

    let rec recurse amps aInput =
        match runLoop amps aInput with
            | Continue (a, nextInput) -> recurse a nextInput
            | Abort result -> result

    recurse amps 0L

let maxFeedbackSignal program = 
    let execute = feedbackSignal program
    let phaseSettingPermutations = permutations [5L;6L;7L;8L;9L]

    phaseSettingPermutations
    |> Seq.map execute
    |> Seq.max
