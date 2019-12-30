module Puzzle02


let parse (s: string) =
    s.Split(",")
    |> Seq.map int
    |> Seq.toArray

type State = {
    index: int
    data: int[]
}

let private read1 state =
    let index = state.data.[state.index + 1];
    state.data.[index]

let private read2 state =
    let index = state.data.[state.index + 2];
    state.data.[index]

let private store state value =
    let index = state.data.[state.index + 3];
    Array.set state.data index value

let createState (s:string) =
    {
        index = 0;
        data = parse s
    }


type StepResult =
    | Continue of State
    | Complete of int
    | Abort of int

let private add state =
    let operand1 = read1 state
    let operand2 = read2 state

    let sum = operand1 + operand2
    store state sum

    { state with index = state.index + 4 }

let private multiply state =
    let operand1 = read1 state
    let operand2 = read2 state

    let product = operand1 * operand2;
    store state product

    { state with index = state.index + 4 }

let step state =
    let command = state.data.[state.index]
    match command with
        | 1 -> Continue (add state)
        | 2 -> Continue (multiply state)
        | 99 -> Complete (state.data.[0])
        | _ -> Abort command

let unwrapContinue result =
    match result with
        | Continue state -> state
        | _ -> failwith "did not get a Continue case"

let rec execute state =
    let result = step state
    match result with
        | Continue s -> execute s
        | Complete result -> result
        | Abort command -> failwith (sprintf "Bad command: '%d'" command)


let executeWithState input noun verb =
    let state0 = createState input
    Array.set state0.data 1 noun
    Array.set state0.data 2 verb
    execute state0

type NounVerb = {
    noun: int;
    verb: int;
}

let calculateNounVerb input =
    let expected = 19690720;

    let nouns = seq { 0..99 }
    let verbs = seq { 0..99 }

    let result =
        nouns
        |> Seq.collect (fun n -> verbs |> Seq.map (fun v -> (n, v)))
        |> Seq.map (fun (n, v) -> (n, v, (executeWithState input n v)))
        |> Seq.find (fun (_, _, result) -> result = expected)

    let (noun, verb, _) = result
    { noun = noun; verb = verb;}
