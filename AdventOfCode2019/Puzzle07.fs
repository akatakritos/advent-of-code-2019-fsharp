module Puzzle07


// http://www.fssnip.net/4u/title/Very-Fast-Permutations
let rec insertions x = function
    | [] -> [[x]]
    | (y::ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | [] -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))


let sequenceInputter inputs =
    let mutable l = inputs
    let inputter' () =
        let temp = List.head l
        l <- List.tail l
        temp

    inputter'

let outputSignal program phaseSettings =
    let run input phase =
        let computer = IntCodeComputer.loadProgram program
        let inputter = sequenceInputter [ phase; input; ]
        let mutable result = 0
        let outputter r =
            result <- r

        IntCodeComputer.run computer inputter outputter |> ignore
        result

    let STARTING_INPUT = 0
    phaseSettings
    |> List.fold run STARTING_INPUT
    

let largestOutputSignal program =
    let execute = outputSignal program
    let possiblePhaseSettings = permutations [0; 1; 2; 3; 4;]

    possiblePhaseSettings
    |> Seq.map execute
    |> Seq.max
    


