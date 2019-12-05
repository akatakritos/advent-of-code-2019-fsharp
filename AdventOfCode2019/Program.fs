open System

let puzzle01 () =
    let sum = fun a b -> a + b

    Puzzle01.readInput "inputs\\puzzle01.txt"
    |> Seq.map Puzzle01.calculateFuel
    |> Seq.reduce sum
    |> printfn "the total fuel is %d"

    Puzzle01.readInput "inputs\\puzzle01.txt"
    |> Seq.map Puzzle01.calculateExhaustiveFuel
    |> Seq.reduce sum
    |> printfn "the total exhaustive fuel is %d"

let puzzle02 () =
    let input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,23,6,27,2,9,27,31,1,5,31,35,1,35,10,39,1,39,10,43,2,43,9,47,1,6,47,51,2,51,6,55,1,5,55,59,2,59,10,63,1,9,63,67,1,9,67,71,2,71,6,75,1,5,75,79,1,5,79,83,1,9,83,87,2,87,10,91,2,10,91,95,1,95,9,99,2,99,9,103,2,10,103,107,2,9,107,111,1,111,5,115,1,115,2,119,1,119,6,0,99,2,0,14,0"
    let state0 = Puzzle02.createState input
    Array.set state0.data 1 12
    Array.set state0.data 2 2

    let result = Puzzle02.execute state0
    printfn "The first index result is %d" result

    let nounVerb = Puzzle02.calculateNounVerb input
    let product = 100 * nounVerb.noun + nounVerb.verb
    printfn "The inputs are %d, %d => %d" nounVerb.noun nounVerb.verb product

let puzzle03 () =
    let input = System.IO.File.ReadAllLines("inputs\\puzzle03.txt");
    let wire1 = Puzzle03.parseInstructions input.[0]
    let wire2 = Puzzle03.parseInstructions input.[1]
    let distance = Puzzle03.findMinimumIntersectionDistance wire1 wire2

    printfn "The minimum distance is %d" distance


[<EntryPoint>]
let main argv =

    // puzzle01 ()
    // puzzle02 ()
    puzzle03 ()
    0

