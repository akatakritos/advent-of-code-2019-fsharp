open System

[<EntryPoint>]
let main argv =

    let sum = fun a b -> a + b

    Puzzle01.readInput "inputs\\puzzle01.txt"
    |> Seq.map Puzzle01.calculateFuel
    |> Seq.reduce sum
    |> printfn "the total fuel is %d"

    Puzzle01.readInput "inputs\\puzzle01.txt"
    |> Seq.map Puzzle01.calculateExhaustiveFuel
    |> Seq.reduce sum
    |> printfn "the total exhaustive fuel is %d"

    0
