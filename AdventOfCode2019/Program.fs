﻿open System

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
    let travel = Puzzle03.findMinimumTravelDistance wire1 wire2

    printfn "The minimum distance is %d" distance
    printfn "However, the minimum travel distance is %d" travel

let puzzle04 () =
    let START = 206938
    let STOP = 679128
    let valid1 = Puzzle04.countValidPasswords START STOP Puzzle04.isValidPassword
    let valid2 = Puzzle04.countValidPasswords START STOP Puzzle04.isValidPassword2
    printfn "There are %d valid passwords in the range." valid1
    printfn "There are %d valid passwords in the range under the new rules." valid2

let puzzle05 () =
    let computer = Puzzle05.loadProgramFromFile "inputs\\puzzle05.txt"
    // let computer = Puzzle05.loadProgram "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
    // let computer = Puzzle05.loadProgram "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    let result = Puzzle05.run computer Puzzle05.consoleInputter Puzzle05.consoleOutputter
    printfn "Done"

let puzzle06 () =
    let tree = Puzzle06.buildFromFile "inputs\\puzzle06.txt"
    let total = Puzzle06.totalDepth tree
    printfn "Total depth of orbits: %d" total

    let distance = Puzzle06.transferDistance tree "YOU" "SAN"
    printfn "You have to make %A transfers from YOU to SAN" distance

[<EntryPoint>]
let main argv =

    // puzzle01 ()
    // puzzle02 ()
    // puzzle03 ()
    // puzzle04 ()
    // puzzle05 ()
    puzzle06 ()
    0

