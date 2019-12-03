module Puzzle01

let calculateFuel mass =
    mass / 3 - 2

let readInput filename = 
    System.IO.File.ReadLines filename
    |> Seq.map int

let rec private recursiveSum mass sum =
    let fuel = calculateFuel mass

    if fuel < 0 then
        sum
    else
        recursiveSum fuel sum + fuel


let calculateExhaustiveFuel mass =
    recursiveSum mass 0
   