module Puzzle19
open System
open IoComputer

type TractorBeamGravity = Stationary | Pulled
module TractorBeamGravity =
    let fromInt64 = function
        | 1L -> Pulled
        | 0L -> Stationary
        | n -> failwithf "Unknown output %d" n

let consolePrint (x, y, c: char) =
    Console.SetCursorPosition(x, y)
    Console.Write(c);

let getBeamGravity (computer: IoComputer) (x: int, y: int) =
    computer.AdvanceToIo ()
    computer.Send (int64 x)
    computer.AdvanceToIo ()
    computer.Send (int64 y)
    computer.AdvanceToIo ()
    computer.Receive () |> TractorBeamGravity.fromInt64



let printBeam program printer =
    let mutable counter = 0
    Console.Clear()

    for y = 0 to 49 do
        for x = 0 to 49 do
            let computer = IoComputer(program)
            let result = getBeamGravity computer (x, y)

            match result with
                | Pulled ->
                    counter <- counter + 1
                    printer (x, y, '#')
                | Stationary -> printer (x, y, '.')

    counter

let part1 input =
    printBeam input consolePrint
    |> printfn "Total affected spaces: %d"

let private findY input x start =
    let rec findYRecursive i =
        let computer = IoComputer(input)
        let gravity = getBeamGravity computer (x, i)
        match gravity with
            | Pulled -> i
            | Stationary -> findYRecursive (i+1)

    findYRecursive start

let private findX input y =
    let rec findXRecursive i =
        let computer = IoComputer(input)
        let gravity = getBeamGravity computer (i, y)
        match gravity with
            | Pulled -> i
            | Stationary -> findXRecursive (i + 1)

    findXRecursive 0

let private canFit input x width =
    let y = findY input x 0

    // we have one corner at x, y
    // so the opposite corner should be at (x-99, y + 99)
    let x' = x - (width - 1)
    let y' = y + (width - 1)

    match getBeamGravity (IoComputer(input)) (x', y') with
        | Pulled -> true
        | Stationary -> false

type private Fit = Perfect | TooSmall | TooBig

let private isMinimumFit input x width =
    let fits = canFit input x width

    if not fits then
        TooSmall
    else
        let fitsSmaller = canFit input (x-1) width

        if fitsSmaller then
            TooBig
        else
            Perfect

type LinearFormula = { m: double; b: double }

module LinearFormula =
    // y = mx + b
    let getY formula x =
        formula.m * x + formula.b

    // x = (y - b) / m
    let getX formula y =
        (y - formula.b) / formula.m

let private isFormulaFit top bottom (x: int) =
    // have (x, y)
    let y = LinearFormula.getY top (double x) |> ceil |> int

    // other corner is (x - 99, y + 99)
    let otherX = LinearFormula.getX bottom (double (y + 99)) |> floor |> int
    otherX <= x - 99

let private isMinimumFormulaFit top bottom x =
    let fits = isFormulaFit top bottom x
    let fitsSmaller = isFormulaFit top bottom (x - 1)

    if not fits then
        TooSmall
    elif fits && fitsSmaller then
        TooBig
    else
        Perfect




// y = mx + b
// b = y - mx
let findFormula (x1:int, y1:int) (x2:int, y2:int) =
    let m = (double y2 - double y1) / (double x2 - double x1)
    let b = double y2 - m * (double x2)

    { m = m; b = b; }

let part2 input =

    let SANTA_SIZE = 100

    // let rec binarySearch min max =
    //     let guess = (min + max) / 2

    //     if max <= min then
    //         None
    //     else
    //         let result = isMinimumFit input guess SANTA_SIZE
    //         printfn "guess: %d result: %A" guess result
    //         match result with
    //             | Perfect -> Some guess
    //             | TooSmall -> binarySearch (guess + 1) max
    //             | TooBig -> binarySearch min (guess - 1)

    // let top = findFormula (50, findY input 50) (100, findY input 100)
    // let bottom = findFormula (findX input 50, 50) (findX input 100, 100)

    // let rec binarySearch' min max =
    //     let guess = (min + max) / 2

    //     if max < min then
    //         None
    //     else
    //         let result = isMinimumFormulaFit top bottom guess
    //         printfn "guess: %d result: %A" guess result
    //         match result with
    //             | Perfect -> Some guess
    //             | TooSmall -> binarySearch (guess + 1) max
    //             | TooBig -> binarySearch min (guess - 1)



    // printfn "top: y = %fx + %f" top.m top.b
    // printfn "bottom y = %fx + %f" bottom.m bottom.b

    // // binarySearch' 1000 99999
    // // |> Option.map (fun x -> (x - 99, findY input x))
    // // |> printfn "%A"

    // binarySearch 1 100
    // |> Option.map (fun x -> (x - SANTA_SIZE - 1, findY input x))
    // // |> Option.map (fun (x, y) -> x * 10000 + y)

    let rec recurse x previousY =
        let y = findY input x previousY

        let x' = x - (SANTA_SIZE - 1)
        let y' = y + (SANTA_SIZE - 1)

        printfn "Checking %A to %A" (x, y) (x', y')

        match getBeamGravity (IoComputer(input)) (x', y') with
            | Pulled -> (x', y)
            | Stationary -> recurse (x+1) y


    recurse 1000 0












