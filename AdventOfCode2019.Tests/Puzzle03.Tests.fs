module Puzzle03Tests
open Xunit
open Puzzle03
open FsUnit.Xunit

[<Fact>]
let ``parses a command`` () =
    let result = parseInstruction "U23"

    result |> should equal { Direction = Up; Distance = 23 }

[<Fact>]
let ``parses multiple commands`` () =
    let result = parseInstructions "L1,R2,U3,D4" |> Seq.toArray

    result |> should equal 
        [| 
            { Direction = Left; Distance = 1};
            { Direction= Right; Distance = 2};
            { Direction=Up; Distance=3};
            { Direction=Down; Distance=4};
        |]

[<Fact>]
let ``traceWire finds the correct sequence of points`` () =
    let result = parseInstructions "R2,U1,L1,D2" |> traceWire |> Seq.toArray

    let expected = [|
        { X = 0; Y = 0 };
        { X = 1; Y = 0 };
        { X = 2; Y = 0 };
        { X = 2; Y = 1 };
        { X = 1; Y = 1 };
        { X = 1; Y = 0 };
        { X = 1; Y = -1 };
    |]

    result |> should equal expected


[<Fact>]
let ``manhattanDistance`` () =
    let result = manhattanDistance { X = 3; Y = 3 } Origin
    result |> should equal 6

[<Fact>]
let ``minimum distance sample 1`` () =
    let wire1 = parseInstructions "R8,U5,L5,D3"
    let wire2 = parseInstructions "U7,R6,D4,L4"
    
    let result = findMinimumIntersectionDistance wire1 wire2
    result |> should equal 6

[<Fact>]
let ``minimum distance sample 2`` () =
    let wire1 = parseInstructions "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    let wire2 = parseInstructions "U62,R66,U55,R34,D71,R55,D58,R83"
    
    let result = findMinimumIntersectionDistance wire1 wire2
    result |> should equal 159

[<Fact>]
let ``minimum distance sample 3`` () =
    let wire1 = parseInstructions "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    let wire2 = parseInstructions "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    
    let result = findMinimumIntersectionDistance wire1 wire2
    result |> should equal 135

[<Fact>]
let ``travel distance sample 1`` () =
    let wire1 = parseInstructions "R8,U5,L5,D3"
    let wire2 = parseInstructions "U7,R6,D4,L4"
    
    let result = findMinimumTravelDistance wire1 wire2
    result |> should equal 30

[<Fact>]
let ``travel distance sample 2`` () =
    let wire1 = parseInstructions "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    let wire2 = parseInstructions "U62,R66,U55,R34,D71,R55,D58,R83"
    
    let result = findMinimumTravelDistance wire1 wire2
    result |> should equal 610

[<Fact>]
let ``travel distance sample 3`` () =
    let wire1 = parseInstructions "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    let wire2 = parseInstructions "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    
    let result = findMinimumTravelDistance wire1 wire2
    result |> should equal 410
