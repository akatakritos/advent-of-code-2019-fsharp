module Puzzle03Tests
open Xunit
open Puzzle03

[<Fact>]
let ``parses a command`` () =
    let result = parseInstruction "U23"
    Assert.Equal(Up, result.Direction)
    Assert.Equal(23, result.Distance)

[<Fact>]
let ``parses multiple commands`` () =
    let result = parseInstructions "L1,R2,U3,D4" |> Seq.toArray
    Assert.Equal<Instruction[]>(
        [| 
            { Direction = Left; Distance = 1};
            { Direction= Right; Distance = 2};
            { Direction=Up; Distance=3};
            { Direction=Down; Distance=4};
        |],
        result)

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

    Assert.Equal<Point[]>(expected, result)

[<Fact>]
let ``manhattanDistance`` () =
    let result = manhattanDistance { X = 3; Y = 3 } Origin
    Assert.Equal(6, result)

[<Fact>]
let ``minimum distance sample 1`` () =
    let wire1 = parseInstructions "R8,U5,L5,D3"
    let wire2 = parseInstructions "U7,R6,D4,L4"
    
    let result = findMinimumIntersectionDistance wire1 wire2
    Assert.Equal(6, result)

[<Fact>]
let ``minimum distance sample 2`` () =
    let wire1 = parseInstructions "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    let wire2 = parseInstructions "U62,R66,U55,R34,D71,R55,D58,R83"
    
    let result = findMinimumIntersectionDistance wire1 wire2
    Assert.Equal(159, result)

[<Fact>]
let ``minimum distance sample 3`` () =
    let wire1 = parseInstructions "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    let wire2 = parseInstructions "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    
    let result = findMinimumIntersectionDistance wire1 wire2
    Assert.Equal(135, result)
