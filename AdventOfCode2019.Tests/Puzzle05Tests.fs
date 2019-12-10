module Puzzle05Tests
open Xunit
open FsUnit.Xunit
open Puzzle05
open IntCodeComputer

[<Fact>]
let ``parseMode - 1`` () =
    let mode0 = (parseMode 1002 0) 1L
    let mode1 = (parseMode 1002 1) 2L
    let mode2 = (parseMode 1002 2) 3L

    mode0 |> should equal (PositionMode 1L)
    mode1 |> should equal (ImmediateMode 2L)
    mode2 |> should equal (PositionMode 3L)

[<Fact>]
let ``parseBinaryOperation test 1`` () =
    let computer = {
        Memory = [|1002L; 4L; 3L; 4L; 33L |];
        Pointer = 0;
        RelativeBase = 0;
    }

    let operation = parseBinaryOperation computer

    operation |> should equal { 
        Left = PositionMode 4L
        Right = ImmediateMode 3L
        Output = PositionMode 4L
    }

