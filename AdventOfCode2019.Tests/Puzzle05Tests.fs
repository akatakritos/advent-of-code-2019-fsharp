module Puzzle05Tests
open Xunit
open FsUnit.Xunit
open Puzzle05
open IntCodeComputer

[<Fact>]
let ``parseMode - 1`` () =
    let mode0 = (parseMode 1002 0) 1
    let mode1 = (parseMode 1002 1) 2
    let mode2 = (parseMode 1002 2) 3

    mode0 |> should equal (PositionMode 1)
    mode1 |> should equal (ImmediateMode 2)
    mode2 |> should equal (PositionMode 3)

[<Fact>]
let ``parseBinaryOperation test 1`` () =
    let computer = {
        Memory = [|1002; 4; 3; 4; 33 |];
        Pointer = 0;
    }

    let operation = parseBinaryOperation computer

    operation |> should equal { 
        Left = PositionMode 4
        Right = ImmediateMode 3
        Output = ImmediateMode 4
    }

