module Puzzle05Tests
open Xunit
open FsUnit.Xunit
open Puzzle05

[<Fact>]
let ``parseMode - 1`` () =
    let mode0 = parseMode 1002 0
    let mode1 = parseMode 1002 1
    let mode2 = parseMode 1002 2

    mode0 |> should equal PositionMode
    mode1 |> should equal ImmediateMode
    mode2 |> should equal PositionMode

[<Fact>]
let ``parseBinaryOperation test 1`` () =
    let computer = {
        Memory = [|1002; 4; 3; 4; 33 |];
        Pointer = 0;
    }

    let operation = parseBinaryOperation computer

    operation |> should equal { 
           LeftMode = PositionMode;
           LeftParameter = 4;
           RightMode = ImmediateMode;
           RightParamter = 3;
           ResultParameter = 4
       }

