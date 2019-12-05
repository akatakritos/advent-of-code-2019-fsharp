module Puzzle04Tests
open Puzzle04
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``it accepts 111111`` () =
    (isValidPassword "111111") |> should be True

[<Fact>]
let ``it rejects 223450`` () =
    (isValidPassword "223450") |> should be False

[<Fact>]
let ``it rejects 123789`` () =
    (isValidPassword "123789") |> should be False

[<Fact>]
let ``it accepts 122345`` () =
    (isValidPassword "122345") |> should be True

[<Fact>]
let ``it accepts 111123`` () =
    (isValidPassword "111123") |> should be True
