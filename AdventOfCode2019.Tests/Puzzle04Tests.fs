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

[<Fact>]
let ``hasDuplicateSequenceOfLengthTwo rejects 1112`` () =
    (hasDuplicateSequenceOfLengthTwo "1112") |> should be False

[<Fact>]
let ``hasDuplicateSequenceOfLengthTwo accepts 112233`` () =
    (hasDuplicateSequenceOfLengthTwo "112233") |> should be True

[<Fact>]
let ``isValidPassword2 accepts 112233`` () =
    (isValidPassword2 "112233") |> should be True

[<Fact>]
let ``isValidPassword2 rejects 123444`` () =
    (isValidPassword2 "123444") |> should be False

[<Fact>]
let ``isValidPassword2 accepts 111122``() =
    (isValidPassword2 "111122") |> should be True

[<Fact>]
let ``sequenceLengths work`` () =
    let sequence = sequenceLengths "111122" |> Seq.toArray
    sequence |> should equal [| 4; 2; |]

