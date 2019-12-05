module Puzzle01Tests

open Xunit
open Puzzle01
open FsUnit.Xunit
    
[<Fact>]
let ``it adds correctly`` () =
    2 + 2 |> should equal 4

[<Fact>]
let ``it calculates the 12 / 3 - 2 = 2`` () =
    let result = calculateFuel 12
    result |> should equal 2

[<Fact>]
let ``it calculates 1969 to 654`` () =
    let result = calculateFuel 1969
    result |> should equal 654

[<Fact>]
let ``it calculates 100756 to 33583`` () =
    let result = calculateFuel 100756
    result |> should equal 33583

[<Fact>]
let ``it exhaustively calculates 14 to 2`` () =
    let result = calculateExhaustiveFuel 14
    result |> should equal 2

[<Fact>]
let ``it exhaustively calculates 1969 to 966`` () =
    let result = calculateExhaustiveFuel 1969
    result |> should equal 966

[<Fact>]
let ``it exhaustively calculates 100756 to 50346`` () =
    let result = calculateExhaustiveFuel 100756
    result |> should equal 50346
