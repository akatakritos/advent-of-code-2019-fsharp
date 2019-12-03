module Puzzle01Tests

open Xunit
open Puzzle01
    
[<Fact>]
let ``it adds correctly`` () =
    Assert.Equal(4, 2 + 2);

[<Fact>]
let ``it calculates the 12 / 3 - 2 = 2`` () =
    let result = calculateFuel 12
    Assert.Equal(2, result);

[<Fact>]
let ``it calculates 1969 to 654`` () =
    let result = calculateFuel 1969
    Assert.Equal(654, result)

[<Fact>]
let ``it calculates 100756 to 33583`` () =
    let result = calculateFuel 100756
    Assert.Equal(33583, result)

[<Fact>]
let ``it exhaustively calculates 14 to 2`` () =
    let result = calculateExhaustiveFuel 14
    Assert.Equal(2, result)

[<Fact>]
let ``it exhaustively calculates 1969 to 966`` () =
    let result = calculateExhaustiveFuel 1969
    Assert.Equal(966, result)

[<Fact>]
let ``it exhaustively calculates 100756 to 50346`` () =
    let result = calculateExhaustiveFuel 100756
    Assert.Equal(50346, result)
