module Puzzle02Tests
open Xunit
open Puzzle02
open FsUnit.Xunit

[<Fact>]
let ``it parses the sample`` () =
    let result = parse "1,9,10,3,2,3,11,0,99,30,40,50"
    result |> should equal [|1;9;10;3;2;3;11;0;99;30;40;50|]

[<Fact>]
let ``it can create state`` () =
    let state = createState "1,2,3"
    state |> should equal { index = 0; data = [|1;2;3|]}

[<Fact>]
let ``it processes the first command`` () =
    let state0 = createState "1,9,10,3,2,3,11,0,99,30,40,50"
    let result = step state0

    let state1 = unwrapContinue result
    state1.index |> should equal 4
    state1.data.[3] |> should equal 70

[<Fact>]
let ``it processes the second command`` () =
    let state0 = createState "1,9,10,3,2,3,11,0,99,30,40,50"
    let state1 = unwrapContinue (step state0)
    let state2 = unwrapContinue (step state1)

    state2.index |> should equal 8
    state2.data.[0] |> should equal 3500

[<Fact>]
let ``it succeeds sample 1`` () =
    let state0 = createState "1,9,10,3,2,3,11,0,99,30,40,50"
    let result = execute state0
    result |> should equal 3500

