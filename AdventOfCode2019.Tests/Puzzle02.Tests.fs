module Puzzle02Tests
open Xunit
open Puzzle02

[<Fact>]
let ``it parses the sample`` () =
    let result = parse "1,9,10,3,2,3,11,0,99,30,40,50"
    Assert.Equal<int[]>([|1;9;10;3;2;3;11;0;99;30;40;50|], result);

[<Fact>]
let ``it can create state`` () =
    let state = createState "1,2,3"
    Assert.Equal({ index = 0; data = [|1;2;3|]}, state)

[<Fact>]
let ``it processes the first command`` () =
    let state0 = createState "1,9,10,3,2,3,11,0,99,30,40,50"
    let result = step state0

    let state1 = unwrapContinue result
    Assert.Equal(4, state1.index)
    Assert.Equal(70, state1.data.[3])

[<Fact>]
let ``it processes the second command`` () =
    let state0 = createState "1,9,10,3,2,3,11,0,99,30,40,50"
    let state1 = unwrapContinue (step state0)
    let state2 = unwrapContinue (step state1)

    Assert.Equal(8, state2.index)
    Assert.Equal(3500, state2.data.[0])

[<Fact>]
let ``it succeeds sample 1`` () =
    let state0 = createState "1,9,10,3,2,3,11,0,99,30,40,50"
    let result = execute state0
    Assert.Equal(3500, result)

