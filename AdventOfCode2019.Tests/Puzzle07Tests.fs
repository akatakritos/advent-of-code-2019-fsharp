module Puzzle07Tests
open Xunit
open FsUnit.Xunit
open Puzzle07

[<Fact>]
let ``permutate should come up with the right count`` () =
    let results = permutations [ 1; 2; 3; ]
    results
    |> Seq.length
    |> should equal 6

[<Fact>]
let ``permutations should be right for a simple case`` () =
    let results = permutations [ 1; 2; 3; ]
    results 
    |> Seq.fold (fun s p -> s + sprintf "%A, " p) ""
    |> should equal "[1; 2; 3], [2; 1; 3], [2; 3; 1], [1; 3; 2], [3; 1; 2], [3; 2; 1], "


[<Fact>]
let ``largestOutputSignal 4,3,2,1,0 is 43210`` () =
    let program  = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    let result = largestOutputSignal program
    result |> should equal 43210

[<Fact>]
let ``outputSignal  4,3,2,1,0 is 43210`` () =
    let program  = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    let result = outputSignal program [4;3;2;1;0]
    result |> should equal 43210

[<Fact>]
let ``largestOutputSignal 2`` () =
    let program = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    program |> largestOutputSignal |> should equal 54321

[<Fact>]
let ``largestOutputSignal 3`` () = 
    let program = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    program |> largestOutputSignal |> should equal 65210

