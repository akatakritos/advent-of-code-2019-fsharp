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
    result |> should equal 43210L

[<Fact>]
let ``outputSignal  4,3,2,1,0 is 43210`` () =
    let program  = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    let result = outputSignal program [4L;3L;2L;1L;0L]
    result |> should equal 43210L

[<Fact>]
let ``largestOutputSignal 2`` () =
    let program = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    program |> largestOutputSignal |> should equal 54321L

[<Fact>]
let ``largestOutputSignal 3`` () = 
    let program = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    program |> largestOutputSignal |> should equal 65210L


[<Fact>]
let ``sample 01 max loop is 139629729 on 9-8-7-6-5`` () =
    let program = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
    (feedbackSignal program [9L;8L;7L;6L;5L]) |> should equal 139629729L

[<Fact>]
let ``sample 01 max loop is 139629729 when testing all`` () =
    let program = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
    maxFeedbackSignal program |> should equal 139629729L


[<Fact>]
let ``sample 02 max loop is 18216`` () =
    let program = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
    (feedbackSignal program [9L;7L;8L;5L;6L]) |> should equal 18216L

