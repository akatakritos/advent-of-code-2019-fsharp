module Puzzle09Tests
open Xunit
open FsUnit.Xunit
open IntCodeComputer

[<Fact>]
let ``quine`` () =
    let program = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    let computer = loadProgram program
    let inputter = fun () -> 0L
    let mutable output = []
    let outputter = fun v -> output <- v::output

    run computer inputter outputter |> ignore

    let expected =[109L;1L;204L;-1L;1001L;100L;1L;100L;1008L;100L;16L;101L;1006L;101L;0L;99L] |> List.rev;

    output |> should equal expected

[<Fact>]
let ``big number`` () =
    let program = "1102,34915192,34915192,7,4,7,99,0"
    let computer = loadProgram program
    let inputter = fun () -> 0L
    let mutable output = 0L
    let outputter = fun v -> output <- v

    run computer inputter outputter |> ignore

    output |> string |> should haveLength 16;

[<Fact>]
let ``big number in middle is 1125899906842624`` () =
    let program = "104,1125899906842624,99"
    let computer = loadProgram program
    let inputter = fun () -> 0L
    let mutable output = 0L
    let outputter = fun v -> output <- v

    run computer inputter outputter |> ignore

    output |> should equal 1125899906842624L
