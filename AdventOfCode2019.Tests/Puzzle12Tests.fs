module Puzzle12Tests
open Xunit
open FsUnit.Xunit
open Puzzle12

[<Fact>]
let ``Vector3.parse test`` () =
    let result = Vector3.parse "<x=-1, y=0, z=2>"
    result |> should equal { x = -1; y = 0; z = 2;}

let assertMoons step (s: string) (moons: Moon array) =
    let lines = s.Split('\n')
    for i = 0 to 3 do
        let actual = Moon.parse lines.[i]
        match moons.[i] = actual with
            | true -> ()
            | false -> failwithf "At step %d moon %d did not match\n\nExpected: %A\n\nActual: %A" step i moons.[i] actual

[<Fact>]
let ``tick tests`` () =
    let moons = [|
        Moon.parseInitial "<x=-1, y=0, z=2>"
        Moon.parseInitial "<x=2, y=-10, z=-7>"
        Moon.parseInitial "<x=4, y=-8, z=8>"
        Moon.parseInitial "<x=3, y=5, z=-1>"
    |]

    moons |> assertMoons 0 "pos=<x=-1, y=  0, z= 2>, vel=<x= 0, y= 0, z= 0>\n\
    pos=<x= 2, y=-10, z=-7>, vel=<x= 0, y= 0, z= 0>\n\
    pos=<x= 4, y= -8, z= 8>, vel=<x= 0, y= 0, z= 0>\n\
    pos=<x= 3, y=  5, z=-1>, vel=<x= 0, y= 0, z= 0>"

    tick moons
    moons |> assertMoons 1 "pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>\n\
    pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>\n\
    pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>\n\
    pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>"

    tick moons
    moons |> assertMoons 2 "pos=<x= 5, y=-3, z=-1>, vel=<x= 3, y=-2, z=-2>\n\
pos=<x= 1, y=-2, z= 2>, vel=<x=-2, y= 5, z= 6>\n\
pos=<x= 1, y=-4, z=-1>, vel=<x= 0, y= 3, z=-6>\n\
pos=<x= 1, y=-4, z= 2>, vel=<x=-1, y=-6, z= 2>"

    tick moons
    moons |> assertMoons 3 "pos=<x= 5, y=-6, z=-1>, vel=<x= 0, y=-3, z= 0>\n\
pos=<x= 0, y= 0, z= 6>, vel=<x=-1, y= 2, z= 4>\n\
pos=<x= 2, y= 1, z=-5>, vel=<x= 1, y= 5, z=-4>\n\
pos=<x= 1, y=-8, z= 2>, vel=<x= 0, y=-4, z= 0>"

    for i = 4 to 10 do
        tick moons

    moons |> assertMoons 10 "pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>\n\
pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>\n\
pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>\n\
pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>"

[<Fact>]
let ``totalEnergy is 179`` () =
    let moons =
        "pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>\n\
        pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>\n\
        pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>\n\
        pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>".Split('\n')
        |> Seq.map Moon.parse

    let result = totalEnergy moons

    result |> should equal 179

[<Fact>]
let ``totalEnergy after 100 steps is 1940`` () =
    let moons = [|
        Moon.parseInitial "<x=-8, y=-10, z=0>";
        Moon.parseInitial "<x=5, y=5, z=10>";
        Moon.parseInitial "<x=2, y=-7, z=3>";
        Moon.parseInitial "<x=9, y=-8, z=-3>";
    |]

    for i = 1 to 100 do
        tick moons

    let result = totalEnergy moons

    result |> should equal 1940

[<Fact>]
let ``cyclesUntilReset is 2772`` () =
    let moons = [|
        Moon.parseInitial "<x=-1, y=0, z=2>"
        Moon.parseInitial "<x=2, y=-10, z=-7>"
        Moon.parseInitial "<x=4, y=-8, z=8>"
        Moon.parseInitial "<x=3, y=5, z=-1>"
    |]

    moons |> cyclesUntilReset |> should equal 2772L

