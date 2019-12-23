module Puzzle18Tests
open Puzzle18
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``keySet set`` () =
    KeySet.empty
    |> KeySet.add 'A'
    |> should equal (1)

    KeySet.empty
    |> KeySet.add 'B'
    |> should equal (2)

    KeySet.empty
    |> KeySet.add 'A'
    |> KeySet.add 'B'
    |> should equal (3)

[<Fact>]
let ``keySet contains`` () =
    KeySet.empty
    |> KeySet.add 'C'
    |> KeySet.contains 'C'
    |> should equal true

    KeySet.empty
    |> KeySet.add 'Z'
    |> KeySet.add 'C'
    |> KeySet.contains 'C'
    |> should equal true

    KeySet.empty
    |> KeySet.add 'Z'
    |> KeySet.add 'C'
    |> KeySet.contains 'Z'
    |> should equal true

[<Fact>]
let ``keySet remove`` () =
    KeySet.empty
    |> KeySet.add 'Z'
    |> KeySet.add 'C'
    |> KeySet.remove 'Z'
    |> KeySet.contains 'Z'
    |> should equal false

    KeySet.empty
    |> KeySet.add 'Z'
    |> KeySet.add 'C'
    |> KeySet.remove 'Z'
    |> KeySet.contains 'C'
    |> should equal true


[<Fact>]
let ``countSteps works on sample 1`` () =
    "#########\n\
     #b.A.@.a#\n\
     #########"
    |> Cave.parse
    |> countSteps
    |> should equal (Some 8)

[<Fact>]
let ``countSteps works on sample 2 getting 86`` () =
    "########################\n\
     #f.D.E.e.C.b.A.@.a.B.c.#\n\
     ######################.#\n\
     #d.....................#\n\
     ########################"
    |> Cave.parse
    |> countSteps
    |> should equal (Some 86)

[<Fact>]
let ``countSteps works on sample 3 getting 136`` () =
    "#################\n\
     #i.G..c...e..H.p#\n\
     ########.########\n\
     #j.A..b...f..D.o#\n\
     ########@########\n\
     #k.E..a...g..B.n#\n\
     ########.########\n\
     #l.F..d...h..C.m#\n\
     #################"
    |> Cave.parse
    |> countSteps
    |> should equal (Some 136)