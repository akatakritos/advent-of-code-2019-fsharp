module Puzzle06Tests
open Xunit
open FsUnit.Xunit
open Puzzle06

[<Fact>]
let ``parseDescription works`` () =
    let result = parseDescription "A)B"
    result |> should equal { Object = "B"; Center = "A" }

let ``depth D`` () =
    let commands = [|
        "COM)B";
        "B)C";
        "C)D";
        "D)E";
        "E)F";
        "B)G";
        "G)H";
        "D)I";
        "E)J";
        "J)K";
        "K)L";
    |]

    let tree = commands |> Seq.map parseDescription |> buildTree

    let d = depth tree "D"
    d |> should equal 3

    let l = depth tree "L"
    l |> should equal 7

    let com = depth tree "COM"
    com |> should equal 0

[<Fact>]
let ``totalDepth should be 42`` () =
    let commands = [|
           "COM)B";
           "B)C";
           "C)D";
           "D)E";
           "E)F";
           "B)G";
           "G)H";
           "D)I";
           "E)J";
           "J)K";
           "K)L";
    |]

    let tree = commands |> Seq.map parseDescription |> buildTree
    tree |> totalDepth |> should equal 42

[<Fact>]
let ``ancestors works`` () =
    let commands = [|
        "COM)B";
        "B)C";
        "C)D";
        "D)E";
        "E)F";
        "B)G";
        "G)H";
        "D)I";
        "E)J";
        "J)K";
        "K)L";
        "K)YOU";
        "I)SAN";
    |]
    let tree = commands |> Seq.map parseDescription |> buildTree

    let path = ancestors tree "YOU"
    path |> should equal ["COM"; "B"; "C"; "D"; "E"; "J"; "K"]
    
[<Fact>]
let ``commonAncestor works`` () =
    let youPath = ["COM"; "B"; "C"; "D"; "E"; "J"; "K"]
    let santaPath = ["COM"; "B"; "C"; "D"; "I"]

    let result = commonAncestor youPath santaPath
    result |> should equal "D"

[<Fact>]
let ``depthTo D from YOU should equal 4`` () =
    let commands = [|
        "COM)B";
        "B)C";
        "C)D";
        "D)E";
        "E)F";
        "B)G";
        "G)H";
        "D)I";
        "E)J";
        "J)K";
        "K)L";
        "K)YOU";
        "I)SAN";
    |]
    let tree = commands |> Seq.map parseDescription |> buildTree

    let depthToD = depthTo tree "D" "YOU"
    depthToD |> should equal 4


[<Fact>]
let ``transferDistance should be 4`` () =
    let commands = [|
        "COM)B";
        "B)C";
        "C)D";
        "D)E";
        "E)F";
        "B)G";
        "G)H";
        "D)I";
        "E)J";
        "J)K";
        "K)L";
        "K)YOU";
        "I)SAN";
    |]
    let tree = commands |> Seq.map parseDescription |> buildTree

    let distance = transferDistance tree "YOU" "SAN"

    distance |> should equal 4
