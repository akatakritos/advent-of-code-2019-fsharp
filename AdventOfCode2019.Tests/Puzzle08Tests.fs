module Puzzle08Tests
open Puzzle08
open Puzzle08.SpaceImage
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``load should work`` () =
    let image = create 3 2 "123456789012"
    let expected = [
        [| 
            [| 1; 2; 3; |];
            [| 4; 5; 6; |];
        |];
        [|
            [| 7; 8; 9; |]
            [| 0; 1; 2; |]
        |]
    ]

    image.layers |> should equal expected

    getPixel (List.head image.layers) (1, 1) |> should equal 5
    getPixel (List.head image.layers) (2, 1) |> should equal 6

    pixels (List.head image.layers) image.width image.height
    |> Seq.toList
    |> should equal [1;2;3;4;5;6]


[<Fact>]
let ``checksum test`` () =
    let image = create 3 2 "111222000000";

    let COUNT_ONES = 3
    let COUNT_TWOS = 3
    image |> checksum |> should equal (COUNT_ONES * COUNT_TWOS)

[<Fact>]
let ``render test`` () =
    let image = create 2 2 "0222112222120000"
    let result = render image
    result |> should equal " #\n# ";
