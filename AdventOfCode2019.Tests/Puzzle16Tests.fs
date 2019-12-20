module Puzzle16
open FsUnit.Xunit
open Xunit
open Puzzle16

[<Fact>]
let ``load gets a string as a array`` () =
    let data = FFT.load "12345"
    data |> should equal [|1;2;3;4;5;|]

[<Fact>]
let ``setMultipliers configures the pattern 0`` () =
    let output = Array.zeroCreate<int> 8
    FFT.setMultipliers output 0
    output |> should equal [|1; 0; -1; 0; 1; 0; -1; 0|]

[<Fact>]
let ``setMultipliers configures the pattern 1`` () =
    let output = Array.zeroCreate<int> 8
    FFT.setMultipliers output 1
    output |> should equal [|0; 1; 1; 0; 0; -1; -1; 0|]

[<Fact>]
let ``setMultipliers configures the pattern 2`` () =
    let output = Array.zeroCreate<int> 8
    FFT.setMultipliers output 2
    output |> should equal [|0; 0; 1; 1; 1; 0; 0; 0|]

[<Fact>]
let ``setMultipliers configures the pattern 3`` () =
    let output = Array.zeroCreate<int> 8
    FFT.setMultipliers output 3
    output |> should equal [|0; 0; 0; 1; 1; 1; 1; 0 |]

[<Fact>]
let ``setMultipliers configures the pattern 4`` () =
    let output = Array.zeroCreate<int> 8
    FFT.setMultipliers output 4
    output |> should equal [|0; 0; 0; 0; 1; 1; 1; 1; |]

[<Fact>]
let ``setMultipliers configures the pattern 5`` () =
    let output = Array.zeroCreate<int> 8
    FFT.setMultipliers output 5
    output |> should equal [|0; 0; 0; 0; 0; 1; 1; 1; |]

[<Fact>]
let ``setMultipliers configures the pattern 6`` () =
    let output = Array.zeroCreate<int> 8
    FFT.setMultipliers output 6
    output |> should equal [|0; 0; 0; 0; 0; 0; 1; 1; |]

[<Fact>]
let ``setMultipliers configures the pattern 7`` () =
    let output = Array.zeroCreate<int> 8
    FFT.setMultipliers output 7
    output |> should equal [|0; 0; 0; 0; 0; 0; 0; 1; |]

[<Fact>]
let ``calculateOutput gets 4 for output 0`` () =
    FFT.calculateOutput 0 [|1;2;3;4;5;6;7;8|] |> should equal 4

[<Fact>]
let ``calculateNextPhase gets correct phase 1`` () =
    let output = FFT.calculateNextPhase [|1;2;3;4;5;6;7;8|] (Array.zeroCreate 8)
    output |> should equal [|4;8;2;2;6;1;5;8|]

[<Fact>]
let ``calculate after 4 phases`` () =
    "12345678"
    |> FFT.load
    |> FFT.calculateAtPhase 4
    |> FFT.format
    |> should equal "01029498"

[<Fact>]
let ``100 phases of 80871224585914546619083218645595 should start with 24176176`` () =
    "80871224585914546619083218645595"
    |> FFT.load
    |> FFT.calculateAtPhase 100
    |> FFT.format
    |> fun s -> s.[0..7]
    |> should equal "24176176"

[<Fact>]
let ``100 phases of 19617804207202209144916044189917 should start with 73745418`` () =
    "19617804207202209144916044189917"
    |> FFT.load
    |> FFT.calculateAtPhase 100
    |> FFT.format
    |> fun s -> s.[0..7]
    |> should equal "73745418"

[<Fact>]
let ``100 phases of 69317163492948606335995924319873 should start with 52432133`` () =
    "69317163492948606335995924319873"
    |> FFT.load
    |> FFT.calculateAtPhase 100
    |> FFT.format
    |> fun s -> s.[0..7]
    |> should equal "52432133"

[<Fact>]
let ``offset gets first 7 digits as number`` () =
    "03036732577212944063491565474664"
    |> FFT.offset
    |> should equal 303673

[<Fact>]
let ``dupe copies an array n times`` () =
    [|1;2;3|]
    |> FFT.dupe 4
    |> should equal [|1;2;3;1;2;3;1;2;3;1;2;3|]

[<Fact>]
let ``decodeSignal 03036732577212944063491565474664 should be 84462026`` () =
    "03036732577212944063491565474664"
    |> FFT.decodeSignal
    |> FFT.format
    |> should equal "84462026"