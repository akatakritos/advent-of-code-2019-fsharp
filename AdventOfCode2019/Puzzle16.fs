module Puzzle16
module FFT =
    let load (s: string)=
        s
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.toArray

    let private pattern = [|0;1;0;-1|]

    let patternValue repeater i =
        if i < repeater then 0
        else
            let index = ((i+1) / (repeater + 1)) % 4
            pattern.[index]

    let setMultipliers (multipliers: int[]) (repeater: int) =
        for i = 0 to multipliers.Length - 1 do
            multipliers.[i] <- patternValue repeater i


    let calculateOutput index input =
        input
        |> Seq.mapi (fun i num -> num * (patternValue index i))
        |> Seq.sum
        |> fun n -> abs (n % 10)

    let calculateNextPhase (input: seq<int>) (output: int array) =
        input
        |> Seq.iteri (fun i _ -> output.[i] <- calculateOutput i input)

        output


    let calculateAtPhase n (input: int array) =
        let output = Array.copy input

        let rec recurse n accumulator =
            if n = 0 then
                accumulator
            else
                recurse (n - 1) (calculateNextPhase accumulator output)

        recurse n output

    let calculateAtPhaseFast n (input: int array) from =
        let mutable cache = Array.zeroCreate input.Length
        let mutable input' = input

        // exploits the fact that i < skip amount degenerates to 0 multiplier

        for _ = 1 to n do
            let mutable sum = 0

            for k = from to input.Length - 1 do
                sum <- sum + input'.[k]

            for i = from to input.Length - 1 do
                cache.[i] <- sum % 10
                sum <- sum - input'.[i]

            let tmp = input'
            input' <- cache
            cache <- tmp

        input'


    let format (input: seq<int>) =
        input
        |> Seq.map string
        |> String.concat("")

    let offset (input: string) =
        input.[0..6] |> int

    let dupe n (input: int array) =
        let output = Array.zeroCreate (input.Length * n)
        for i = 0 to n - 1 do
            for j = 0 to input.Length - 1 do
                let idx = i * input.Length + j
                try
                    output.[idx] <- input.[j]
                with
                    | :? System.IndexOutOfRangeException -> failwithf "Out of range: %d (max: %d %d %d)" idx output.Length i j

        output

    let decodeSignal (s: string) =
        let input = load s
        let workspace = dupe 10000 input
        let skip = offset s
        let final = calculateAtPhaseFast 100 workspace skip
        let result = final.[skip..skip+7]
        result




let part1 input =
    let result =
        input
        |> FFT.load
        |> FFT.calculateAtPhase 100
        |> FFT.format

    printfn "After 100 phases we have '%s' (prefix '%s')" result result.[0..7]



let part2 input =
    input
    |> FFT.decodeSignal
    |> FFT.format
    |> printfn "Signal: %s"

