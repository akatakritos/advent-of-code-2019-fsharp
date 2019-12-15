module Puzzle14

[<Struct>]
type Chemical = Chemical of string

module Chemical =
    let Fuel = Chemical "FUEL"
    let Ore = Chemical "ORE"

type Ingredient = {
    chemical: Chemical;
    quantity: int;
}

module Ingredient =
    let create name quantity =
        { chemical = Chemical name; quantity = quantity; }

    let parse (s: string) =
        let parts = s.Split(" ")
        let qty = parts.[0]
        let name = parts.[1]
        { chemical = Chemical name; quantity = int qty }

type Formula = {
    output: Ingredient;
    inputs: Ingredient list
}

module Formula =
    let parse (s: string) =
        let parts = s.Split(" => ")
        let output = Ingredient.parse parts.[1]

        let inputs =
            parts.[0].Split(", ")
            |> Seq.map Ingredient.parse
            |> Seq.toList

        { output = output; inputs = inputs }

type FormulaTree = Map<Chemical, Formula>

module FormulaTree =
    let build (formulas: seq<Formula>) =
        let pairs =
            formulas
            |> Seq.map (fun f -> (f.output.chemical, f))

        new Map<Chemical, Formula>(pairs)

    let parse (s: string) =
        s.Split("\n")
        |> Seq.map Formula.parse
        |> build

type NanoFactory(tree: FormulaTree) =
    let mutable stock = new Map<Chemical, int64>([])
    let mutable oreConsumed = 0L

    let setStock c amt =
        stock <- Map.add c amt stock

    let getStock c =
        match stock.ContainsKey c with
            | true -> stock.[c]
            | false ->
                setStock c 0L
                0L

    let incrememntStock c amt =
        let current = getStock c
        // printfn "Increment stock of %A by %d to %d" c amt (current + amt)
        setStock c (current + amt)

    let decrementStock c amt =
        if c = Chemical.Ore then
            oreConsumed <- oreConsumed + int64 amt
            // printfn "ORE consumed +%d to %d" amt oreConsumed
        else
            let current = getStock c
            if current < amt then failwithf "Tried to decrement stock of %A by %d but we only had %d" c amt current

            // printfn "Decrement stock of %A by %d to %d" c amt (current - amt)
            setStock c (current - amt)


    let rec produce chemical (amount: int64) =
        // printfn "Asked to produce %d of %A" amount chemical

        if chemical = Chemical.Ore then
            decrementStock chemical amount
            ()
        elif getStock chemical >= amount then
            // printfn "We have enough"
            ()
        else
            let formula = tree.[chemical]

            let current = getStock chemical
            let needToMake = amount - current |> double
            let batches = ceil (needToMake / (double formula.output.quantity)) |> int64

            // printfn "Making %d of %A" formula.output.quantity chemical
            for input in formula.inputs do
                produce input.chemical (int64 input.quantity * batches)
                if input.chemical <> Chemical.Ore then
                    decrementStock input.chemical (int64 input.quantity * batches)


            // printfn "produced %d of %A" produced chemical
            incrememntStock formula.output.chemical (int64 formula.output.quantity * batches)


    member this.ProduceMultipleFuel (n: int64) =
        produce Chemical.Fuel n
        oreConsumed

    member this.ProduceFuel () =
        this.ProduceMultipleFuel 1L

let maxFuelFor tree maxOre =
    let getOreUsed fuel =
        let factory = NanoFactory tree
        factory.ProduceMultipleFuel fuel

    let rec binarySearch min (max: int64) =
        if min >= max then
            failwithf "Did not converge"

        let guess = (min + max) / 2L
        let ore1 = getOreUsed guess
        let ore2 = getOreUsed (guess+1L)
        // printfn "trying %d (midpoint of %d and %d) produced %d and guess+1 produced %d" guess min max ore1 ore2

        if ore1 <= maxOre && ore2 > maxOre then
            guess
        elif ore1 > maxOre then
            binarySearch min (guess - 1L)
        elif ore1 < maxOre then
            binarySearch (guess + 1L) max
        else
            failwith "Shouldnt hit this since we've handled all ore1 cases"

    binarySearch 1L 99999999999L
