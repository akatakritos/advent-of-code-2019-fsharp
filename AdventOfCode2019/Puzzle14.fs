module Puzzle14

type Chemical = Chemical of string

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
    let mutable stock = new Map<Chemical, int>([])
    let mutable oreConsumed = 0

    let setStock c amt =
        stock <- Map.add c amt stock

    let getStock c =
        match stock.ContainsKey c with
            | true -> stock.[c]
            | false ->
                setStock c 0
                0

    let incrememntStock c amt =
        let current = getStock c
        // printfn "Increment stock of %A by %d to %d" c amt (current + amt)
        setStock c (current + amt)

    let decrementStock c amt =
        if c = Chemical "ORE" then
            oreConsumed <- oreConsumed + amt
            // printfn "ORE consumed +%d to %d" amt oreConsumed
        else
            let current = getStock c
            if current < amt then failwithf "Tried to decrement stock of %A by %d but we only had %d" c amt current

            // printfn "Decrement stock of %A by %d to %d" c amt (current - amt)
            setStock c (current - amt)


    let rec produce chemical amount =
        // printfn "Asked to produce %d of %A" amount chemical

        if chemical = Chemical "ORE" then
            decrementStock chemical amount
            ()
        elif getStock chemical >= amount then
            // printfn "We have enough"
            ()
        else
            let formula = tree.[chemical]

            let mutable produced = 0
            let current = getStock chemical
            while current + produced < amount do
                // printfn "Making %d of %A" formula.output.quantity chemical
                for input in formula.inputs do
                    produce input.chemical input.quantity
                    if input.chemical <> Chemical "ORE" then
                        decrementStock input.chemical input.quantity

                produced <- produced + formula.output.quantity

            // printfn "produced %d of %A" produced chemical
            incrememntStock formula.output.chemical produced



    member this.ProduceFuel () =
        produce (Chemical "FUEL") 1
        oreConsumed
