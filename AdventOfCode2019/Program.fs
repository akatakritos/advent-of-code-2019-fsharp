let puzzle01 () =

    Puzzle01.readInput "inputs\\puzzle01.txt"
    |> Seq.map Puzzle01.calculateFuel
    |> Seq.reduce (+)
    |> printfn "the total fuel is %d"

    Puzzle01.readInput "inputs\\puzzle01.txt"
    |> Seq.map Puzzle01.calculateExhaustiveFuel
    |> Seq.reduce (+)
    |> printfn "the total exhaustive fuel is %d"

let puzzle02 () =
    let input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,23,6,27,2,9,27,31,1,5,31,35,1,35,10,39,1,39,10,43,2,43,9,47,1,6,47,51,2,51,6,55,1,5,55,59,2,59,10,63,1,9,63,67,1,9,67,71,2,71,6,75,1,5,75,79,1,5,79,83,1,9,83,87,2,87,10,91,2,10,91,95,1,95,9,99,2,99,9,103,2,10,103,107,2,9,107,111,1,111,5,115,1,115,2,119,1,119,6,0,99,2,0,14,0"
    let state0 = Puzzle02.createState input
    Array.set state0.data 1 12
    Array.set state0.data 2 2

    let result = Puzzle02.execute state0
    printfn "The first index result is %d" result

    let nounVerb = Puzzle02.calculateNounVerb input
    let product = 100 * nounVerb.noun + nounVerb.verb
    printfn "The inputs are %d, %d => %d" nounVerb.noun nounVerb.verb product

let puzzle03 () =
    let input = System.IO.File.ReadAllLines("inputs\\puzzle03.txt");
    let wire1 = Puzzle03.parseInstructions input.[0]
    let wire2 = Puzzle03.parseInstructions input.[1]
    let distance = Puzzle03.findMinimumIntersectionDistance wire1 wire2
    let travel = Puzzle03.findMinimumTravelDistance wire1 wire2

    printfn "The minimum distance is %d" distance
    printfn "However, the minimum travel distance is %d" travel

let puzzle04 () =
    let start = 206938
    let stop = 679128
    let valid1 = Puzzle04.countValidPasswords start stop Puzzle04.isValidPassword
    let valid2 = Puzzle04.countValidPasswords start stop Puzzle04.isValidPassword2
    printfn "There are %d valid passwords in the range." valid1
    printfn "There are %d valid passwords in the range under the new rules." valid2

let puzzle05 () =
    let computer = IntCodeComputer.loadProgramFromFile "inputs\\puzzle05.txt"
    // let computer = Puzzle05.loadProgram "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
    // let computer = Puzzle05.loadProgram "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    let result = IntCodeComputer.run computer IntCodeComputer.consoleInputter IntCodeComputer.consoleOutputter
    printfn "Done"

let puzzle06 () =
    let tree = Puzzle06.buildFromFile "inputs\\puzzle06.txt"
    let total = Puzzle06.totalDepth tree
    printfn "Total depth of orbits: %d" total

    let distance = Puzzle06.transferDistance tree "YOU" "SAN"
    printfn "You have to make %A transfers from YOU to SAN" distance

let puzzle07 () =
    let program = System.IO.File.ReadAllText("inputs\\puzzle07.txt")
    let part1 = Puzzle07.largestOutputSignal program
    printfn "The largest possible output is %d" part1

    let part2 = Puzzle07.maxFeedbackSignal program
    printfn "The max possible output from the feedback loop is %d" part2

let puzzle08 () =
    let data = System.IO.File.ReadAllText("inputs\\puzzle08.txt")
    let image = Puzzle08.SpaceImage.create 25 6 data
    Puzzle08.SpaceImage.checksum image |> printfn "The image checksum is %d"

    image |> Puzzle08.SpaceImage.render |> printfn "%s"

let puzzle09 () =
    let computer = IntCodeComputer.loadProgramFromFile "inputs\\puzzle09.txt"
    IntCodeComputer.run computer IntCodeComputer.consoleInputter IntCodeComputer.consoleOutputter |> ignore

let puzzle10 () =
    let map = AsteroidMap.loadMap (System.IO.File.ReadAllText("inputs\\puzzle10.txt"))
    let best = Puzzle10.bestLocation map
    let visible = Puzzle10.visibleAsteroids map best |> Seq.length
    printfn "Best location is %A with %d visible asteroids" best visible

    let targets = Puzzle10.vaporization map best |> Seq.toArray
    let (x, y) = targets.[199]
    let result = x * 100 + y
    printfn "200th target is (%d, %d) for result %d" x y result

let puzzle11 () =
    let program = System.IO.File.ReadAllText("inputs\\puzzle11.txt")
    Puzzle11.countPanels program |> printfn "There are %d painted panels"

    Puzzle11.printRegistration program

let puzzle12 () =
    let moons = [|
        Puzzle12.Moon.parseInitial "<x=-9, y=-1, z=-1>"
        Puzzle12.Moon.parseInitial "<x=2, y=9, z=5>"
        Puzzle12.Moon.parseInitial "<x=10, y=18, z=-12>"
        Puzzle12.Moon.parseInitial "<x=-6, y=15, z=-7>"
    |]

    for i = 1 to 1000 do
        Puzzle12.tick moons

    moons |> Puzzle12.totalEnergy |> printfn "The total energy after 1000 runs is %d"

    [|
        Puzzle12.Moon.parseInitial "<x=-9, y=-1, z=-1>"
        Puzzle12.Moon.parseInitial "<x=2, y=9, z=5>"
        Puzzle12.Moon.parseInitial "<x=10, y=18, z=-12>"
        Puzzle12.Moon.parseInitial "<x=-6, y=15, z=-7>"
    |] |> Puzzle12.cyclesUntilReset |> printfn "It takes %d runs to reset";

let puzzle13 () =
    let program = System.IO.File.ReadAllText("inputs\\puzzle13.txt")
    //Puzzle13.blockTileCount program |> printfn "THere are %d Block tiles"

    Puzzle13.play program

let puzzle14 () =
    let tree = System.IO.File.ReadAllText("inputs\\puzzle14.txt") |> Puzzle14.FormulaTree.parse
    let factory = Puzzle14.NanoFactory(tree)
    factory.ProduceFuel () |> printfn "Consumed %d ORE"

    Puzzle14.maxFuelFor tree 1000000000000L |> printfn "With a trillion ore, we can make %d fuel units"

let puzzle15 () =
    let code = System.IO.File.ReadAllText("inputs\\puzzle15.txt")
    Puzzle15.exploreMaze code |> printfn "It takes %A steps to find the oxygen system"

[<EntryPoint>]
let main argv =

    // puzzle01 ()
    // puzzle02 ()
    // puzzle03 ()
    // puzzle04 ()
    // puzzle05 ()
    // puzzle06 ()
    // puzzle07 ()
    // puzzle08 ()
    // puzzle09 ()
    // puzzle10 ()
    // puzzle11 ()
    // puzzle12 ()
    // puzzle13 ()
    // puzzle14 ()
    puzzle15 ()
    0

