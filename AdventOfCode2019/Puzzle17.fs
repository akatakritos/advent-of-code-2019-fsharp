module Puzzle17
open Utils
open IoComputer

module Chars =
    let isNewline c = c |> int = 10

    let Scaffold = '#'
    let Empty = '.'

type Vacuum(program: string) =
    let computer = IoComputer(program)
    let mutable map = Map<Point, char>([])
    let mutable position = { x = 0; y = 0 }

    let isIntersection point c =
        c = Chars.Scaffold &&
            map.TryFind (point |> Point.translate North) = Some Chars.Scaffold &&
            map.TryFind (point |> Point.translate South) = Some Chars.Scaffold &&
            map.TryFind (point |> Point.translate East) = Some Chars.Scaffold &&
            map.TryFind (point |> Point.translate West) = Some Chars.Scaffold


    member this.Map = map

    member this.ReadView () =
        let mutable p = { x = 0; y = 0 }

        computer.AdvanceToIo ()

        while not (computer.IsComplete || computer.IsAwaitingInput) do
            let c = computer.Receive () |> char
            printf "%c" c
            map <- map.Add (p, c)

            if c = '^' then
                position <- p

            if Chars.isNewline c then
                p <- { p with y = p.y + 1; x = 0 }
            else
                p <- { p with x = p.x + 1 }



            computer.AdvanceToIo ()

    member this.Intersections () =
        map
        |> Seq.filter (fun kvp -> isIntersection kvp.Key kvp.Value)
        |> Seq.map (fun kvp -> kvp.Key)

    member this.Position = position

    member this.Computer = computer



type Instruction =
    | Left
    | Right

module Instruction =
    let turn instruction direction =
        match (direction, instruction) with
            | (North, Left) -> West
            | (North, Right) -> East
            | (East, Left) -> North
            | (East, Right) -> South
            | (South, Left) -> East
            | (South, Right) -> West
            | (West, Left) -> South
            | (West, Right) -> North


type WalkState = { point: Point; direction: Direction; }

[<StructuredFormatDisplayAttribute("{instruction}{steps}")>]
type PathSegment = { instruction: Instruction; steps: int }

type InstructionSolver(map: Map<Point, char>, startingPoint: Point, startingDirection: Direction) =
    let mutable position = startingPoint
    let mutable direction = startingDirection


    let canStep point =
        match map.TryFind point with
            | Some c when c = Chars.Scaffold -> true
            | _ -> false

    let canGoStraight point =
        let p' = point |> Point.translate direction
        canStep p'

    let canTurn turnDirection =
        let direction' = direction |> Instruction.turn turnDirection
        let p' = position |> Point.translate direction'
        canStep p'

    let walk () =
        let mutable steps = 0
        while canGoStraight position do
            steps <- steps + 1
            position <- position |> Point.translate direction

        steps


    member this.FindPath () =
        let rec recurse sequence =
            if canTurn Right then
                direction <- direction |> Instruction.turn Right
                let steps = walk ()
                let cmd = { instruction = Right; steps = steps }
                recurse (cmd::sequence)

            else if canTurn Left then
                direction <- direction |> Instruction.turn Left
                let steps = walk ()
                let cmd = { instruction = Left; steps = steps }
                recurse (cmd::sequence)

            else
                sequence

        recurse [] |> List.rev






let puzzle1 program =
    let vacuum = Vacuum(program)
    vacuum.ReadView ()

    vacuum.Intersections ()
    |> Seq.sumBy (fun point -> (point.x) * (point.y))
    |> printfn "The sum of the alignment parameters is %d"


let puzzle2 program =
    let vacuum = Vacuum(program)
    vacuum.Computer.OverrideMemory 0 2L
    vacuum.ReadView ()

    let solver = InstructionSolver(vacuum.Map, vacuum.Position, North)
    let path = solver.FindPath ()
    printfn "%A" path

    path
    |> List.map (fun item ->
        match item.instruction with
            | Left -> "L," + string item.steps
            | Right -> "R," + string item.steps
    )
    |> List.iter (fun i -> printf "%s," i)
    printfn ""

    let a = "L,8,R,12,R,12,R,10"
    let b = "R,10,R,12,R,10"
    let c = "L,10,R,10,L,6"
    let code = "A,B,A,B,C,C,B,A,B,C"

    let test = code.Replace("A", a).Replace("B", b).Replace("C", c)
    printfn "%s" test

    let computer = vacuum.Computer

    computer.SendLine code
    computer.ReadLine () |> printf "%s"
    computer.SendLine a
    computer.ReadLine () |> printf "%s"
    computer.SendLine b
    computer.ReadLine () |> printf "%s"
    computer.SendLine c
    computer.ReadLine () |> printf "%s"
    computer.SendLine "n"

    while not computer.IsComplete do
        let c = computer.Receive ()
        if c > 255L then
            printfn "Space dust collected: %d" c
        computer.AdvanceToIo ()





