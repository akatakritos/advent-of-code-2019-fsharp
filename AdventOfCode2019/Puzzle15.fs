module Puzzle15
open System

[<Struct>]
type MovementCommand =
    | North
    | South
    | East
    | West

module MovementCommand =
    let toInt64 = function
        | North -> 1L
        | South -> 2L
        | West -> 3L
        | East -> 4L

    let otherDirections cmd =
        seq {
            if cmd <> North then yield North
            if cmd <> South then yield South
            if cmd <> East then yield East
            if cmd <> West then yield West
        }

    let all = [North; West; South; East;]

    let opposite = function
        | North -> South
        | East -> West
        | South -> North
        | West -> East

[<Struct>]
type StatusCode =
    | HitWall
    | MovedSuccessfully
    | MovedToTarget

module StatusCode =
    let fromInt64 n =
        match n with
            | 0L -> HitWall
            | 1L -> MovedSuccessfully
            | 2L -> MovedToTarget
            | n -> failwithf "Unrecognized status code %d" n

[<Struct>]
type Position = { x: int; y: int }

module Position =
    let translate direction position =
        match direction with
            | North -> { position with y = position.y - 1 }
            | South -> { position with y = position.y + 1 }
            | East -> { position with x = position.x + 1 }
            | West -> { position with x = position.x - 1 }

[<Struct>]
type Cell =
    | Explored of int
    | Wall

type Robot(width: int, height: int) =
    //let map = Array.create (width * height) Unexplored
    let mutable map = Map<Position, Cell>([])
    let mutable position = { x = width / 2; y = height / 2 }


    let draw pos (char: string) =
        Console.SetCursorPosition(pos.x, pos.y)
        Console.Write char

    do
        Console.Clear()
        // Console.CursorVisible <- false
        // map.[position.y * width + position.x] <- Explored
        map <- map.Add (position, Explored 0)
        draw position "*"


    let isUnexplored direction =
        let check = position |> Position.translate direction
        // let cell = map.[check.y * width + check.x]
        match map.TryFind check with
            | None -> true
            | Some _ -> false

    let getStepCount position =
        match map.TryFind position with
            | None -> None
            | Some cell ->
                match cell with
                    | Explored count -> Some count
                    | _ -> None


    member this.Set position cell =
        // map.[y * width + x] <- cell
        map <- map.Add (position, cell)

    member this.MarkWall direction =
        let wall = position |> Position.translate direction
        draw wall "█"
        // draw wall "8"
        this.Set wall Wall

    member this.Move direction =
        System.Threading.Thread.Sleep(50)
        let position' = position |> Position.translate direction
        let step = getStepCount position |> Option.get

        draw position "."
        draw position' "*"
        position <- position'

        match map.TryFind position' with
            | None -> this.Set position' (Explored (step + 1))
            | _ -> ()

    member this.StepCountAtCurrentPosition () =
        getStepCount position


    member this.UnexploredDirection () =
        if isUnexplored North then Some North
        elif isUnexplored East then Some East
        elif isUnexplored West then Some West
        elif isUnexplored South then Some South
        else None

let drawstatus cmd =
    Console.CursorLeft <- 0
    Console.CursorTop <- 40
    Console.Write (sprintf "%A              " cmd)

let exploreMaze input =
    let robot = Robot(72, 72)
    let mutable program = IntCodeComputer.loadProgram input

    let sendCommand cmd =
        program <- IntCodeComputer.advanceToIo program |> IntCodeComputer.ComputerState.unwrapInput
        program <- IntCodeComputer.provideInput (cmd |> MovementCommand.toInt64) program

        program <- IntCodeComputer.advanceToIo program |> IntCodeComputer.ComputerState.unwrapOutput
        let (program', output) = IntCodeComputer.retrieveOutput program
        program <- program'

        output |> StatusCode.fromInt64

    let rec explore direction =
        // System.Threading.Thread.Sleep(25)
        // Console.ReadKey(false) |> ignore
        let status = sendCommand direction

        match status with
            | MovedToTarget ->
                robot.Move direction
                robot.StepCountAtCurrentPosition ()

            | MovedSuccessfully ->
                robot.Move direction

                // try each other direction recursively
                let mutable dir = robot.UnexploredDirection ()
                let mutable found = None
                while dir.IsSome && found.IsNone do
                    found <- explore dir.Value
                    dir <- robot.UnexploredDirection ()

                if found.IsNone then
                    // dont need to backtrackif we found it
                    let backtrack = direction |> MovementCommand.opposite
                    sendCommand backtrack |> ignore // ignore because we know its a safe spot
                    robot.Move backtrack

                found

            | HitWall ->
                robot.MarkWall direction
                None

    explore North
