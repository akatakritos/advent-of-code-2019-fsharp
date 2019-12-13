module Puzzle13
open IntCodeComputer

type Tile =
    | Empty
    | Wall
    | Block
    | Paddle
    | Ball
    | Score of int64

module Tile =
    let fromInt64 = function
        | 0L -> Empty
        | 1L -> Wall
        | 2L -> Block
        | 3L -> Paddle
        | 4L -> Ball
        | n -> Score n

    let unwrapScore = function
        | Score n -> n
        | _ -> failwith "Tile was not a score"

type Point = { x: int64; y: int64 }

type ProgramOutput = { point: Point; tile: Tile }

module ProgramOutput =
    let fromInts tile x y =
        { tile = Tile.fromInt64 tile; point = { x = x; y = y; } }


let blockTileCount program =
    let computer = loadProgram program

    let inputter = fun () -> 0L

    let mutable stack = []
    let mutable count = 0
    let outputter = fun (n: int64) ->
        stack <- n::stack

        match stack with
            | tile::y::x::tail ->
                stack <- tail
                let output = ProgramOutput.fromInts tile x  y
                if output.tile = Block then count <- count + 1
            | _ -> ()

    run computer inputter outputter |> ignore

    count

type JoystickPosition =
    | Neutral
    | Left
    | Right

module JoystickPosition =
    let toInt64 = function
        | Neutral -> 0L
        | Left -> -1L
        | Right -> 1L

    let fromKeyboard () =
        match System.Console.ReadKey(false).Key with
            | System.ConsoleKey.LeftArrow -> Left
            | System.ConsoleKey.RightArrow -> Right
            | _ -> Neutral


let play program =
    let computer = loadProgram program
    computer.Memory.[0] <- 2L // play for free


    System.Console.Clear()
    let mutable score = 0L
    let mutable ballPosition = { x = 0L; y = 0L; }
    let mutable paddlePosition = {x = 0L; y = 0L; }

    let decision ball paddle =
        if ball.x < paddle.x then Left
        elif ball.x > paddle.x then Right
        else Neutral

    let joypadInputter () =
        decision ballPosition paddlePosition |> JoystickPosition.toInt64

    let draw (output: ProgramOutput) =
        System.Console.CursorLeft <- output.point.x |> int
        System.Console.CursorTop <- output.point.y |> int
        match output.tile with
            | Empty -> printf " "
            | Wall -> printf "█"
            | Ball ->
                printf "o"
                ballPosition <- output.point
            | Block -> printf "X"
            | Paddle ->
                printf "-"
                paddlePosition <- output.point
            | Score _ -> ()

    let mutable stack = []
    let outputter = fun (n: int64) ->
        stack <- n::stack

        match stack with
            | tile::y::x::tail ->
                stack <- tail
                let output = ProgramOutput.fromInts tile x y
                match output.point with
                    | { x = -1L; y = 0L } ->
                        score <- tile
                    | _ ->
                        draw output

            | _ -> ()

    run computer joypadInputter outputter |> ignore

    printfn "Score: %d" score
