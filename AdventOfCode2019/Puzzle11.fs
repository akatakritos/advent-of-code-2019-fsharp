module Puzzle11
open IntCodeComputer


type Color =
    | Black
    | White

module Color =
    let fromInt64 = function
        | 0L -> Black
        | 1L -> White
        | n -> failwithf "Unrecognized color code %d" n

    let toInt64 = function
        | Black -> 0L
        | White -> 1L

type PanelState =
    | Unpainted
    | Painted of Color

type TurnDirection =
    | TurnLeft
    | TurnRight

module TurnDirection =
    let fromInt64 = function
        | 0L -> TurnLeft
        | 1L -> TurnRight
        | n -> failwithf "Unrecognized turn code %d" n

    let toInt64 = function
        | TurnLeft -> 0L
        | TurnRight -> 1L

type Direction =
    | Up
    | Down
    | Right
    | Left

[<Struct>]
type Point = { x: int; y: int; }

type Robot =
    { direction: Direction;
      position: Point; }

module Robot =
    let create =
        { direction = Up; position = { x = 0; y = 0; } }

type StepResult = Continue | Abort

type Painter(computer: Computer, initialColor: Color) =
    let mutable program = computer
    let mutable robot = Robot.create
    let mutable data = new Map<Point, PanelState>([
        ({ x = 0; y = 0; }, Painted initialColor)
    ])


    let turnDirection facing turn =
        match (facing, turn) with
            | (Up, TurnRight) -> Right
            | (Up, TurnLeft) -> Left
            | (Right, TurnRight) -> Down
            | (Right, TurnLeft) -> Up
            | (Down, TurnRight) -> Left
            | (Down, TurnLeft) -> Right
            | (Left, TurnRight) -> Up
            | (Left, TurnLeft) -> Down

    let setColor (point, color) =
        data <- data.Add (point, color)

    let getColor point =
        match data.TryFind point with
            | Some (Painted color) -> color
            | Some (Unpainted)
            | None ->
                setColor (point, Unpainted)
                Black

    let getOutput () =
        match advanceToIo program with
            | PausedForOutput computer ->
                program <- computer
                let (computer', output) = retrieveOutput program
                program <- computer'
                output
            | _ -> failwith "InvalidProgram: Should be getting an output"


    let getPaintInstruction () =
        let color = getOutput () |> Color.fromInt64
        let direction = getOutput () |> TurnDirection.fromInt64
        (color, direction)

    let sendColor () =
        match advanceToIo program with
            | PausedForInput computer ->
                program <- computer
                let input = getColor robot.position |> Color.toInt64
                program <- provideInput input program
            | _ -> failwith "InvalidProgram: Should be waiting for input"

    let turnAndMove direction =
        let direction' = turnDirection robot.direction direction
        let point = robot.position
        let position' = match direction' with
            | Left -> { point with x = point.x - 1}
            | Right -> { point with x = point.x + 1}
            | Up -> { point with y = point.y + 1 }
            | Down -> { point with y = point.y - 1 }

        robot <- { robot with direction = direction'; position = position'; }


    let runOrAbort () =
        match advanceToIo program with
            | PausedForInput computer ->
                program <- computer
                Continue
            | PausedForOutput _ -> failwith "InvalidProgram: Shouldnt have received an output here"
            | Aborted -> Abort


    member this.Step () =
        sendColor ()
        let (color, direction) = getPaintInstruction ()
        setColor (robot.position, Painted color)
        turnAndMove direction

        runOrAbort ()

    member this.Data = data;


let executeProgram (painter: Painter) =
    let rec recurse () =
        match painter.Step () with
            | Continue -> recurse ()
            | Abort -> painter.Data

    recurse ()

let private painted = function
    | Unpainted -> false
    | Painted _ -> true

let countPanels program =
    let computer = loadProgram program
    let painter = Painter(computer, Black)
    let result = executeProgram painter

    result
    |> Seq.filter (fun kvp -> painted kvp.Value)
    |> Seq.length

type private Range =
    { minX: int; minY: int; maxX: int; maxY: int }

let private dimensions points =
    let mutable minX = None
    let mutable minY = None
    let mutable maxX = None
    let mutable maxY = None

    points
    |> Seq.iter (fun p ->
        match minX with
            | None -> maxX <- Some p.x
            | Some max when p.x > max -> maxX <- Some p.x
            | Some _ -> ()

        match maxY with
            | None -> maxY <- Some p.y
            | Some max when p.y > max -> maxY <- Some p.y
            | Some _ -> ()

        match minY with
            | None -> minY <- Some p.y
            | Some min when p.y < min -> minY <- Some p.y
            | Some _ -> ()

        match minX with
            | None -> minX <- Some p.x
            | Some min when p.x < min -> minX <- Some p.x
            | Some _ -> ()
    )

    { minX = minX.Value; minY = minY.Value; maxX = maxX.Value; maxY = maxY.Value; }

let private printCell cell =
    match cell with
        | None
        | Some (Unpainted)
        | Some (Painted Black) -> printf " "
        | Some (Painted White) -> printf "#"

let printRegistration program =
    let computer = loadProgram program
    let painter = Painter(computer, White)
    let result = executeProgram painter

    let range = dimensions (result |> Seq.map (fun kvp -> kvp.Key))

    for y = range.maxY downto range.minY do
        for x = range.minX to range.maxX do
            result.TryFind { x = x; y = y; } |> printCell

        printfn ""


