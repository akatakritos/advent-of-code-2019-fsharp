module Puzzle03
open System.Linq

type Direction = Left | Right | Up | Down

type Instruction = {
    Direction: Direction;
    Distance: int;
}

type Point = {
    X: int;
    Y: int;
}

let Origin = { X = 0; Y = 0 }

type Wire = seq<Instruction>

let private step point direction =
    match direction with
    | Left -> { point with X = point.X - 1}
    | Right -> { point with X = point.X + 1}
    | Up -> { point with Y = point.Y + 1 }
    | Down -> { point with Y = point.Y - 1 }

let private walk point direction distance =
    match direction with
    | Left -> { point with X = point.X - distance}
    | Right -> { point with X = point.X + distance}
    | Up -> { point with Y = point.Y + distance }
    | Down -> { point with Y = point.Y - distance }

let private traceSegment start segment =
    seq { for i in 0..segment.Distance do yield walk start segment.Direction (i+1) }


let traceWire (wire:Wire) =
    let mutable point = Origin;
    seq {
        yield point
        for segment in wire do 
            for i = 1 to segment.Distance do
                point <- step point segment.Direction;
                yield point;
    }

        

let private parseDirection c =
    match c with
        | 'L' -> Left
        | 'R' -> Right
        | 'U' -> Up
        | 'D' -> Down
        | _ -> failwith (sprintf "unrecognized direction code %c" c)

let parseInstruction (instruction: string) =
    let direction = parseDirection instruction.[0]
    let distance = instruction.Substring(1) |> int
    { Distance = distance; Direction = direction }
    


let parseInstructions (instructions: string) =
    instructions.Split(",")
    |> Seq.map parseInstruction
    
let manhattanDistance p0 p1 =
    abs (p1.X - p0.X) + abs (p1.Y - p0.Y)

let intersect (a: 'a seq) (b: 'a seq) = a.Intersect(b)

let countDistance (wire: seq<Point>) (point:Point) =
    wire |> Seq.findIndex (fun p -> p = point)


let findMinimumIntersectionDistance wire1 wire2 =
    let points1 = traceWire wire1 |> Seq.toArray
    let points2 = traceWire wire2 |> Seq.toArray
    let duplicates = intersect points1 points2

    duplicates
    |> Seq.map (fun point -> manhattanDistance point Origin)
    |> Seq.filter (fun distance -> distance > 0)
    |> Seq.min

let findMinimumTravelDistance wire1 wire2 =
    let points1 = traceWire wire1 |> Seq.toArray
    let points2 = traceWire wire2 |> Seq.toArray
    let duplicates = intersect points1 points2

    duplicates
    |> Seq.filter (fun point -> point <> Origin)
    |> Seq.map (fun point -> (countDistance points1 point) + (countDistance points2 point))
    |> Seq.min


