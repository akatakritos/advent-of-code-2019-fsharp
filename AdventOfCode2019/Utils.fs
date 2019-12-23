module Utils

[<Struct>]
type Direction =
    | North
    | South
    | East
    | West

module Direction =
    let all = [North; South; East; West;]

[<Struct>]
type Point = { x: int; y: int }

module Point =
    let translate direction point =
        match direction with
            | North -> { point with y = point.y - 1 }
            | South -> { point with y = point.y + 1 }
            | East -> { point with x = point.x + 1 }
            | West -> { point with x = point.x - 1 }

module Chars =
    let between (a: char) (b: char) (x: char) =
        (int a) <= (int x) && (int x) <= (int b)

    let upper c = System.Char.ToUpperInvariant(c)