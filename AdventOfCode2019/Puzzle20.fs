module Puzzle20
open Utils

type PortalPointer =
    {
        Exit: Point;
        Name: string;
    }

module PortalPointer =
    let create tuple name =
        { Exit = Point.fromTuple tuple; Name = name }

type DonutMazeCell =
    | Empty
    | InsidePortal of PortalPointer
    | OutsidePortal of PortalPointer

let (|Portal|_|) = function
    | (InsidePortal p) | (OutsidePortal p) -> Some p
    | _ -> None


type DonutMaze = Map<Point, DonutMazeCell>

module DonutMaze =

    let private isPortalChar c =
        int c >= int 'A' && int c <= int 'Z'

    let rec private findDimension selector n =
        match selector (n+2) with
            | '#' | '.' -> findDimension selector (n+1)
            | _ -> n

    let private findHorizontal (data: string array) =
        let midpoint = data.Length / 2
        let selector n =
            data.[midpoint].[n]

        findDimension selector 0

    let private findVertical (data: string array) =
        let midpoint = data.[0].Length / 2
        let selector n =
            data.[n].[midpoint]

        findDimension selector 0

    let findDimensions (data: string array) =
        findHorizontal data, findVertical data

    type ReadDirection = Vertical | Horizontal

    let readLabel (data: string array) start d =
        match d with
            | Vertical ->
                let c1 = data.[start.y].[start.x]
                let c2 = data.[start.y + 1].[start.x]
                sprintf "%c%c" c1 c2
            | Horizontal ->
                let c1 = data.[start.y].[start.x];
                let c2 = data.[start.y].[start.x + 1]
                sprintf "%c%c" c1 c2


    let tryReadPortal (data: string array) p d =
        if isPortalChar data.[p.y].[p.x] then
            let label =
                 match d with
                    | North -> readLabel data (p |> Point.translate North) Vertical
                    | South -> readLabel data p Vertical
                    | East -> readLabel data p Horizontal
                    | West -> readLabel data (p |> Point.translate West) Horizontal

            Some label
        else
            None

    let parse s: DonutMaze =
        let rows = splitLines s
        let height = rows.Length
        let width = rows.[0].Length

        let checkPortal p d =
            let point = p |> Point.translate d
            if point.x < 0 || point.x >= width || point.y < 0 || point.y >= height then
                None
            else
                tryReadPortal rows point d
                |> Option.map (fun label -> (point, { Exit = p; Name = label }))

        let portalType p =
            if p.x <= 2 || p.x >= width - 2 || p.y <= 2 || p.y >= height - 2 then
                OutsidePortal
            else
                InsidePortal

        seq {
            for y = 0 to height - 1 do
                for x = 0 to width - 1 do
                    if rows.[y].[x] = '.' then
                        let p = { x = x; y = y; }
                        yield (p, Empty)

                        let surroundingPortals =
                            Direction.all
                            |> List.toSeq
                            |> Seq.choose (fun dir -> checkPortal p dir)
                            |> Seq.map (fun (p, pointer) -> (p, pointer |> portalType p))

                        yield! surroundingPortals

        }
        |> Map.ofSeq

    let findPortals name (maze: DonutMaze) =
        let isTarget (point, cell) =
            match cell with
                | Portal p when p.Name = name -> true
                | _ -> false

        maze
        |> Map.toSeq
        |> Seq.filter isTarget


    let start maze =
        let cell = findPortals "AA" maze |> Seq.exactlyOne |> snd
        match cell with
            | InsidePortal ptr -> ptr.Exit
            | OutsidePortal ptr -> ptr.Exit
            | _ -> failwithf "couldnt find start point"

    let stop maze =
        let cell = findPortals "ZZ" maze |> Seq.exactlyOne |> snd
        match cell with
            | Portal ptr -> ptr.Exit
            | _ -> failwithf "couldnt find stop point"

    let findExitPoint portal startingPoint maze =
        let isOppositePortal (p, cell) =
            match cell with
                | Portal _ when p <> startingPoint -> true
                | _ -> false

        let (_, oppositeCell) =
            findPortals portal.Name maze
            |> Seq.filter isOppositePortal
            |> Seq.exactlyOne

        oppositeCell

[<Struct>]
type VisitLocation = { depth: int; point: Point }

type Descender = int -> DonutMazeCell -> int

// https://brilliant.org/wiki/dijkstras-short-path-finder/
let private dijkstra (maze: DonutMaze) (depthDelta: Descender) =
    let createFloor depth =
        maze |> Seq.map (fun kvp -> { depth = depth; point = kvp.Key })

    let distances = System.Collections.Generic.Dictionary<VisitLocation, int>()
    let visited = System.Collections.Generic.HashSet<VisitLocation>()
    let queue = System.Collections.Generic.HashSet<VisitLocation>(createFloor 0)
    let visitedFloor = System.Collections.Generic.Dictionary<int, bool>()
    // let bestPath = Map<Point, Point> track best previous node, can walk this later to get path

    let startPoint = DonutMaze.start maze
    let exitPoint = DonutMaze.stop maze
    visitedFloor.Add(0, true);
    distances.[{ depth = 0; point = startPoint }] <- 0


    let distance p =
        match distances.ContainsKey(p) with
            | true -> distances.[p]
            | false -> System.Int32.MaxValue

    let popMin () =
        let min = queue |> Seq.minBy distance
        queue.Remove min |> ignore
        min

    while queue.Count > 0 do
        let v = popMin ()
        visited.Add v |> ignore
        let dist = distance v

        match maze.[v.point] with
            | Empty ->
                for d in Direction.all do
                    let next = { v with point = v.point |> Point.translate d }
                    if maze.ContainsKey next.point && not (visited.Contains next) then
                        if dist + 1 < distance next then
                            distances.[next] <- dist + 1
                            // bestPath[v] = next
            | Portal pointer ->
                if pointer.Name <> "AA" && pointer.Name <> "ZZ" then
                    let nextCell = DonutMaze.findExitPoint pointer v.point maze
                    match nextCell with
                        | Portal pointer ->
                            let newDepth = depthDelta v.depth nextCell
                            let next = { point = pointer.Exit; depth = newDepth; }
                            printfn "decend to %d" newDepth

                            if not (visitedFloor.ContainsKey newDepth) then
                                createFloor newDepth |> Seq.iter (fun loc -> queue.Add(loc) |> ignore)
                                visitedFloor.Add(newDepth, true)

                            if maze.ContainsKey next.point && not (visited.Contains next) then
                                // portal exits are 0 cost
                                if dist + 0 < distance next then
                                    distances.[next] <- dist + 0
                        | _ -> ()
            | _ -> ()

    distances.[{ depth = 0; point = exitPoint }]


let noopDescender depth cell = depth

let recursiveDescender depth cell =
    match cell with
        | InsidePortal _ -> depth + 1
        | OutsidePortal _ -> depth - 1
        | _ -> depth

let part1 maze =
    dijkstra maze noopDescender

let part2 maze =
    dijkstra maze recursiveDescender





