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
    | Portal of PortalPointer

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
                            |> Seq.map (fun (p, pointer) -> (p, Portal pointer))

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
            | Portal ptr -> ptr.Exit
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

        match oppositeCell with
            | Portal ptr -> ptr.Exit
            | _ -> failwithf "couldnt find opposite pointer of %A" startingPoint


let dijkstra (maze: DonutMaze) =
    let distances = System.Collections.Generic.Dictionary<Point, int>()
    let visited = System.Collections.Generic.HashSet<Point>()
    let queue = System.Collections.Generic.HashSet<Point>(maze |> Seq.map (fun kvp -> kvp.Key))

    let startPoint = DonutMaze.start maze
    let exitPoint = DonutMaze.stop maze
    distances.[startPoint] <- 0


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

        match maze.[v] with
            | Empty ->
                for d in Direction.all do
                    let next = v |> Point.translate d
                    if maze.ContainsKey next && not (visited.Contains next) then
                        if dist + 1 < distance next then
                            distances.[next] <- dist + 1
            | Portal pointer ->
                if pointer.Name <> "AA" && pointer.Name <> "ZZ" then
                    let next = DonutMaze.findExitPoint pointer v maze
                    if maze.ContainsKey next && not (visited.Contains next) then
                        // portal exits are 0 cost
                        if dist + 0 < distance next then
                            distances.[next] <- dist + 0

    distances.[exitPoint]










