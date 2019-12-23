module Puzzle18
open Utils

type Tile =
    | Empty
    | Wall
    | Key of char
    | Door of char

module Tile =
    let parse = function
        | '.'
        | '@' -> Empty
        | '#' -> Wall
        | c when c |> Chars.between 'a' 'z' -> Key (Chars.upper c)
        | c when c |> Chars.between 'A' 'Z' -> Door c
        | c -> failwithf "undefined character %c in parse" c

type KeySet = int

module KeySet =
    let A = int 'A'

    let empty = 0

    let contains  char keySet =
        let idx = (int char) - A
        (keySet >>> idx &&& 1) = 1

    let add char keySet =
        let idx = (int char) - A
        (keySet ||| (1 <<< idx))

    let remove char keySet =
        let idx = (int char) - A
        (keySet &&& ~~~(1 <<< idx))


type Cave =
    {
        map: Tile[];
        position: Point;
        keys: KeySet;
        width: int;
        height: int;
    }


module Cave =
    let parse (s: string) =
        let lines = s.Split("\n")

        let width = lines.[0].Length
        let height = lines.Length
        let map = Array.create (width * height) Empty
        let mutable position = None
        let mutable keys = KeySet.empty

        for y = 0 to height - 1 do
            let line = lines.[y]
            for x = 0 to width - 1 do
                let tile = Tile.parse line.[x]

                if line.[x] = '@' then
                    position <- Some {x = x; y = y;}

                match tile with
                    | Key c ->
                        keys <- KeySet.add c keys
                    | _ -> ()

                map.[y * width + x] <- tile

        { map = map; position = position.Value; keys = keys; width = width; height = height }

    let get (cave: Cave) (pos: Point) =
        if pos.x >= 0 && pos.x < cave.width && pos.y >= 0 && pos.y < cave.height then
            Some cave.map.[pos.y * cave.width + pos.x]
        else
            None

    let set (cave: Cave) (pos: Point) tile =
        cave.map.[pos.y * cave.width + pos.x] <- tile

    let canStep cave position =
        match get cave position with
            | Some Empty
            | Some (Key _)
            | Some (Door _) -> true
            | _ -> false

    let private findKeys cave =
        let seen = System.Collections.Generic.HashSet<Point>()

        let rec find pos keys =
            seen.Add(pos) |> ignore

            let mutable found =
                match get cave pos with
                    | Some (Key c) -> KeySet.empty |> KeySet.add c
                    | _ -> KeySet.empty

            for d in Direction.all do
                let nextPosition = pos |> Point.translate d

                if (not (seen.Contains(nextPosition))) && (canStep cave nextPosition) then
                    found <- found ||| find nextPosition keys

            keys ||| found

        find cave.position KeySet.empty





    let partition (cave: Cave) =
        Direction.all
        |> Seq.iter (fun d ->
            let p = cave.position |> Point.translate d
            set cave p Wall
        )

        set cave cave.position Wall

        [(-1,-1); (1, 1); (1, -1); (-1, 1);]
        |> List.map (fun (dx, dy) ->
            let partitionPosition = { x = cave.position.x + dx; y = cave.position.y + dy; }
            let map = Array.copy cave.map;
            let cave' = {
                cave with
                    map = Array.copy cave.map;
                    position = partitionPosition;
                    keys = 0
            }

            let keys = findKeys cave'
            { cave' with keys = keys; }
        )





type SearchState = {
    missingKeys: KeySet;
    position: Point;
}


module SearchState =
    let seed cave =
        {
            missingKeys = cave.keys;
            position = cave.position;
        }

    let next cave state =
        Direction.all
        |> List.choose (fun d ->
            let nextPosition = state.position |> Point.translate d

            match Cave.get cave nextPosition with
                | Some (Door key) ->
                    match state.missingKeys |> KeySet.contains key with
                        | true -> None // dont have the key, cant go there
                        | false -> Some { state with position = nextPosition } // have the key, can go
                | Some (Key key) ->
                    Some { state with missingKeys = state.missingKeys |> KeySet.remove key; position = nextPosition; }
                | Some Empty -> Some { state with position = nextPosition }
                | _ -> None
        )

type Queue<'a> =
    | Queue of 'a list * 'a list


module Queue =
    let empty = Queue([], [])

    let enqueue q item =
        match q with
            | Queue(fs, bs) -> Queue(item::fs, bs)

    let dequeue q =
        match q with
            | Queue([], []) -> None
            | Queue(fs, b::bs) -> Some (b, Queue(fs, bs))
            | Queue(fs, []) ->
                let bs = List.rev fs
                Some (bs.Head, Queue([], bs.Tail))

let tryGetFromDict (dictionary: System.Collections.Generic.Dictionary<'key, 'value>) (key: 'key) =
    let mutable value = Unchecked.defaultof<'value>;
    match dictionary.TryGetValue(key, &value) with
        | true -> Some value
        | _ -> None

let tryDequeue (queue: System.Collections.Generic.Queue<'a>) =
    let mutable value = Unchecked.defaultof<'a>
    match queue.TryDequeue (&value) with
        | true -> Some value
        | _ -> None



let countSteps cave =
    // use dictionary instead of F# map because its way faster for the size we're going to put in it
    let stepsTo = System.Collections.Generic.Dictionary<SearchState, int>();
    // let mutable queue: Queue<SearchState> = seed |> Queue.enqueue Queue.empty
    // use C# queue because its faster (being mutable)
    let queue = System.Collections.Generic.Queue<SearchState>()

    let seed = SearchState.seed(cave)
    queue.Enqueue(seed)
    stepsTo.Add(seed, 0)

    let rec recurse () =
        match tryDequeue queue with
            | Some state ->
                // queue <- q
                // printfn "Checking %A" state.position

                match tryGetFromDict stepsTo state with
                    | Some steps ->
                        if state.missingKeys = KeySet.empty then
                            Some steps
                        else
                            for nextState in SearchState.next cave state do

                                if not (stepsTo.ContainsKey(nextState)) then
                                    stepsTo.Add(nextState, steps + 1)

                                    queue.Enqueue nextState
                                    // queue <- Queue.enqueue queue nextState

                            recurse ()
                    | _ -> None
            | _ -> None

    recurse ()

let part1 input =
    input
    |> Cave.parse
    |> countSteps

let part2 input =
    input
    |> Cave.parse
    |> Cave.partition
    |> Seq.choose countSteps
    |> Seq.sum