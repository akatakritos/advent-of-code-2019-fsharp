module AsteroidMap 
type Point = int * int

type MapState = Empty | Occupied

type T = {
    width: int;
    height: int;
    map: Map<Point, MapState>;
}

let private createMapState = function
    | '#' -> Occupied
    | '.' -> Empty
    | x -> failwithf "Unrecognized map character: '%c'" x

let loadMap (data: string) =
    let lines = data.Split([| "\n"; "\r\n" |], System.StringSplitOptions.RemoveEmptyEntries)
    let width = lines.[0].Length
    let height = lines.Length

    let points = seq {
        for y in 0..height-1 do
            for x in 0..width-1 do
                yield ((x, y), createMapState lines.[y].[x])
    }

    {
        width = width;
        height = height;
        map = new Map<Point, MapState>(points);
    }

let private x = fst
let private y = snd


let asteroids map =
    map.map
    |> Seq.filter (fun kvp -> kvp.Value = Occupied)
    |> Seq.map (fun kvp -> kvp.Key)

//type private Slope =
//    | Infinite of int
//    | Finite of double

//let private isPointAlongLine m (b: double option) (p: Point) =
//    match m with
//        | Finite m -> 
//            let y0 =  (m * double (x p)) + b.Value
//            let y1 = double (y p)
//            let result = y0 = y1
//            if not result then printfn "y0=%f y1=%f result=%b" y0 y1 result
//            result
//        | Infinite xintercept -> x p = xintercept



//let private slope (p0: Point) (p1: Point) =
//    let dx = double (x p1 - x p0)
//    let dy = double (y p1 - y p0)

//    if dx = 0.0 then Infinite (x p0) else Finite (dy / dx)

//let private intercept (p: Point) slope =
//    match slope with
//        | Infinite _ -> None
//        | Finite m -> Some (double(y p) - (m * double(x p)))

let rtod r = r * 180.0 / System.Math.PI
    

type Vector = {
    dx: int;
    dy: int;
} with 
    member this.magnitude = sqrt( double this.dx ** 2.0 + double this.dy ** 2.0)
    member this.angle = 
        let a = (atan2 (double this.dy) (double this.dx) |> rtod) + 90.0
        if a < 0.0 then a + 360.0 else a

let rec gcd a b =
    if b = 0 then a else gcd b (a % b)

let createVector p0 p1 =
    let dx = x p1 - x p0;
    let dy = y p1 - y p0;
    //let div = gcd (abs dx) (abs dy)

    {
        dx = dx;
        dy = dy;
    }

let normalize v =
    let div = gcd (abs v.dx) (abs v.dy)

    {
        dx = v.dx / div;
        dy = v.dy / div;
    }

//part - use polar coordinates to calculate along vector. can use for part 2 by sorting by degrees

let distance (p0: Point) (p1: Point) =
    let dx = double (x p0 - x p1)
    let dy = double (y p0 - y p1)
    sqrt ( dx ** 2.0 + dy ** 2.0)

let private walk p0 p1 v = 
    let mutable p = p0;
    seq {
        while p <> p1 do
            p <- (x p + v.dx, y p + v.dy)
            yield p
    }

let isVisible (map: T) (p0: Point) (p1: Point) =
    let v = createVector p0 p1 |> normalize

    let blockers =
       walk p0 p1 v
       |> Seq.filter (fun p -> p <> p1 && map.map.[p] = Occupied)
       |> Seq.length
    blockers = 0
