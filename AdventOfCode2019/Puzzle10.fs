module Puzzle10   
open AsteroidMap

let invisibleAsteroids map p0 =
    asteroids map
    |> Seq.filter (fun p -> not (isVisible map p0 p))

let visibleAsteroids map p0 =
    asteroids map
    |> Seq.filter (fun p -> p <> p0)
    |> Seq.filter (fun p -> isVisible map p0 p)

let bestLocation map =
    asteroids map
    |> Seq.maxBy (fun p -> visibleAsteroids map p |> Seq.length)

type TargetingGroup = {
    angle: float;
    points: Point list;
}
   
let vaporization map p0 =
    let point v = 
        let (x, y) = p0
        (x + v.dx, y + v.dy)

    let targets = 
        asteroids map
        |> Seq.filter (fun p -> p <> p0)
        |> Seq.map (fun p -> createVector p0 p)
        |> Seq.groupBy (fun p -> p.angle)
        |> Seq.map (fun (angle, vectors) -> 
            {
            angle = angle;
            points = vectors |> Seq.sortBy (fun v -> v.magnitude) |> Seq.map point |> Seq.toList
            })
        |> Seq.sortBy (fun target -> target.angle)
        |> Seq.toArray

    let mutable asteroidsRemaining = true
    seq {
        while asteroidsRemaining do
            asteroidsRemaining <- false

            for i in 0..targets.Length-1 do
                match targets.[i].points with
                    | t::points ->
                        yield t
                        asteroidsRemaining <- true
                        targets.[i] <- { targets.[i] with points = points }
                    | [] -> ()
    }

