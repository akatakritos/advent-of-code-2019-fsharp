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
