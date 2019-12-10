module Puzzle10Tests
open Puzzle10;
open AsteroidMap;
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``load correctly fills the map`` () =
    let data = ".#..#\n\
    .....\n\
    #####\n\
    ....#\n\
    ...##\n"

    let state = loadMap data
    
    state.width |> should equal 5
    state.height |> should equal 5
    state.map.[(0,0)] |> should equal AsteroidMap.Empty
    state.map.[(2, 2)] |> should equal Occupied
    state.map.[(4, 4)] |> should equal Occupied


[<Fact>]
let ``isVisible checks the lines of sight`` () =
    let data = ".#..#\n\
    .....\n\
    #####\n\
    ....#\n\
    ...##\n"

    let map = loadMap data

    let SOURCE = (3, 4)
    isVisible map SOURCE (4, 0) |> should equal true
    isVisible map SOURCE (1, 0) |> should equal false
    isVisible map SOURCE (3, 2) |> should equal true

[<Fact>]
let ``bestLocation - example 0`` () =
    let data = ".#..#\n\
    .....\n\
    #####\n\
    ....#\n\
    ...##\n"

    let map = loadMap data

    bestLocation map |> should equal (3, 4)
    visibleAsteroids map (3, 4) |> Seq.length |> should equal 8

[<Fact>]
let ``bestLocation - example 1 is 5,8 with 33 visible`` () =
    let map = loadMap "......#.#.\n\
    #..#.#....\n\
    ..#######.\n\
    .#.#.###..\n\
    .#..#.....\n\
    ..#....#.#\n\
    #..#....#.\n\
    .##.#..###\n\
    ##...#..#.\n\
    .#....####\n"

    visibleAsteroids map (5, 8) |> Seq.length |> should equal 33
    bestLocation map |> should equal (5, 8)

[<Fact>]
let ``Best is 1,2 with 35 other asteroids detected`` () =
    let map = loadMap "#.#...#.#.\n\
    .###....#.\n\
    .#....#...\n\
    ##.#.#.#.#\n\
    ....#.#.#.\n\
    .##..###.#\n\
    ..#...##..\n\
    ..##....##\n\
    ......#...\n\
    .####.###.\n"

    visibleAsteroids map (1, 2) |> Seq.length |> should equal 35
    bestLocation map |> should equal (1, 2)

[<Fact>]
let ``Best is 11,13 with 210 other asteroids detected`` () =
    let map = loadMap ".#..##.###...#######\n\
    ##.############..##.\n\
    .#.######.########.#\n\
    .###.#######.####.#.\n\
    #####.##.#.##.###.##\n\
    ..#####..#.#########\n\
    ####################\n\
    #.####....###.#.#.##\n\
    ##.#################\n\
    #####.##.###..####..\n\
    ..######..##.#######\n\
    ####.##.####...##..#\n\
    .#####..#.######.###\n\
    ##...#.##########...\n\
    #.##########.#######\n\
    .####.#.###.###.#.##\n\
    ....##.##.###..#####\n\
    .#.#.###########.###\n\
    #.#.#.#####.####.###\n\
    ###.##.####.##.#..##\n"
    

    visibleAsteroids map (11, 13) |> Seq.length |> should equal 210
    bestLocation map |> should equal (11,13)

