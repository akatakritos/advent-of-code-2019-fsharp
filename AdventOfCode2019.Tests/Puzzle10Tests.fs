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


[<Fact>]
let ``vector tests`` () =
    (createVector (1, 1) (1, 0)).angle |> should equal 0.0
    (createVector (1, 1) (2, 1)).angle |> should equal 90.0
    (createVector (1,1) (1, 2)).angle |> should equal 180.0
    (createVector (1,1) (0, 1)).angle |> should equal 270.0
    (createVector (0,0) (1,1)).angle |> should equal 135.0

[<Fact>]
let ``vaporization order 0`` () =
    let map = loadMap ".#....#####...#..\n\
    ##...##.#####..##\n\
    ##...#...#.#####.\n\
    ..#.....#...###..\n\
    ..#.#.....#....##\n"

    vaporization map (8, 3) |> Seq.take 5 |> Seq.toArray |> should equal [|
        (8, 1);
        (9, 0);
        (9, 1);
        (10, 0);
        (9, 2);
    |]

[<Fact>]
let ``11,13 map has right target order`` () =
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
    

    let targets = vaporization map (11,13) |> Seq.toArray
    targets.Length |> should equal 299
    targets.[1 - 1] |> should equal (11,12)
    targets.[2 - 1] |> should equal (12,1)
    targets.[3 - 1] |> should equal (12,2)
    targets.[20 - 1] |> should equal (16,0)
    targets.[50 - 1] |> should equal (16,9)
    targets.[100 - 1] |> should equal (10,16)
    targets.[199 - 1] |> should equal (9,6)
    targets.[200 - 1] |> should equal (8,2)
    targets.[201 - 1] |> should equal (10,9)
    targets.[299 - 1] |> should equal (11,1)
    
