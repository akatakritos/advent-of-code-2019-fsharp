module Puzzle20Tests
open FsUnit.Xunit
open Xunit
open Puzzle20
open Utils

let sample () =
    [
        "         A           ";
        "         A           ";
        "  #######.#########  ";
        "  #######.........#  ";
        "  #######.#######.#  ";
        "  #######.#######.#  ";
        "  #######.#######.#  ";
        "  #####  B    ###.#  ";
        "BC...##  C    ###.#  ";
        "  ##.##       ###.#  ";
        "  ##...DE  F  ###.#  ";
        "  #####    G  ###.#  ";
        "  #########.#####.#  ";
        "DE..#######...###.#  ";
        "  #.#########.###.#  ";
        "FG..#########.....#  ";
        "  ###########.#####  ";
        "             Z       ";
        "             Z       ";
    ]
    |> String.concat "\n"

let largerSample () =
    [
    "                   A               ";
    "                   A               ";
    "  #################.#############  ";
    "  #.#...#...................#.#.#  ";
    "  #.#.#.###.###.###.#########.#.#  ";
    "  #.#.#.......#...#.....#.#.#...#  ";
    "  #.#########.###.#####.#.#.###.#  ";
    "  #.............#.#.....#.......#  ";
    "  ###.###########.###.#####.#.#.#  ";
    "  #.....#        A   C    #.#.#.#  ";
    "  #######        S   P    #####.#  ";
    "  #.#...#                 #......VT";
    "  #.#.#.#                 #.#####  ";
    "  #...#.#               YN....#.#  ";
    "  #.###.#                 #####.#  ";
    "DI....#.#                 #.....#  ";
    "  #####.#                 #.###.#  ";
    "ZZ......#               QG....#..AS";
    "  ###.###                 #######  ";
    "JO..#.#.#                 #.....#  ";
    "  #.#.#.#                 ###.#.#  ";
    "  #...#..DI             BU....#..LF";
    "  #####.#                 #.#####  ";
    "YN......#               VT..#....QG";
    "  #.###.#                 #.###.#  ";
    "  #.#...#                 #.....#  ";
    "  ###.###    J L     J    #.#.###  ";
    "  #.....#    O F     P    #.#...#  ";
    "  #.###.#####.#.#####.#####.###.#  ";
    "  #...#.#.#...#.....#.....#.#...#  ";
    "  #.#####.###.###.#.#.#########.#  ";
    "  #...#.#.....#...#.#.#.#.....#.#  ";
    "  #.###.#####.###.###.#.#.#######  ";
    "  #.#.........#...#.............#  ";
    "  #########.###.###.#############  ";
    "           B   J   C               ";
    "           U   P   P               ";
    ] |> String.concat "\n"

[<Fact>]
let ``splitLines works correctly``  () =
    let lines = splitLines <| sample ()
    lines.Length |> should equal 19

    lines
    |> Seq.countBy (fun line -> line.Length)
    |> Seq.length
    |> should equal 1

[<Fact>]
let ``DonutMaze.parse finds top portals`` () =
    let map = DonutMaze.parse <| sample ()

    map.[Point.fromTuple (9, 2)] |> should equal Empty

    map.[Point.fromTuple (9, 1)] |> should equal (Portal <| PortalPointer.create (9, 2) "AA")


[<Fact>]
let ``DonutMaze.parse finds left side portals`` () =
    let map = DonutMaze.parse <| sample ()

    map.[Point.fromTuple (1, 8)] |> should equal (Portal <| PortalPointer.create (2, 8) "BC")
    map.[Point.fromTuple (1, 13)] |> should equal (Portal <| PortalPointer.create (2, 13) "DE")
    map.[Point.fromTuple (1, 15)] |> should equal (Portal <| PortalPointer.create (2, 15) "FG")

[<Fact>]
let ``DonutMaze.parse finds the bottom side portals`` () =
    let map = DonutMaze.parse <| sample ()

    map.[Point.fromTuple (13, 17)] |> should equal (Portal <| PortalPointer.create (13, 16) "ZZ")

[<Fact>]
let ``DonutMaze.findDimensions gets the dimensions`` () =
    let data = splitLines <| sample ()
    let result = DonutMaze.findDimensions data

    result |> should equal (5, 5)

[<Fact>]
let ``DonutMaze.parse finds inside portals`` () =
    let map = DonutMaze.parse <| sample ()

    map.[Point.fromTuple (9, 7)] |> should equal (Portal <| PortalPointer.create (9, 6) "BC")
    map.[Point.fromTuple (7, 10)] |> should equal (Portal <| PortalPointer.create (6, 10) "DE")
    map.[Point.fromTuple (11, 11)] |> should equal (Portal <| PortalPointer.create (11, 12) "FG")

[<Fact>]
let ``dijkstra finds 23 steps for the sample`` () =
    let map = DonutMaze.parse <| sample ()

    dijkstra map |> should equal 23

[<Fact>]
let ``dijkstra finds 58 for the large sample`` () =
    largerSample ()
    |> DonutMaze.parse
    |> dijkstra
    |> should equal 58