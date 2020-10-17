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

    map.[Point.fromTuple (9, 1)] |> should equal (OutsidePortal <| PortalPointer.create (9, 2) "AA")


[<Fact>]
let ``DonutMaze.parse finds left side portals`` () =
    let map = DonutMaze.parse <| sample ()

    map.[Point.fromTuple (1, 8)] |> should equal (OutsidePortal <| PortalPointer.create (2, 8) "BC")
    map.[Point.fromTuple (1, 13)] |> should equal (OutsidePortal <| PortalPointer.create (2, 13) "DE")
    map.[Point.fromTuple (1, 15)] |> should equal (OutsidePortal <| PortalPointer.create (2, 15) "FG")

[<Fact>]
let ``DonutMaze.parse finds the bottom side portals`` () =
    let map = DonutMaze.parse <| sample ()

    map.[Point.fromTuple (13, 17)] |> should equal (OutsidePortal <| PortalPointer.create (13, 16) "ZZ")

[<Fact>]
let ``DonutMaze.findDimensions gets the dimensions`` () =
    let data = splitLines <| sample ()
    let result = DonutMaze.findDimensions data

    result |> should equal (5, 5)

[<Fact>]
let ``DonutMaze.parse finds inside portals`` () =
    let map = DonutMaze.parse <| sample ()

    map.[Point.fromTuple (9, 7)] |> should equal (InsidePortal <| PortalPointer.create (9, 6) "BC")
    map.[Point.fromTuple (7, 10)] |> should equal (InsidePortal <| PortalPointer.create (6, 10) "DE")
    map.[Point.fromTuple (11, 11)] |> should equal (InsidePortal <| PortalPointer.create (11, 12) "FG")

[<Fact>]
let ``dijkstra finds 23 steps for the sample`` () =
    let map = DonutMaze.parse <| sample ()

    part1 map |> should equal 23

[<Fact>]
let ``dijkstra finds 58 for the large sample`` () =
    largerSample ()
    |> DonutMaze.parse
    |> part1
    |> should equal 58

let largeExample =
    [
        "             Z L X W       C                 ";
        "             Z P Q B       K                 ";
        "  ###########.#.#.#.#######.###############  ";
        "  #...#.......#.#.......#.#.......#.#.#...#  ";
        "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  ";
        "  #.#...#.#.#...#.#.#...#...#...#.#.......#  ";
        "  #.###.#######.###.###.#.###.###.#.#######  ";
        "  #...#.......#.#...#...#.............#...#  ";
        "  #.#########.#######.#.#######.#######.###  ";
        "  #...#.#    F       R I       Z    #.#.#.#  ";
        "  #.###.#    D       E C       H    #.#.#.#  ";
        "  #.#...#                           #...#.#  ";
        "  #.###.#                           #.###.#  ";
        "  #.#....OA                       WB..#.#..ZH";
        "  #.###.#                           #.#.#.#  ";
        "CJ......#                           #.....#  ";
        "  #######                           #######  ";
        "  #.#....CK                         #......IC";
        "  #.###.#                           #.###.#  ";
        "  #.....#                           #...#.#  ";
        "  ###.###                           #.#.#.#  ";
        "XF....#.#                         RF..#.#.#  ";
        "  #####.#                           #######  ";
        "  #......CJ                       NM..#...#  ";
        "  ###.#.#                           #.###.#  ";
        "RE....#.#                           #......RF";
        "  ###.###        X   X       L      #.#.#.#  ";
        "  #.....#        F   Q       P      #.#.#.#  ";
        "  ###.###########.###.#######.#########.###  ";
        "  #.....#...#.....#.......#...#.....#.#...#  ";
        "  #####.#.###.#######.#######.###.###.#.#.#  ";
        "  #.......#.......#.#.#.#.#...#...#...#.#.#  ";
        "  #####.###.#####.#.#.#.#.###.###.#.###.###  ";
        "  #.......#.....#.#...#...............#...#  ";
        "  #############.#.#.###.###################  ";
        "               A O F   N                     ";
        "               A A D   M                     ";
    ]
    |> String.concat "\n"

[<Fact>]
let ``dijkstra finds 396 for the large sample`` () =
    largerSample ()
    |> DonutMaze.parse
    |> part2
    |> should equal 396