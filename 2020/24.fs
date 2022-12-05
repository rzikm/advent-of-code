module AoC202024

open Utils
open AdventOfCode
open FSharpPlus
open FParsec

type Direction =
    | East
    | Southeast
    | Southwest
    | West
    | Northwest
    | Northeast

let parser =
    let dir =
        choice [ stringReturn "e" East
                 stringReturn "se" Southeast
                 stringReturn "sw" Southwest
                 stringReturn "w" West
                 stringReturn "nw" Northwest
                 stringReturn "ne" Northeast ]

    sepEndBy1 (many1 dir) (skipChar '\n')

let tupleAdd (l1: int32, l2: int32) (r1: int32, r2: int32) = (l1 + r1, l2 + r2)

let dirToVector =
    function
    | East -> (2, 0)
    | Southeast -> (1, -1)
    | Southwest -> (-1, -1)
    | West -> (-2, 0)
    | Northwest -> (-1, 1)
    | Northeast -> (1, 1)

let solve1 input =
    input
    |> List.countBy (List.map dirToVector >> List.reduce tupleAdd)
    |> List.filter (fun (_, count) -> count % 2 = 1)
    |> List.length

let neighbors6 tile =
    [ East; Southeast; Southwest; West; Northwest; Northeast ] |> List.map (dirToVector >> tupleAdd tile)

let solve2 iters input =
    let iter blackTiles =
        let toRemove =
            blackTiles
            |> Set.filter (fun tile ->
                let blackNeighbors =
                    neighbors6 tile |> Set.ofList |> Set.intersect blackTiles |> Set.count

                blackNeighbors = 0 || blackNeighbors > 2)

        let toAdd =
            let whiteNeighbors =
                blackTiles |> List.ofSeq |> List.map (neighbors6 >> Set.ofList) |> Set.unionMany

            Set.difference whiteNeighbors blackTiles
            |> Set.filter (fun tile ->
                let blackNeighbors =
                    neighbors6 tile |> Set.ofList |> Set.intersect blackTiles |> Set.count

                blackNeighbors = 2)

        toRemove |> Set.difference blackTiles |> Set.union toAdd

    let blackTiles =
        input
        |> List.countBy (List.map dirToVector >> List.reduce tupleAdd)
        |> List.filter (fun (_, count) -> count % 2 = 1)
        |> List.map fst
        |> Set.ofList

    applyN iter iters blackTiles |> Set.count

let solution = makeSolution parser solve1 (solve2 100)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "sesenwnenenewseeswwswswwnenewsewsw"
           "neeenesenwnwwswnenewnwwsewnenwseswesw"
           "seswneswswsenwwnwse"
           "nwnwneseeswswnenewneswwnewseswneseene"
           "swweswneswnenwsewnwneneseenw"
           "eesenwseswswnenwswnwnwsewwnwsene"
           "sewnenenenesenwsewnenwwwse"
           "wenwwweseeeweswwwnwwe"
           "wsweesenenewnwwnwsenewsenwwsesesenwne"
           "neeswseenwwswnwswswnw"
           "nenwswwsewswnenenewsenwsenwnesesenew"
           "enewnwewneswsewnwswenweswnenwsenwsw"
           "sweneswneswneneenwnewenewwneswswnese"
           "swwesenesewenwneswnwwneseswwne"
           "enesenwswwswneneswsenwnewswseenwsese"
           "wnwnesenesenenwwnenwsewesewsesesew"
           "nenewswnwewswnenesenwnesewesw"
           "eneswnwswnwsenenwnwnwwseeswneewsenese"
           "neswnwewnwnwseenwseesewsenwsweewe"
           "wseweeenwnesenwwwswnew"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 10

    [<Theory>]
    [<InlineData(0, 10)>]
    [<InlineData(1, 15)>]
    let ``Simple part 1`` iters expected =
        parseTestInput parser input |> solve2 iters |> should equal expected

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 2208
