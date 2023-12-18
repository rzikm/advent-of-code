module AoC202318

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pDir =
        choice [ charReturn 'U' (0L, -1L)
                 charReturn 'D' (0L, 1L)
                 charReturn 'L' (-1L, 0L)
                 charReturn 'R' (1L, 0L) ]

    let color = between (skipString "(#") (skipChar ')') (many1Satisfy isHex)

    ParseUtils.lines (tuple3 (pDir .>> spaces) (pint64 .>> spaces) color)

let getPoints input =
    List.scan (fun s (d, c, _) -> Tuple2.add s (Tuple2.smul c d)) (0L, 0L) input

let solve1 input =
    getPoints input |> Geometry.axisAlignedPolygonArea

let solve2 input =
    input
    |> List.map (fun (_, _, color) ->
        let count = String.take 5 color |> Utils.parseInt 16 |> int64

        let dir =
            match String.item 5 color with
            | '0' -> (1L, 0L)
            | '1' -> (0L, 1L)
            | '2' -> (-1L, 0L)
            | '3' -> (0L, -1L)
            | _ -> failwith "Invalid direction"

        (dir, count, ""))
    |> solve1

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "R 6 (#70c710)"
           "D 5 (#0dc571)"
           "L 2 (#5713f0)"
           "D 2 (#d2c081)"
           "R 2 (#59c680)"
           "D 2 (#411b91)"
           "L 5 (#8ceee2)"
           "U 2 (#caa173)"
           "L 1 (#1b58a2)"
           "U 2 (#caa171)"
           "R 2 (#7807d2)"
           "U 3 (#a77fa3)"
           "L 2 (#015232)"
           "U 2 (#7a21e3)" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 62L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 952408144115L
