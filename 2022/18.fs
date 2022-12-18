module AoC202218

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    sepEndBy1 (tuple3 (pint32 .>> pchar ',') (pint32 .>> pchar ',') pint32) (pchar '\n')

let neighbors (x, y, z) =
    seq {
        yield (x + 1, y, z)
        yield (x - 1, y, z)
        yield (x, y + 1, z)
        yield (x, y - 1, z)
        yield (x, y, z + 1)
        yield (x, y, z - 1)
    }

let dist (x1, y1, z1) (x2, y2, z2) =
    abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

let solve1 input =
    let allCubes = Set.ofList input

    input |> Seq.ofList |> Seq.collect (neighbors >> Seq.filter (flip Set.contains allCubes >> not)) |> Seq.length

let solve2 input =
    let allCubes = Set.ofList input
    let first = List.head input

    let (x0, y0, z0), (x1, y1, z1) =
        allCubes
        |> fold
            (fun ((x0, y0, z0), (x1, y1, z1)) (x, y, z) ->
                (min x0 x, min y0 y, min z0 z), (max x1 x, max y1 y, max z1 z))
            (first, first)

    let fNeighbors =
        neighbors
        >> Seq.choose (fun (x, y, z) ->
            if Set.contains (x, y, z) allCubes then
                None
            else if x0 - 1 <= x && x <= x1 + 1 && y0 - 1 <= y && y <= y1 + 1 && z0 - 1 <= z && z <= z1 + 1 then
                Some((x, y, z), 1)
            else
                None)

    let fHeuristic = dist (0, 0, 0)

    input
    |> Seq.ofList
    |> Seq.collect (neighbors >> Seq.filter (flip Set.contains allCubes >> not))
    |> Seq.choose (fun p -> Graph.aStar fHeuristic fNeighbors ((=) (0, 0, 0)) [ p ])
    |> Seq.length

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "2,2,2"
           "1,2,2"
           "3,2,2"
           "2,1,2"
           "2,3,2"
           "2,2,1"
           "2,2,3"
           "2,2,4"
           "2,2,6"
           "1,2,5"
           "3,2,5"
           "2,1,5"
           "2,3,5"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 64

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 58
