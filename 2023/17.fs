module AoC202317

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.grid (satisfy isDigit |>> fun c -> int c - int '0')

let solve minMovement maxMovement (input: int32 array array) =
    let mkStart dir = (0, 0), dir, 0
    let goalPos = Tuple2.sub (Array.bounds2d input) (1, 1)
    let fHeuristic (p, _, _) = Tuple2.manhattanDist p goalPos
    let fFinish (p, _, _) = p = goalPos

    let fNeighbors (p, dir, count) =
        if count < minMovement then
            let newp = (Tuple2.add p dir)

            Array.tryItem2dp newp input
            |> Option.map (fun heat -> (newp, dir, count + 1), heat)
            |> Option.toList
            |> List.toSeq
        else
            Tuple2.neighbors4 p
            |> Seq.choose (fun p -> Array.tryItem2dp p input |> Option.map (Tuple2.create p))
            |> Seq.choose (fun (pp, heat) ->
                let delta = Tuple2.sub pp p
                let reverse = Tuple2.neg dir

                if delta = reverse then
                    None
                else if delta = dir then
                    if count = maxMovement then
                        None
                    else
                        Some((pp, dir, count + 1), heat)
                else
                    Some((pp, delta, 1), heat))

    Graph.aStar fHeuristic fNeighbors fFinish [ mkStart (0, 1); mkStart (1, 0) ] |> Option.get |> snd

let solve1 = solve 1 3

let solve2 = solve 4 10

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "2413432311323"
           "3215453535623"
           "3255245654254"
           "3446585845452"
           "4546657867536"
           "1438598798454"
           "4457876987766"
           "3637877979653"
           "4654967986887"
           "4564679986453"
           "1224686865563"
           "2546548887735"
           "4322674655533" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 102

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 94
