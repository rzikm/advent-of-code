module AoC202420

open AdventOfCode
open FSharpPlus
open FParsec

let parser = AoC202416.parser

let solve cheatLen threshold (grid, (s, e)) =
    let makeLookup from =
        let lookup =
            Graph.flood
                (Graph.Grid.makeFNeighbors grid (fun (_, f) (_, t) -> if not f && not t then Some 1 else None))
                from
            |> Map.ofSeq

        fun p -> Map.tryFind p lookup

    let distToStart = makeLookup s
    let distToEnd = makeLookup e

    let withoutCheat = distToEnd s |> Option.get
    let maxCost = withoutCheat - threshold

    Array.allIndexes2d grid
    |> Seq.filter (not << flip Array.item2dp grid)
    |> Seq.sumBy (fun cs ->
        let ss = (cheatLen, cheatLen)

        Tuple2.allIndexesInRange2d (Tuple2.sub cs ss) (Tuple2.add cs ss)
        |> Seq.filter (flip Array.tryItem2dp grid >> (=) (Some(false)))
        |> Seq.count (fun ce ->
            let delta = Tuple2.manhattanDist cs ce

            match distToStart cs, distToEnd ce with
            | Some distToStart, Some distToEnd -> delta <= cheatLen && distToStart + delta + distToEnd <= maxCost
            | _ -> false))

let solution = makeSolution ((0, 0), (0, 0)) parser (solve 2 100) (solve 20 100)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "###############"
           "#...#...#.....#"
           "#.#.#.#.#.###.#"
           "#S#...#.#.#...#"
           "#######.#.#.###"
           "#######.#.#...#"
           "#######.#.###.#"
           "###..E#...#...#"
           "###.#######.###"
           "#...###...#...#"
           "#.#####.#.###.#"
           "#.#...#.#.#...#"
           "#.#.#.#.#.#.###"
           "#...#...#...###"
           "###############" |]

    [<Theory>]
    [<InlineData(64, 1)>]
    [<InlineData(40, 2)>]
    [<InlineData(38, 3)>]
    [<InlineData(36, 4)>]
    [<InlineData(20, 5)>]
    [<InlineData(12, 8)>]
    let ``Example part 1`` treshold expected =
        parseTestInputWithState parser ((0, 0), (0, 0)) input |> solve 2 treshold |> should equal expected

    [<Theory>]
    [<InlineData(76, 3)>]
    [<InlineData(74, 7)>]
    [<InlineData(72, 29)>]
    [<InlineData(70, 41)>]
    let ``Example part 2`` treshold expected =
        parseTestInputWithState parser ((0, 0), (0, 0)) input |> solve 50 treshold |> should equal expected
