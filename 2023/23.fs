module AoC202323

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.grid (anyOf "#.^>v<")

let findLongestPath input fNeighbors =
    let start = input |> Array.head |> Array.findIndex (fun c -> c = '.'), 0

    let finish =
        input |> Array.last |> Array.findIndex (fun c -> c = '.'), Array.length (Array.last input) - 1

    Graph.findLongestPath (Graph.withPathFolding ((<>) finish) fNeighbors) ((=) finish) start |> snd

let solve1 input =
    let fNeighbors =
        Graph.Grid.makeFNeighbors input (fun (pos, src) (pos2, dst) ->
            let delta = Tuple2.sub pos2 pos

            match delta, src, dst with
            | _, _, '#' -> None
            | (1, 0), '>', _ -> Some(1)
            | (-1, 0), '<', _ -> Some(1)
            | (0, 1), 'v', _ -> Some(1)
            | (0, -1), '^', _ -> Some(1)
            | _, '.', _ -> Some(1)
            | _, _, _ -> None)

    findLongestPath input fNeighbors

let solve2 input =
    let fNeighbors =
        Graph.Grid.makeFNeighbors input (fun (pos, src) (pos2, dst) ->
            let delta = Tuple2.sub pos2 pos

            match delta, src, dst with
            | _, _, '#' -> None
            | _, '#', _ -> None
            | _, _, _ -> Some(1))

    findLongestPath input fNeighbors

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "#.#####################"
           "#.......#########...###"
           "#######.#########.#.###"
           "###.....#.>.>.###.#.###"
           "###v#####.#v#.###.#.###"
           "###.>...#.#.#.....#...#"
           "###v###.#.#.#########.#"
           "###...#.#.#.......#...#"
           "#####.#.#.#######.#.###"
           "#.....#.#.#.......#...#"
           "#.#####.#.#.#########v#"
           "#.#...#...#...###...>.#"
           "#.#.#v#######v###.###v#"
           "#...#.>.#...>.>.#.###.#"
           "#####v#.#.###v#.#.###.#"
           "#.....#...#...#.#.#...#"
           "#.#########.###.#.#.###"
           "#...###...#...#...#.###"
           "###.###.#.###v#####v###"
           "#...#...#.#.>.>.#.>.###"
           "#.###.###.#.###.#.#v###"
           "#.....###...###...#...#"
           "#####################.#"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 94

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 154
