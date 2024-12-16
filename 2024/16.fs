module AoC202416

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pStart =
        charReturn 'S' false .>>. getPosition .>>. getUserState
        >>= fun ((b, pos), (_, epos)) -> setUserState ((int pos.Column - 2, int pos.Line - 1), epos) >>% b

    let pEnd =
        charReturn 'E' false .>>. getPosition .>>. getUserState
        >>= fun ((b, pos), (spos, _)) -> setUserState (spos, (int pos.Column - 2, int pos.Line - 1)) >>% b

    ParseUtils.grid (
        choice [ pStart
                 pEnd
                 charReturn '#' true
                 charReturn '.' false ]
    )
    .>>. getUserState


let fNeighbors grid (pos, dir) =
    seq {
        let forward = Tuple2.add pos dir
        let left = Tuple2.rotLeft dir
        let right = Tuple2.rotRight dir

        if Array.item2dp forward grid = false then
            yield (forward, dir), 1

        if Array.item2dp (Tuple2.add pos left) grid = false then
            yield (pos, left), 1000

        if Array.item2dp (Tuple2.add pos right) grid = false then
            yield (pos, right), 1000
    }

let fHeuristic e (pos, _) =
    let mannh = Tuple2.manhattanDist pos e
    let turn = (Tuple2.sub pos e |> Tuple2.reduce (*) |> sign |> abs) * 1000

    mannh + turn

let solve1 (grid, (s, e)) =
    let fNeighbors = Graph.withPathFolding (fst >> (<>) e) (fNeighbors grid)

    let _, cost =
        Graph.aStar (fHeuristic e) fNeighbors (fst >> (=) e) [ (s, (1, 0)) ] |> Option.get

    cost

let solve2 (grid, (s, e)) =
    let fNeighbors = fNeighbors grid

    let paths =
        Graph.aStarAllPaths (fHeuristic e) fNeighbors (fst >> (=) e) [ (s, (1, 0)) ]

    paths |> Seq.fold (fun acc (path, _) -> Set.union acc (Set.ofList path |> Set.map fst)) Set.empty |> Set.count

let solution = makeSolution ((0, 0), (0, 0)) parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "###############"
           "#.......#....E#"
           "#.#.###.#.###.#"
           "#.....#.#...#.#"
           "#.###.#####.#.#"
           "#.#.#.......#.#"
           "#.#.#####.###.#"
           "#...........#.#"
           "###.#.#####.#.#"
           "#...#.....#.#.#"
           "#.#.#.###.#.#.#"
           "#.....#...#.#.#"
           "#.###.#.#.#.#.#"
           "#S..#.....#...#"
           "###############" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 7036

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 45
