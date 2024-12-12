module AoC202412

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.grid (satisfy isAsciiLetter)

let getRegions grid =
    let fneighbors p =
        let c = Array.tryItem2dp p grid
        Tuple2.neighbors4 p |> filter (flip Array.tryItem2dp grid >> (=) c) |> map (fun p -> p, 1)

    Array.allIndexes2d grid
    |> Seq.fold
        (fun (acc, used) p ->
            match Set.contains p used with
            | true -> acc, used
            | false ->
                let region = Graph.flood fneighbors p |> Seq.map fst |> List.ofSeq
                (region :: acc, Set.union used <| Set.ofList region))
        ([], Set.empty)
    |> fst

let solve getPerimeter input =
    getRegions input
    |> List.sumBy (fun region ->
        let perimeter = getPerimeter region
        let area = List.length region
        perimeter * area)

let getPerimeter1 region =
    region
    |> fold
        (fun (acc, used) p ->
            let neighbors = Tuple2.neighbors4 p |> Set.ofSeq |> Set.intersect used |> Set.count
            (acc + 4 - neighbors * 2, Set.add p used))
        (0, Set.empty)
    |> fst

let getPerimeter2 region =
    region
    |> sortBy (Tuple2.swap) // left-to-right and top-to-bottom
    |> fold
        (fun (acc, used) p ->
            let delta =
                // left, up-left, up, up-right
                match [ (-1, 0); (-1, -1); (0, -1); (1, -1) ] |> map (Tuple2.add p >> flip Set.contains used) with
                // new region
                | [ false; _; false; _ ] -> 4
                // extending a line
                | [ false; false; true; false ]
                | [ true; false; false; _ ] -> 0
                // adding at a corner
                | [ true; true; false; _ ]
                | [ false; false; true; true ]
                | [ false; true; true; false ] -> 2
                // filling a corner
                | [ true; _; true; false ] -> -2
                // attaching to a line
                | [ false; true; true; true ] -> 4
                // filling a corner with lip
                | [ true; _; true; true ] -> 0
                | _ -> failwith "Unreachable"

            (acc + delta, Set.add p used))
        (0, Set.empty)
    |> fst

let solve1 = solve getPerimeter1
let solve2 = solve getPerimeter2

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "RRRRiiccFF"
           "RRRRiicccF"
           "VVRRRccFFF"
           "VVRcccJFFF"
           "VVVVcJJCFE"
           "VVIVccJJEE"
           "VVIIIcJJEE"
           "MIIIIIJJEE"
           "MIIISIJEEE"
           "MMMISSJEEE" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 1930

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 1206

    [<Theory>]
    [<InlineData('R', 10)>]
    [<InlineData('i', 4)>]
    [<InlineData('c', 22)>]
    [<InlineData('F', 12)>]
    [<InlineData('V', 10)>]
    [<InlineData('J', 12)>]
    [<InlineData('C', 4)>]
    [<InlineData('E', 8)>]
    [<InlineData('I', 16)>]
    [<InlineData('M', 6)>]
    [<InlineData('S', 6)>]
    let ``Example part 2 - individual regions perimeters`` c perimeter =
        let input = parseTestInput parser input

        input
        |> getRegions
        |> List.find (fun region -> Array.item2dp (region.Head) input = c)
        |> getPerimeter2
        |> should equal perimeter
