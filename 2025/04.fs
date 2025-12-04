module AoC202504

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    ParseUtils.grid (
        choice [ pchar '@' >>% true
                 pchar '.' >>% false ]
    )

let getRemovableIndices input =
    Array.allIndexes2d input
    |> Seq.filter (fun pos -> Array.item2dp pos input && Array.neighbors2d8p pos input |> Seq.count id < 4)

let solve1 input = getRemovableIndices input |> length

let solve2 input =
    let rec loop acc input =
        let indicesToRemove = getRemovableIndices input |> List.ofSeq

        if List.isEmpty indicesToRemove then
            acc
        else
            let removed =
                indicesToRemove |> Seq.fold (fun arr pos -> Array.mapAt2dp pos (fun _ -> false) arr) input

            loop (acc + List.length indicesToRemove) removed

    loop 0 input

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "..@@.@@@@."
           "@@@.@.@.@@"
           "@@@@@.@.@@"
           "@.@@@@..@."
           "@@.@@@@.@@"
           ".@@@@@@@.@"
           ".@.@.@.@@@"
           "@.@@@.@@@@"
           ".@@@@@@@@."
           "@.@.@@@.@." |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 13

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 43
