module AoC202505

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let range = pint64 .>> pchar '-' .>>. pint64 |>> Tuple2.mapItem2 ((+) 1L)
    sepEndBy1 range (pchar '\n') .>> pchar '\n' .>>. ParseUtils.lines pint64

let solve1 (freshRanges, ids) =
    let ranges = Range.unionMany freshRanges
    ids |> Seq.filter (fun id -> Seq.exists (Range.contains id) ranges) |> Seq.length

let solve2 (input, _) =
    Range.unionMany input |> Seq.sumBy Range.length

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "3-5"; "10-14"; "16-20"; "12-18"; ""; "1"; "5"; "8"; "11"; "17"; "32" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 3

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 14L
