module AoC202422

open AdventOfCode
open FParsec
open Utils

let parser = ParseUtils.lines pint32

let evolve n =
    let n = n ^^^ (n <<< 6) &&& ((1 <<< 24) - 1)
    let n = n ^^^ (n >>> 5) &&& ((1 <<< 24) - 1)
    n ^^^ (n <<< 11) &&& ((1 <<< 24) - 1)

let solve1 input =
    input |> List.map (evolve ^ 2000) |> List.sumBy int64

let solve2 input =
    input
    |> Seq.collect (fun n ->
        Seq.unfold (fun n -> Some((n % 10), evolve n)) n
        |> Seq.pairwise
        |> Seq.map (fun (l, r) -> (r, r - l))
        |> Seq.scan
            (fun (_, hash) (price, delta) ->
                let hash = (hash * 20 + (delta + 10)) % (20 * 20 * 20 * 20)
                (price, hash))
            (0, 0)
        |> Seq.take 2000
        |> Seq.skip 4
        |> Seq.fold
            (fun m (price, pattern) ->
                match Map.containsKey pattern m with
                | true -> m
                | false -> Map.add pattern price m)
            Map.empty
        |> Map.toSeq)
    |> Seq.groupBy fst
    |> Seq.map (fun (_, l) -> l |> Seq.sumBy snd)
    |> Seq.max

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "1"; "10"; "100"; "2024" |]
    let input2 = [| "1"; "2"; "3"; "2024" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 37327623L

    [<Theory>]
    [<InlineData(123, 15887950)>]
    [<InlineData(15887950, 16495136)>]
    [<InlineData(16495136, 527345)>]
    let ```Simple evolve`` n expected = evolve n |> should equal expected

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input2 |> should equal 23
