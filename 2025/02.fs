module AoC202502

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let prange = pint64 .>> pstring "-" .>>. pint64
    sepBy1 prange (pstring ",")

let listInvalidIds getDivisions (a, b) =
    seq { a..b }
    |> Seq.filter (fun i ->
        let digits = string i

        getDivisions digits
        |> Seq.exists (fun d ->
            if digits.Length % d <> 0 then
                false
            else
                let groups =
                    digits |> Seq.chunkBySize (digits.Length / d) |> Seq.map (fun g -> System.String(g))

                groups |> Seq.distinct |> Seq.length |> (=) 1))

let solve1 input =
    input |> List.sumBy (listInvalidIds (Utils.konst [ 2 ]) >> sum)

let solve2 input =
    input |> List.sumBy (listInvalidIds (fun digits -> [ 2 .. digits.Length ]) >> sum)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124" |]

    [<Theory>]
    [<InlineData(11L, 22L, "11,22")>]
    [<InlineData(95L, 115L, "99")>]
    [<InlineData(998L, 1012L, "1010")>]
    [<InlineData(1188511880L, 1188511890L, "1188511885")>]
    [<InlineData(222220L, 222224L, "222222")>]
    [<InlineData(1698522L, 1698528L, "")>]
    [<InlineData(446443L, 446449L, "446446")>]
    [<InlineData(38593856L, 38593862L, "38593859")>]
    [<InlineData(565653L, 565659L, "")>]
    [<InlineData(824824821L, 824824827L, "")>]
    [<InlineData(2121212118L, 2121212124L, "")>]
    let ``Example part 1 - individual ranges`` a b expected =
        let expected =
            if String.length expected > 0 then
                String.split [ "," ] expected |> Seq.map int64 |> Seq.toList
            else
                []

        listInvalidIds (Utils.konst [ 2 ]) (a, b) |> Seq.toList |> should equal expected

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 1227775554L


    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 4174379265L

    [<Theory>]
    [<InlineData(11L, 22L, "11,22")>]
    [<InlineData(95L, 115L, "99,111")>]
    [<InlineData(998L, 1012L, "999,1010")>]
    [<InlineData(1188511880L, 1188511890L, "1188511885")>]
    [<InlineData(222220L, 222224L, "222222")>]
    [<InlineData(1698522L, 1698528L, "")>]
    [<InlineData(446443L, 446449L, "446446")>]
    [<InlineData(38593856L, 38593862L, "38593859")>]
    [<InlineData(565653L, 565659L, "565656")>]
    [<InlineData(824824821L, 824824827L, "824824824")>]
    [<InlineData(2121212118L, 2121212124L, "2121212121")>]
    let ``Example part 2 - individual ranges`` a b expected =
        let expected =
            if String.length expected > 0 then
                String.split [ "," ] expected |> Seq.map int64 |> Seq.toList
            else
                []

        listInvalidIds (fun digits -> [ 2 .. digits.Length ]) (a, b) |> Seq.toList |> should equal expected
