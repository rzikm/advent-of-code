module AoC202010

open AdventOfCode
open FSharpPlus
open FParsec

let parser = many1 (pint32 .>> skipRestOfLine true)

let solve input = 0

let easy input =
    let counts =
        [ 0; List.max (input) + 3 ] @ input |> List.sort |> Seq.pairwise |> Seq.countBy (fun (a, b) -> b - a)

    (counts |> find (fst >> (=) 1) |> snd) * ((counts |> find (fst >> (=) 3) |> snd))

let hard input =
    let max = List.max input
    let nums = Set.ofList input

    let f frec i =
        if i = 0 then
            1L
        else if Set.contains i nums then
            [ 1; 2; 3 ] |> List.sumBy (fun delta -> frec (i - delta))
        else
            0L

    Utils.memoizerec f max

let solution = makeSolution () parser easy hard

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "28"
           "33"
           "18"
           "42"
           "31"
           "14"
           "46"
           "20"
           "48"
           "47"
           "24"
           "23"
           "49"
           "45"
           "19"
           "38"
           "39"
           "11"
           "1"
           "32"
           "25"
           "35"
           "8"
           "17"
           "7"
           "9"
           "4"
           "2"
           "34"
           "10"
           "3" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal (22 * 10)

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 19208L
