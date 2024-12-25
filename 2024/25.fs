module AoC202425

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let row = parray 5 (anyOf "#.")

    let plock =
        stringReturn "#####" true .>> skipNewline .>>. parray 6 (row .>> spaces)
        |>> Tuple2.mapItem2 (fun r ->
            Array.transpose r
            |> Array.map (String.ofArray >> String.takeWhile ((=) '#') >> String.length)
            |> List.ofArray)

    let pkey =
        stringReturn "....." false .>> skipNewline .>>. parray 6 (row .>> spaces)
        |>> Tuple2.mapItem2 (fun r ->
            Array.transpose r
            |> Array.map (String.ofArray >> String.rev >> String.takeWhile ((=) '#') >> String.length)
            |> List.ofArray)

    sepEndBy1 ((attempt plock) <|> (attempt pkey)) spaces

let solve1 input =
    let keys, locks = input |> List.partition fst

    let keys = keys |> List.map snd
    let locks = locks |> List.map snd

    keys
    |> List.sumBy (fun key ->
        locks |> List.count (fun lock -> List.zip key lock |> List.forall (fun (k, l) -> k + l <= 6)))

let solve2 _ = "*"

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "#####"
           ".####"
           ".####"
           ".####"
           ".#.#."
           ".#..."
           "....."
           ""
           "#####"
           "##.##"
           ".#.##"
           "...##"
           "...#."
           "...#."
           "....."
           ""
           "....."
           "#...."
           "#...."
           "#...#"
           "#.#.#"
           "#.###"
           "#####"
           ""
           "....."
           "....."
           "#.#.."
           "###.."
           "###.#"
           "###.#"
           "#####"
           ""
           "....."
           "....."
           "....."
           "#...."
           "#.#.."
           "#.#.#"
           "#####" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 3

//     [<Fact>]
//     let ``Example part 2`` () =
//         testPart2 solution input |> should equal 0
