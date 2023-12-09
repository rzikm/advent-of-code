module AoC202309

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let seq = sepBy1 pint32 (skipChar ' ')
    sepEndBy1 seq (skipChar '\n')

let computeNextInSeq seq =
    let unfolder seq =
        List.tryFind ((<>) 0) seq // continue until all zeros
        |> Option.map (fun _ -> List.last seq, List.pairwise seq |> List.map (fun (a, b) -> b - a))

    List.unfold unfolder seq |> List.rev |> List.scan (+) 0 |> List.last

let solve1 input =
    input |> List.map computeNextInSeq |> List.sum

let solve2 input = input |> List.map List.rev |> solve1

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "0 3 6 9 12 15"; "1 3 6 10 15 21"; "10 13 16 21 30 45"; "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 114

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 2
