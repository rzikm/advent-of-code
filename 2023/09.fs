module AoC202309

open AdventOfCode
open FSharpPlus
open FParsec

let parser = sepEndBy1 (sepBy1 pint32 (skipChar ' ')) (skipChar '\n')

let computeNextInSeq =
    List.unfold (fun s ->
        List.tryFind ((<>) 0) s |> Option.map (fun _ -> List.last s, List.pairwise s |> List.map (fun (a, b) -> b - a)))
    >> List.sum

let solve1 input = input |> List.sumBy computeNextInSeq

let solve2 input =
    input |> List.sumBy (List.rev >> computeNextInSeq)

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
