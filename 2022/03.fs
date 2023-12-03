module AoC202203

open AdventOfCode
open FSharpPlus
open FParsec

let parser = sepEndBy (many1 (satisfy isLetter) |>> Array.ofList) spaces

let getPriority =
    function
    | x when isLower x -> int x - int 'a' + 1
    | x -> int x - int 'A' + 27

let solve1 input =
    input
    |> List.map (fun bp ->
        let l = Array.length bp / 2
        let left = Array.take l bp |> Set.ofArray
        let right = Array.skip l bp |> Set.ofArray
        Set.intersect left right)
    |> List.sumBy (Set.toSeq >> Seq.sumBy getPriority)

let solve2 input =
    input
    |> List.chunkBySize 3
    |> List.sumBy (List.map Set.ofArray >> Set.intersectMany >> Set.toSeq >> Seq.sumBy getPriority)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "vJrwpWtwJgWrhcsFMMfFFhFp"
           "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
           "PmmdzqPrVvPwwTWBwg"
           "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
           "ttgJtRGJQctTZtZT"
           "CrZsJsPPZsGzwwsLwLmpwMDw" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 157

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 70
