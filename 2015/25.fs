module AoC201525

open AdventOfCode
open FSharpPlus
open FParsec

let parser = spaces

let solve1 input = 0

let solve2 input = 0

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 0

//     [<Fact>]
//     let ``Example part 2`` () =
//         testPart2 solution input |> should equal 0
