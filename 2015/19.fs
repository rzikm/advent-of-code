module AoC201519

open AdventOfCode
open FSharpPlus
open FParsec

let parser = pint32

let solve input = 0

let solution = makeSolution () parser solve solve

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 0

// [<Fact>]
// let ``Example part 2`` () =
//     testPart2 solution input |> should equal 0
