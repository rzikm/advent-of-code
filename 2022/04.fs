module AoC202204

open AdventOfCode
open FSharpPlus
open FParsec

type Range = int32 * int32

let parser =
    let prange = pint32 .>> skipChar '-' .>>. pint32 |>> Range
    let ppair = prange .>> skipChar ',' .>>. prange
    sepEndBy1 ppair (skipChar '\n')

let rangeIntersect (ll, lr) (rl, rr) = (max ll rl), (min lr rr)

let fullyOverlap (l, r) =
    let i = rangeIntersect l r
    i = l || i = r

let anyOverlap (l, r) =
    let (il, ir) = rangeIntersect l r
    il <= ir

let solve filter input =
    input |> List.filter filter |> List.length

let solution = makeSolution parser (solve fullyOverlap) (solve anyOverlap)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "2-4,6-8"; "2-3,4-5"; "5-7,7-9"; "2-8,3-7"; "6-6,4-6"; "2-6,4-8" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 2

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 4
