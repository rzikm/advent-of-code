module AoC202401

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.lines (pint32 .>> spaces .>>. pint32)

let solve1 input =
    input |> List.unzip |> Tuple2.map List.sort |> uncurry List.zip |> List.sumBy (uncurry (-) >> abs)

let solve2 input =
    let (left, right) = input |> List.unzip
    let occ = right |> List.countBy id |> Map.ofList

    left |> List.sumBy (fun n -> Map.tryFind n occ |> Option.defaultValue 0 |> (fun x -> x * n))

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "3   4"; "4   3"; "2   5"; "1   3"; "3   9"; "3   3" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 11

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 31
