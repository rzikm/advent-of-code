module AoC202407

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pline = pint64 .>> skipChar ':' .>>. many1 (skipChar ' ' >>. pint64)
    ParseUtils.lines pline

let isSolveable operators (total: int64) (numbers: int64 list) =
    let rec f subtotal numbers =
        match numbers with
        | a :: rest -> operators |> List.exists (fun ff -> f (ff subtotal a) rest)
        | _ -> subtotal = total

    f (List.head numbers) (List.tail numbers)

let solve operators input =
    input |> List.filter (uncurry <| isSolveable operators) |> List.sumBy fst

let solve1 = solve [ (+); (*) ]
let solve2 = solve [ (+); (*); (fun l r -> (string l + string r) |> int64) ]

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "190: 10 19"
           "3267: 81 40 27"
           "83: 17 5"
           "156: 15 6"
           "7290: 6 8 6 15"
           "161011: 16 10 13"
           "192: 17 8 14"
           "21037: 9 7 18 13"
           "292: 11 6 16 20" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 3749L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 11387L
