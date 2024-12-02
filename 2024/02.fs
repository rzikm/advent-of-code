module AoC202402

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.grid (pint32 .>> opt (pchar ' '))

let isSafe dampening row =
    let inRange diff = abs diff >= 1 && abs diff <= 3

    let rec f row delta dampening =
        match row with
        | left :: right :: rest ->
            let diff = right - left

            // happy path - the difference is within the range, delta is 0 only on the first iteration
            if diff * delta >= 0 && inRange diff && f (right :: rest) diff dampening then
                true

            else if dampening > 0 then
                // skip `right`
                f (left :: rest) delta (dampening - 1)
                // skip `left`
                || match delta with
                   // very first iteration, we can safely skip it
                   | 0 -> f (right :: rest) 0 (dampening - 1)
                   | _ ->
                       // here we need to check the difference against the previously
                       // discarded head: since `delta = left - head` and `diff = right - left`, we get
                       // `diff = right - head` as if `left` was never there
                       let newDiff = diff + delta

                       (newDiff * delta > 0 // check same direction as before
                        && inRange newDiff // check range
                        && f (right :: rest) newDiff (dampening - 1))
            else
                false
        | _ -> true

    f (List.ofArray row) 0 dampening

let solve dampening input =
    Array.filter (isSafe dampening) input |> Array.length

let solve1 = solve 0
let solve2 = solve 1

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "7 6 4 2 1"
           "1 2 7 8 9"
           "9 7 6 2 1"
           "1 3 2 4 5"
           "8 6 4 4 1"
           "1 3 6 7 9" |]


    [<Theory>]
    [<InlineData("7 6 4 2 1", 0, true)>]
    [<InlineData("1 3 6 7 9", 0, true)>]
    [<InlineData("1 3 2 4 5", 1, true)>]
    [<InlineData("8 6 4 4 1", 1, true)>]
    [<InlineData("1 2 7 8 9", 1, false)>]
    [<InlineData("88 86 88 89 90 93 95", 1, true)>]
    [<InlineData("5 9 11 12 13 16 18 21", 1, true)>]
    [<InlineData("25 24 23 22 21 24 26", 1, false)>]
    let ``Is Safe`` data damp res =
        let data = String.split [ " " ] data |> Seq.map int32 |> Array.ofSeq
        isSafe damp data |> should equal res

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 2

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 4
