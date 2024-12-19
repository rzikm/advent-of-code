module AoC202419

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let word = many1Chars <| satisfy isAsciiLetter
    let available = sepBy1 word (skipString ", ")
    available .>> skipNewline .>> skipNewline .>>. ParseUtils.lines word

let makeCountCompositions words =
    Utils.memoizerec (fun frec w ->
        if w = "" then
            1L
        else
            words
            |> Seq.filter (fun prefix -> String.startsWith prefix w)
            |> Seq.sumBy (fun prefix -> frec (String.drop (String.length prefix) w)))

let solve1 (available, toSolve) =
    let getComposition = makeCountCompositions <| Set.ofList available
    toSolve |> List.count (getComposition >> flip (>) 0)

let solve2 (available, toSolve) =
    let getComposition = makeCountCompositions <| Set.ofList available
    toSolve |> List.sumBy getComposition

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "r, wr, b, g, bwu, rb, gb, br"
           ""
           "brwrr"
           "bggr"
           "gbbr"
           "rrbgbr"
           "ubwu"
           "bwurrg"
           "brgr"
           "bbrgwb" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 6

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 16L
