module AoC202006

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let nonwhite = satisfy (System.Char.IsWhiteSpace >> not)

    let pline = many1 nonwhite |>> Set.ofList
    let pgroup = sepEndBy1 pline (pchar '\n')
    sepEndBy1 pgroup (pchar '\n')

let solve combiner input =
    input |> Seq.map (combiner >> Seq.length) |> Seq.sum

let solution =
    makeSolution () parser (solve Set.unionMany) (solve Set.intersectMany)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "abc"; ""; "a"; "b"; "c"; ""; "ab"; "ac"; ""; "a"; "a"; "a"; "a"; ""; "b" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 11

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 6
