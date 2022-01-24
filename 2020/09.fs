module AoC202009

open AdventOfCode
open FSharpPlus
open FParsec

let parser = many1 (pint64 .>> skipRestOfLine true)

let solve input = 0

let easy len (input: int64 list) =
    let preamble = take len input
    let rest = List.skip len input

    let rec f preamble input =
        match input with
        | [] -> failwith "Not found"
        | i :: rest ->
            match
                Utils.subsets 2 preamble |> Seq.map List.sum
                |> tryFind ((=) i)
                with
            | Some _ -> f (List.skip 1 preamble @ [ i ]) rest
            | None -> i

    f preamble rest

let hard len (input: int64 list) =
    let n = easy len input
    let input = input |> Array.ofList

    let rec f i0 i1 sum =
        if sum < n then
            f i0 (i1 + 1) (sum + Array.item (i1 + 1) input)
        else if sum > n then
            f (i0 + 1) i1 (sum - Array.item i0 input)
        else
            let nums = input[i0..i1]
            Array.min nums + Array.max nums

    f 0 0 (Array.item 0 input)

let makeSolution len =
    makeSolution parser (easy len) (hard len)

let solution = makeSolution 25

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input5 =
        [| "35"
           "20"
           "15"
           "25"
           "47"
           "40"
           "62"
           "55"
           "65"
           "95"
           "102"
           "117"
           "150"
           "182"
           "127"
           "219"
           "299"
           "277"
           "309"
           "576" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 (makeSolution 5) input5
        |> should equal 127L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 (makeSolution 5) input5
        |> should equal 62L
