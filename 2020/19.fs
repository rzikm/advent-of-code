module AoC202019

open AdventOfCode
open FSharpPlus
open FParsec

type Rule =
    | Char of char
    | SubRules of int list list

let parser =
    let pcharrule = skipChar '"' >>. anyChar .>> skipChar '"' |>> Char

    let psubRules =
        sepBy1 (sepEndBy1 pint32 (skipChar ' ')) (skipString "| ") |>> SubRules

    let pRule =
        pint32 .>> skipString ": " .>>. choice [ pcharrule; psubRules ] .>> skipChar '\n'

    let prules = many1Till pRule (skipChar '\n') |>> Map.ofList
    let messages = sepEndBy1 (many1 (satisfy isLetter)) (skipChar '\n')

    prules .>>. messages

let isMatch rules message =
    let rec doMatch parts rule =
        match parts with
        | [] -> []
        | _ ->
            match rule with
            | Char c ->
                parts
                |> List.choose (fun part ->
                    match part with
                    | cc :: rest when cc = c -> Some rest
                    | _ -> None)
            | SubRules subrules ->
                subrules
                |> List.collect (fun sr -> sr |> List.map (fun i -> Map.find i rules) |> List.fold doMatch parts)

    doMatch [ message ] (Map.find 0 rules) |> List.contains []

let solve1 (rules, messages) =
    messages |> List.filter (isMatch rules) |> List.length

let solve2 (rules, messages) =
    let rules = Map.add 8 (SubRules [ [ 42 ]; [ 42; 8 ] ]) rules
    let rules = Map.add 11 (SubRules [ [ 42; 31 ]; [ 42; 11; 31 ] ]) rules
    messages |> List.filter (isMatch rules) |> List.length

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "0: 4 1 5"
           "1: 2 3 | 3 2"
           "2: 4 4 | 5 5"
           "3: 4 5 | 5 4"
           "4: \"a\""
           "5: \"b\""
           ""
           "ababbb"
           "bababa"
           "abbbab"
           "aaabbb"
           "aaaabbb" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 2
