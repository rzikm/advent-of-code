module AoC202405

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let prules = ParseUtils.lines (pint32 .>> pchar '|' .>>. pint32)
    let pageLists = ParseUtils.lines (sepBy1 pint32 (pchar ','))

    prules .>> pchar '\n' .>>. pageLists

let orderByRules rules =
    let ruleSet = Set.ofList rules

    List.sortWith (fun a b ->
        if Set.contains (a, b) ruleSet then -1
        else if Set.contains (b, a) ruleSet then 1
        else 0)

let getScore (pages: 'a list) = pages.[(List.length pages / 2)]

let solve1 (rules, lists) =
    let sort = orderByRules rules
    // lists |> List.filter (isCorrectlyOrdered rules) |> List.sumBy getScore
    lists |> List.sumBy (fun pages -> if (sort pages) = pages then getScore pages else 0)

let solve2 (rules, lists) =
    let sort = orderByRules rules

    lists
    |> List.sumBy (fun pages ->
        let sorted = sort pages
        if sorted = pages then 0 else getScore sorted)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "47|53"
           "97|13"
           "97|61"
           "97|47"
           "75|29"
           "61|13"
           "75|53"
           "29|13"
           "97|29"
           "53|29"
           "61|53"
           "97|53"
           "61|29"
           "47|13"
           "75|47"
           "97|75"
           "47|61"
           "75|61"
           "47|29"
           "75|13"
           "53|13"
           ""
           "75,47,61,53,29"
           "97,61,53,29,13"
           "75,29,13"
           "75,97,47,61,53"
           "61,13,29"
           "97,13,75,29,47" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 143

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 123
