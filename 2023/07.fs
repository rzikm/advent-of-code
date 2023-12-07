module AoC202307

open AdventOfCode
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens
open FParsec

let parser =
    let hand = parray 5 (anyOf "23456789TJQKA") |>> List.ofArray .>> spaces .>>. pint32
    sepEndBy1 hand (skipChar '\n')

let cardToStrength =
    function
    | c when isDigit c -> int c - int '0'
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "invalid card"

let cardToStrengthWithWildcard =
    function
    | 'J' -> 1
    | c -> cardToStrength c

let getCardCounts cards =
    List.countBy id cards |> List.sortByDescending snd |> List.map snd

let getCardCountsWithWildcard cards =
    // replace jokers with most frequent card
    let mostFrequent =
        cards
        |> List.filter ((<>) 1)
        |> List.countBy id
        |> List.sortByDescending snd
        |> List.map fst
        |> List.tryHead
        |> Option.defaultValue 1 // to cover the case with all joker cards

    cards |> setl (_all 1) mostFrequent |> getCardCounts

let compareHands cardCountsFun cards1 cards2 =
    let counts1 = cardCountsFun cards1
    let counts2 = cardCountsFun cards2

    if counts1 <> counts2 then
        // lexical ordering works here to determine a better hand
        compare counts1 counts2
    else
        compare cards1 cards2

let solve cardToStr handToStr input =
    input
    |> List.map (over (_1 << List.traverse) cardToStr)
    |> List.sortWith (fun l r -> compareHands handToStr (fst l) (fst r))
    |> List.indexed
    |> List.sumBy (fun (rankMinus1, (_, bid)) -> (rankMinus1 + 1) * bid)

let solve1 = solve cardToStrength getCardCounts
let solve2 = solve cardToStrengthWithWildcard getCardCountsWithWildcard

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483"; "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 6440

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 5905

    [<Theory>]
    [<InlineData("JKKK2", "QQQQ2")>]
    let ``Part2 - Compare hands joker rule`` less great =
        let toCards = List.ofSeq >> List.map cardToStrengthWithWildcard
        compareHands getCardCountsWithWildcard (toCards less) (toCards great) |> should equal -1
