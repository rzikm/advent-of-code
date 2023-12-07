module AoC202307

open AdventOfCode
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens
open FParsec

let parser =
    let hand = parray 5 (anyOf "23456789TJQKA") |>> List.ofArray .>> spaces .>>. pint32
    sepEndBy1 hand (skipChar '\n')

let cardToStrengthEasy =
    function
    | c when isDigit c -> int c - int '0'
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "invalid card"

let cardToStrengthHard =
    function
    | 'J' -> 1
    | c -> cardToStrengthEasy c

let handToStrengthEasy cards =
    match List.countBy id cards |> List.sortByDescending snd |> List.map snd with
    | [ 5 ] -> 7
    | [ 4; 1 ] -> 6
    | [ 3; 2 ] -> 5
    | [ 3; 1; 1 ] -> 4
    | [ 2; 2; 1 ] -> 3
    | [ 2; 1; 1; 1 ] -> 2
    | [ 1; 1; 1; 1; 1 ] -> 1
    | _ -> failwith "invalid hand"

let handToStrengthHard cards =
    // replace jokers with most frequent card
    let mostFrequent =
        cards
        |> List.filter ((<>) 1)
        |> List.countBy id
        |> List.sortByDescending snd
        |> List.map fst
        |> List.tryHead
        |> Option.defaultValue 1

    cards |> setl (_all 1) mostFrequent |> handToStrengthEasy

let compareHands strengthFun cards1 cards2 =
    let strength1 = strengthFun cards1
    let strength2 = strengthFun cards2

    if strength1 <> strength2 then
        compare strength1 strength2
    else
        compare cards1 cards2

let solve cardToStr handToStr input =
    input
    |> List.map (over (_1 << List.traverse) cardToStr)
    |> List.sortWith (fun l r -> compareHands handToStr (fst l) (fst r))
    |> List.indexed
    |> List.sumBy (fun (rankMinus1, (_, bid)) -> (rankMinus1 + 1) * bid)

let solve1 = solve cardToStrengthEasy handToStrengthEasy
let solve2 = solve cardToStrengthHard handToStrengthHard

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
    [<InlineData(12, 12, 12, 1, 14, 6)>]
    let ``Example part 2 - calc rank`` c1 c2 c3 c4 c5 strength =
        [ c1; c2; c3; c4; c5 ] |> handToStrengthHard |> should equal strength

    [<Theory>]
    [<InlineData("JKKK2", "QQQQ2")>]
    let ``Part2 - Compare hands joker rule`` less great =
        let toCards = List.ofSeq >> List.map cardToStrengthHard
        compareHands handToStrengthHard (toCards less) (toCards great) |> should equal -1
