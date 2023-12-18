module AoC202304

open AdventOfCode
open FSharpPlus
open FParsec

type Card = { id: int; numbers: int list; winning: int list }

let parser =
    let n = attempt (spaces >>. pint32)
    let number = skipString "Card " >>. n .>> skipString ": "
    let winList = many1 n
    let numList = skipString " | " >>. many1 n

    let card =
        pipe3 number winList numList <| fun id numbers winning -> { id = id; numbers = numbers; winning = winning }

    sepEndBy1 card (pchar '\n')

let getCardMatchCount card =
    card.numbers |> List.filter (flip List.contains card.winning) |> List.length

let solve1 input =
    input |> List.map getCardMatchCount |> List.sumBy (fun n -> if n > 0 then 1 <<< (n - 1) else 0)

let solve2 input =
    let wins = input |> List.map getCardMatchCount |> Array.ofList
    let copiesCounts = Array.init wins.Length (fun i -> 1)

    for i = 0 to wins.Length - 1 do
        for w = i + 1 to min (i + wins[i]) (wins.Length - 1) do
            copiesCounts.[w] <- copiesCounts.[w] + copiesCounts.[i]

    copiesCounts |> Array.sum

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
           "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
           "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
           "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
           "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
           "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
           "" |]

    [<Fact>]
    let ``Example part 1 - card 1`` () =
        solve1 [ { id = 1
                   winning = [ 41; 48; 83; 86; 17 ]
                   numbers = [ 83; 86; 6; 31; 17; 9; 48; 53 ] } ]
        |> should equal 8

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 13

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 30
