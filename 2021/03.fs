module AoC202103

open System
open AdventOfCode
open FSharpPlus
open FParsec
open Utils

let parser = sepEndBy (many1Chars (pchar '0' <|> pchar '1')) spaces |>> Array.ofList

let solve1 input =
    let counts =
        let c = Array.zeroCreate (Array.head input |> String.length)

        let updateCounts line =
            line |> Seq.iteri (fun i ch -> c.[i] <- c.[i] + if ch = '1' then 1 else 0)

        input |> Array.iter updateCounts
        c

    let reversed = Array.rev counts

    let (gamma, epsilon, _) =
        reversed
        |> Array.fold
            (fun (gamma, epsilon, exp) count ->
                let mostCommon = if count > input.Length / 2 then 1 else 0
                (gamma + mostCommon * exp, epsilon + (1 - mostCommon) * exp, exp * 2))
            (0, 0, 1)

    gamma * epsilon

let solve2 input =
    let filterByChar (index: int) (data: string array) sortBy =
        Array.groupBy (fun (line: string) -> line.[index]) data
        |> Array.sortWith (fun (c0, a0) (c1, a1) -> sortBy * (compare (a0.Length, c0) (a1.Length, c1)))
        |> Array.head
        |> snd

    let filter (data: string array) minMax =
        let mutable myData = data
        let mutable index = 0

        while myData.Length > 1 do
            myData <- filterByChar index myData minMax
            index <- index + 1

        myData.[0]

    let oxygenRating = Convert.ToInt32((filter input 1), 2)
    let co2Rating = Convert.ToInt32((filter input -1), 2)

    oxygenRating * co2Rating

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "00100"
           "11110"
           "10110"
           "10111"
           "10101"
           "01111"
           "00111"
           "11100"
           "10000"
           "11001"
           "00010"
           "01010" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 198

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 230
