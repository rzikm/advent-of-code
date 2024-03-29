module AoC202301

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    sepEndBy1 (manyChars <| satisfy (fun c -> isLetter c || isDigit c)) (pchar '\n')

let digits = [ 1..9 ] |> List.map (fun d -> string d, d)

let digitsAsWords =
    [ "one", 1
      "two", 2
      "three", 3
      "four", 4
      "five", 5
      "six", 6
      "seven", 7
      "eight", 8
      "nine", 9 ]

let solve digits input =
    let getDigit s find minMax =
        digits |> List.choose (fun (w, d) -> find w s |> Option.map (fun i -> d, i)) |> minMax snd |> fst

    let f s =
        let first = getDigit s String.tryFindSliceIndex List.minBy
        let last = getDigit s String.tryFindLastSliceIndex List.maxBy

        10 * first + last

    input |> List.map f |> List.sum

let solution =
    makeSolution () parser (solve digits) (solve <| List.append digits digitsAsWords)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 142

    let input2 =
        [| "two1nine"
           "eightwothree"
           "abcone2threexyz"
           "xtwone3four"
           "4nineeightseven2"
           "zoneight234"
           "7pqrstsixteen" |]

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input2 |> should equal 281
