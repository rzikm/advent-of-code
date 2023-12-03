module AoC202225

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pdigit =
        choice [ charReturn '=' -2
                 charReturn '-' -1
                 charReturn '0' 0
                 charReturn '1' 1
                 charReturn '2' 2 ]
        |>> int64

    let pnum = many1 pdigit

    sepEndBy1 pnum (pchar '\n')

let snafuToDecimal digits =
    List.fold (fun acc d -> acc * 5L + d) 0L digits

let decimalToSnafu num =
    let digits = "=-012"

    let rec f acc =
        function
        | 0L -> acc
        | num ->
            let num = num + 2L
            f (string digits.[num % 5L |> int32] :: acc) (num / 5L)

    f [] num |> String.concat ""

let solve input =
    List.sumBy snafuToDecimal input |> decimalToSnafu

let solution = makeSolution () parser solve (Utils.constf "*")

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "1=-0-2"
           "12111"
           "2=0="
           "21"
           "2=01"
           "111"
           "20012"
           "112"
           "1=-1="
           "1-12"
           "12"
           "1="
           "122"
           "" |]

    [<Theory>]
    [<InlineData(1, "1")>]
    [<InlineData(2, "2")>]
    [<InlineData(3, "1=")>]
    [<InlineData(4, "1-")>]
    [<InlineData(5, "10")>]
    [<InlineData(6, "11")>]
    [<InlineData(7, "12")>]
    [<InlineData(8, "2=")>]
    [<InlineData(9, "2-")>]
    [<InlineData(10, "20")>]
    let ``Decimal to SNAFU`` num expected =
        decimalToSnafu num |> should equal expected

    [<Fact>]
    let ``Example`` () =
        testPart1 solution input |> should equal "2=-1=0"
