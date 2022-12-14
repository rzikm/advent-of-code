module AoC201502

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let ppresent = tuple3 pint32 (pchar 'x' >>. pint32) (pchar 'x' >>. pint32)
    sepEndBy1 ppresent (pchar '\n')

let sides (w, l, h) = [ l * w; w * h; h * l ]

let sidePerimeters (w, l, h) =
    [ l + w; w + h; h + l ] |> List.map ((*) 2)

let area dims = sides dims |> List.sum |> (*) 2

let volume (w, l, h) = w * l * h

let measure1 dim = area dim + (List.min (sides dim))

let measure2 dim =
    (sidePerimeters dim |> List.min) + volume dim

let solution = makeSolution parser (List.sumBy measure1) (List.sumBy measure2)

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Theory>]
    [<InlineData("2x3x4", 58)>]
    [<InlineData("1x1x10", 43)>]
    let ``Example part 1`` input expected =
        testPart1 solution [| input |] |> should equal expected

    [<Theory>]
    [<InlineData("2x3x4", 34)>]
    [<InlineData("1x1x10", 14)>]
    let ``Example part 2`` input expected =
        testPart2 solution [| input |] |> should equal expected
