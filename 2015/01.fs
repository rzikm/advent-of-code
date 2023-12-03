module AoC201501

open AdventOfCode
open FSharpPlus
open FParsec

type Paren =
    | Open
    | Close

let parser = many1 ((charReturn '(' Open) <|> (charReturn ')' Close))

let parenToi =
    function
    | Open -> 1
    | Close -> -1

let solve1 input = input |> List.sumBy parenToi

let solve2 input =
    input |> Seq.scan (fun s p -> s + parenToi p) 0 |> Seq.findIndex ((=) -1)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Theory>]
    [<InlineData("(())", 0)>]
    [<InlineData("(((", 3)>]
    [<InlineData("(()(()(", 3)>]
    [<InlineData("())", -1)>]
    let ``Example part 1`` input expected =
        testPart1 solution [| input |] |> should equal expected

    [<Theory>]
    [<InlineData(")", 1)>]
    [<InlineData("()())", 5)>]
    let ``Example part 2`` input expected =
        testPart2 solution [| input |] |> should equal expected
