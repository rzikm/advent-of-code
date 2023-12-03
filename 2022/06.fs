module AoC202206

open AdventOfCode
open FSharpPlus
open FParsec

let parser = many1 (satisfy isLetter)

let findStart len input =
    (input |> Seq.windowed len |> Seq.findIndex (Seq.distinct >> Seq.length >> (=) len)) + len

let solution = makeSolution () parser (findStart 4) (findStart 14)

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Theory>]
    [<InlineData("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7)>]
    [<InlineData("bvwbjplbgvbhsrlpgdmjqwftvncz", 5)>]
    [<InlineData("nppdvjthqldpwncqszvftbrmjlhg", 6)>]
    [<InlineData("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10)>]
    [<InlineData("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)>]
    let ``Example part 1`` input expected =
        testPart1 solution [| input |] |> should equal expected
