module AoC202503

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.lines (many1 (digit |>> fun c -> int64 c - int64 '0'))

let getMaxJoltage n digits =
    let f =
        Utils.memoizerec (fun loop (n, digits) ->
            let exp = pown 10L (n - 1)

            if n = 0 then
                0L
            else
                match digits with
                | a :: rest ->
                    let ifTaken = a * exp + loop (n - 1, rest)

                    if List.length rest >= n then
                        let ifNotTaken = loop (n, rest)
                        max ifTaken ifNotTaken
                    else
                        ifTaken

                | _ -> failwith "Nonempty list expected")

    f (n, digits)


let solve1 input = List.sumBy (getMaxJoltage 2) input

let solve2 input = List.sumBy (getMaxJoltage 12) input


let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "987654321111111"; "811111111111119"; "234234234234278"; "818181911112111" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 357L

    [<Theory>]
    [<InlineData("987654321111111", 98)>]
    [<InlineData("811111111111119", 89)>]
    [<InlineData("234234234234278", 78)>]
    [<InlineData("818181911112111", 92)>]
    let ``Example part 1 - individual lines`` input expected =
        getMaxJoltage 2 (input |> Seq.map (string >> int64) |> Seq.toList) |> int32 |> should equal expected

//     [<Fact>]
//     let ``Example part 2`` () =
//         testPart2 solution input |> should equal 0
