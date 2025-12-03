module AoC202503

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.lines (many1 (digit |>> fun c -> int64 c - int64 '0'))

let getMaxJoltage n digits =
    let rec findSplit d digits =
        match digits with
        | [] -> None
        | x :: xs when x = d -> Some xs
        | _ :: xs -> findSplit d xs

    let rec findMax n digits =
        if n = 0 then
            Some []
        else
            [ 1L .. 9L ]
            |> List.rev
            |> List.tryPick (fun d ->
                findSplit d digits |> Option.bind (fun rest -> findMax (n - 1) rest |> Option.map (fun r -> d :: r)))

    findMax n digits |> Option.get |> List.fold (fun acc x -> acc * 10L + x) 0L

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
