module AoC202023

open Utils
open AdventOfCode
open FSharpPlus
open FParsec

let parser = many1 (satisfy isDigit |>> (fun c -> int c - int '0'))

// let initFollowers input =
//     List.fold (fun (prev, map) i -> (i, Map.add prev i map)) (List.head input, Map.empty) (List.tail input)
//     |> snd
//     |> Map.add (List.last input) (List.head input)

let initFollowers input =
    let arr = Array.zeroCreate (List.length input + 1)
    input |> List.pairwise |> List.iter (fun (a, b) -> Array.set arr a b)
    Array.set arr (List.last input) (List.head input)

    arr

let doMove (current, followers) =
    let size = Array.length followers - 1

    let picked =
        [ 1..3 ] |> List.scan (fun c _ -> Array.item c followers) current |> List.tail

    let lastPicked = picked |> List.last
    let nextForCurrent = Array.item lastPicked followers

    let destLabel =
        [ 1..4 ]
        |> Seq.map (fun i -> (current - i + size - 1) % size + 1)
        |> Seq.find (fun i -> List.contains i picked |> not)

    // (nextForCurrent,
    //  followers
    //  |> Map.add current nextForCurrent
    //  |> Map.add lastPicked (Map.find destLabel followers)
    //  |> Map.add destLabel (List.head picked))

    Array.set followers current nextForCurrent
    Array.set followers lastPicked (Array.item destLabel followers)
    Array.set followers destLabel (List.head picked)
    (nextForCurrent, followers)

let solve1 iters input =
    let followers = initFollowers input
    let res = applyN doMove iters (List.head input, followers) |> snd

    [ 1..8 ] |> List.scan (fun c _ -> Array.item c res) 1 |> Seq.tail |> Seq.map (string) |> String.concat ""
// [ 1..8 ] |> List.scan (fun c _ -> Map.find c res) 1 |> Seq.tail |> Seq.map (string) |> String.concat ""

let solve2 input =
    let followers = initFollowers (input @ [ 10..1_000_000 ])

    let res = applyN doMove 10_000_000 (List.head input, followers) |> snd
    [ 1..2 ] |> List.scan (fun c _ -> Array.item c res) 1 |> List.map int64 |> List.reduce (*)
// [ 1..2 ] |> List.scan (fun c _ -> Map.find c res) 1 |> List.map int64 |> List.reduce (*)

let solution = makeSolution parser (solve1 100) solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "389125467" |]

    [<Theory>]
    [<InlineData("54673289", 1)>]
    [<InlineData("32546789", 2)>]
    [<InlineData("34672589", 3)>]
    [<InlineData("32584679", 4)>]
    [<InlineData("36792584", 5)>]
    [<InlineData("93672584", 6)>]
    [<InlineData("92583674", 7)>]
    [<InlineData("58392674", 8)>]
    [<InlineData("83926574", 9)>]
    [<InlineData("92658374", 10)>]
    let ``Small examples`` result iteration =
        parseTestInput parser input |> solve1 iteration |> should equal result

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal "67384529"

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 149245887792L
