module AoC202507

open AdventOfCode
open FSharpPlus
open FParsec

type Cell =
    | Empty
    | Start
    | Splitter

let parser = ParseUtils.gridOf [ '.', Empty; 'S', Start; '^', Splitter ]

let solve1 input =
    let startPos =
        Array.allIndexes2d input |> Seq.find (fun pos -> Array.item2dp pos input = Start)

    let processRow (splitCount, beams) splitters =
        let rec loop beams splitters =
            match beams, splitters with
            | [], _ -> []
            | _, [] -> beams
            | b :: br, s :: sr ->
                if b < s then
                    b :: loop br splitters
                elif b = s then
                    s - 1 :: s + 1 :: loop br sr
                else // b > s
                    loop beams sr

        let newBeams = loop beams splitters
        let newSplits = List.length newBeams - List.length beams
        splitCount + newSplits, newBeams |> List.distinct

    let folder beams row =
        Array.allIndexes row |> Seq.filter (flip Array.item row >> (=) Splitter) |> List.ofSeq |> processRow beams

    Array.fold folder (0, [ startPos |> fst ]) input |> fst


let solve2 input =
    let startPos =
        Array.allIndexes2d input |> Seq.find (fun pos -> Array.item2dp pos input = Start)

    let f =
        Utils.memoizerec (fun loop pos ->
            match Array.tryItem2dp pos input with
            | None -> 1L
            | Some Empty
            | Some Start -> loop (Tuple2.add (0, 1) pos)
            | Some Splitter -> loop (Tuple2.add (1, 0) pos) + loop (Tuple2.add (-1, 0) pos))

    f startPos

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| ".......S......."
           "..............."
           ".......^......."
           "..............."
           "......^.^......"
           "..............."
           ".....^.^.^....."
           "..............."
           "....^.^...^...."
           "..............."
           "...^.^...^.^..."
           "..............."
           "..^...^.....^.."
           "..............."
           ".^.^.^.^.^...^."
           "..............." |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 21

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 40L
