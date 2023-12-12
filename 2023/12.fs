module AoC202312

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    ParseUtils.lines (many1 (anyOf ".?#") .>> skipChar ' ' .>>. sepBy1 pint32 (skipChar ','))

let getArrangementCount chars counts =
    let parts =
        List.split [ [ '.' ] ] chars |> Seq.filter (not << List.isEmpty) |> List.ofSeq

    let f =
        Utils.memoizerec (fun frec (parts, counts) ->
            match (parts, counts) with
            | _, [] ->
                match parts |> List.exists (List.contains ('#')) with
                | true -> 0L // some # is anaccounted for
                | false -> 1L
            | [], _ -> 0L // no more parts, but we still have number left
            | (part :: parts), (count :: counts) ->
                let ifTaken =
                    let firstBroken =
                        List.tryFindIndex ((=) '#') part |> Option.defaultValue part.Length

                    // for each way to fit 'count' consecutive #s into the current part
                    [ 0 .. min firstBroken (part.Length - count) ]
                    |> List.sumBy (fun start ->
                        let before = part |> List.tryItem (start - 1) |> Option.defaultValue '?'
                        let after = part |> List.tryItem (start + count) |> Option.defaultValue '?'

                        match (before, after) with
                        // sum the ways to fit the rest of the parts
                        | '?', '?' ->
                            frec ((List.trySkip (start + count + 1) part |> Option.defaultValue []) :: parts, counts)
                        | _ -> 0)

                // skip this part for this busequence
                let ifNotTaken =
                    match List.contains '#' part with
                    | true -> 0L // can't skip this part
                    | false -> frec (parts, (count :: counts))

                ifTaken + ifNotTaken)

    f (parts, counts)

let solve1 input =
    // List.map (uncurry getArrangementCount) input
    List.sumBy (uncurry getArrangementCount) input

let solve2 =
    List.map (Tuple2.apply (List.replicate 5 >> List.intercalate [ '?' ], List.replicate 5 >> List.concat)) >> solve1

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "???.### 1,1,3"
           ".??..??...?##. 1,1,3"
           "?#?#?#?#?#?#?#? 1,3,1,6"
           "????.#...#... 4,1,1"
           "????.######..#####. 1,6,5"
           "?###???????? 3,2,1"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 21L

    [<Theory>]
    [<InlineData("???.### 1,1,3", 1)>]
    [<InlineData(".??..??...?##. 1,1,3", 4)>]
    [<InlineData("?#?#?#?#?#?#?#? 1,3,1,6", 1)>]
    [<InlineData("????.#...#... 4,1,1", 1)>]
    [<InlineData("????.######..#####. 1,6,5", 4)>]
    [<InlineData("?###???????? 3,2,1", 10)>]
    [<InlineData("?###??????????###???????? 3,2,1,3,2,1", 15 * 10)>]
    let ``Example part 1 - individual lines`` line (expected: int64) =
        parseTestInput parser [| line |] |> solve1 |> should equal expected

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 525152L

    [<Theory>]
    [<InlineData("???.### 1,1,3", 1)>]
    [<InlineData(".??..??...?##. 1,1,3", 16384)>]
    [<InlineData("?#?#?#?#?#?#?#? 1,3,1,6", 1)>]
    [<InlineData("????.#...#... 4,1,1", 16)>]
    [<InlineData("????.######..#####. 1,6,5", 2500)>]
    [<InlineData("?###???????? 3,2,1", 506250)>]
    let ``Example part 2 - individual lines`` line (expected: int64) =
        parseTestInput parser [| line |] |> solve2 |> should equal expected
