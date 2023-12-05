module AoC202305

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let seeds = skipString "seeds: " >>. sepBy1 pint64 (pchar ' ') .>> spaces

    let map =
        charsTillString " map:" true 100 .>> spaces
        .>>. (sepEndBy1 (tuple3 (pint64 .>> spaces) (pint64 .>> spaces) pint64) (pchar '\n')
              |>> List.sortBy (fun (_, a, _) -> a))

    seeds .>>. sepEndBy1 map (pchar '\n')

let transformSeedRange map (start, stop) =
    let rec loop acc (start, stop) maps =
        if (start > stop) then
            acc
        else
            match maps with
            | [] -> acc @ [ (start, stop) ]
            | (dstStart, srcStart, len) :: rest ->
                let cmnStart = max srcStart start
                let cmnEnd = min (srcStart + len - 1L) stop

                if start < srcStart then
                    let nstop = min stop (srcStart - 1L)
                    loop ((start, nstop) :: acc) (srcStart, stop) ((dstStart, srcStart, len) :: rest)
                else if cmnStart <= cmnEnd then
                    let delta = dstStart - srcStart
                    loop ((cmnStart + delta, cmnEnd + delta) :: acc) (cmnEnd + 1L, stop) rest
                else
                    loop acc (start, stop) rest

    loop [] (start, stop) (snd map)

let solve preprocess input =
    let inRanges = preprocess input

    input
    |> snd
    |> List.fold (fun ranges map -> List.collect (transformSeedRange map) ranges |> Range.mergeList) inRanges
    |> List.map fst
    |> List.min

let solve1 input =
    solve (fst >> List.map (fun s -> (s, s))) input

#nowarn "25"

let solve2 input =
    solve (fst >> List.chunkBySize 2 >> List.map (fun [ a; b ] -> (a, a + b - 1L))) input

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "seeds: 79 14 55 13"
           ""
           "seed-to-soil map:"
           "50 98 2"
           "52 50 48"
           ""
           "soil-to-fertilizer map:"
           "0 15 37"
           "37 52 2"
           "39 0 15"
           ""
           "fertilizer-to-water map:"
           "49 53 8"
           "0 11 42"
           "42 0 7"
           "57 7 4"
           ""
           "water-to-light map:"
           "88 18 7"
           "18 25 70"
           ""
           "light-to-temperature map:"
           "45 77 23"
           "81 45 19"
           "68 64 13"
           ""
           "temperature-to-humidity map:"
           "0 69 1"
           "1 0 69"
           ""
           "humidity-to-location map:"
           "60 56 37"
           "56 93 4"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 35L

    [<Fact>]
    let ``Example part 2 - first level`` () =
        let inRanges = [ (79L, 92L); (55L, 67L) ]
        let map = ("", [ (52L, 50L, 48L); (50L, 98L, 2L) ])

        [ map ]
        |> List.fold (fun ranges map -> List.collect (transformSeedRange map) ranges |> Range.mergeList) inRanges
        |> should equal [ (57L, 69L); (81L, 94L) ]

    [<Fact>]
    let ``Example part 2 - split`` () =
        let map = ("", [ (52L, 50L, 48L); (50L, 98L, 2L) ])
        transformSeedRange map (96L, 99L) |> Range.mergeList |> should equal [ (50L, 51L); (98L, 99L) ]

    [<Fact>]
    let ``Example part 2 - skip range`` () =
        let map = ("", [ (52L, 10L, 48L); (50L, 98L, 2L) ])
        transformSeedRange map (96L, 99L) |> Range.mergeList |> should equal [ (50L, 51L); (96L, 97L) ]

    [<Fact>]
    let ``Merge touching`` () =
        Range.mergeList [ (1L, 2L); (3L, 4L) ] |> should equal [ (1L, 4L) ]

    [<Fact>]
    let ``Merge overlapping`` () =
        Range.mergeList [ (1L, 5L); (3L, 6L) ] |> should equal [ (1L, 6L) ]

    [<Fact>]
    let ``Merge subset `` () =
        Range.mergeList [ (1L, 10L); (3L, 4L) ] |> should equal [ (1L, 10L) ]

    [<Fact>]
    let ``Merge longer `` () =
        Range.mergeList [ (1L, 2L); (1L, 10L) ] |> should equal [ (1L, 10L) ]

    [<Theory>]
    [<InlineData(79L, "79 81 81 81 74 78 78 82")>]
    [<InlineData(14L, "14 14 53 49 42 42 43 43")>]
    [<InlineData(55L, "55 57 57 53 46 82 82 86")>]
    [<InlineData(13L, "13 13 52 41 34 34 35 35")>]
    let ``Example part 2 - using part 2 - expected steps`` seed steps =
        let (_, maps) = parseTestInput parser input

        maps
        |> List.scan
            (fun ranges map -> List.collect (transformSeedRange map) ranges |> Range.mergeList)
            [ (seed, seed) ]
        |> List.collect (List.map fst)
        |> should equal (steps |> String.split [ " " ] |> List.ofSeq |> List.map int64)

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 46L
