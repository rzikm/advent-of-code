module AoC202305

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let seeds = skipString "seeds: " >>. sepBy1 pint64 (pchar ' ') .>> spaces

    let range =
        tuple3 (pint64 .>> spaces) (pint64 .>> spaces) pint64
        |>> (fun (dst, src, count) -> (Range.create src count, dst - src))

    let map =
        charsTillString " map:" true 100 .>> spaces .>>. (sepEndBy1 (range) (pchar '\n') |>> List.sort)

    seeds .>>. sepEndBy1 map (pchar '\n')

let transformSeedRanges map ranges =
    let folder (seeds, mappedSeeds) (mapRange, delta) =
        let matched = seeds |> List.choose (fun r -> Range.tryIntersect r mapRange)
        let left = Range.diffMany seeds matched
        let newMapped = matched |> List.map (Tuple2.map ((+) delta))
        (left, newMapped @ mappedSeeds)

    let (unmapped, mapped) = snd map |> List.fold folder (ranges, [])
    unmapped @ mapped |> Range.unionMany

let solve preprocess input =
    let inRanges = preprocess input
    input |> snd |> List.fold (fun ranges map -> transformSeedRanges map ranges) inRanges |> List.map fst |> List.min

let solve1 input =
    solve (fst >> List.map (fun s -> Range.create s 1L)) input

#nowarn "25"

let solve2 input =
    solve (fst >> List.chunkBySize 2 >> List.map (fun [ a; b ] -> Range.create a b)) input

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
        let inRanges = [ (79L, 93L); (55L, 68L) ]
        let map = ("", [ ((50L, 98L), 2L); ((98L, 100L), -48L) ])

        [ map ]
        |> List.fold (fun ranges map -> transformSeedRanges map ranges) inRanges
        |> should equal [ (57L, 70L); (81L, 95L) ]

    [<Fact>]
    let ``Example part 2 - split`` () =
        let map = ("", [ ((50L, 98L), 2L); ((98L, 100L), -48L) ])
        transformSeedRanges map [ (96L, 100L) ] |> Range.unionMany |> should equal [ (50L, 52L); (98L, 100L) ]

    [<Fact>]
    let ``Example part 2 - skip range`` () =
        let map = ("", [ ((50L, 68L), -42L); ((98L, 100L), -48L) ])
        transformSeedRanges map [ (96L, 100L) ] |> Range.unionMany |> should equal [ (50L, 52L); (96L, 98L) ]

    [<Theory>]
    [<InlineData(79L, "79 81 81 81 74 78 78 82")>]
    [<InlineData(14L, "14 14 53 49 42 42 43 43")>]
    [<InlineData(55L, "55 57 57 53 46 82 82 86")>]
    [<InlineData(13L, "13 13 52 41 34 34 35 35")>]
    let ``Example part 2 - using part 2 - expected steps`` seed steps =
        let (_, maps) = parseTestInput parser input

        maps
        |> List.scan (fun ranges map -> transformSeedRanges map ranges) [ Range.create seed 1L ]
        |> List.collect (List.map fst)
        |> should equal (steps |> String.split [ " " ] |> List.ofSeq |> List.map int64)

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 46L
