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

let transformSeed map seed =
    map
    |> snd
    |> List.tryPick (fun (dstStart, srcStart, len) ->
        if srcStart <= seed && seed < srcStart + len then
            Some(dstStart + seed - srcStart)
        else
            None)
    |> Option.defaultValue seed

let solve1 (seeds, maps) =
    maps |> List.fold (fun seeds map -> seeds |> List.map (transformSeed map)) seeds |> List.min

let transformSeedRange map (start, count) =
    let rec loop acc (start, count) maps =
        if (count = 0L) then
            acc
        else
            match maps with
            | [] -> acc @ [ (start, count) ]
            | (dstStart, srcStart, len) :: rest ->
                let cmnStart = max srcStart start
                let cmnEnd = min (srcStart + len) (start + count)

                if start < srcStart then
                    let nlen = min count (srcStart - start)
                    loop ((start, nlen) :: acc) (srcStart, count - nlen) ((dstStart, srcStart, len) :: rest)
                else if cmnStart < cmnEnd then
                    let cmnLen = cmnEnd - cmnStart
                    loop ((dstStart - srcStart + cmnStart, cmnLen) :: acc) (cmnEnd, count - cmnLen) rest
                else
                    loop acc (start, count) rest

    loop [] (start, count) (snd map)

let merge list =
    let rec loop =
        function
        | [] -> []
        | [ x ] -> [ x ]
        | (start1, count1) :: (start2, count2) :: rest ->
            if start1 + count1 >= start2 then
                let count = max (start2 - start1 + count2) count1
                loop ((start1, count) :: rest)
            else
                (start1, count1) :: loop ((start2, count2) :: rest)

    List.sortBy fst list |> loop

let solve2 input =
    let inRanges = fst input |> List.chunkBySize 2 |> List.map (fun [ a; b ] -> (a, b))

    input
    |> snd
    |> List.fold (fun ranges map -> List.collect (transformSeedRange map) ranges |> merge) inRanges
    |> List.map fst
    |> List.min

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
        let inRanges = [ (79L, 14L); (55L, 13L) ]
        let map = ("", [ (52L, 50L, 48L); (50L, 98L, 2L) ])

        [ map ]
        |> List.fold (fun ranges map -> List.collect (transformSeedRange map) ranges |> merge) inRanges
        |> should equal [ (57L, 13L); (81L, 14L) ]

    [<Fact>]
    let ``Example part 2 - split`` () =
        let map = ("", [ (52L, 50L, 48L); (50L, 98L, 2L) ])
        transformSeedRange map (96L, 4L) |> merge |> should equal [ (50L, 2L); (98L, 2L) ]

    [<Fact>]
    let ``Example part 2 - skip range`` () =
        let map = ("", [ (52L, 10L, 48L); (50L, 98L, 2L) ])
        transformSeedRange map (96L, 4L) |> merge |> should equal [ (50L, 2L); (96L, 2L) ]

    [<Fact>]
    let ``Merge ranges`` () =
        merge [ (1L, 2L)
                (3L, 4L)
                (7L, 2L)
                (9L, 1L) ]
        |> should equal [ (1L, 9L) ]

    [<Fact>]
    let ``Merge overlapping`` () =
        merge [ (1L, 5L); (3L, 4L) ] |> should equal [ (1L, 6L) ]

    [<Fact>]
    let ``Merge subset `` () =
        merge [ (1L, 10L); (3L, 4L) ] |> should equal [ (1L, 10L) ]

    [<Fact>]
    let ``Merge longer `` () =
        merge [ (1L, 2L); (1L, 10L) ] |> should equal [ (1L, 10L) ]

    [<Theory>]
    [<InlineData(79L, "79 81 81 81 74 78 78 82")>]
    [<InlineData(14L, "14 14 53 49 42 42 43 43")>]
    [<InlineData(55L, "55 57 57 53 46 82 82 86")>]
    [<InlineData(13L, "13 13 52 41 34 34 35 35")>]
    let ``Example part 2 - using part 2 - expected steps`` seed steps =
        let (_, maps) = parseTestInput parser input

        maps
        |> List.scan (fun ranges map -> List.collect (transformSeedRange map) ranges |> merge) [ (seed, 1L) ]
        |> List.collect (List.map fst)
        |> should equal (steps |> String.split [ " " ] |> List.ofSeq |> List.map int64)

    [<Fact>]
    let ``Example part 1 - using part 2`` () =
        let (seeds, maps) = parseTestInput parser input
        let seeds = seeds |> List.collect (fun s -> [ s; 1L ])

        solve2 (seeds, maps) |> should equal 35L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 46L
