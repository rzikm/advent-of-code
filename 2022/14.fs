module AoC202214

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let ppoint = pint32 .>> pchar ',' .>>. pint32
    let ppath = sepBy1 ppoint (pstring " -> ")
    sepEndBy1 ppath (pchar '\n')

let simulateSandPour input =
    let bottom = input |> List.map (List.map snd >> List.max) |> List.max

    let occupied =
        input
        |> Seq.collect (
            Seq.pairwise
            >> Seq.collect (fun (start, stop) ->
                let len = Tuple2.manhattanDist start stop + 1
                let diff = Tuple2.sub stop start |> Tuple2.map sign
                List.init len (fun i -> Tuple2.add start (Tuple2.map ((*) i) diff)))
        )
        |> Set.ofSeq

    let pourSand occupied =
        let tryMove occupied (x, y) =
            [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ] |> List.tryFind (flip Set.contains occupied >> not)

        (500, 0)
        |> Seq.unfold (tryMove occupied >> Option.map (fun newPos -> (newPos, newPos)))
        |> Seq.takeWhile (fun pos -> snd pos <= bottom)
        |> Seq.tryLast
        |> Option.bind (fun pos ->
            if snd pos < bottom then
                Some(Set.add pos occupied)
            else
                None)

    Seq.unfold (pourSand >> Option.map (fun s -> (s, s))) occupied

let solve1 input = simulateSandPour input |> Seq.length

let solve2 input =
    let bottom = (input |> List.map (List.map snd >> List.max) |> List.max) + 2

    simulateSandPour (List.append [ [ (500 - bottom - 10, bottom); (500 + bottom + 10, bottom) ] ] input)
    |> Seq.takeWhile (not << Set.contains (500, 0))
    |> Seq.length
    |> (+) 1

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "498,4 -> 498,6 -> 496,6"; "503,4 -> 502,4 -> 502,9 -> 494,9"; "" |]

    [<Theory>] // ignoring the first, cause reasons
    [<InlineData(2, 499, 8)>]
    [<InlineData(3, 501, 8)>]
    [<InlineData(4, 500, 7)>]
    [<InlineData(5, 498, 8)>]
    [<InlineData(22, 500, 2)>]
    [<InlineData(23, 497, 5)>]
    [<InlineData(24, 495, 8)>]
    let ``Example part 1 - drops `` count tx ty =
        parseTestInput parser input
        |> simulateSandPour
        |> Seq.pairwise
        |> Seq.item (count - 2)
        |> uncurry (fun s1 s2 -> Set.difference s2 s1)
        |> Set.toList
        |> List.exactlyOne
        |> should equal (tx, ty)

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 24

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 93
