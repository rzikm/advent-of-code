module AoC202223

open Utils
open AdventOfCode
open FSharpPlus
open FSharpPlus.Data
open FParsec

let parser =
    sepEndBy1 (many1 (charReturn '.' false <|> charReturn '#' true)) (pchar '\n')

let inputToSet input =
    input
    |> List.indexed
    |> List.collect (fun (y, row) -> row |> List.choosei (fun x b -> if b then Some(x, y) else None))
    |> Set.ofList

let stepAll (elves, dirs) =
    let proposeStep dirs elves elf =
        let anyAround = Tuple2.neighbors8 elf |> Seq.exists (flip Set.contains elves)

        if not <| anyAround then
            None
        else
            dirs
            |> Seq.tryPick (fun dir ->
                let stepped = Tuple2.add dir elf
                let steppedL = Tuple2.add stepped <| Tuple2.rotLeft dir
                let steppedR = Tuple2.add stepped <| Tuple2.rotRight dir

                let isFree =
                    [ steppedL; stepped; steppedR ] |> Seq.forall (not << flip Set.contains elves)

                if isFree then Some stepped else None)

    let proposedSteps =
        Set.toSeq elves |> Seq.map (fun elf -> elf, proposeStep dirs elves elf) |> Seq.toList

    let conflicted = proposedSteps |> Seq.choose snd |> Seq.countBy id |> Map.ofSeq

    let afterMove =
        proposedSteps
        |> Seq.map (fun (elf, proposed) ->
            match proposed with
            | None -> elf // no move proposed
            | Some pos ->
                if Map.find pos conflicted <> 1 then
                    elf // conflict with some other elf
                else
                    pos) // moved
        |> Set.ofSeq

    (afterMove, dirs |> List.rotate 1)

let doSteps count input =
    let elves = inputToSet input
    let dirs = [ (0, -1); (0, 1); (-1, 0); (1, 0) ]

    let (finalElves, _) = (stepAll ^ count) (elves, dirs)
    finalElves

let getBounds elves =
    elves
    |> Set.fold
        (fun (minPos, maxPos) pos -> Tuple2.map2 min minPos pos, Tuple2.map2 max maxPos pos)
        (Set.minElement elves |> Tuple2.broadcast)

let solve1 input =
    let finalElves = doSteps 10 input

    let (x0, y0), (x1, y1) = getBounds finalElves
    (x1 - x0 + 1) * (y1 - y0 + 1) - Set.count finalElves

let solve2 input =
    let elves = inputToSet input
    let dirs = [ (0, -1); (0, 1); (-1, 0); (1, 0) ]

    let lastRound =
        Seq.unfold
            (fun (i, state) ->
                let res = (i + 1, stepAll state)

                if res |> snd |> fst = fst state then
                    None
                else
                    Some(res, res))
            (0, (elves, dirs))
        |> Seq.last
        |> fst

    lastRound + 1

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let visualize elves =
        let (x0, y0), (x1, y1) = getBounds elves

        [ y0..y1 ]
        |> List.map (fun y ->
            [ x0..x1 ] |> List.map (fun x -> if Set.contains (x, y) elves then "#" else ".") |> String.concat "")
        |> String.concat "\n"


    let input =
        [| "....#.."
           "..###.#"
           "#...#.#"
           ".#...##"
           "#.###.."
           "##.#.##"
           ".#..#.."
           "" |]

    [<Fact>]
    let ``Simple example one step`` () =
        let input = [| "....."; "..##."; "..#.."; "....."; "..##."; "....."; "" |]

        parseTestInput parser input
        |> doSteps 1
        |> visualize
        |> should
            equal
            "##
..
#.
.#
#."

    [<Fact>]
    let ``Simple example two step`` () =
        let input = [| "....."; "..##."; "..#.."; "....."; "..##."; "....."; "" |]

        parseTestInput parser input
        |> doSteps 2
        |> visualize
        |> should
            equal
            ".##.
#...
...#
....
.#.."


    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 110

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 20
