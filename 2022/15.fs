module AoC202215

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pcoords = pstring "x=" >>. pint32 .>> pstring ", y=" .>>. pint32

    let pline =
        pstring "Sensor at " >>. pcoords .>> pstring ": closest beacon is at " .>>. pcoords

    sepEndBy1 pline (pchar '\n')

let coveredIntervalsOnRow input row =
    input
    |> List.choose (fun (sensor, beacon) ->
        let dist = Tuple2.manhattanDist sensor beacon
        let (sx, sy) = sensor
        let dy = abs (sy - row)

        if dy > dist then
            None
        else
            let dx = dist - dy
            Some(sx - dx, sx + dx))
    |> Utils.unionIntervals

let solve1 row input =
    let beaconsOnRow =
        input |> List.map snd |> Seq.distinct |> Seq.filter (snd >> ((=) row)) |> Seq.length

    coveredIntervalsOnRow input row |> List.sumBy (fun (s, e) -> e - s + 1) |> flip (-) beaconsOnRow

let solve2 size input =
    (seq { 1..size })
    |> Seq.pick (fun row ->
        match coveredIntervalsOnRow input row with
        | [ (s1, e1); (s2, e2) ] -> Some(int64 (e1 + 1) * 4000000L + int64 row)
        | _ -> None)

let solution = makeSolution parser (solve1 2000000) (solve2 4000000)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
           "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
           "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
           "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
           "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
           "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
           "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
           "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
           "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
           "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
           "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
           "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
           "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
           "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        parseTestInput parser input |> solve1 10 |> should equal 26

    [<Fact>]
    let ``Example part 2`` () =
        parseTestInput parser input |> solve2 20 |> should equal 56000011L
