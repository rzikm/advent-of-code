module AoC202210

open AdventOfCode
open FSharpPlus
open FParsec

type Instr =
    | Addx of int
    | Noop

let parser =
    sepEndBy1
        (choice [ stringReturn "noop" Noop
                  pstring "addx " >>. pint32 |>> Addx ])
        (pchar '\n')

let simulate input =
    let folder prevStates instr =
        let (cycle, x) = List.last prevStates

        match instr with
        | Noop -> [ (cycle + 1, x) ]
        | Addx i -> [ (cycle + 1, x); (cycle + 2, x + i) ]

    input |> List.toSeq |> Seq.scan folder [ (1, 1) ] |> Seq.collect id

let solve1 input =
    simulate input |> Seq.filter (fun (c, _) -> (c - 20) % 40 = 0) |> Seq.sumBy (fun (c, x) -> c * x)

let solve2 input =
    "\n"
    + (simulate input
       |> Seq.take 240
       |> Seq.map (fun (c, x) -> if abs ((c - 1) % 40 - x) <= 1 then "#" else ".")
       |> Seq.chunkBySize 40
       |> Seq.map (String.concat "")
       |> String.concat "\n")

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "addx 15"
           "addx -11"
           "addx 6"
           "addx -3"
           "addx 5"
           "addx -1"
           "addx -8"
           "addx 13"
           "addx 4"
           "noop"
           "addx -1"
           "addx 5"
           "addx -1"
           "addx 5"
           "addx -1"
           "addx 5"
           "addx -1"
           "addx 5"
           "addx -1"
           "addx -35"
           "addx 1"
           "addx 24"
           "addx -19"
           "addx 1"
           "addx 16"
           "addx -11"
           "noop"
           "noop"
           "addx 21"
           "addx -15"
           "noop"
           "noop"
           "addx -3"
           "addx 9"
           "addx 1"
           "addx -3"
           "addx 8"
           "addx 1"
           "addx 5"
           "noop"
           "noop"
           "noop"
           "noop"
           "noop"
           "addx -36"
           "noop"
           "addx 1"
           "addx 7"
           "noop"
           "noop"
           "noop"
           "addx 2"
           "addx 6"
           "noop"
           "noop"
           "noop"
           "noop"
           "noop"
           "addx 1"
           "noop"
           "noop"
           "addx 7"
           "addx 1"
           "noop"
           "addx -13"
           "addx 13"
           "addx 7"
           "noop"
           "addx 1"
           "addx -33"
           "noop"
           "noop"
           "noop"
           "addx 2"
           "noop"
           "noop"
           "noop"
           "addx 8"
           "noop"
           "addx -1"
           "addx 2"
           "addx 1"
           "noop"
           "addx 17"
           "addx -9"
           "addx 1"
           "addx 1"
           "addx -3"
           "addx 11"
           "noop"
           "noop"
           "addx 1"
           "noop"
           "addx 1"
           "noop"
           "noop"
           "addx -13"
           "addx -19"
           "addx 1"
           "addx 3"
           "addx 26"
           "addx -30"
           "addx 12"
           "addx -1"
           "addx 3"
           "addx 1"
           "noop"
           "noop"
           "noop"
           "addx -9"
           "addx 18"
           "addx 1"
           "addx 2"
           "noop"
           "noop"
           "addx 9"
           "noop"
           "noop"
           "noop"
           "addx -1"
           "addx 2"
           "addx -37"
           "addx 1"
           "addx 3"
           "noop"
           "addx 15"
           "addx -21"
           "addx 22"
           "addx -6"
           "addx 1"
           "noop"
           "addx 2"
           "addx 1"
           "noop"
           "addx -10"
           "noop"
           "noop"
           "addx 20"
           "addx 1"
           "addx 2"
           "addx 2"
           "addx -6"
           "addx -11"
           "noop"
           "noop"
           "noop"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 13140

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal
        <| String.concat
            "\n"
            [ ""
              "##..##..##..##..##..##..##..##..##..##.."
              "###...###...###...###...###...###...###."
              "####....####....####....####....####...."
              "#####.....#####.....#####.....#####....."
              "######......######......######......####"
              "#######.......#######.......#######....." ]
