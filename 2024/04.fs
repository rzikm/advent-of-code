module AoC202404

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.grid <| anyOf "XMAS"

let solve input occurrences search =
    occurrences input
    |> Seq.count (List.map (fun (x, y) -> Array.item2d x y input) >> String.ofList >> flip List.contains search)

let solve1 input =
    solve input (Array.allIndexesInLine 4) [ "XMAS"; "SAMX" ]

let solve2 input =
    let allCrosses input =
        let (xx, yy) = Array.bounds2d input

        seq {
            for (x0, y0) in Array.allIndexes2d input do
                if x0 + 2 < xx && y0 + 2 < yy then
                    yield [ (x0, y0); (x0 + 1, y0 + 1); (x0 + 2, y0 + 2); (x0, y0 + 2); (x0 + 2, y0) ]
        }

    solve input allCrosses [ "MASMS"; "SAMMS"; "MASSM"; "SAMSM" ]

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "MMMSXXMASM"
           "MSAMXMSMSA"
           "AMXSXMAAMM"
           "MSAMASMSMX"
           "XMASAMXAMM"
           "XXAMMXXAMA"
           "SMSMSASXSS"
           "SAXAMASAAA"
           "MAMMMXMMMM"
           "MXMXAXMASX" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 18

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 9
