module AoC202314

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.grid (anyOf "O.#")

let tiltGridLeft grid =
    let sortKey =
        function
        | 'O' -> 0
        | '.' -> 1
        | _ -> failwith "Invalid tile"

    let tiltRow row =
        Array.split [ [| '#' |] ] row |> Seq.map (Array.sortBy sortKey) |> Array.intercalate [| '#' |]

    grid |> Array.map tiltRow

let getNorthLoad grid =
    Array.foldBack (fun row (acc, i) -> acc + (Seq.count ((=) 'O') row) * i, i + 1) grid (0, 1) |> fst

let solve1 input =
    input |> Array.rotate2dCounterclockwise |> tiltGridLeft |> Array.rotate2dClockwise |> getNorthLoad

let tilt4times grid =
    grid
    |> Array.rotate2dCounterclockwise
    |> Utils.applyN 4 (tiltGridLeft >> Array.rotate2dClockwise)
    |> Array.rotate2dClockwise

let solve2 input =
    Utils.applyNWithRepeatDetection 1000000000 tilt4times input |> getNorthLoad

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "O....#...."
           "O.OO#....#"
           ".....##..."
           "OO.#O....O"
           ".O.....O#."
           "O.#..O.#.#"
           "..O..#O..O"
           ".......O.."
           "#....###.."
           "#OO..#...."
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 136

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 64

    let after1Cycle =
        [| ".....#...."
           "....#...O#"
           "...OO##..."
           ".OO#......"
           ".....OOO#."
           ".O#...O#.#"
           "....O#...."
           "......OOOO"
           "#...O###.."
           "#..OO#...." |]

    let after2Cycle =
        [| ".....#...."
           "....#...O#"
           ".....##..."
           "..O#......"
           ".....OOO#."
           ".O#...O#.#"
           "....O#...O"
           ".......OOO"
           "#..OO###.."
           "#.OOO#...O" |]

    let after3Cycle =
        [| ".....#...."
           "....#...O#"
           ".....##..."
           "..O#......"
           ".....OOO#."
           ".O#...O#.#"
           "....O#...O"
           ".......OOO"
           "#...O###.O"
           "#.OOO#...O" |]

    [<Fact>]
    let ``Example part 2 - after 1 cycle`` () =
        parseTestInput parser input |> tilt4times |> should equal (parseTestInput parser after1Cycle)

    [<Fact>]
    let ``Example part 2 - after 2 cycles`` () =
        parseTestInput parser input |> Utils.applyN 2 tilt4times |> should equal (parseTestInput parser after2Cycle)

    [<Fact>]
    let ``Example part 2 - after 3 cycles`` () =
        parseTestInput parser input |> Utils.applyN 3 tilt4times |> should equal (parseTestInput parser after3Cycle)
