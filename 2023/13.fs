module AoC202313

open AdventOfCode
open FSharpPlus
open FParsec

type Cell =
    | Ash
    | Mirror

let parser =
    let cell = charReturn '.' Ash <|> charReturn '#' Mirror
    sepEndBy1 (ParseUtils.grid cell) spaces

type Axis =
    | Horizontal
    | Vertical

let findHorizontalAxes grid =
    seq { 1 .. Array.length grid - 1 }
    |> Seq.filter (fun axis ->
        seq { 0 .. (axis - 1) }
        |> Seq.forall (fun yy ->
            let reflectedY = (axis - 1 - yy) + axis
            Array.tryItem reflectedY grid |> Option.map ((=) (Array.item yy grid)) |> Option.defaultValue true))

let findMirrorAxis grid =
    seq {
        yield! findHorizontalAxes grid |> Seq.map (fun pos -> Horizontal, pos)
        yield! grid |> Array.transpose |> findHorizontalAxes |> Seq.map (fun pos -> Vertical, pos)
    }

let summarizeAxis (axis, pos) =
    match axis with
    | Horizontal -> pos * 100
    | Vertical -> pos

let solve1 input =
    List.sumBy (findMirrorAxis >> Seq.exactlyOne >> summarizeAxis) input

let solve2 input =
    let replace =
        function
        | Ash -> Mirror
        | Mirror -> Ash

    let calculate grid =
        let original = findMirrorAxis grid |> Seq.exactlyOne

        Array.allIndexes2d grid
        |> Seq.map (fun pos -> Array.mapAt2dp pos replace grid)
        |> Seq.collect findMirrorAxis
        |> Seq.find ((<>) original)
        |> summarizeAxis

    List.sumBy calculate input

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "#.##..##."
           "..#.##.#."
           "##......#"
           "##......#"
           "..#.##.#."
           "..##..##."
           "#.#.##.#."
           ""
           "#...##..#"
           "#....#..#"
           "..##..###"
           "#####.##."
           "#####.##."
           "..##..###"
           "#....#..#"
           "" |]

    let input1 =
        [| ".##......"
           "###.####."
           "##.##...#"
           "..###..##"
           "...##..##"
           "#..#.##.#"
           "..#......"
           ".##..##.."
           ".##..##.."
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 405

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 400

    [<Fact>]
    let ``Horizontal still valid after smudge`` () =
        testPart2 solution input1 |> should equal 6
