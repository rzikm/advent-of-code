module AoC202003

open AdventOfCode
open FSharpPlus
open FParsec
open Utils

type Cell =
    | Tree
    | Empty

let parser =
    let ptree = (pchar '#' >>% Tree)
    let pempty = (pchar '.' >>% Empty)
    let row = many1 (ptree <|> pempty) |>> Array.ofSeq
    sepEndBy row spaces |>> Array.ofSeq

let countTrees (dx, dy) input =
    let rowLength = Array.length (Array.item 0 input)

    Seq.initInfinite (fun i -> (i * dx % rowLength, i * dy))
    |> takeWhile (snd >> (>) (Array.length input))
    |> map (fun (x, y) -> Array.item2d x y input)
    |> Seq.sumBy (fun c -> if c = Tree then 1 else 0)

let part1 input = countTrees (3, 1) input

let part2 input =
    [ (1, 1)
      (3, 1)
      (5, 1)
      (7, 1)
      (1, 2) ]
    |> List.map (fun d -> countTrees d input |> int64)
    |> List.reduce (*)

let solution = makeSolution parser part1 part2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "..##......."
           "#...#...#.."
           ".#....#..#."
           "..#.#...#.#"
           ".#...##..#."
           "..#.##....."
           ".#.#.#....#"
           ".#........#"
           "#.##...#..."
           "#...##....#"
           ".#..#...#.#" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 7

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 336L
