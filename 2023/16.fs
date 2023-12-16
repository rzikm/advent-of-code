module AoC202316

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.grid (anyOf ".|\\/-")

type Dir =
    | Up
    | Down
    | Left
    | Right

let dirToDelta =
    function
    | Up -> 0, -1
    | Down -> 0, 1
    | Left -> -1, 0
    | Right -> 1, 0

let cellToDirs dir cell =
    match (dir, cell) with
    | _, '.' -> Seq.singleton dir

    | _, '|' when dir = Up || dir = Down -> Seq.singleton dir
    | _, '|' -> [ Up; Down ]

    | _, '-' when dir = Left || dir = Right -> Seq.singleton dir
    | _, '-' -> [ Left; Right ]

    | Right, '/' -> Seq.singleton Up
    | Up, '/' -> Seq.singleton Right
    | Left, '/' -> Seq.singleton Down
    | Down, '/' -> Seq.singleton Left

    | Right, '\\' -> Seq.singleton Down
    | Down, '\\' -> Seq.singleton Right
    | Up, '\\' -> Seq.singleton Left
    | Left, '\\' -> Seq.singleton Up

    | _ -> failwith "Not exhaustive match"

let countEnergised grid start =
    let bounds = (Array.length (Array.item 0 grid), Array.length grid)

    let fNeighbors (p, dir) =
        cellToDirs dir (Array.item2dp p grid)
        |> Seq.map (fun dir -> (Tuple2.add (dirToDelta dir) p, dir), 1)
        |> Seq.filter (fun ((p, _), _) -> Tuple2.le (0, 0) p && Tuple2.lt p bounds)

    Graph.flood fNeighbors start |> Seq.map (fst >> fst) |> Seq.distinct |> Seq.length

let solve1 input = countEnergised input ((0, 0), Right)

let solve2 input =
    let allStarts =
        Seq.concat [ seq { 0 .. Array.length (Array.item 0 input) - 1 } |> Seq.map (fun x -> (x, 0), Down)
                     seq { 0 .. Array.length (Array.item 0 input) - 1 }
                     |> Seq.map (fun x -> (x, Array.length input - 1), Up)
                     seq { 0 .. Array.length input - 1 } |> Seq.map (fun y -> (0, y), Right)
                     seq { 0 .. Array.length input - 1 }
                     |> Seq.map (fun y -> (Array.length (Array.item 0 input) - 1, y), Left) ]

    allStarts |> Seq.map (countEnergised input) |> Seq.max

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| @".|...\...."
           @"|.-.\....."
           @".....|-..."
           @"........|."
           @".........."
           @".........\"
           @"..../.\\.."
           @".-.-/..|.."
           @".|....-|.\"
           @"..//.|...."
           @"" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 46

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 51
