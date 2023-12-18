module AoC202310

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.grid (anyOf "|-LJ7F.S")

type Dir =
    | Up
    | Down
    | Left
    | Right

let reverseDir =
    function
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

let dirToDelta =
    function
    | Up -> 0, -1
    | Down -> 0, 1
    | Left -> -1, 0
    | Right -> 1, 0

let pipeToDirs =
    function
    | '|' -> [ Up; Down ]
    | '-' -> [ Left; Right ]
    | 'L' -> [ Up; Right ]
    | 'J' -> [ Up; Left ]
    | '7' -> [ Down; Left ]
    | 'F' -> [ Down; Right ]
    | '.' -> []
    | 'S' -> []
    | c -> failwithf "Invalid pipe %c" c

let findStartPos input =
    input
    |> Array.indexed
    |> Array.pick (fun (r, row) -> Array.tryFindIndex ((=) 'S') row |> Option.map (fun i -> i, r))

let getPipe input =
    let startPos = findStartPos input

    let startDir =
        [ Up; Down; Left; Right ]
        |> List.find (fun dir ->
            Array.tryItem2dp (dirToDelta dir |> Tuple2.add startPos) input
            |> Option.map (pipeToDirs >> List.contains (reverseDir dir))
            |> Option.defaultValue false)

    let generator (dir, pos) =
        let next = dirToDelta dir |> Tuple2.add pos

        if next = startPos then
            None
        else
            let nextPipe =
                Array.item2dp next input |> pipeToDirs |> List.find ((<>) (reverseDir dir))

            Some(next, (nextPipe, next))

    startPos :: (List.unfold generator (startDir, startPos))

let solve1 input = (getPipe input |> List.length) / 2

let solve2 input =
    let pipe = getPipe input

    let corners =
        pipe |> List.filter (fun p -> not <| List.contains (Array.item2dp p input) [ '-'; '|' ])

    Geometry.axisAlignedPolygonArea corners - List.length pipe

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "..F7."; ".FJ|."; "SJ.L7"; "|F--J"; "LJ..."; "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 8

    let input2 =
        [| "FF7FSF7F7F7F7F7F---7"
           "L|LJ||||||||||||F--J"
           "FL-7LJLJ||||||LJL-77"
           "F--JF--7||LJLJ7F7FJ-"
           "L---JF-JLJ.||-FJLJJ7"
           "|F|F-JF---7F7-L7L|7|"
           "|FFJF7L7F-JF7|JL---7"
           "7-L-JL7||F7|L7F-7F7|"
           "L.L7LFJ|||||FJL7||LJ"
           "L7JLJL-JLJLJL--JLJ.L"
           "" |]

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input2 |> should equal 10
