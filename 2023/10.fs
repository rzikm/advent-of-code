module AoC202310

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    sepEndBy1 (many1 (anyOf "|-LJ7F.S") |>> Array.ofList) (skipChar '\n') |>> Array.ofList

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

let fNeighborsByDestWithDir map point =
    seq {
        for dir in [ Up; Down; Left; Right ] do
            let delta = dirToDelta dir
            let (dx, dy) = Tuple2.add point delta

            if Array.tryItem2d dx dy map
               |> Option.map (pipeToDirs >> List.contains (reverseDir dir))
               |> Option.defaultValue false then
                yield dir, (dx, dy)
    }

let fNeighborsByDest map point =
    fNeighborsByDestWithDir map point |> Seq.map snd

let fNeighborsByPipe map (px, py) =
    Array.item2d px py map
    |> pipeToDirs
    |> List.map (fun dir -> Tuple2.add (px, py) (dirToDelta dir))
    |> List.filter (fun (x, y) -> Array.tryItem2d x y map |> Option.isSome)

let findStartPos input =
    input
    |> Array.indexed
    |> Array.pick (fun (r, row) -> Array.tryFindIndex ((=) 'S') row |> Option.map (fun i -> i, r))

let fneihbors input (x, y) =
    if Array.item2d x y input = 'S' then
        fNeighborsByDest input (x, y)
    else
        fNeighborsByPipe input (x, y) |> Seq.ofList

let solve1 input =
    let startPos = findStartPos input
    let pipe = Graph.connectedComponent (fneihbors input) startPos
    List.length pipe / 2

let solve2 input =
    let startPos = findStartPos input
    let startPipe = fNeighborsByDestWithDir input startPos |> Seq.map fst |> List.ofSeq

    let pipeToDirs = function
        | 'S' -> startPipe
        | c -> pipeToDirs c

    let pipe = Graph.connectedComponent (fneihbors input) startPos 
    let x0, y0 = pipe |> List.reduce (Tuple2.map2 min)
    let x1, y1 = pipe |> List.reduce (Tuple2.map2 max)

    let isOnPipe =
        let pipeAsSet = pipe |> Set.ofList
        fun p -> Set.contains p pipeAsSet

    [y0 .. y1]
    |> List.sumBy (fun y ->
        [x0 .. x1] |> List.fold (fun (count, up, down) x ->
            if isOnPipe (x, y) then
                let dirs = pipeToDirs (Array.item2d x y input)
                count, up + (List.filter ((=) Up) dirs |> List.length), down + (List.filter ((=) Down) dirs |> List.length)
            else
                // if up and down are both odd, then we are inside the area
                count + (up % 2) * (down % 2), up, down
        ) (0, 0, 0) |> fun (count, _, _) -> count)

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
