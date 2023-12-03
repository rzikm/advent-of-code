module AoC202017

open Utils
open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pcell =
        choice [ charReturn '.' false
                 charReturn '#' true ]

    sepEndBy (many1 pcell |>> Array.ofList) (pchar '\n') |>> Array.ofList |>> Array.singleton

let boolToInt =
    function
    | true -> 1
    | false -> 0

let nextCellState isLive liveNeighborCount =
    match isLive, liveNeighborCount with
    | _, 3 -> true
    | true, 2 -> true
    | _ -> false

let nextIteration3d input =
    let newz = Array.length input + 2
    let newy = (Array.item 0 input |> Array.length) + 2
    let newx = (Array.item2d 0 0 input |> Array.length) + 2

    Array.init newz (fun z ->
        Array.init newy (fun y ->
            Array.init newx (fun x ->
                let liveNeighborCount =
                    input |> Array.neighbors3d26 (x - 1) (y - 1) (z - 1) |> Seq.sumBy boolToInt

                let isLive =
                    Array.tryItem3d (x - 1) (y - 1) (z - 1) input |> Option.defaultValue false

                nextCellState isLive liveNeighborCount)))

let nextIteration4d input =
    let neww = Array.length input + 2
    let newz = (Array.item 0 input |> Array.length) + 2
    let newy = (Array.item2d 0 0 input |> Array.length) + 2
    let newx = (Array.item3d 0 0 0 input |> Array.length) + 2

    Array.init neww (fun w ->
        Array.init newz (fun z ->
            Array.init newy (fun y ->
                Array.init newx (fun x ->
                    let liveNeighborCount =
                        input |> Array.neighbors4d (x - 1) (y - 1) (z - 1) (w - 1) |> Seq.sumBy boolToInt

                    let isLive =
                        Array.tryItem4d (x - 1) (y - 1) (z - 1) (w - 1) input |> Option.defaultValue false

                    nextCellState isLive liveNeighborCount))))


let solve1 input =
    (nextIteration3d ^ 6) input |> Array.sumBy (Array.sumBy (Array.sumBy boolToInt))

let solve2 input =
    (nextIteration4d ^ 6) [| input |] |> Array.sumBy (Array.sumBy (Array.sumBy (Array.sumBy boolToInt)))

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| ".#."; "..#"; "###" |]

    let stringify grid =
        grid |> Array.map (Array.map (Array.map (fun c -> if c then "#" else ".") >> String.concat ""))

    [<Fact>]
    let ``First iteration on example input`` () =
        parseTestInput parser input
        |> nextIteration3d
        |> stringify
        |> should
            equal
            [| [| "....."
                  "....."
                  ".#..."
                  "...#."
                  "..#.."

                  |]
               [|

                  "....."
                  "....."
                  ".#.#."
                  "..##."
                  "..#.."

                  |]
               [| "....."
                  "....."
                  ".#..."
                  "...#."
                  "..#.."

                  |] |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 112
