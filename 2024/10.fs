module AoC202410

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.grid (satisfy isDigit |>> (fun s -> int s - int '0'))

let solve score input =
    let trailheads =
        Array.allIndexes2d input |> filter (flip Array.item2dp input >> (=) 0)

    let tops =
        Array.allIndexes2d input |> filter (flip Array.item2dp input >> (=) 9) |> Set.ofSeq

    let fNeighbors pos =
        let atPos = Array.item2dp pos input

        Tuple2.neighbors4 pos
        |> Seq.filter (fun p ->
            Array.tryItem2dp p input |> Option.map (fun v -> (v - atPos) = 1) |> Option.defaultValue false)
        |> Seq.map (fun p -> p, 1)

    trailheads |> Seq.sumBy (score fNeighbors tops)

let solve1 input =
    let score fNeighbors tops start =
        Graph.flood fNeighbors start |> Seq.filter (fun (p, _) -> Set.contains p tops) |> Seq.length

    solve score input

let solve2 input =
    let score fNeighbors tops start =
        Graph.allPathsBetween fNeighbors (flip Seq.contains tops) start |> Seq.length

    solve score input

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "89010123"
           "78121874"
           "87430965"
           "96549874"
           "45678903"
           "32019012"
           "01329801"
           "10456732" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 36

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 81
