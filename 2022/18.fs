module AoC202218

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    sepEndBy1 (tuple3 (pint32 .>> pchar ',') (pint32 .>> pchar ',') pint32) (pchar '\n')

let solve1 input =
    let allCubes = Set.ofList input

    input
    |> Seq.ofList
    |> Seq.collect (Tuple3.neighbors6 >> Seq.filter (flip Set.contains allCubes >> not))
    |> Seq.length

let solve2 input =
    let allCubes = Set.ofList input

    let neighborspp =
        allCubes |> Seq.collect Tuple3.neighbors26 |> Set.ofSeq |> flip Set.difference allCubes

    let fNeighbors = Tuple3.neighbors6 >> Seq.filter (flip Set.contains neighborspp)

    let start = neighborspp |> Seq.maxBy (fun (x, _, _) -> x)
    let connected = Graph.connectedComponent fNeighbors start |> Set.ofList

    input
    |> Seq.ofList
    |> Seq.collect (Tuple3.neighbors6 >> Seq.filter (flip Set.contains allCubes >> not))
    |> Seq.filter (flip Set.contains connected)
    |> Seq.length

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "2,2,2"
           "1,2,2"
           "3,2,2"
           "2,1,2"
           "2,3,2"
           "2,2,1"
           "2,2,3"
           "2,2,4"
           "2,2,6"
           "1,2,5"
           "3,2,5"
           "2,1,5"
           "2,3,5"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 64

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 58
