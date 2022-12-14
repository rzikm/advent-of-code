module AoC201503

open AdventOfCode
open FSharpPlus
open FParsec

type Direction =
    | North
    | East
    | West
    | South

let parser =
    many1 (
        choice [ charReturn '^' North
                 charReturn '>' East
                 charReturn '<' West
                 charReturn 'v' South ]
    )

let dirToVector =
    function
    | North -> (0, 1)
    | East -> (1, 0)
    | West -> (-1, 0)
    | South -> (0, -1)

let solve1 input =
    List.scan (fun s dir -> Tuple2.add s (dirToVector dir)) (0, 0) input |> List.distinct |> List.length

let solve2 input =
    Seq.scan
        (fun (santa, robot, santaToMove) dir ->
            match santaToMove with
            | true ->
                let newSanta = Tuple2.add santa (dirToVector dir)
                (newSanta, robot, false)
            | false ->
                let newRobot = dirToVector dir |> Tuple2.add robot
                (santa, newRobot, true))
        ((0, 0), (0, 0), true)
        input
    |> Seq.collect (fun (s, r, _) -> [ s; r ])
    |> Seq.distinct
    |> Seq.length

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Theory>]
    [<InlineData(">", 2)>]
    [<InlineData("^>v<", 4)>]
    [<InlineData("^v^v^v^v^v", 2)>]
    let ``Example part 1`` input expected =
        testPart1 solution [| input |] |> should equal expected


    [<Theory>]
    [<InlineData("^v", 3)>]
    [<InlineData("^>v<", 3)>]
    [<InlineData("^v^v^v^v^v", 11)>]
    let ``Example part 2`` input expected =
        testPart2 solution [| input |] |> should equal expected
