module AoC202209

open Utils
open AdventOfCode
open FSharpPlus
open FParsec

type Direction =
    | Up
    | Right
    | Down
    | Left

let parser =
    let pdir =
        choice [ charReturn 'U' Up
                 charReturn 'R' Right
                 charReturn 'D' Down
                 charReturn 'L' Left ]

    sepEndBy1 (pdir .>> pchar ' ' .>>. pint32) (pchar '\n')

let solve ropeLen input =
    let dist l r =
        Tuple2.sub l r |> Tuple2.map abs |> uncurry max

    let move (head, tail) dir =
        let delta =
            match dir with
            | Up -> (0, 1)
            | Down -> (0, -1)
            | Left -> (-1, 0)
            | Right -> (1, 0)

        let moveTail prevLink knot =
            if dist prevLink knot > 1 then
                Tuple2.sub prevLink knot |> Tuple2.map sign |> Tuple2.add knot
            else
                knot

        let newHead = Tuple2.add head delta
        (newHead, List.scan moveTail newHead tail |> List.tail)

    input
    |> List.toSeq
    |> Seq.collect (uncurry (flip Seq.replicate))
    |> Seq.scan move ((0, 0), List.replicate (ropeLen - 1) (0, 0))
    |> Seq.map (snd >> List.last)
    |> Set.ofSeq
    |> Set.count

let solution = makeSolution parser (solve 2) (solve 10)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "R 4"; "U 4"; "L 3"; "D 1"; "R 4"; "D 1"; "L 5"; "R 2"; "" |]
    let input2 = [| "R 5"; "U 8"; "L 8"; "D 3"; "R 17"; "D 10"; "L 25"; "U 20"; "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 13

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input2 |> should equal 36
