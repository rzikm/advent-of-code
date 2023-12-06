module AoC202306

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let times =
        skipString "Time:" >>. spaces >>. sepBy1 pint64 (many1 <| skipChar ' ') .>> spaces

    let distances =
        skipString "Distance:" >>. spaces >>. sepBy1 pint64 (many1 <| skipChar ' ') .>> spaces

    times .>>. distances |>> (fun (times, distances) -> List.zip times distances)

let winningRange (time, distance) =
    // we try to solve
    //     (time - x) * x > distance
    // which is equivalent to
    //     x^2 - time * x + distance < 0
    let (x1, x2) =
        Math.solveQuadraticEquation 1.0 (-double time) (double distance) |> Option.get

    // range repr is [begin, end)
    (floor x1 + 1.0 |> int64), ceil x2 |> int64

let solve1 input =
    input |> List.map winningRange |> List.map Range.length |> List.reduce (*)

let solve2 input =
    input
    |> List.map (Tuple2.map string)
    |> List.reduce (Tuple2.map2 (+))
    |> Tuple2.map int64
    |> winningRange
    |> Range.length

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "Time:      7  15   30"; "Distance:  9  40  200"; "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 288L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 71503L
