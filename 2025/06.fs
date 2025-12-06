module AoC202506

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let grid = ParseUtils.grid (digit <|> pchar ' ')
    let ops = ParseUtils.skipSpaces >>. ParseUtils.sepBySpaces (anyOf "*+")
    grid .>>. ops

let foldWithOp c list =
    match c with
    | '*' -> List.fold (*) 1L list
    | '+' -> List.fold (+) 0L list
    | _ -> failwith "unexpected operator"

let solve1 (grid, ops) =
    let grid =
        grid
        |> Array.map (Array.split [ [| ' ' |] ] >> Seq.choose (String.ofArray >> tryParse<int64>) >> Seq.toList)
        |> List.ofArray

    List.transpose grid |> List.zip ops |> List.sumBy (fun (op, col) -> foldWithOp op col)

let solve2 (grid, ops) =
    let grid =
        grid
        |> Array.transpose
        |> Array.map (Array.split [ [| ' ' |] ] >> Seq.choose (String.ofArray >> tryParse<int64>) >> Seq.toList)
        |> List.ofArray
        |> List.split [ [ [] ] ]
        |> List.ofSeq
        |> List.map (List.map List.exactlyOne)

    grid |> Seq.zip ops |> Seq.sumBy (fun (op, col) -> foldWithOp op col)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "123 328  51 64 "; " 45 64  387 23 "; "  6 98  215 314"; "*   +   *   +  " |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 4277556L

    [<Fact>]
    let ``Split test`` () =
        let data = [ [ 1 ]; [ 2 ]; []; [ 3 ]; [ 4 ] ]
        let result = data |> List.split [ [ [] ] ] |> List.ofSeq
        result |> should equal [ [ [ 1 ]; [ 2 ] ]; [ [ 3 ]; [ 4 ] ] ]

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 3263827L
