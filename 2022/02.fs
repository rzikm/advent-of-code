module AoC202202

open AdventOfCode
open FParsec

type Play =
    | Rock
    | Paper
    | Scissors

let parser =
    let p1 =
        choice [ (pchar 'A' >>% Rock); (pchar 'B' >>% Paper); (pchar 'C' >>% Scissors) ]

    let p2 =
        choice [ (pchar 'X' >>% Rock); (pchar 'Y' >>% Paper); (pchar 'Z' >>% Scissors) ]

    sepEndBy1 (p1 .>> (pchar ' ') .>>. p2) (pchar '\n')

let solve1 =
    let getPlayResultPoints =
        function
        | (Rock, Paper)
        | (Paper, Scissors)
        | (Scissors, Rock) -> 6 // Win
        | (Rock, Rock)
        | (Paper, Paper)
        | (Scissors, Scissors) -> 3 // Draw
        | _ -> 0 // Loss

    let evalMyPlay =
        function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    List.sumBy (fun (l, r) -> getPlayResultPoints (l, r) + evalMyPlay r)

let solve2 input =
    let selectPlay =
        function
        | (Rock, Rock) -> Scissors // Rock = Lose
        | (Paper, Rock) -> Rock
        | (Scissors, Rock) -> Paper
        | (Rock, Scissors) -> Paper // Scissors = Win
        | (Paper, Scissors) -> Scissors
        | (Scissors, Scissors) -> Rock
        | (x, Paper) -> x // Paper = Draw

    input |> List.map (fun (l, r) -> (l, selectPlay (l, r))) |> solve1

let solution = makeSolution parser (solve1) (solve2)

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``Example part 1`` () =
        let input = [| "A Y"; "B X"; "C Z" |]
        testPart1 solution input |> should equal 15

    [<Fact>]
    let ``Example part 2`` () =
        let input = [| "A Y"; "B X"; "C Z" |]
        testPart2 solution input |> should equal 12
