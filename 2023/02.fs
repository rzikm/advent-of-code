module AoC202302

open AdventOfCode
open FSharpPlus
open FParsec

type Draw = { Red: int; Green: int; Blue: int }
type Game = { Index: int; Draws: Draw list }

let drawMap2 f (l: Draw) (r: Draw) = { Red = f l.Red r.Red; Green = f l.Green r.Green; Blue = f l.Blue r.Blue }

let parser =
    let cube = choice [ 
            pint32 .>>? pstring " red" |>> fun count -> { Red = int count; Green = 0; Blue = 0 }
            pint32 .>>? pstring " green" |>> fun count -> { Red = 0; Green = int count; Blue = 0 }
            pint32 .>>? pstring " blue" |>> fun count -> { Red = 0; Green = 0; Blue = int count }
        ]

    let draw = sepBy1 cube (skipString ", ") |>> List.reduce (drawMap2 (+))

    let game =
        pstring "Game " >>. pint32 .>> pstring ": " .>>. sepBy1 draw (skipString "; ")
        |>> fun (index, draws) -> { Index = index; Draws = draws }

    sepEndBy1 game (skipChar '\n')

let solve1 input =
    let testDraw = { Red = 12; Green = 13; Blue = 14 }

    let isPossible cubes game =
        let isPossibleDraw (draw: Draw) =
            testDraw.Red >= draw.Red && testDraw.Green >= draw.Green && testDraw.Blue >= draw.Blue

        game.Draws |> List.reduce (drawMap2 max) |> isPossibleDraw

    input |> List.filter (isPossible testDraw) |> List.sumBy _.Index

let solve2 input =
    let powerOfGame (game: Game) =
        game.Draws |> List.reduce (drawMap2 max) |> (fun draw -> draw.Red * draw.Green * draw.Blue)

    input |> List.sumBy powerOfGame

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
           "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
           "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
           "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
           "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 8

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 2286
