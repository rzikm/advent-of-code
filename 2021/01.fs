module AoC202101

open AdventOfCode
open FParsec
open FSharpPlus

let parser = sepEndBy pint32 spaces

let uncurry f (a, b) = f a b

let solve1 input =
    input |> List.pairwise |> List.filter (uncurry (<)) |> List.length

let solve2 input =
    input |> List.windowed 3 |> List.map List.sum |> solve1

let solution = makeSolution parser solve1 solve2
