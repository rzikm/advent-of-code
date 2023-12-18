module AoC202201

open AdventOfCode
open FParsec
open FSharpPlus

let parser =
    let pline = pint32
    let pgroup = sepEndBy1 pline (pchar '\n')
    sepEndBy1 pgroup (pchar '\n')

let solve count input =
    List.map List.sum input |> List.sortBy (~-) |> List.take count |> List.sum

let solution = makeSolution () parser (solve 1) (solve 3)
