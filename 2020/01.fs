module AoC202001

open AdventOfCode
open FParsec
open FSharpPlus
open Utils

let parser = sepEndBy pint32 spaces

let solve diff input =
    let count = match diff with | Easy -> 2 | Hard -> 3
    subsets count input |> filter (List.sum >> (=) 2020) |> Seq.head |> List.reduce (*)

let solution = makeSolution parser solve
