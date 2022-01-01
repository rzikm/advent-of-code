module AoC202001

open AdventOfCode
open FParsec
open FSharpPlus
open Utils

let parser = sepEndBy pint32 spaces

let solve count input =
    subsets count input
    |> filter (List.sum >> (=) 2020)
    |> Seq.head
    |> List.reduce (*)

let solution = makeSolution parser (solve 2) (solve 3)
