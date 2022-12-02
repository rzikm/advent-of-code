module AoC202107

open AdventOfCode
open FSharpPlus
open FParsec
open Utils

let parser = sepBy pint32 (pchar ',') |>> Array.ofList

let run cost input =
    let min = Array.min input
    let max = Array.max input

    seq { min..max } |> Seq.map (fun x -> Array.sumBy ((-) x >> abs >> cost) input) |> Seq.min

let solution = makeSolution parser (run id) (run (fun x -> x * (x + 1) / 2))
