module AoC202106

open AdventOfCode
open FSharpPlus
open FParsec

let parser = sepBy pint32 (pchar ',') |>> Array.ofList

let run iterCount input =
    let decrement age =
        match age with
        | (0, c) -> [ (6, c); (8, c) ]
        | (x, c) -> [ (x - 1, c) ]

    let fold (state: (int * int64) list) _ =
        let sixes, rest = List.collect decrement state |> List.partition (fst >> (=) 6)
        List.append rest [ (6, List.sumBy (snd) sixes) ]

    let grouped =
        input |> List.ofArray |> List.countBy id |> List.map (fun (x, c) -> (x, int64 c))

    Seq.fold fold grouped (Seq.replicate iterCount 0) |> Seq.sumBy snd

let solution = makeSolution () parser (run 80) (run 256)
