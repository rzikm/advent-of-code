module AoC202002

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let line =
        tuple3
            (pint32 .>> pchar '-' .>>. pint32)
            (spaces >>. anyChar .>> pchar ':')
            (spaces >>. many1CharsTill anyChar (pchar '\n'))

    many line

let isMatchEasy (min, max) c pass =
    let count =
        pass |> Seq.countBy id |> Seq.tryFind (fst >> (=) c) |> Option.map snd |> Option.defaultValue 0

    min <= count && count <= max

let isMatchHard (i0, i1) c (pass: string) =
    (pass[i0 - 1] = c) <> (pass[i1 - 1] = c)

let solve matcher input =
    input |> Seq.filter (fun (r, c, p) -> matcher r c p) |> Seq.length

let solution = makeSolution () parser (solve isMatchEasy) (solve isMatchHard)
