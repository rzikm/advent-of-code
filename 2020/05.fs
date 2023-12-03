module AoC202005

open AdventOfCode
open FSharpPlus
open FParsec
open Utils

let parser = sepEndBy1 (anyString 10) (pchar '\n')

let getSeat (seat: string) =
    let row =
        seat.Substring(0, 7) |> String.map (fun c -> if c = 'F' then '0' else '1') |> parseInt 2

    let col =
        seat.Substring(7) |> String.map (fun c -> if c = 'L' then '0' else '1') |> parseInt 2

    (row, col)

let getSeatId seat =
    let row, col = getSeat seat
    row * 8 + col

let part1 input = input |> Seq.map getSeatId |> Seq.max

let part2 input =
    (input |> Seq.map getSeatId |> Seq.sort |> Seq.pairwise |> Seq.find (fun (l, r) -> r - l > 1) |> fst) + 1

let solution = makeSolution () parser part1 part2
