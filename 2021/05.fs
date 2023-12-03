module AoC202105

open AdventOfCode
open FSharpPlus
open FParsec

type Point = int * int
type Line = Point * Point

let parser =
    let pPoint = pint32 .>> pchar ',' .>>. pint32 |>> Point
    let pLine = pPoint .>> pstring " -> " .>>. pPoint |>> Line
    sepEndBy pLine (pchar '\n')

let solve1 input =
    let lineToPoints ((x1, y1), (x2, y2)) =
        let dx, dy = (x2 - x1, y2 - y1)
        let len = max (abs dx) (abs dy)
        let dirx, diry = (dx / len), (dy / len)

        if (abs dx) <> (abs dy) then
            seq { for i in 0..len -> (x1 + i * dirx, y1 + i * diry) }
        else
            seq []

    input |> Seq.collect lineToPoints |> Seq.countBy id |> Seq.filter (snd >> (<>) 1) |> Seq.length

let solve2 input =
    let lineToPoints ((x1, y1), (x2, y2)) =
        let dx, dy = (x2 - x1, y2 - y1)
        let len = max (abs dx) (abs dy)
        let dirx, diry = (dx / len), (dy / len)
        seq { for i in 0..len -> (x1 + i * dirx, y1 + i * diry) }

    input |> Seq.collect lineToPoints |> Seq.countBy id |> Seq.filter (snd >> (<>) 1) |> Seq.length

let solution = makeSolution () parser solve1 solve2
