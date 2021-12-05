open FParsec;
open System.IO;

type Point = int * int
type Line = Point * Point

let pPoint = pint32 .>> pchar ',' .>>. pint32 |>> Point
let pLine = pPoint .>> pstring " -> " .>>. pPoint |>> Line
let pLineList = sepEndBy pLine (pchar '\n')

let input =
    match run pLineList (File.ReadAllText("input.txt")) with
    | Success(res, _, _) -> res
    | Failure(err, _, _) -> failwith err

let part1 =
    let lineToPoints ((x1, y1), (x2, y2)) =
        let dx, dy = (x2 - x1, y2 - y1)
        let len = max (abs dx) (abs dy)
        let dirx, diry = (dx / len), (dy / len)
        if (abs dx) <> (abs dy) then
            seq { for i in 0..len -> (x1 + i * dirx, y1 + i * diry)}
        else
            seq []

    input
    |> Seq.collect lineToPoints
    |> Seq.countBy id
    |> Seq.filter (snd >> (<>) 1)
    |> Seq.length

printfn "%A" part1

let part2 =
    let lineToPoints ((x1, y1), (x2, y2)) =
        let dx, dy = (x2 - x1, y2 - y1)
        let len = max (abs dx) (abs dy)
        let dirx, diry = (dx / len), (dy / len)
        seq { for i in 0..len -> (x1 + i * dirx, y1 + i * diry)}

    input
    |> Seq.collect lineToPoints
    |> Seq.countBy id
    |> Seq.filter (snd >> (<>) 1)
    |> Seq.length

printfn "%A" part2
