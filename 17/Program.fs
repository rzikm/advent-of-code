open System.IO;
open FParsec;

let pRange = pint32 .>> pstring ".." .>>.pint32
let pInput = pstring "target area: x=" >>. pRange .>> pstring ", y=" .>>. pRange

let input =
    match run pInput (File.ReadAllText("input.txt")) with
    | Success (res, _, _) -> res;
    | Failure (err, _, _) -> failwith err

let alterVelocity (x, y) = (x - sign x, y - 1)

let addTuple (x0, x1) (y0, y1) = (x0 + y0, x1 + y1)

let isInBounds ((x0, x1), (y0, y1)) (x, y) = x0 <= x && x <= x1 && y0 <= y && y <= y1

let isBeyond ((_, maxX), (minY, _)) (x, y) = x > maxX || y < minY

let doesThrowHit target initVel =
    let rec doesThrowHit' pos vel =
        let nextPos = addTuple pos vel
        if isInBounds target nextPos then
            true
        else if not <| isBeyond target nextPos then
            doesThrowHit' nextPos (alterVelocity vel)
        else
            false
    doesThrowHit' (0, 0) initVel

let part1 = // just use closed-form formula...
    let (_, (maxY, _)) = input
    let initV = -maxY - 1 // we want last iter to get it from 0 to the lower bound of target
    (initV + 1) * initV / 2

printfn "%A" part1

let part2 =
    let ((_, maxInitX), (minY, _)) = input
    let maxInitY = -minY - 1
    List.allPairs [1..maxInitX] [minY..maxInitY] |> Seq.filter (doesThrowHit input) |> Seq.length

printfn "%A" part2
