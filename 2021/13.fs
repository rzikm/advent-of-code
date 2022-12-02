module AoC202113

open AdventOfCode
open FSharpPlus
open FParsec
open Utils

type Point = int32 * int32

type Fold =
    | FoldX of int
    | FoldY of int

let parser =
    let pDot = pint32 .>> pchar ',' .>>. pint32

    let pFold =
        pstring "fold along " >>. ((pstring "x=" >>. pint32 |>> FoldX) <|> (pstring "y=" >>. pint32 |>> FoldY))

    sepEndBy pDot (pchar '\n') .>> (pchar '\n') .>>. sepEndBy pFold (pchar '\n')

let fold ds f =
    let mapper, selector, bound =
        match f with
        | FoldX xx -> (fun (x, y) -> (2 * xx - x, y)), fst, xx
        | FoldY yy -> (fun (x, y) -> (x, 2 * yy - y)), snd, yy

    let toFold, rest = ds |> List.partition (selector >> (<) bound)
    toFold |> List.map mapper |> List.append rest |> List.distinct

let solve1 (dots, folds) =
    fold dots (List.head folds) |> List.length

let solve2 (dots, folds) =
    let finalDots = List.fold fold dots folds |> Set.ofList

    let maxX, maxY =
        finalDots |> Seq.map fst |> Seq.max, finalDots |> Seq.map snd |> Seq.max

    "\n"
    + (Seq.init (maxY + 1) (fun y ->
        String.init (maxX + 1) (fun x -> if Set.contains (x, y) finalDots then "#" else " "))
       |> String.concat "\n")

let solution = makeSolution parser solve1 solve2
