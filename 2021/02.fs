module AoC202102

open AdventOfCode
open FSharpPlus
open FParsec

type Command =
    | Up of int
    | Down of int
    | Forward of int

// parsing
let parser =
    let pUp = pstring "up" >>. spaces1 >>. pint32 |>> Up
    let pDown = pstring "down" >>. spaces1 >>. pint32 |>> Down
    let pForward = pstring "forward" >>. spaces1 >>. pint32 |>> Forward
    let pCommand = pUp <|> pDown <|> pForward
    many (pCommand .>> spaces)

let solve1 input =
    let applyState (depth, dist) (c: Command) =
        match c with
        | Up x -> (depth - x, dist)
        | Down x -> (depth + x, dist)
        | Forward x -> (depth, dist + x)

    let (depth, dist) = input |> List.fold applyState (0, 0)
    depth * dist

let solve2 input =
    let applyState (depth, dist, aim) (c: Command) =
        match c with
        | Up x -> (depth, dist, aim - x)
        | Down x -> (depth, dist, aim + x)
        | Forward x -> (depth + x * aim, dist + x, aim)

    let (depth, dist, _) = input |> List.fold applyState (0, 0, 0)
    depth * dist

let solution = makeSolution parser solve1 solve2
