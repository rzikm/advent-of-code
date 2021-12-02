open System.IO;
open FParsec;

type Command =
    | Up of int
    | Down of int
    | Forward of int

// parsing
let pUp = pstring "up" >>. spaces1 >>. pint32 |>> Up
let pDown = pstring "down" >>. spaces1 >>. pint32 |>> Down
let pForward = pstring "forward" >>. spaces1 >>. pint32 |>> Forward
let pCommand = pUp <|> pDown <|> pForward
let pCommandList = many (pCommand .>> spaces)

let commands =
    match run pCommandList (File.ReadAllText("input.txt")) with
    | Success(res, _, _) -> res
    | _ -> []

let part1 =
    let applyState (depth, dist) (c : Command) =
        match c with
        | Up x -> (depth - x, dist)
        | Down x -> (depth + x, dist)
        | Forward x -> (depth, dist + x)

    let (depth, dist) = commands |> List.fold applyState (0, 0)
    depth * dist

printfn "%d" part1

let part2 =
    let applyState (depth, dist, aim) (c : Command) =
        match c with
        | Up x -> (depth, dist, aim - x)
        | Down x -> (depth, dist, aim + x)
        | Forward x -> (depth + x * aim, dist + x, aim)

    let (depth, dist, _) = commands |> List.fold applyState (0, 0, 0)
    depth * dist

printfn "%d" part2
