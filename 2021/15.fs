module AoC202115

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pDigit = anyOf ("0123456789" |> String.toSeq) |>> (fun c -> int c - int '0')
    let pLine = many1 pDigit |>> Array.ofSeq
    sepEndBy pLine spaces |>> Array.ofSeq

let getNeighbors fCost (maxX, maxY) (x, y) =
    Tuple2.neighbors4 (x, y)
    |> Seq.filter (fun (xx, yy) -> xx >= 0 && yy >= 0 && xx <= maxX && yy <= maxY)
    |> Seq.map (fun c -> (c, fCost c))

let getCost (input: int [] []) (x, y) = input.[y].[x]

let heuristic (maxX, maxY) (x, y) = abs (maxX - x) + abs (maxY - y)

let solve1 (input: int [] []) =
    let finish = (input.[0].Length - 1, input.Length - 1)
    let fNeighbors = getNeighbors (getCost input) finish
    let fHeuristic = heuristic finish

    Graph.aStar fHeuristic fNeighbors ((=) finish) [ (0, 0) ] |> Option.get |> snd

let solve2 (input: int [] []) =
    let finish = (input.[0].Length * 5 - 1, input.Length * 5 - 1)

    let getCost' (x, y) =
        let baseCost = getCost input (x % input.[0].Length, y % input.Length) - 1
        (baseCost + x / input.[0].Length + y / input.Length) % 9 + 1

    let fNeighbors = getNeighbors getCost' finish
    let fHeuristic = heuristic finish

    Graph.aStar fHeuristic fNeighbors ((=) finish) [ (0, 0) ] |> Option.get |> snd

let solution = makeSolution () parser solve1 solve2
