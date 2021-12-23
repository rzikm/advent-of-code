open System.IO;
open FSharpx.Collections;

let input =
    File.ReadLines("input.txt")
    |> Seq.map (Seq.map (fun x -> int x - int '0') >> Array.ofSeq) |> Array.ofSeq

let getNeighbors fCost (maxX, maxY) (x, y) =
    seq { (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) }
    |> Seq.filter (fun (xx, yy) -> xx >= 0 && yy >= 0 && xx <= maxX && yy <= maxY)
    |> Seq.map (fun c -> (c, fCost c))

let getCost (x, y) = input.[y].[x]

let heuristic (maxX, maxY) (x, y) = abs (maxX - x) + abs (maxY - y)

let part1 =
    let finish = (input.[0].Length - 1, input.Length - 1)
    let fNeighbors = getNeighbors getCost finish
    let fHeuristic = heuristic finish

    Utils.Graph.graphAStar fHeuristic fNeighbors (0, 0) finish |> snd

printfn "%A" part1

let part2 =
    let finish = (input.[0].Length * 5 - 1, input.Length * 5 - 1)
    let getCost' (x, y) =
        let baseCost = getCost (x % input.[0].Length, y % input.Length) - 1
        (baseCost + x / input.[0].Length + y / input.Length) % 9 + 1
    let fNeighbors = getNeighbors getCost' finish
    let fHeuristic = heuristic finish

    Utils.Graph.graphAStar fHeuristic fNeighbors (0, 0) finish |> snd

printfn "%A" part2

