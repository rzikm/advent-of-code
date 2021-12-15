open System.IO;
open System.Collections.Generic;
open FSharpx.Collections;

let input =
    File.ReadLines("input.txt")
    |> Seq.map (Seq.map (fun x -> int x - int '0') >> Array.ofSeq) |> Array.ofSeq

let inline graphAStar
    (fHeuristic : 'vertex -> ^cost)
    (fNeighbors : 'vertex -> ('vertex * ^cost) seq)
    (start      : 'vertex)
    (finish     : 'vertex) =

    let preds = Dictionary<'vertex, 'cost * 'vertex>()
    let fringe = PriorityQueue<'cost * 'vertex * 'vertex, 'cost>()

    let addNeighbor v (n, nCost) =
        match preds.TryGetValue n with
        | true, (nPrevCost, _) when nPrevCost <= nCost -> () // already have a better path to n
        | _ -> // found a better path to n via v
            preds.Item n <- (nCost, v)
            fringe.Enqueue((nCost, n, v), (nCost + fHeuristic n))

    fNeighbors start |> Seq.iter (addNeighbor start)

    let rec finalPath v tail =
        if v = start then
            v::tail
        else
            match preds.TryGetValue v with
            | false, _ -> v::tail
            | true, (_, x) -> finalPath x (v::tail)

    let rec doSearch () =
        let (vCost, v, from) = fringe.Dequeue()
        // found shortest path to 'v' via 'from'
        preds.Item v <- (vCost, from)

        if v = finish then
            (finalPath v [], vCost)
        else
            for (n, nCost) in fNeighbors v do
                addNeighbor v (n, vCost + nCost)

            doSearch ()
    doSearch ()

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

    graphAStar fHeuristic fNeighbors (0, 0) finish |> snd

printfn "%A" part1

let part2 =
    let finish = (input.[0].Length * 5 - 1, input.Length * 5 - 1)
    let getCost' (x, y) =
        let baseCost = getCost (x % input.[0].Length, y % input.Length) - 1
        (baseCost + x / input.[0].Length + y / input.Length) % 9 + 1
    let fNeighbors = getNeighbors getCost' finish
    let fHeuristic = heuristic finish

    graphAStar fHeuristic fNeighbors (0, 0) finish |> snd

printfn "%A" part2

