open System.IO;
open FSharpx.Collections;

let input =
    File.ReadLines("input.txt")
    |> Seq.map (Seq.map (fun x -> int x - int '0') >> Array.ofSeq) |> Array.ofSeq

let graphAStar
    (fHeuristic : 'vertex -> ^cost)
    (fNeighbors : 'vertex -> ('vertex * ^cost) list)
    (start      : 'vertex)
    (finish     : 'vertex) =

    let addNeighbor v (n, nCost) fr preds =
        match Map.tryFind n preds with
        | Some _ -> fr // already processed with better path
        | _ -> // found a better path to n yet via v
            Heap.insert (nCost + fHeuristic n, nCost, n, v) fr

    let predecessors = Map.empty
    let fringe = fNeighbors start |> List.fold (fun fr (n, c) -> addNeighbor start (n, c) fr predecessors) (Heap.empty false)

    let rec finalPath v tail predecessors =
        if v = start then
            v::tail
        else
            match Map.tryFind v predecessors with
            | None -> v::tail
            | Some x -> finalPath x (v::tail) predecessors

    let rec doSearch (fringe : Heap<'cost * 'cost * 'vertex * 'vertex>) (preds : Map<'vertex, 'vertex>) =
        let (_, vCost, v, from), fringe = Heap.uncons fringe
        if Map.containsKey v preds then
            doSearch fringe preds
        else
            // found shortest path to 'v' via 'from'
            let preds = Map.add v from preds
            if v = finish then
                (finalPath v [] preds, vCost)
            else
                let fringe' =
                    fNeighbors v |> List.fold (fun fr (n, nCost) ->
                        let nCost = vCost + nCost
                        addNeighbor v (n, nCost) fr preds
                        ) fringe
                doSearch fringe' preds
    doSearch fringe predecessors

let getNeighbors fCost (maxX, maxY) (x, y) =
    [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]
    |> List.filter (fun (xx, yy) -> xx >= 0 && yy >= 0 && xx <= maxX && yy <= maxY)
    |> List.map (fun c -> (c, fCost c))

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

