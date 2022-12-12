module Graph

open System.Collections.Generic

let aStar
    (fHeuristic: 'vertex -> ^cost)
    (fNeighbors: 'vertex -> ('vertex * ^cost) seq)
    (fFinish: 'vertex -> bool)
    (start: 'vertex)
    =

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
            v :: tail
        else
            match preds.TryGetValue v with
            | false, _ -> v :: tail
            | true, (_, x) -> finalPath x (v :: tail)

    let rec doSearch () =
        let (vCost, v, from) = fringe.Dequeue()
        // found shortest path to 'v' via 'from'
        preds.Item v <- (vCost, from)

        if fFinish v then
            (finalPath v [], vCost)
        else
            for (n, nCost) in fNeighbors v do
                addNeighbor v (n, vCost + nCost)

            doSearch ()

    doSearch ()