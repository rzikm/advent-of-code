module Graph

open System.Collections.Generic

let inline aStar
    (fHeuristic: 'vertex -> ^cost)
    (fNeighbors: 'vertex -> ('vertex * ^cost) seq)
    (fFinish: 'vertex -> bool)
    (startVertices: 'vertex list)
    =

    let preds = Dictionary<'vertex, 'cost * 'vertex>()
    let fringe = PriorityQueue<'cost * 'vertex * 'vertex, 'cost>()
    let starts = Set.ofList startVertices

    let addNeighbor v (n, nCost) =
        match preds.TryGetValue n with
        | true, (nPrevCost, _) when nPrevCost <= nCost -> () // already have a better path to n
        | _ -> // found a better path to n via v
            preds.Item n <- (nCost, v)
            fringe.Enqueue((nCost, n, v), (nCost + fHeuristic n))

    startVertices |> Seq.iter (fun v -> fNeighbors v |> Seq.iter (addNeighbor v))

    let rec finalPath v tail =
        if Set.contains v starts then
            v :: tail
        else
            match preds.TryGetValue v with
            | false, _ -> v :: tail
            | true, (_, x) -> finalPath x (v :: tail)

    let rec doSearch () =
        match fringe.TryDequeue () with
        | true, (vCost, v, from), _ ->
            // found shortest path to 'v' via 'from'
            preds.Item v <- (vCost, from)

            if fFinish v then
                Some (finalPath v [], vCost)
            else
                for (n, nCost) in fNeighbors v do
                    if not <| Set.contains n starts then
                        addNeighbor v (n, vCost + nCost)

                doSearch ()
        | _ -> None

    doSearch ()

let shortestPaths
    (fNeighbors: 'vertex -> ('vertex * ^cost) seq)
    (start: 'vertex)
    (ends: 'vertex list)
    =

    let preds = Dictionary<'vertex, 'cost * 'vertex>()
    let fringe = PriorityQueue<'cost * 'vertex * 'vertex, 'cost>()

    let addNeighbor v (n, nCost) =
        match preds.TryGetValue n with
        | true, (nPrevCost, _) when nPrevCost <= nCost -> () // already have a better path to n
        | _ -> // found a better path to n via v
            fringe.Enqueue((nCost, n, v), nCost)

    fNeighbors start |> Seq.iter (addNeighbor start)

    let rec finalPath v tail =
        if v = start then
            v :: tail
        else
            match preds.TryGetValue v with
            | false, _ -> v :: tail
            | true, (_, x) -> finalPath x (v :: tail)

    let rec doSearch ends acc =
        if ends = [] then acc
        else
            let (vCost, v, from) = fringe.Dequeue()

            if not <| preds.ContainsKey(v) then
                // found shortest path to 'v' via 'from'
                preds.Item v <- (vCost, from)

                for (n, nCost) in fNeighbors v do
                    if v <> start then
                        addNeighbor v (n, vCost + nCost)

                if List.contains v ends then
                    doSearch (List.except [v] ends) ((v, (finalPath v [], vCost)):: acc)
                else
                    doSearch ends acc
            else
                doSearch ends acc

    doSearch ends []

let connectedComponent
    (fNeighbors: 'vertex -> ('vertex) seq)
    (start: 'vertex )
    =
    let visited = HashSet<'vertex>()
    let fringe = Queue<'vertex>()

    let addNeighbor v n =
        if visited.Add(n) then
            fringe.Enqueue(n)

    visited.Add(start) |> ignore
    fringe.Enqueue(start)

    let rec doSearch () =
        match fringe.TryDequeue () with
        | true, v ->

            for n in fNeighbors v do
                addNeighbor v n

            doSearch()
        | false, _->
            Seq.toList visited

    doSearch ()
