module Graph

open System.Collections.Generic
open FSharpPlus

let rec foldPath fNeighbors start (next, cost) =
    match fNeighbors next |> Seq.filter (fun (n, _) -> n <> start) |> Seq.tryExactlyOne with
    | None -> next, cost
    | Some (n, c) -> foldPath fNeighbors next (n, cost + c)

let withPathFolding fShouldFold fNeighbors =
    let rec foldPath acc previous current =
        if not <| fShouldFold current then
            current, acc
        else
            match fNeighbors current |> Seq.filter (fun (n, _) -> n <> previous) |> Seq.tryExactlyOne with
            | None -> current, acc
            | Some (n, c) -> foldPath (acc + c) current n

    Utils.memoize <| fun from' -> from' |> fNeighbors |> Seq.map (fun (n, c) -> foldPath c from' n) |> Seq.cache

let inline aStar
    (fHeuristic: 'vertex -> int32)
    (fNeighbors: 'vertex -> ('vertex * int32) seq)
    (fFinish: 'vertex -> bool)
    (startVertices: 'vertex list)
    =

    let preds = Dictionary<'vertex, int32 * 'vertex>()
    let fringe = PriorityQueue<int32 * 'vertex * 'vertex, int32>()
    let starts = Set.ofList startVertices

    let addNeighbor v (n, nCost) =
        match preds.TryGetValue n with
        | true, (nPrevCost, _) when nPrevCost <= nCost -> () // already have a better path to n
        | _ -> // found a better path to n via v
            preds.Item(n) <- (nCost, v)
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
        match fringe.TryDequeue() with
        | true, (vCost, v, from), _ ->
            // found shortest path to 'v' via 'from'
            preds.Item(v) <- (vCost, from)

            if fFinish v then
                Some(finalPath v [], vCost)
            else
                for (n, nCost) in fNeighbors v do
                    if not <| Set.contains n starts then
                        addNeighbor v (n, vCost + nCost)

                doSearch ()
        | _ -> None

    doSearch ()

let shortestPaths (fNeighbors: 'vertex -> ('vertex * int32) seq) (start: 'vertex) (ends: 'vertex list) =

    let preds = Dictionary<'vertex, int32 * 'vertex>()
    let fringe = PriorityQueue<int32 * 'vertex * 'vertex, int32>()

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
        if ends = [] || fringe.Count = 0 then
            acc
        else
            let (vCost, v, from) = fringe.Dequeue()

            if not <| preds.ContainsKey(v) then
                // found shortest path to 'v' via 'from'
                preds.Item(v) <- (vCost, from)

                for (n, nCost) in fNeighbors v do
                    if v <> start then addNeighbor v (n, vCost + nCost)

                if List.contains v ends then
                    doSearch (List.except [ v ] ends) ((v, (finalPath v [], vCost)) :: acc)
                else
                    doSearch ends acc
            else
                doSearch ends acc

    doSearch ends []

let connectedComponent (fNeighbors: 'vertex -> ('vertex) seq) (start: 'vertex) =
    let visited = HashSet<'vertex>()
    let fringe = Queue<'vertex>()

    let addNeighbor n =
        if visited.Add(n) then fringe.Enqueue(n)

    visited.Add(start) |> ignore
    fringe.Enqueue(start)

    let rec doSearch () =
        match fringe.TryDequeue() with
        | true, v ->

            for n in fNeighbors v do
                addNeighbor n

            doSearch ()
        | false, _ -> Seq.toList visited

    doSearch ()

let flood (fNeighbors: 'vertex -> ('vertex * int) seq) (start: 'vertex) =
    let visited = HashSet<'vertex>()
    let fringe = PriorityQueue<'vertex, int32>()

    visited.Add(start) |> ignore
    fringe.Enqueue(start, 0)

    seq {
        yield (start, 0)

        while fringe.Count > 0 do
            let _, v, c = fringe.TryDequeue()

            for (n, cc) in fNeighbors v do
                match visited.Add(n) with
                | true ->
                    fringe.Enqueue(n, c + cc)
                    yield (n, c + cc)
                | false -> ()
    }
    |> Seq.cache

let allPathsBetween (fNeighbors: 'vertex -> ('vertex * int) seq) (fFinish: 'vertex -> bool) (start: 'vertex) =

    let stack = Stack<'vertex list * int>()
    stack.Push([ start ], 0)

    seq {
        while stack.Count > 0 do
            let visited, cost = stack.Pop()
            let current = List.head visited

            if fFinish current then
                yield visited |> List.rev, cost
            else
                fNeighbors current
                |> Seq.filter (fun (n, _) -> not <| List.contains n visited)
                |> Seq.iter (fun (n, c) -> stack.Push(n :: visited, cost + c))
    }
    |> Seq.cache

let findLongestPath (fNeighbors: 'vertex -> ('vertex * int) seq) (fFinish: 'vertex -> bool) (start: 'vertex) =
    allPathsBetween fNeighbors fFinish start |> Seq.maxBy snd

// Implements the Edmonds-Karp algorithm for finding the maximal flow in a network.
let inline findMaxFlow (fNeighbors: 'vertex -> ('vertex * 'capacity) seq) (source: 'vertex) (sink: 'vertex) =
    let flow: Dictionary<'vertex * 'vertex, _> = Dictionary()

    let getFlow a b =
        match flow.TryGetValue((a, b)) with
        | true, v -> v
        | _ -> LanguagePrimitives.GenericZero

    let updateFlow from' to' delta =
        let setflow a b value = flow.Item((a, b)) <- value

        getFlow from' to' + delta |> setflow from' to'
        getFlow to' from' - delta |> setflow to' from'

    let findpath () =
        let fNeighbors node =
            let neighbors = fNeighbors node

            neighbors
            |> Seq.filter (fun (n, capacity) -> capacity - getFlow node n > LanguagePrimitives.GenericZero)
            |> Seq.map (mapItem2 <| konst 1)

        shortestPaths fNeighbors source [ sink ] |> List.tryExactlyOne

    let rec loop () =
        match findpath () with
        | Some (_, (path, _)) ->
            let delta =
                path
                |> List.pairwise
                |> List.map (fun (a, b) -> fNeighbors a |> Seq.find (fst >> (=) b) |> snd)
                |> List.min

            path |> List.pairwise |> List.iter (fun (a, b) -> updateFlow a b delta) |> loop
        | None -> ()

    loop ()
    let maxFlow = fNeighbors source |> Seq.sumBy (fun (n, _) -> getFlow source n)

    let edges =
        flow.Keys
        |> Seq.map (fun (a, b) -> a, b, getFlow a b)
        |> Seq.choose (Option.returnIf (item3 >> flip (>) LanguagePrimitives.GenericZero))
        |> Seq.cache

    maxFlow, edges

module Grid =
    let makeFNeighbors (grid: 'a array array) (costF: (int * int) * 'a -> (int * int) * 'a -> int option) =
        let fNeighbors pos =
            Tuple2.neighbors4 pos
            |> Seq.filter (flip Array.isInBounds2d grid)
            |> Seq.choose (fun n ->
                let from' = pos, Array.item2dp pos grid
                let to' = n, Array.item2dp n grid
                costF from' to' |> Option.map (fun cost -> n, cost))

        fNeighbors
