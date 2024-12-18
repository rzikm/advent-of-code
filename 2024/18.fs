module AoC202418

open AdventOfCode
open FParsec

let parser = ParseUtils.lines (pint32 .>> skipString "," .>>. pint32)

let makeLookup incomingBytes =
    let data = incomingBytes |> Seq.indexed |> Seq.map Tuple2.swap |> Map.ofSeq
    fun c -> data |> Map.tryFind c |> Option.defaultValue (System.Int32.MaxValue - 1)

let solve isAccessible bounds =
    let fNeighbors (pos) =
        Tuple2.neighbors4 pos
        |> Seq.map (fun p -> p, 1)
        |> Seq.filter (fun (p, _) -> isAccessible p && Tuple2.inBounds (0, 0) (Tuple2.broadcast (bounds + 1)) p)

    Graph.aStar (Tuple2.manhattanDist (bounds, bounds)) fNeighbors ((=) (bounds, bounds)) [ (0, 0) ]

let solve1 t bounds input =
    let lookup = makeLookup input
    let isAccessible p = lookup p >= t
    solve isAccessible bounds |> Option.get |> snd

let solve2 bounds input =
    let lookup = makeLookup input
    let isAccessible t = fun p -> lookup p >= t

    let rec f lastFallen t =
        match solve (isAccessible t) bounds with
        | None -> lastFallen
        | Some (path, _) ->
            // get first position which will cease to be accessible in the future
            let (pos, accessibleUntil) =
                path |> List.map (fun p -> p, lookup p) |> List.minBy snd

            f pos (accessibleUntil + 1)

    let (x, y) = f (0, 0) 0
    $"{x},{y}"

let solution = makeSolution () parser (solve1 1024 70) (solve2 70)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "5,4"
           "4,2"
           "4,5"
           "3,0"
           "2,1"
           "6,3"
           "2,4"
           "1,5"
           "0,6"
           "3,3"
           "2,6"
           "5,1"
           "1,2"
           "5,5"
           "2,5"
           "6,5"
           "1,4"
           "0,4"
           "6,4"
           "1,1"
           "6,1"
           "1,0"
           "0,5"
           "1,6"
           "2,0" |]

    [<Fact>]
    let ``Example part 1`` () =
        parseTestInput parser input |> solve1 12 6 |> should equal 22

    [<Fact>]
    let ``Example part 2`` () =
        parseTestInput parser input |> solve2 6 |> should equal "6,1"
