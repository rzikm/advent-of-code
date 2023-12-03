module AoC202224

open AdventOfCode
open FSharpPlus
open FParsec

type Blizzard =
    | North
    | East
    | South
    | West

let parser =
    let pblizzard =
        choice [ charReturn '^' North
                 charReturn '>' East
                 charReturn 'v' South
                 charReturn '<' West ]

    let pcell = charReturn '.' None <|> (pblizzard |>> Some)

    parse {
        let! len = pstring "#." >>. many1Chars (pchar '#') .>> pchar '\n' |>> (String.length >> (+) 2)

        let line = pchar '#' >>. parray (len - 2) (pcell) .>> pchar '#'
        let! res = many1Till (line .>> pchar '\n') (lookAhead (pstring "##"))

        do! skipArray (len - 2) (pchar '#')
        do! skipString ".#"

        let bounds = (len - 2), res |> List.length

        let blizzards =
            res
            |> List.indexed
            |> List.collect (fun (y, row) ->
                row |> Array.toList |> List.choosei (fun x c -> Option.map (fun b -> ((x, y), b)) c))
            |> Set.ofList

        return bounds, blizzards
    }

let moveBlizzards (bounds, blizzards, _) =
    let toDir =
        function
        | North -> (0, -1)
        | East -> (1, 0)
        | South -> (0, 1)
        | West -> (-1, 0)

    let newb =
        blizzards
        |> Set.map (fun (pos, b) ->
            let dir = toDir b
            // handle wrap around
            let newPos = Tuple2.map2 (%) (Tuple2.add pos dir |> Tuple2.add bounds) bounds
            newPos, b)

    let occupied = Set.map fst newb

    bounds, newb, occupied

let pathFind (bounds, blizzards) requiredStops =
    let getState =
        Utils.memoizerec (fun getState i ->
            match i with
            | 0 -> bounds, blizzards, Set.map fst blizzards
            | _ -> getState (i - 1) |> moveBlizzards)

    let goal = Tuple2.add (-1, 0) bounds
    let start = (0, -1)

    let fHeuristic (_, pos, stops) =
        pos :: stops |> List.pairwise |> List.sumBy (uncurry Tuple2.manhattanDist)

    let fNeighbors (i, pos, stops) =
        let (_, _, occupied) = getState (i + 1)

        let afterMove =
            [ (0, -1); (1, 0); (0, 1); (-1, 0) ]
            |> Seq.choose (fun dir ->
                let newPos = Tuple2.add pos dir

                if newPos = start || newPos = goal || (Tuple2.le (0, 0) newPos && Tuple2.lt newPos bounds) then
                    Some newPos
                else
                    None)

        Seq.append (Seq.singleton pos) afterMove
        |> Seq.filter (not << flip Set.contains occupied)
        |> Seq.map (fun pos ->
            if List.head stops = pos then
                (i + 1, pos, List.tail stops), 1
            else
                (i + 1, pos, stops), 1)

    match Graph.aStar fHeuristic fNeighbors (fun (_, _, stops) -> List.isEmpty stops) [ (0, start, requiredStops) ] with
    | None -> failwith "Failed to find a path to goal"
    | Some (_, len) -> len


let solve1 (bounds, blizzards) =
    let goal = Tuple2.add (-1, 0) bounds
    pathFind (bounds, blizzards) [ goal ]

let solve2 (bounds, blizzards) =
    let start = (0, -1)
    let goal = Tuple2.add (-1, 0) bounds
    pathFind (bounds, blizzards) [ goal; start; goal ]


let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "#.######"; "#>>.<^<#"; "#.<..<<#"; "#>v.><>#"; "#<^v^^>#"; "######.#"; "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 18

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 54
