module AoC202222

open AdventOfCode
open FSharpPlus
open FSharpPlus.Lens
open FSharpPlus.Data
open FParsec

type Instruction =
    | Go of int
    | Left
    | Right

let parser =
    let pline =
        lookAhead (anyOf [ '.'; ' '; '#' ]) >>. (manyChars (pchar ' ') |>> String.length)
        .>>. (many1 (pchar '.' <|> pchar '#' |>> (=) '#') |>> Array.ofList)

    let board = sepEndBy1 pline (pchar '\n') |>> Array.ofList

    let pPath =
        many1 (
            choice [ pint32 |>> Go
                     stringReturn "L" Left
                     stringReturn "R" Right ]
        )

    board .>> pchar '\n' .>>. pPath

let tryGet board (x, y) =
    monad {
        let! offset, row = Array.tryItem y board
        return! Array.tryItem (x - offset) row
    }

let move1 board (pos, dir) =
    let afterMove = Tuple2.add pos dir

    match tryGet board afterMove with
    | Some _ -> afterMove, dir
    | None ->
        // some wraparound involved
        let ((x, y), (dx, dy)) = (pos, dir)

        if dy = 0 then
            let offset, row = Array.item y board
            let newx = (x - offset + dx + Array.length row) % (Array.length row) + offset
            (newx, y), (dx, dy)
        else // dx = 0
            // scan in the reverse direction until we find plausible cell
            let delta =
                { 1 .. (Array.length board) }
                |> Seq.filter (fun i -> tryGet board (x, y - i * dy) |> Option.isSome)
                |> Seq.last

            (x, y - delta * dy), (dx, dy)

let move2 board =
    // this just happens to work with both test and input data, but may give wront
    // size for some inputs
    let sideLen =
        min (Array.head board |> fst) (Array.head board |> snd |> Array.length)

    //
    // returns outer edges starting from the first top one and going clockwise.
    // the format of the edges is (first point, clockwise direction vector)
    //
    let getEdges board =
        let mutable start = (Array.head board |> fst, 0)

        let mutable dir = (1, 0)
        let mutable norm = (0, -1)

        seq {
            // every such diagram must have 14 outer edges
            for _ in 1..14 do
                yield (start, dir)

                let endP = Tuple2.add start (Tuple2.map ((*) (sideLen - 1)) dir)

                if tryGet board (endP |> Tuple2.add dir |> Tuple2.add norm) |> Option.isSome then
                    // next edge rotates left
                    start <- endP |> Tuple2.add dir |> Tuple2.add norm
                    norm <- Tuple2.rotLeft norm
                    dir <- Tuple2.rotLeft dir
                else if tryGet board (endP |> Tuple2.add dir) |> Option.isSome then
                    // next edge continues in the same direction
                    start <- endP |> Tuple2.add dir
                else
                    // next edge rotates right
                    start <- endP
                    norm <- Tuple2.rotRight norm
                    dir <- Tuple2.rotRight dir
        }

    //
    // Composes mapping between edges in the above mentioned representation
    //
    let getEdgeMap board =
        //
        // idea: When going clockwise along an edge, and the next edge rotates to the left, then we know
        // these two edges will fold on each other. We can then remove this edge pair from the list, rotate
        // all edges to the right of the pair anticlockwise and continue until we match all edges.
        //
        // This algorithm can be implemented on one pass by maintaining a stack of unpaired edges and
        // maintaining an andjustment rotation to be applied to all future edges.
        //

        // rotation implemented as complex number multiplication, this is to easily rotate currently the
        // currently processed edge x times to the right. THe neutral direction is (1, 0), i.e., right
        let applyRot (x, y) (xx, yy) = (x * xx - y * yy, x * yy + y * xx)

        let folder (stack, map, adjustmentRotation) edge =
            // direction after applying the adjustment rotation
            let adjustedDir = applyRot adjustmentRotation (snd edge)

            match stack with
            | [] -> ([ (edge, adjustedDir) ], map, adjustmentRotation)
            | (prev, prevAdjustedDir) :: restStack ->
                if Tuple2.rotLeft prevAdjustedDir = adjustedDir then
                    // next edge rotates left, it will fold onto the top of the stack
                    let newMap = map |> Map.add prev edge |> Map.add edge prev

                    // all subsequent edges need to be rotated left, increase adjustment rotation
                    (restStack, newMap, Tuple2.rotLeft adjustmentRotation)
                else
                    // no folding yet, push to stack
                    ((edge, adjustedDir) :: stack, map, adjustmentRotation)

        let (_, m, _) = getEdges board |> Seq.fold folder ([], Map.empty, (1, 0))
        m

    // calculate the edge mapping only once
    let edgeMap = getEdgeMap board

    //
    // The actual move function
    //
    let doMove (pos, dir) =
        let afterMove = Tuple2.add pos dir

        match tryGet board afterMove with
        | Some _ -> afterMove, dir // move normally
        | None ->
            // some wraparound involved. find starting point of the edge (i.e. the first when going clockwise)
            let edgeStart =
                let (x, y) = pos

                match dir with
                | (1, 0) -> x, y - y % sideLen
                | (-1, 0) -> x, y - y % sideLen + (sideLen - 1)
                | (0, -1) -> x - x % sideLen, y
                | (0, 1) -> x - x % sideLen + (sideLen - 1), y
                | _ -> failwith "unreachable"

            // get target edge
            let (targetEdgeStart, targetEdgeDir) =
                Map.find (edgeStart, Tuple2.rotRight dir) edgeMap

            // calculate position along the edge
            let delta = Tuple2.manhattanDist edgeStart pos

            // the delta needs to be applied in the reverse direction on the target edge
            let newPos =
                Tuple2.add targetEdgeStart (Tuple2.map ((*) (sideLen - 1 - delta)) targetEdgeDir)

            // convert clockwise direction to inward
            let dir = Tuple2.rotRight targetEdgeDir

            (newPos, dir)

    doMove

let rec tryMove move board (pos, dir) count =
    match count with
    | 0 -> pos, dir // at destination
    | _ ->
        let nextPos, nextDir = move (pos, dir)

        if tryGet board nextPos |> Option.get then
            // hit wall
            pos, dir
        else
            tryMove move board (nextPos, nextDir) (count - 1)

let solve move (board, path) =
    let start = (Array.head board |> fst, 0)

    let doMove (pos, dir) instruction =
        match instruction with
        | Go steps -> tryMove (move board) board (pos, dir) steps
        | Left -> pos, Tuple2.rotLeft dir
        | Right -> pos, Tuple2.rotRight dir

    let dirToScore =
        function
        | (1, 0) -> 0
        | (0, 1) -> 1
        | (-1, 0) -> 2
        | (0, -1) -> 3
        | _ -> failwith "Unexpected direction"

    let ((col, row), finalDir) = List.fold doMove (start, (1, 0)) path

    1000 * (row + 1) + 4 * (col + 1) + dirToScore finalDir

let solution = makeSolution () parser (solve move1) (solve move2)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "        ...#"
           "        .#.."
           "        #..."
           "        ...."
           "...#.......#"
           "........#..."
           "..#....#...."
           "..........#."
           "        ...#...."
           "        .....#.."
           "        .#......"
           "        ......#."
           ""
           "10R5L5R10L4R5L5"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 6032

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 5031
