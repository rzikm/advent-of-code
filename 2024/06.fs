module AoC202406

open AdventOfCode
open FSharpPlus
open FParsec

type Cell =
    | Empty
    | Obstacle

let parser =
    let pguard =
        pchar '^' >>. userStateSatisfies (Option.isNone) >>. getPosition
        >>= (fun pos -> updateUserState (fun _ -> Some(pos)) >>% Empty)

    let pcell =
        choice [ charReturn '.' Empty
                 charReturn '#' Obstacle
                 pguard ]

    let getGuardPosition =
        userStateSatisfies Option.isSome >>. getUserState
        |>> (Option.get >> fun (pos: Position) -> (int pos.Column - 2, int pos.Line - 1))

    ParseUtils.grid pcell .>>. getGuardPosition

let followGuard grid pos dir =
    Seq.append
        (Seq.singleton (pos, dir))
        (Seq.unfold
            (fun (pos, dir) ->
                let next = Tuple2.add pos dir

                match Array.tryItem2dp next grid with
                | Some (Empty) -> Some((next, dir), (next, dir))
                | Some (Obstacle) -> Some((pos, Tuple2.rotRight dir), (pos, Tuple2.rotRight dir))
                | _ -> None) // end, outside of the grid
            (pos, dir))

let wouldLoop grid visited pos dir =
    Seq.scan
        (fun (isLoop, visited) (pos, dir) -> (isLoop || Set.contains (pos, dir) visited, Set.add (pos, dir) visited))
        (false, visited)
        (followGuard grid pos dir)
    |> Seq.exists fst

let solve1 (grid, pos) =
    followGuard grid pos (0, -1) |> Seq.map fst |> Seq.distinct |> Seq.length

let solve2 (grid, pos) =
    let wouldLoopIfPlacedInFront (pos, dir) visited =
        let next = Tuple2.add pos dir
        let right = Tuple2.rotRight dir

        match Array.tryItem2dp next grid with
        | Some (Empty) ->
            // if we already visited the cell, can't place an obstacle there
            not (List.exists (fun dir -> Set.contains (next, dir) visited) [ (0, -1); (1, 0); (0, 1); (-1, 0) ])
            && wouldLoop (Array.mapAt2dp next (fun _ -> Obstacle) grid) visited pos right
        | _ -> false

    followGuard grid pos (0, -1)
    |> Seq.fold
        (fun (acc, visited) (pos, dir) ->
            let visited = Set.add (pos, dir) visited

            if wouldLoopIfPlacedInFront (pos, dir) visited then
                (pos :: acc, visited)
            else
                (acc, visited))
        ([], Set.empty)
    |> fst
    |> List.length

let solution = makeSolution None parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "....#....."
           ".........#"
           ".........."
           "..#......."
           ".......#.."
           ".........."
           ".#..^....."
           "........#."
           "#........."
           "......#..." |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 41

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 6
