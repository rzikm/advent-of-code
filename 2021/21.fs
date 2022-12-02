module AoC202121

open AdventOfCode
open FSharpPlus
open FParsec

open System.IO
open System.Collections.Generic

type PlayerState = PlayerState of position: int * score: int

module PlayerState =
    let play (PlayerState(pos, score)) roll =
        let newPos = (pos + roll - 1) % 10 + 1
        PlayerState(newPos, score + newPos)

    let score (PlayerState(_, score)) = score

type Die = DeterministicDie of int

module Die =
    let makeDeterministic () = DeterministicDie 1

    let roll =
        function
        | DeterministicDie x -> (x, x % 100 + 1 |> DeterministicDie)

    let rollN n die =
        seq { 1..n }
        |> Seq.fold
            (fun (sum, die) _ ->
                let (num, newDie) = roll die
                (sum + num, newDie))
            (0, die)

type GameState = { currentPlayer: PlayerState; otherPlayer: PlayerState }

module GameState =
    let currentPlayer (state: GameState) = state.currentPlayer
    let otherPlayer (state: GameState) = state.otherPlayer

    let nextState (state: GameState) dieRoll =
        { currentPlayer = state |> otherPlayer
          otherPlayer = state |> currentPlayer |> PlayerState.play <| dieRoll }

    let newGame player0 player1 =
        { currentPlayer = PlayerState(player0, 0)
          otherPlayer = PlayerState(player1, 0) }

let parser =
    let p1 = pstring "Player 1 starting position: " >>. pint32
    let p2 = pstring "Player 2 starting position: " >>. pint32
    p1 .>> spaces .>>. p2

let solve1 (p0, p1) =
    let rec play state die rollCount =
        if state |> GameState.otherPlayer |> PlayerState.score >= 1000 then
            state, rollCount
        else
            let (roll, nextDie) = Die.rollN 3 die
            play (GameState.nextState state roll) nextDie (rollCount + 3)

    let (finalState, rollCount) =
        play (GameState.newGame p0 p1) (Die.makeDeterministic ()) 0

    let loserScore = finalState |> GameState.currentPlayer |> PlayerState.score
    loserScore * rollCount

let solve2 (p0, p1) =
    let flip (l, r) = (r, l)

    let rolls =
        [ 1..3 ] |> List.allPairs [ 1..3 ] |> List.allPairs [ 1..3 ] |> List.map (fun (r1, (r2, r3)) -> r1 + r2 + r3)

    let cache = Dictionary<_, _>()

    let rec getWinnersCount state =
        if state |> GameState.otherPlayer |> PlayerState.score >= 21 then
            (0L, 1L)
        else
            match cache.TryGetValue state with
            | true, res -> res
            | false, _ ->
                let res =
                    rolls
                    |> Seq.map (GameState.nextState state >> getWinnersCount)
                    |> Seq.reduce (fun (x0, y0) (x1, y1) -> (x0 + x1, y0 + y1))
                    |> flip

                cache.Add(state, res)
                res

    GameState.newGame p0 p1 |> getWinnersCount ||> max

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit

    let input = [| "Player 1 starting position: 4"; "Player 2 starting position: 8" |]

    [<Fact>]
    let ``Rolling deterministic die`` () =
        [ 1..100 ] @ [ 1..100 ]
        |> Seq.fold
            (fun die expected ->
                let (num, nextDie) = Die.roll die
                num |> should equal expected
                nextDie)
            (Die.makeDeterministic ())

    [<Fact>]
    let ``Player rolls 5 -> moves from 7 to 2 and score increases by 2`` () =
        PlayerState.play (PlayerState(7, 20)) 5 |> should equal <| PlayerState(2, 22)

    [<Fact>]
    let ``Example Part 1`` () =
        testPart1 solution input |> should equal 739785

    [<Fact>]
    let ``Example Part 2`` () =
        testPart2 solution input |> should equal 444356092776315L
