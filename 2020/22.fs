module AoC202022

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pplayer st =
        skipString st >>. skipRestOfLine true >>. (sepEndBy1 pint32 (skipChar '\n'))

    pplayer "Player 1:" .>> skipChar '\n' .>>. pplayer "Player 2:"



let play1 (l, r) =
    let rec play l r =
        match l, r with
        | [], r -> r
        | l, [] -> l
        | lc :: lr, rc :: rr ->
            if lc > rc then
                play (lr @ [ lc; rc ]) rr
            else
                play lr (rr @ [ rc; lc ])

    play l r

let play2 (left, right) =
    let rec play l r history =
        if Set.contains (l, r) history then
            l, true
        else
            match l, r with
            | [], r -> r, false
            | l, [] -> l, true
            | lc :: lr, rc :: rr ->
                let p1Wins =
                    if lc <= List.length lr && rc <= List.length rr then
                        // recurse
                        play (List.take lc lr) (List.take rc rr) Set.empty |> snd
                    else
                        lc > rc

                if p1Wins then
                    play (lr @ [ lc; rc ]) rr (Set.add (l, r) history)
                else
                    play lr (rr @ [ rc; lc ]) (Set.add (l, r) history)

    play left right Set.empty |> fst

let solve play input =
    play input |> List.rev |> List.fold (fun (acc, c) i -> (acc + c * i, c + 1)) (0, 1) |> fst

let solution = makeSolution parser (solve play1) (solve play2)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "Player 1:"
           "9"
           "2"
           "6"
           "3"
           "1"
           ""
           "Player 2:"
           "5"
           "8"
           "4"
           "7"
           "10"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 306

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 291
