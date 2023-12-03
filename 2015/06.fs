module AoC201506

open AdventOfCode
open FSharpPlus
open FParsec

type Rectangle = (int * int) * (int * int)

type Instruction =
    | Turn of bool
    | Toggle

let parser =
    let ppair = pint32 .>> pchar ',' .>>. pint32
    let prect = ppair .>> skipString " through " .>>. ppair
    let pbool = stringReturn " on " true <|> stringReturn " off " false

    let pturn = skipString "turn" >>. pbool |>> Turn
    let ptoggle = stringReturn "toggle " Toggle

    sepEndBy1 ((pturn <|> ptoggle) .>>. prect) (pchar '\n')

let isIn ((t1, l1), (b1, r1)) (r, c) =
    t1 <= r && r <= b1 && l1 <= c && c <= r1

let solve1 input =
    let applyInstr arr (i, r) =
        match i with
        | Turn b -> arr |> Array.map2d (fun x y it -> if isIn r (x, y) then b else it)
        | Toggle -> arr |> Array.map2d (fun x y it -> if isIn r (x, y) then not it else it)

    input
    |> List.fold applyInstr (Array.init2d 1000 1000 (fun _ _ -> false))
    |> Array.sumBy (Array.sumBy (fun b -> if b then 1 else 0))

let solve2 input =
    let applyInstr arr (i, r) =
        match i with
        | Turn b ->
            arr
            |> Array.map2d (fun x y it ->
                if isIn r (x, y) then
                    if b then it + 1 else max 0 (it - 1)
                else
                    it)
        | Toggle -> arr |> Array.map2d (fun x y it -> if isIn r (x, y) then it + 2 else it)

    input |> List.fold applyInstr (Array.init2d 1000 1000 (fun _ _ -> 0)) |> Array.sumBy (Array.sum)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Theory>]
    [<InlineData(0, 0, 999, 999, 432, 23, true)>]
    [<InlineData(20, 20, 40, 40, 40, 40, true)>]
    [<InlineData(20, 20, 40, 40, 20, 20, true)>]
    [<InlineData(20, 20, 40, 40, 10, 20, false)>]
    [<InlineData(20, 20, 40, 40, 20, 10, false)>]
    let ``isIn tests`` t l b r x y is =
        isIn ((t, l), (b, r)) (x, y) |> should equal is

// [<Fact>]
// let ``Example part 2`` () =
//     testPart2 solution input |> should equal 0
