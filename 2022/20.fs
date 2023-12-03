module AoC202220

open Utils
open AdventOfCode
open FSharpPlus
open FSharpPlus.Lens
open FParsec

let parser = sepEndBy1 pint64 (pchar '\n')

let getSuccessors (current: int64) succ = Array.item (int32 current) succ |> snd
let getFirst succ = 0
let getKey current = fst current
let getCount succ = Array.length succ
let getValue (c: int64) succ = Array.item (int32 c) succ |> fst

let updateL current newL succ =
    let (c, (_, r)) = Array.item current succ
    Array.set succ current (c, (newL, r))
    succ

let updateR current newR succ =
    let (c, (l, _)) = Array.item current succ
    Array.set succ current (c, (l, newR))
    succ

let update current value succ =
    let (c, _) = Array.item current succ
    Array.set succ current (c, value)
    succ

let makeSuccessorMap input =
    let arr = Array.zeroCreate (List.length input)

    input
    |> Seq.replicate 2
    |> Seq.collect id
    |> Seq.windowed 3
    |> Seq.take (List.length input)
    |> Seq.iter (fun window ->
        match Seq.toArray window with
        | [| (li, _); (mi, m); (ri, _) |] -> Array.set arr mi (m, (li, ri))
        | _ -> failwith "unreachable")

    arr

let stepChain succ (steps: int64) (start: int32) =
    let rec stepChain' succ (steps: int64) (current: int32) =
        let s = sign steps

        match s with
        | 0 -> current
        | _ ->
            let (l, r) = getSuccessors current succ

            if s < 0 then
                stepChain' succ (steps - int64 s) l
            else
                stepChain' succ (steps - int64 s) r

    let count = int64 (getCount succ)

    stepChain' succ (steps % count) start

let mix order succ =
    let mix' succ (current: int32) =
        // remove i from linked list
        let (il, ir) = getSuccessors current succ
        let succ = succ |> updateR il ir |> updateL ir il

        // optimization: don't wrap around endlessly, here the
        let steps = (getValue current succ) % (int64 (getCount succ - 1)) |> int32

        // find destination pair
        let (dl, dr) =
            if steps = 0 then
                (il, ir) // no move, plug it back in
            else
                let target = stepChain succ steps current
                let (tl, tr) = getSuccessors target succ

                if steps > 0 then (target, tr) else (tl, target)

        let newxt =
            succ |> updateR dl current |> updateL dr current |> update current (dl, dr)

        newxt

    order |> Seq.map getKey |> Seq.fold mix' succ

let solve iterations mult input =
    let myInput = input |> List.mapi (fun i l -> (i, mult * l))
    let succ = makeSuccessorMap myInput
    let succ = ((mix myInput) ^ iterations) succ
    let zero = (List.findIndex ((=) 0L) input, 0L) |> getKey

    [ 1000; 2000; 3000 ] |> List.map (fun i -> stepChain succ i zero) |> List.sumBy (fun i -> getValue i succ)

let solution = makeSolution () parser (solve 1 1L) (solve 10 811589153L)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let visualize succ =
        let start = getFirst succ

        let rec f acc current =
            let (p, _) = getSuccessors current succ

            match List.tryHead acc with
            | Some x when x = start -> acc |> List.map (int64 >> flip getValue succ)
            | _ -> f (p :: acc) p

        f [] start


    [<Fact>]
    let ``Wrapping around`` () =
        let input = [ 0; 7; 1; 2; -3; 3; -2 ] |> List.map int64 |> List.indexed
        mix [ (1, 7L) ] (makeSuccessorMap input) |> visualize |> should equal (List.map int64 [ 0; 1; 7; 2; -3; 3; -2 ])

    [<Fact>]
    let ``Example part 1 - final position`` () =
        let input = [ 1; 2; -3; 3; -2; 0; 4 ] |> List.map int64 |> List.indexed
        mix input (makeSuccessorMap input) |> visualize |> should equal (List.map int64 [ 1; 2; -3; 4; 0; 3; -2 ])

    let input = [| "1"; "2"; "-3"; "3"; "-2"; "0"; "4"; "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 3L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 1623178306L
