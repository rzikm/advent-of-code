module AoC202411

open AdventOfCode
open FSharpPlus
open FParsec

let parser = sepBy1 pint64 (pchar ' ')

let (|Zero|_|) n = if n = 0L then Some() else None

let (|EvenDigits|_|) n =
    string n
    |> Option.returnIf (fun s -> String.length s % 2 = 0)
    |> Option.map (fun s -> int64 s.[0 .. (String.length s / 2 - 1)], int64 s.[(String.length s / 2) ..])

let solve iters input =
    let f stones =
        let addValue k v acc =
            Map.change k (Option.defaultValue 0L >> (+) v >> Option.returnIf (flip (>) 0)) acc

        stones
        |> Map.fold
            (fun acc k v ->
                let acc = addValue k (-v) acc

                match k with
                | Zero -> addValue 1L v acc
                | EvenDigits (a, b) -> addValue a v acc |> addValue b v
                | _ -> addValue (2024L * k) v acc)
            stones

    input
    |> List.countBy id
    |> List.map (Tuple2.mapItem2 int64)
    |> Map.ofList
    |> Utils.applyN iters f
    |> Map.fold (fun acc _ v -> acc + v) 0L

let solve1 input = solve 25 input
let solve2 input = solve 75 input

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [ 125; 17 ] |> List.map int64

    [<Theory>]
    [<InlineData(0, 2L)>]
    [<InlineData(1, 3L)>]
    [<InlineData(2, 4L)>]
    [<InlineData(5, 13L)>]
    [<InlineData(6, 22L)>]
    let ``Example`` iter expected =
        solve iter input |> should equal expected

    [<Theory>]
    [<InlineData(1000L, 10L, 0L)>]
    [<InlineData(2024L, 20L, 24L)>]
    let ``Even split`` num left right =
        match num with
        | EvenDigits (l, r) ->
            l |> should equal left
            r |> should equal right
        | _ -> failwith "not even split"

    [<Theory>]
    [<InlineData(2)>]
    let ``Even split - failure`` num =
        match num with
        | EvenDigits _ -> failwith "not even split"
        | _ -> ()
