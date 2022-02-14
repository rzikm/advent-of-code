module AoC202013

open AdventOfCode
open FSharpPlus
open FParsec
open Utils

let parser = 
    pint64 .>> skipRestOfLine true .>>. sepBy ((pint64 |>> Some) <|> (pchar 'x' >>% None)) (pchar ',')

let waitTime departure id = (departure + id - 1L) / id * id - departure

let easy (departure, busses) =
    busses 
    |> List.choose id
    |> List.map (fun id -> (waitTime departure id), id)
    |> List.minBy fst
    ||> (*)

let hard (_, busses) =
    let busses = busses |> List.choosei (fun i b -> ( b |> Option.map (fun b -> (int64 i, b))))

    let first = List.head busses |> snd

    let rec f time processed busses =
        match busses with
        | [] -> time
        | (offset, period)::rest ->
            let largePeriod = lcm processed
            // find d such that
            //    time + d * largePeriod + offset == 0                     (mod period)
            // which means
            //                    d * largeOffset == -(time + offset)      (mod period)
            //
            // -> d = -(time + offset) * largeOffset^-1 (mod period)

            let d = multmod (negmod (time + offset) period) (multinvmod largePeriod period) period
            let newTime = time + d * largePeriod
            f newTime (period::processed) rest

    f first [first] (List.tail busses)


let solution = makeSolution parser easy hard

module Tests =
    open Xunit;
    open FsUnit.Xunit;

    let input = [|
        "939"; 
        "7,13,x,x,59,x,31,19"
    |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 295L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 1068781L
    