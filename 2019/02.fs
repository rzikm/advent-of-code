module AoC201902

open AdventOfCode
open FSharpPlus
open FParsec

let parser = sepBy1 pint64 (pchar ',') |>> Array.ofList

let rec run input i =
    let opCode = Array.item i input
    let a = Array.item (i + 1) input
    let b = Array.item (i + 2) input
    let c = Array.item (i + 3) input

    match opCode with
    | 1L ->
        input.[int32 c] <- input.[int32 a] + input.[int32 b]
        run input (i + 4)
    | 2L ->
        input.[int32 c] <- input.[int32 a] * input.[int32 b]
        run input (i + 4)
    | 99L -> ()
    | _ -> failwithf "Unknown opcode %d" opCode

let runWith (input: int64 array) noun verb =
    input.[1] <- noun
    input.[2] <- verb
    run input 0
    input |> Array.head

let solve1 input = runWith input 12L 2L

let solve2 input =
    List.allPairs [ 0L .. 99L ] [
        0L .. 99L
    ]
    |> Seq.find (fun (noun, verb) -> runWith (input |> Array.copy) noun verb = 19690720L)
    |> (fun (noun, verb) -> 100L * noun + verb)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "1,9,10,3,2,3,11,0,99,30,40,50" |]

    [<Fact>]
    let ``Example part 1`` () =
        let input = parseTestInput parser input
        run input 0
        input.[0] |> should equal 3500L
