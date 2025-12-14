module AoC202511

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let plabel = many1Chars letter
    ParseUtils.lines (plabel .>> skipString ": " .>>. sepEndBy1 plabel (skipChar ' ')) |>> Map.ofList .>> eof

let getPaths input =
    let getPathsBetween =
        Utils.memoizerec (fun loop (v0, v1) ->
            if v0 = v1 then
                1L
            else
                Map.tryFind v0 input |> Option.defaultValue [] |> List.sumBy (fun n -> loop (n, v1)))

    fun v0 v1 -> getPathsBetween (v0, v1)

let getPathsAlong fGetPaths vList input =
    List.pairwise vList |> List.map (fun (v0, v1) -> fGetPaths v0 v1) |> List.reduce (*)


let solve1 input =
    getPathsAlong (getPaths input) [ "you"; "out" ] input

let solve2 input =
    let searcher = getPaths input

    getPathsAlong searcher [ "svr"; "dac"; "fft"; "out" ] input
    + getPathsAlong searcher [ "svr"; "fft"; "dac"; "out" ] input

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input1 =
        [| "aaa: you hhh"
           "you: bbb ccc"
           "bbb: ddd eee"
           "ccc: ddd eee fff"
           "ddd: ggg"
           "eee: out"
           "fff: out"
           "ggg: out"
           "hhh: ccc fff iii"
           "iii: out" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input1 |> should equal 5L

    let input2 =
        [| "svr: aaa bbb"
           "aaa: fft"
           "fft: ccc"
           "bbb: tty"
           "tty: ccc"
           "ccc: ddd eee"
           "ddd: hub"
           "hub: fff"
           "eee: dac"
           "dac: fff"
           "fff: ggg hhh"
           "ggg: out"
           "hhh: out" |]

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input2 |> should equal 2L
