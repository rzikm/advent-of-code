module AoC202501

open AdventOfCode
open FSharpPlus
open FParsec

type Dir =
    | Left
    | Right

let parser =
    let pDir = charReturn 'L' Left <|> charReturn 'R' Right
    ParseUtils.lines (pDir .>>. pint32)

let rotate (dir, dist) pos =
    match dir with
    | Left -> pos - dist
    | Right -> pos + dist

let solve1 input =
    List.scan (fun pos i -> rotate i pos |> flip Math.modulo 100) 50 input |> Seq.filter ((=) 0) |> Seq.length

let solve2 input =
    let f (pos, acc) (dir, dist) =
        let newPos = rotate (dir, dist % 100) pos
        let newPosMod = Math.modulo newPos 100

        let acc =
            if newPosMod = 0 || newPosMod <> newPos && pos <> 0 then
                acc + 1 + dist / 100
            else
                acc + dist / 100

        newPosMod, acc

    List.fold f (50, 0) input |> snd

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 3

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 6

    [<Fact>]
    let ``Rotate large`` () =
        testPart2 solution [| "R1000" |] |> should equal 10
