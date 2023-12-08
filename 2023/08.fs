module AoC202308

open AdventOfCode
open FSharpPlus
open FSharpPlus.Data
open FParsec

let parser =
    let label = many1Chars (satisfy (fun c -> isLetter c || isDigit c))
    let directions = many1Chars (anyOf "LR")

    let row =
        label .>> skipString " = (" .>>. (label .>> skipString ", " .>>. label .>> skipChar ')')

    directions .>> spaces .>>. (sepEndBy1 row (skipChar '\n') |>> Map.ofList)

let getNextLabel map label dir =
    match dir with
    | 'L' -> Map.find label map |> fst
    | 'R' -> Map.find label map |> snd
    | _ -> failwith "invalid direction"

let findDistance map dirs start fstop =
    Seq.loop dirs |> Seq.scan (getNextLabel map) start |> Seq.indexed |> Seq.skip 1 |> Seq.find (snd >> fstop) |> fst

let solve1 (dirs, map) = findDistance map dirs "AAA" ((=) "ZZZ")

let solve2 (dirs, map) =
    Map.keys map
    |> filter (String.endsWith "A")
    |> List.ofSeq
    |> List.map (fun l -> findDistance map dirs l (String.endsWith "Z") |> int64)
    // trivial lcm is possible because assignment guarantees exactly 1 *Z state per each *A state
    |> Math.lcm

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "LLR"; ""; "AAA = (BBB, BBB)"; "BBB = (AAA, ZZZ)"; "ZZZ = (ZZZ, ZZZ)"; "" |]

    let input2 =
        [| "LR"
           ""
           "11A = (11B, XXX)"
           "11B = (XXX, 11Z)"
           "11Z = (11B, XXX)"
           "22A = (22B, XXX)"
           "22B = (22C, 22C)"
           "22C = (22Z, 22Z)"
           "22Z = (22B, 22B)"
           "XXX = (XXX, XXX)" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 6

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input2 |> should equal 6L
