module AoC201505

open AdventOfCode
open FSharpPlus
open FParsec

let parser = sepEndBy1 (many1Chars (satisfy isLetter)) (pchar '\n')

let isNice1 str =
    (str |> String.toSeq |> Seq.filter (fun c -> String.contains c "aeiou") |> Seq.length) >= 3
    && str |> String.toSeq |> Seq.pairwise |> Seq.exists (uncurry (=))
    && not <| ([ "ab"; "cd"; "pq"; "xy" ] |> List.exists (fun s -> String.isSubString s str))

let isNice2 str =
    let pairs =
        str |> String.toSeq |> Seq.pairwise |> Seq.map (fun (c1, c2) -> string c1 + string c2)

    pairs |> Seq.exists (fun s -> str.LastIndexOf(s) - str.IndexOf(s) > 1)
    && pairs |> Seq.exists (fun s -> String.isSubString (s + string (s.Chars 0)) str)

let solve nicef input = input |> Seq.filter nicef |> Seq.length

let solution = makeSolution () parser (solve isNice1) (solve isNice2)

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Theory>]
    [<InlineData("ugknbfddgicrmopn", true)>]
    [<InlineData("aaa", true)>]
    [<InlineData("jchzalrnumimnmhp", false)>]
    [<InlineData("haegwjzuvuyypxyu", false)>]
    [<InlineData("dvszwmarrgswjxmb", false)>]
    let ``Example part 1`` input expected = isNice1 input |> should equal expected

    [<Theory>]
    [<InlineData("qjhvhtzxzqqjkmpb", true)>]
    [<InlineData("xxyxx", true)>]
    [<InlineData("uurcxstgmygtbstg", false)>]
    [<InlineData("ieodomkazucvgmuy", false)>]
    let ``Example part 2`` input expected = isNice2 input |> should equal expected
