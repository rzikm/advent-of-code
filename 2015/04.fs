module AoC201504

open AdventOfCode
open FSharpPlus
open FParsec

let parser = restOfLine false

let md5 (str: string) =
    let bytes = System.Text.Encoding.ASCII.GetBytes str
    let hash = System.Security.Cryptography.MD5.HashData(bytes)
    System.Convert.ToHexString(hash)

let solve zeros input =
    let prefix = String.init zeros (Utils.konst "0")
    Seq.initInfinite id |> Seq.find (fun n -> input + string n |> md5 |> String.startsWith prefix)

let solution = makeSolution () parser (solve 5) (solve 6)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "" |]

    [<Theory>]
    [<InlineData("abcdef", 609043)>]
    [<InlineData("pqrstuv", 1048970)>]
    let ``Example part 1`` input expected = solve 5 input |> should equal expected
