module AoC201508

open AdventOfCode
open FSharpPlus
open FParsec

let parser = many1 (many1Chars (satisfy ((<>) '\n')) .>> restOfLine true)

let unquote str =
    let rec unquote' str =
        let replaceAt i len s str =
            String.concat "" [ String.truncate i str; s; unquote' <| String.skip (i + len) str ]

        match String.tryFindIndex ((=) '\\') str with
        | None -> str
        | Some i ->
            match str.[i + 1] with
            | '\\' -> replaceAt i 2 "\\" str
            | '"' -> replaceAt i 2 "\"" str
            | 'x' ->
                let s = str.[i + 2 .. i + 3]
                let c = System.Convert.ToInt32(s, 16) |> char |> string
                replaceAt i 4 c str
            | _ -> failwith "invalid escape sequence"

    str |> String.take (String.length str - 1) |> String.skip 1 |> unquote'

let quote str =
    let rec quote' str =
        let replaceAt i s str =
            String.concat "" [ String.truncate i str; s; quote' <| String.skip (i + 1) str ]

        match str.IndexOfAny([| '"'; '\\' |]) with
        | -1 -> str
        | i ->
            match str.[i] with
            | '\\' -> replaceAt i "\\\\" str
            | '"' -> replaceAt i "\\\"" str
            | _ -> failwith "invalid escape sequence"

    "\"" + quote' str + "\""

let solve f input =
    input
    |> List.sumBy (fun s ->
        let u = f s
        String.length s - String.length u |> abs)

let solution = makeSolution () parser (solve unquote) (solve quote)

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Theory>]
    [<InlineData("\"abc\"", "abc")>]
    [<InlineData("\"aaa\\\"aaa\"", "aaa\"aaa")>]
    [<InlineData("\"\\x27\"", "'")>]
    [<InlineData("\"\\x27\\\\\"", "'\\")>]
    let ``Unquote`` quoted expected = unquote quoted |> should equal expected

    [<Theory>]
    [<InlineData("abc", "\"abc\"")>]
    [<InlineData("aaa\"aaa", "\"aaa\\\"aaa\"")>]
    let ``Quote`` quoted expected = quote quoted |> should equal expected
// [<Fact>]
// let ``Example part 2`` () =
//     testPart2 solution input |> should equal 0
