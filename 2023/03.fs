module AoC202303

open AdventOfCode
open FSharpPlus
open FParsec

type Input =
    { numbers: (int64 * (int64 * int64 * int64)) list
      symbols: (char * (int64 * int64)) list }

let parser =
    let emptyInput = { numbers = []; symbols = [] }

    let number =
        lookAhead (satisfy isDigit)
        >>. (pipe3 getPosition pint64 getPosition
             <| fun pos n pos2 -> { numbers = [ (n, (pos.Line, pos.Column, pos2.Column - 1L)) ]; symbols = [] })

    let space = skipChar '.' >>% emptyInput
    let newline = skipChar '\n' >>% emptyInput

    let symbol =
        getPosition .>>. satisfy (not << System.Char.IsWhiteSpace)
        |>> fun (pos, c) -> { numbers = []; symbols = [ (c, (pos.Line, pos.Column)) ] }

    let reduce (a: Input) (b: Input) =
        { numbers = (a.numbers @ b.numbers); symbols = (a.symbols @ b.symbols) }

    many1 (
        choice [ number
                 space
                 symbol
                 newline ]
    )
    |>> List.reduce reduce

let isAdjacentToSymbol (n, (r, c, c2)) (c, (r', c')) =
    abs (r - r') <= 1L && (c - 1L <= c' && c' <= c2 + 1L)

let solve1 (input: Input) =
    input.numbers |> List.filter (fun n -> List.exists (isAdjacentToSymbol n) input.symbols) |> List.sumBy fst

let solve2 (input: Input) =
    input.symbols
    |> List.filter (fst >> (=) '*')
    |> List.map (fun c -> List.filter (fun n -> isAdjacentToSymbol n c) input.numbers)
    |> List.filter (List.length >> (=) 2)
    |> List.sumBy (List.map fst >> List.reduce (*))

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "467..114.."
           "...*......"
           "..35..633."
           "......#..."
           "617*......"
           ".....+.58."
           "..592....."
           "......755."
           "...$.*...."
           ".664.598.." |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 4361L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 467835L
