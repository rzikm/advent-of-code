module AoC202018

open AdventOfCode
open FSharpPlus
open FParsec

type Token =
    | Number of int64
    | Plus
    | Times
    | Parens of Token list

let parser =
    let pnumber = pint64 |>> Number
    let pplus = stringReturn " + " Plus
    let ptimes = stringReturn " * " Times
    let ptoken, tokenR = createParserForwardedToRef ()
    let pparen = skipChar '(' >>. many1 ptoken .>> skipChar ')' |>> Parens

    tokenR := choice [ pnumber; pplus; ptimes; pparen ]

    sepEndBy1 (many1 ptoken) (skipChar '\n')

let rec evalExpr1 expr =
    let evalSingle =
        function
        | Number n -> n
        | Parens innerEx -> evalExpr1 innerEx

    match expr with
    | [ single ] -> evalSingle single
    | l :: Plus :: r :: rest -> evalExpr1 (Number(evalSingle l + evalSingle r) :: rest)
    | l :: Times :: r :: rest -> evalExpr1 (Number(evalSingle l * evalSingle r) :: rest)

let rec evalExpr2 expr =
    let evalSingle =
        function
        | Number n -> n
        | Parens innerEx -> evalExpr2 innerEx

    match expr with
    | [ single ] -> evalSingle single
    | l :: Plus :: r :: rest -> evalExpr2 (Number(evalSingle l + evalSingle r) :: rest)
    | l :: Times :: r :: Plus :: rr :: rest -> evalExpr2 (l :: Times :: Number(evalSingle r + evalSingle rr) :: rest)
    | l :: Times :: r :: rest -> evalExpr2 (Number(evalSingle l * evalSingle r) :: rest)

let solve eval input = input |> List.sumBy eval

let solution = makeSolution parser (solve evalExpr1) (solve evalExpr2)

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``Simple example`` () =
        parseTestInput parser [| "1 + 2 * 3 + 4 * 5 + 6" |] |> List.exactlyOne |> evalExpr1 |> should equal 71L

    [<Theory>]
    [<InlineData("2 * 3 + (4 * 5)", 26L)>]
    [<InlineData("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437L)>]
    [<InlineData("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240L)>]
    [<InlineData("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632L)>]
    let ``Example part 1`` input expected =
        testPart1 solution [| input |] |> should equal expected

    [<Theory>]
    [<InlineData("1 + 2 * 3 + 4 * 5 + 6", 231L)>]
    [<InlineData("2 * 3 + (4 * 5)", 46L)>]
    [<InlineData("5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445L)>]
    [<InlineData("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060L)>]
    [<InlineData("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340L)>]
    let ``Example part 2`` input expected =
        testPart2 solution [| input |] |> should equal expected
