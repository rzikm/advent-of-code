module AoC202221

open AdventOfCode
open FSharpPlus
open FSharpPlus.Lens
open FParsec

type Operator =
    | Plus
    | Minus
    | Times
    | Divide

type Expression =
    | Number of int64
    | Operator of string * Operator * string

let parser =
    let pmonkey = many1Chars (satisfy isLetter)

    let pOperator =
        choice [ charReturn '+' Plus
                 charReturn '-' Minus
                 charReturn '*' Times
                 charReturn '/' Divide ]

    let pExpr =
        choice [ pint64 |>> Number
                 tuple3 (pmonkey) (pchar ' ' >>. pOperator .>> pchar ' ') pmonkey |>> Operator ]

    sepEndBy1 (pmonkey .>> pstring ": " .>>. pExpr) (pchar '\n')

let solve1 input =
    let exprs = Map.ofList input

    let eval =
        Utils.memoizerec (fun eval ex ->
            match Map.find ex exprs with
            | Number i -> i
            | Operator (l, op, r) ->
                match op with
                | Plus -> eval l + eval r
                | Minus -> eval l - eval r
                | Times -> eval l * eval r
                | Divide -> eval l / eval r)

    eval "root"

// prism for targeting the Operator case of Expression
let inline _Operator e =
    (prism Operator (fun e ->
        match e with
        | Operator (l, op, r) -> Result.Ok((l, op, r))
        | _ -> Result.Error(e)))
        e

let solve2 input =
    let exprs =
        Map.ofList input
        // update root to minus expression, we solve for 0
        |> setl (Map._item "root" << _Some << _Operator << _2) Minus

    // eval the expression to constant or None if contains the free variable "humn"
    let eval =
        Utils.memoizerec (fun eval ex ->
            if ex = "humn" then
                None
            else
                match Map.find ex exprs with
                | Number i -> Some i
                | Operator (l, op, r) ->
                    match op with
                    | Plus -> Option.map2 (+) (eval l) (eval r)
                    | Minus -> Option.map2 (-) (eval l) (eval r)
                    | Times -> Option.map2 (*) (eval l) (eval r)
                    | Divide -> Option.map2 (/) (eval l) (eval r))

    // Assume the free variable appears only on a single path in the expression tree, then we can
    // go backwards from tree and solve for "humn"
    let rec reverse targetValue expr =
        if expr = "humn" then
            targetValue
        else
            match Map.find expr exprs with
            | Number _ -> failwith "Expression is a constant"
            | Operator (l, op, r) ->
                // targetValue = l `op` r =>
                match (eval l, eval r) with
                | Some l, None ->
                    match op with
                    | Plus -> reverse (targetValue - l) r
                    | Minus -> reverse (l - targetValue) r
                    | Times -> reverse (targetValue / l) r
                    | Divide -> reverse (l / targetValue) r
                | None, Some r ->
                    match op with
                    | Plus -> reverse (targetValue - r) l
                    | Minus -> reverse (targetValue + r) l
                    | Times -> reverse (targetValue / r) l
                    | Divide -> reverse (targetValue * r) l
                | _ -> failwith "Expected exactly one subexpression to eval to a constant"

    reverse 0 "root"


let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "root: pppw + sjmn"
           "dbpl: 5"
           "cczh: sllz + lgvd"
           "zczc: 2"
           "ptdq: humn - dvpt"
           "dvpt: 3"
           "lfqf: 4"
           "humn: 5"
           "ljgn: 2"
           "sjmn: drzm * dbpl"
           "sllz: 4"
           "pppw: cczh / lfqf"
           "lgvd: ljgn * ptdq"
           "drzm: hmdt - zczc"
           "hmdt: 32"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 152L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 301L
