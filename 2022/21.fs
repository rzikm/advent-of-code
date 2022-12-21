module AoC202221

open AdventOfCode
open FSharpPlus
open FParsec

type Expression =
    | Number of int64
    | Sum of string * string
    | Diff of string * string
    | Product of string * string
    | Ratio of string * string

let parser =
    let pmonkey = many1Chars (satisfy isLetter)

    let pExpr =
        choice [ pint64 |>> Number
                 pmonkey .>> pchar ' ' .>>. anyOf [ '+'; '-'; '*'; '/' ] .>> pchar ' ' .>>. pmonkey
                 |>> (fun ((l, op), r) ->
                     match op with
                     | '+' -> Sum(l, r)
                     | '-' -> Diff(l, r)
                     | '*' -> Product(l, r)
                     | '/' -> Ratio(l, r)
                     | _ -> failwith "unreachable") ]

    sepEndBy1 (pmonkey .>> pstring ": " .>>. pExpr) (pchar '\n')

let solve1 input =
    let exprs = Map.ofList input

    let eval =
        Utils.memoizerec (fun frec ex ->
            match Map.find ex exprs with
            | Number i -> i
            | Sum (l, r) -> frec l + frec r
            | Diff (l, r) -> frec l - frec r
            | Product (l, r) -> frec l * frec r
            | Ratio (l, r) -> frec l / frec r)

    eval "root"

let solve2 input =
    let exprs = Map.ofList input

    let (left, right) =
        match Map.find "root" exprs with
        | Sum (l, r) -> l, r
        | Diff (l, r) -> l, r
        | Product (l, r) -> l, r
        | Ratio (l, r) -> l, r
        | _ -> failwith "unreachable"

    let eval =
        Utils.memoizerec (fun frec ex ->
            if ex = "humn" then
                None
            else
                match Map.find ex exprs with
                | Number i -> Some i
                | Sum (l, r) -> Option.map2 (+) (frec l) (frec r)
                | Diff (l, r) -> Option.map2 (-) (frec l) (frec r)
                | Product (l, r) -> Option.map2 (*) (frec l) (frec r)
                | Ratio (l, r) -> Option.map2 (/) (frec l) (frec r))

    let rec reverse targetValue expr =
        if expr = "humn" then
            targetValue
        else
            match Map.find expr exprs with
            | Sum (l, r) ->
                // targetValue = l + r
                match (eval l, eval r) with
                | Some l, None -> reverse (targetValue - l) r
                | None, Some r -> reverse (targetValue - r) l
                | _ -> failwith "Unexpected"
            | Diff (l, r) ->
                // targetValue = l - r
                match (eval l, eval r) with
                | Some l, None -> reverse (l - targetValue) r
                | None, Some r -> reverse (targetValue + r) l
                | _ -> failwith "Unexpected"
            | Product (l, r) ->
                // targetValue = l * r
                match (eval l, eval r) with
                | Some l, None -> reverse (targetValue / l) r
                | None, Some r -> reverse (targetValue / r) l
                | _ -> failwith "Unexpected"
            | Ratio (l, r) ->
                // targetValue = l / r
                match (eval l, eval r) with
                | Some l, None -> reverse (l / targetValue) r
                | None, Some r -> reverse (targetValue * r) l
                | _ -> failwith "Unexpected"
            | _ -> failwith "unreachable"

    match (eval left, eval right) with
    | Some i, None -> reverse i right
    | None, Some i -> reverse i left
    | _ -> failwith "Unexpected"

let solution = makeSolution parser solve1 solve2

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
