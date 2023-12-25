module AoC202319

open AdventOfCode
open FSharpPlus
open FSharpPlus.Lens
open FSharpPlus.Data
open FParsec

type Part = { X: int64; M: int64; A: int64; S: int64 }

type Category =
    | X
    | M
    | A
    | S

type Condition =
    | All
    | Less of Category * int64
    | Greater of Category * int64

type WorkflowResult =
    | Accept
    | Reject
    | Transfer of string

type WorkflowRule = { condition: Condition; result: WorkflowResult }

type Workflow = { name: string; rules: WorkflowRule list }

let parser =
    let word = many1Chars (satisfy isLetter)

    let pcategory =
        choice [ charReturn 'x' X
                 charReturn 'm' M
                 charReturn 'a' A
                 charReturn 's' S ]

    let prel =
        choice [ attempt (pcategory .>> skipChar '<' .>>. pint64 .>> skipChar ':' |>> Less)
                 attempt (pcategory .>> skipChar '>' .>>. pint64 .>> skipChar ':' |>> Greater)
                 preturn All ]

    let presult =
        choice [ charReturn 'A' Accept
                 charReturn 'R' Reject
                 word |>> Transfer ]

    let prule =
        prel .>>. presult |>> fun (condition, result) -> { condition = condition; result = result }

    let workflow =
        word .>> skipChar '{' .>>. sepBy1 prule (skipChar ',') .>> skipChar '}'
        |>> fun (name, rules) -> { name = name; rules = rules }

    let workflowList = sepEndBy1 workflow (skipChar '\n')

    let pdimension char =
        skipChar char .>> skipChar '=' >>. pint64

    let part =
        skipChar '{'
        >>. tuple4
                (pdimension 'x' .>> skipChar ',')
                (pdimension 'm' .>> skipChar ',')
                (pdimension 'a' .>> skipChar ',')
                (pdimension 's')
        .>> skipChar '}'
        |>> fun (x, m, a, s) -> { X = x; M = m; A = a; S = s }

    let partList = sepEndBy1 part (skipChar '\n')

    workflowList .>> spaces .>>. partList

let getRating part category =
    match category with
    | X -> part.X
    | M -> part.M
    | A -> part.A
    | S -> part.S

let matchesRule part rule =
    match rule.condition with
    | All -> true
    | Less (category, x) -> getRating part category < x
    | Greater (category, x) -> getRating part category > x

let runWorkflow workflow part =
    let rule = workflow.rules |> List.find (matchesRule part)
    rule.result

let isAccepted workflows part =
    let rec run name part =
        match runWorkflow (Map.find name workflows) part with
        | Accept -> true
        | Reject -> false
        | Transfer name -> run name part

    run "in" part

let sumPart part = part.X + part.M + part.A + part.S

let getLookup workflows = workflows |> Seq.toLookupMap _.name

let solve1 (workflows, parts) =
    let workflows = getLookup workflows
    parts |> List.filter (isAccepted workflows) |> List.sumBy sumPart

let splitLess x (x0, x1) =
    if x <= x0 then None
    else if x <= x1 then Some (x0, x - 1L)
    else Some (x0, x1)

let splitGreater x (x0, x1) =
    if x >= x1 then None
    else if x >= x0 then Some (x + 1L, x1)
    else Some (x0, x1)

let solve2 (workflows, _) =
    let workflows = getLookup workflows
    let zero = ((0L, 0L), (0L, 0L), (0L, 0L), (0L, 0L))

    let rec run name part =
        let sumResf result ((x0, x1), (m0, m1), (a0, a1), (s0, s1)) =
            match result with
            | Accept -> (x1 - x0 + 1L) * (m1 - m0 + 1L) * (a1 - a0 + 1L) * (s1 - s0 + 1L)
            | Reject -> 0L
            | Transfer name -> run name ((x0, x1), (m0, m1), (a0, a1), (s0, s1))

        let workflow = Map.find name workflows

        let folder (acc, part) rule =
            if part = zero then acc, part
            else
                let doSplits category toTake toLeave =
                    let ifTaken, ifNot =
                        let doWork get map =
                            (get part |> toTake |> Option.map (fun p -> map (Utils.konst p) part)),
                            (get part |> toLeave |> Option.map (fun p -> map (Utils.konst p) part))

                        match category with
                        | X -> doWork item1 mapItem1
                        | M -> doWork item2 mapItem2
                        | A -> doWork item3 mapItem3
                        | S -> doWork item4 mapItem4

                    let ifTaken = ifTaken |> Option.map (sumResf rule.result) |> Option.defaultValue 0L
                    (acc + ifTaken), ifNot |> Option.defaultValue zero

                match rule.condition with
                | All -> acc + sumResf rule.result part, zero
                | Less (c, x) -> doSplits c (splitLess x) (splitGreater (x - 1L))
                | Greater (c, x) -> doSplits c (splitGreater x) (splitLess (x + 1L))

        List.fold folder (0L, part) workflow.rules |> fst
    
    run "in" ((1L, 4000L), (1L, 4000L), (1L, 4000L), (1L, 4000L))

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "px{a<2006:qkq,m>2090:A,rfg}"
           "pv{a>1716:R,A}"
           "lnx{m>1548:A,A}"
           "rfg{s<537:gd,x>2440:R,A}"
           "qs{s>3448:A,lnx}"
           "qkq{x<1416:A,crn}"
           "crn{x>2662:A,R}"
           "in{s<1351:px,qqz}"
           "qqz{s>2770:qs,m<1801:hdj,R}"
           "gd{a>3333:R,R}"
           "hdj{m>838:A,pv}"
           ""
           "{x=787,m=2655,a=1222,s=2876}"
           "{x=1679,m=44,a=2067,s=496}"
           "{x=2036,m=264,a=79,s=2244}"
           "{x=2461,m=1339,a=466,s=291}"
           "{x=2127,m=1623,a=2188,s=1013}" |]

    let input2 =
        [| "in{s>1000:R,s<1000:R,A}"
           ""
           "{x=2127,m=1623,a=2188,s=1013}" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 19114L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 167409079868000L

    [<Fact>]
    let ``Part 2 - simple`` () =
        parseTestInput parser input2 |> solve2 |> should equal (1L * 4000L * 4000L * 4000L)

    [<Theory>]
    [<InlineData(1, 5, 10, 0, 0)>]
    [<InlineData(5, 5, 10, 0, 0)>]
    [<InlineData(6, 5, 10, 5, 5)>]
    [<InlineData(10, 5, 10, 5, 9)>]
    [<InlineData(11, 5, 10, 5, 10)>]
    let ``splitLess`` x x0 x1 e0 e1 =
        let expected = if (e0, e1) = (0, 0) then None else Some (int64 e0, int64 e1)
        splitLess x (x0, x1) |> should equal expected

    [<Theory>]
    [<InlineData(1, 5, 10, 5, 10)>]
    [<InlineData(5, 5, 10, 6, 10)>]
    [<InlineData(9, 5, 10, 10, 10)>]
    [<InlineData(10, 5, 10, 0, 0)>]
    [<InlineData(11, 5, 10, 0, 0)>]
    let ``splitGreater`` x x0 x1 e0 e1 =
        let expected = if (e0, e1) = (0, 0) then None else Some (int64 e0, int64 e1)
        splitGreater x (x0, x1) |> should equal expected

    