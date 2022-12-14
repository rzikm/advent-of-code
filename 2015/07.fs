module AoC201507

open AdventOfCode
open FSharpPlus
open FParsec

type Input =
    | Number of uint16
    | Wire of string

type Connection =
    | Value of Input
    | And of Input * Input
    | Or of Input * Input
    | Lshift of Input * Input
    | Rshift of Input * Input
    | Not of Input

type Instruction = Connection * string

let parser =
    let ident = many1Chars (satisfy isLetter)
    let input = (ident |>> Wire) <|> (puint16 |>> Number)

    let pinstr =
        choice [ pstring "NOT " >>. input |>> Not
                 attempt <| (input .>> pstring " AND " .>>. input |>> And)
                 attempt <| (input .>> pstring " OR " .>>. input |>> Or)
                 attempt <| (input .>> pstring " LSHIFT " .>>. input |>> Lshift)
                 attempt <| (input .>> pstring " RSHIFT " .>>. input |>> Rshift)
                 attempt <| (input |>> Value) ]

    let pline = pinstr .>> pstring " -> " .>>. ident
    sepEndBy1 pline (pchar '\n')

let evaluate instrs =
    let connMap = instrs |> List.map Tuple2.swap |> Map.ofList

    Utils.memoizerec (fun frec n ->
        let evalInput =
            function
            | Number i -> i
            | Wire l -> frec l

        match Map.find n connMap with
        | Value i -> evalInput i
        | And (l, r) -> evalInput l &&& evalInput r
        | Or (l, r) -> evalInput l ||| evalInput r
        | Lshift (l, s) -> evalInput l <<< int32 (evalInput s)
        | Rshift (l, s) -> evalInput l >>> int32 (evalInput s)
        | Not l -> ~~~(evalInput l))

let solve1 input = evaluate input "a"

let solve2 input =
    let newA = solve1 input
    input |> List.map (fun i -> if snd i <> "b" then i else (Value(Number newA), "b")) |> solve1

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "123 -> x"
           "456 -> y"
           "x AND y -> d"
           "x OR y -> e"
           "x LSHIFT 2 -> f"
           "y RSHIFT 2 -> g"
           "NOT x -> h"
           "NOT y -> i"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        let input = parseTestInput parser input
        let eval = evaluate input

        input
        |> List.map (fun (_, n) -> (n, eval n))
        |> Map.ofList
        |> should
            equal
            (Map.ofList [ ("d", 72us)
                          ("e", 507us)
                          ("f", 492us)
                          ("g", 114us)
                          ("h", 65412us)
                          ("i", 65079us)
                          ("x", 123us)
                          ("y", 456us) ])

// [<Fact>]
// let ``Example part 2`` () =
//     testPart2 solution input |> should equal 0
