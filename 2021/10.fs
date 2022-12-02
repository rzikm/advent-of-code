module AoC202110

open AdventOfCode
open FSharpPlus
open FParsec

let getOpeningChar =
    function
    | ')' -> '('
    | ']' -> '['
    | '>' -> '<'
    | '}' -> '{'
    | _ -> failwith "Invalid brace"

type ParseResult =
    | Incomplete of char list
    | InvalidChar of char

let parser =
    let ppar = anyOf (String.toSeq "()[]<>{}")
    let pline = many1 ppar
    sepEndBy pline spaces

let preprocess input =
    let parse list =
        let rec parse' stack list =
            match list with
            | [] -> Incomplete stack
            | head :: tail ->
                match head with
                | '('
                | '['
                | '<'
                | '{' -> parse' (head :: stack) tail
                | ')'
                | ']'
                | '>'
                | '}' ->
                    if Some(getOpeningChar head) = List.tryHead stack then
                        parse' (List.tail stack) tail
                    else
                        InvalidChar head
                | _ -> failwithf "Invalid input '%c'" head

        parse' [] list

    List.map parse input

let getCompletionStack =
    function
    | Incomplete x -> Some x
    | _ -> None

let getInvalidChar =
    function
    | InvalidChar x -> Some x
    | _ -> None

let solve1 input =
    let scoreMap = Map.ofList [ (')', 3); (']', 57); ('}', 1197); ('>', 25137) ]
    input |> preprocess |> List.choose getInvalidChar |> List.sumBy (fun x -> Map.find x scoreMap)

let solve2 input =
    let scoreMap = Map.ofList [ ('(', 1L); ('[', 2L); ('{', 3L); ('<', 4L) ]

    let scores =
        input
        |> preprocess
        |> List.choose getCompletionStack
        |> List.map (List.fold (fun total c -> total * 5L + (scoreMap.Item c)) 0L)
        |> List.sort

    scores.[scores.Length / 2]

let solution = makeSolution parser solve1 solve2
