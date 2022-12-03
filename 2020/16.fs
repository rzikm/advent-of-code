module AoC202016

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let newline = skipChar '\n'
    let prange = pint64 .>> skipChar '-' .>>. pint64

    let pfield =
        tuple3 (manyCharsTill anyChar (skipChar ':') .>> skipChar ' ') (prange .>> pstring " or ") (prange .>> newline)

    let pfields = many1Till pfield newline

    let values = sepBy1 pint64 (skipChar ',')

    let myticket = skipString "your ticket:" >>. newline >>. values

    let otherTickets =
        skipString "nearby tickets:" >>. newline >>. sepEndBy1 values newline

    tuple3 pfields myticket (newline .>> newline >>. otherTickets)

let isValidFor (label, r1, r2) value =
    let isValidForRange (from, To) value = value >= from && value <= To
    isValidForRange r1 value || isValidForRange r2 value

let isValidForAnyField fields value =
    fields |> List.exists (fun f -> isValidFor f value)

let solve1 (fields, _, otherTickets) =
    otherTickets |> List.sumBy (List.filter (not << isValidForAnyField fields) >> List.sum)

let findMapping (fields, myTicket, otherTickets) =
    // filter out invalid tickets
    let otherTickets =
        otherTickets |> List.filter (not << List.exists (not << isValidForAnyField fields))

    let allValuesValid field values =
        not <| List.exists (not << isValidFor field) values

    let rec mixnmatch fields values =
        match fields with
        | [] -> Some []
        | _ ->
            // heuristic, pick field with minimum choices
            let counts =
                fields
                |> List.groupBy (fun f -> values |> List.sumBy (fun l -> if (allValuesValid f l) then 1 else 0))
                |> List.sortBy fst

            match List.head counts with
            | (0, _) -> None
            | (_, field :: _) ->
                values
                |> List.filter (allValuesValid field)
                |> List.tryPick (fun l ->
                    mixnmatch (List.except [ field ] fields) (List.except [ l ] values)
                    |> Option.map (fun matches -> (field, l) :: matches))

    let fieldValues = List.transpose (otherTickets |> List.append [ myTicket ])

    let mapping = mixnmatch fields fieldValues |> Option.get
    mapping |> List.map (fun ((name, _, _), vals) -> name, List.head vals)

let solve2 input =
    findMapping input
    |> List.filter (fun (name, _) -> String.startsWith "departure" name)
    |> List.map snd
    |> List.reduce (*)

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let private parse input =
        match FParsec.CharParsers.run parser (String.concat "\n" input) with
        | Success(res, _, _) -> res
        | Failure(err, _, _) -> failwith err


    let input1 =
        [| "class: 1-3 or 5-7"
           "row: 6-11 or 33-44"
           "seat: 13-40 or 45-50"
           ""
           "your ticket:"
           "7,1,14"
           ""
           "nearby tickets:"
           "7,3,47"
           "40,4,50"
           "55,2,20"
           "38,6,12" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input1 |> should equal 71L

    let input2 =
        [| "class: 0-1 or 4-19"
           "row: 0-5 or 8-19"
           "seat: 0-13 or 16-19"
           ""
           "your ticket:"
           "11,12,13"
           ""
           "nearby tickets:"
           "3,9,18"
           "15,1,5"
           "5,14,9" |]

    [<Fact>]
    let ``Example part 2`` () =
        parse input2
        |> findMapping
        |> Set.ofList
        |> should equal (Set.ofList [ ("class", 12L); ("row", 11L); ("seat", 13L) ])
