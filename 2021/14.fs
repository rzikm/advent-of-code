module AoC202114

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pTemplate = many1CharsTill anyChar (pchar '\n') |>> List.ofSeq
    let pRule = (anyChar .>>. anyChar) .>> pstring " -> " .>>. anyChar
    let pRuleList = sepEndBy pRule (pchar '\n') |>> Map.ofList

    pTemplate .>> pchar '\n' .>>. pRuleList

let mapMerge f m1 m2 =
    Map.fold
        (fun map key value ->
            match Map.tryFind key map with
            | Some v -> Map.add key (f v value) map
            | None -> Map.add key value map)
        m1
        m2

let run n (template, rules) =
    let doStep counts _ =
        counts
        |> Map.toSeq
        |> Seq.map (fun ((l, r), count) ->
            let m = Map.find (l, r) rules in

            Map.ofList [ ((l, m), count)
                         ((m, r), count) ])
        |> Seq.reduce (mapMerge (+))

    let initCounts =
        template |> Seq.pairwise |> Seq.countBy id |> Seq.map (fun (k, v) -> (k, int64 v)) |> Map.ofSeq

    let finalCounts = seq { 1..n } |> Seq.fold doStep initCounts

    let letterCounts =
        finalCounts
        |> Map.toSeq
        |> Seq.append [ ((List.head template, List.last template), 1L) ]
        |> Seq.collect (fun ((l, r), c) -> [ Map.add l c Map.empty; Map.add r c Map.empty ])
        |> Seq.reduce (mapMerge (+))
        |> Map.toSeq
        |> Seq.map (fun (_, c) -> c / 2L)
        |> Seq.toList

    List.max letterCounts - List.min letterCounts

let solution = makeSolution () parser (run 10) (run 40)
