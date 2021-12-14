open System.IO;
open FParsec;

let pTemplate = many1CharsTill anyChar (pchar '\n') |>> List.ofSeq
let pRule = (anyChar .>>. anyChar) .>> pstring " -> " .>>. anyChar
let pRuleList = sepEndBy pRule (pchar '\n') |>> Map.ofList

let template, rules =
    let parser = pTemplate .>> pchar '\n' .>>. pRuleList
    match run parser (File.ReadAllText("input.txt")) with
    | Success(res, _, _) -> res
    | Failure(err, _, _) -> failwith err

let mapMerge f m1 m2 =
    Map.fold (fun map key value ->
        match Map.tryFind key map with
        | Some v -> Map.add key (f v value) map
        | None -> Map.add key value map) m1 m2

let doStep counts _ =
    counts |> Map.toSeq
    |> Seq.map (fun ((l, r), count) -> let m = Map.find (l, r) rules in Map.ofList [((l, m), count); ((m, r), count)])
    |> Seq.reduce (mapMerge (+))

let run n =
    let initCounts = template |> Seq.pairwise |> Seq.countBy id
                     |> Seq.map (fun (k, v) -> (k, int64 v)) |> Map.ofSeq
    let finalCounts = seq {1..n} |> Seq.fold doStep initCounts
    let letterCounts =
        finalCounts |> Map.toSeq |> Seq.append [((List.head template, List.last template), 1L)]
        |> Seq.collect (fun ((l, r), c) -> [ Map.add l c Map.empty; Map.add r c Map.empty])
        |> Seq.reduce (mapMerge (+)) |> Map.toSeq |> Seq.map (fun (_, c) -> c / 2L) |> Seq.toList

    List.max letterCounts - List.min letterCounts

let part1 = run 10
printfn "%d" part1

let part2 = run 40
printfn "%d" part2
