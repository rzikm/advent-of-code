open System.IO;

let getOpeningChar c =
    match c with
    | ')' -> '(' | ']' -> '[' | '>' -> '<' | '}' -> '{'
    | _ -> failwith "Invalid brace"

type ParseResult =
    | Incomplete of char list
    | InvalidChar of char

let input =
    let parse list =
        let rec parse' stack list =
            match list with
            | [] -> Incomplete stack
            | head::tail ->
                match head with
                | '(' | '[' | '<' | '{' -> parse' (head::stack) tail
                | ')' | ']' | '>' | '}' ->
                    if Some(getOpeningChar head) = List.tryHead stack then
                        parse' (List.tail stack) tail
                    else
                        InvalidChar head
                | _ -> failwithf "Invalid input '%c'" head

        parse' [] list

    File.ReadAllLines("input.txt")
    |> Array.map (List.ofSeq >> parse)

let getCompletionStack res =
    match res with
    | Incomplete x -> Some x
    | _ -> None

let getInvalidChar res =
    match res with
    | InvalidChar x -> Some x
    | _ -> None

let part1 =
    let scoreMap = Map.ofList [(')', 3); (']', 57); ('}', 1197); ('>', 25137)]
    input |> Array.choose getInvalidChar |> Array.sumBy (fun x -> Map.find x scoreMap)

printfn "%A" part1

let part2 =
    let scoreMap = Map.ofList [('(', 1L); ('[', 2L); ('{', 3L); ('<', 4L)]

    let scores =
        input
        |> Array.choose getCompletionStack
        |> Array.map (List.fold (fun total c -> total * 5L + (Map.find c scoreMap)) 0L)
        |> Array.sort

    scores.[scores.Length / 2]

printfn "%A" part2
