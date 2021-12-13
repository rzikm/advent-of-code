open System.IO;
open FParsec;

type Point = int32 * int32
type Fold = | FoldX of int | FoldY of int

let pDot = pint32 .>> pchar ',' .>>. pint32
let pFold = pstring "fold along "
            >>. ((pstring "x=" >>. pint32 |>> FoldX) <|> (pstring "y=" >>. pint32 |>> FoldY))

let dots, folds =
    let parser = sepEndBy pDot (pchar '\n')  .>> (pchar '\n') .>>. sepEndBy pFold (pchar '\n')
    match run parser (File.ReadAllText("input.txt")) with
    | Success(res, _, _) -> res
    | Failure(err, _, _) -> failwith err

let fold ds f =
    let mapper, selector, bound =
        match f with
        | FoldX xx -> (fun (x, y) -> (2 * xx - x, y)), fst, xx
        | FoldY yy -> (fun (x, y) -> (x, 2 * yy - y)), snd, yy

    let toFold, rest = ds |> List.partition (selector >> (<) bound)
    toFold |> List.map mapper |> List.append rest |> List.distinct

let part1 =
    fold dots (List.head folds) |> List.length

printfn "%A" part1

let part2 =
    let finalDots = List.fold fold dots folds |> Set.ofList
    let maxX, maxY = finalDots |> Seq.map fst |> Seq.max, finalDots |> Seq.map snd |> Seq.max
    Seq.init (maxY + 1) (fun y -> String.init (maxX + 1) (fun x -> if Set.contains (x, y) finalDots then "#" else "."))
    |> String.concat "\n"

printfn "%s" part2
