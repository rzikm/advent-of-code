module AoC202111

open AdventOfCode
open FSharpPlus
open FParsec
open Utils

let parser =
    let pDigit = anyOf ("0123456789" |> String.toSeq) |>> (fun c -> int c - int '0')
    let pLine = many1 pDigit |>> Array.ofSeq
    sepEndBy pLine spaces |>> Array.ofSeq

let cloneMap arr = Array.map (Array.map id) arr

let mapAt (grid: int [] []) f (x, y) = grid.[y].[x] <- f grid.[y].[x]

let increment (grid: int [] []) =
    for x in 0 .. (grid.[0].Length - 1) do
        for y in 0 .. (grid.Length - 1) do
            mapAt grid ((+) 1) (x, y)

let processFlashAt grid coord =
    let getNeighbors (x, y) =
        let xs =
            [ x - 1; x; x + 1 ] |> List.filter (fun x -> x >= 0 && x < (Array.item 0 grid |> Array.length))

        let ys =
            [ y - 1; y; y + 1 ] |> List.filter (fun y -> y >= 0 && y < Array.length grid)

        List.allPairs xs ys |> List.filter ((<>) (x, y))

    let flashInc =
        function
        | 0 -> 0
        | x -> x + 1

    mapAt grid (fun _ -> 0) coord
    getNeighbors coord |> Seq.iter (mapAt grid flashInc)

let doStep grid =
    let findFlashes grid =
        grid
        |> Seq.indexed
        |> Seq.collect (fun (y, row) ->
            row |> Seq.indexed |> Seq.choose (fun (x, i) -> if i > 9 then Some(x, y) else None))
        |> List.ofSeq

    let mutable flashCount = 0
    increment grid
    let mutable flashes = findFlashes grid

    while not <| List.isEmpty flashes do
        flashCount <- flashCount + List.length flashes
        flashes |> List.iter (processFlashAt grid)
        flashes <- findFlashes grid

    flashCount

let solve1 input =
    seq { 1..100 } |> Seq.fold (fun (g, c) _ -> (g, c + doStep g)) (cloneMap input, 0) |> snd

let solve2 input =
    let rec doStep' grid i =
        doStep grid |> ignore

        if grid |> Array.forall (Array.forall ((=) 0)) then
            i
        else
            doStep' grid (i + 1)

    doStep' (cloneMap input) 1

let solution = makeSolution () parser solve1 solve2
