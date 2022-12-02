module AoC202109

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pDigit = anyOf ("0123456789" |> String.toSeq) |>> (fun c -> int c - int '0')
    let pLine = many1 pDigit |>> Array.ofSeq
    sepEndBy pLine spaces |>> Array.ofSeq

let item (input: int[][]) (x, y) = input.[y].[x]

let tryItem (input: int[][]) (x, y) =
    input |> Array.tryItem y |> Option.bind (Array.tryItem x)

let getNeighborCoords (x, y) =
    [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]

let getNeighbors input (x, y) =
    getNeighborCoords (x, y) |> List.choose (tryItem input)

let lowestPoints (input: int[][]) =
    Seq.allPairs (seq { 0 .. input.[0].Length - 1 }) (seq { 0 .. input.Length - 1 })
    |> Seq.map (fun coords -> (item input coords), coords)
    |> Seq.filter (fun (i, coords) -> getNeighbors input coords |> List.forall ((<) i))
    |> Seq.map snd

let solve1 input =
    input |> lowestPoints |> Seq.map (item input) |> Seq.sumBy ((+) 1)

let getBasinSize input coord =
    let rec getBasinSize' (inBasin: Set<int * int>) (next: Set<int * int>) =
        let getAdjacentCoords coord =
            let height = item input coord

            let isAdjacentAllowed c =
                c |> tryItem input |> Option.bind (fun h -> if h < 9 && h > height then Some(c) else None)

            getNeighborCoords coord |> List.choose isAdjacentAllowed

        let adjacent = next |> Seq.collect getAdjacentCoords |> Set.ofSeq
        let nextInBasin = Set.union inBasin next
        let nextNext = Set.difference adjacent nextInBasin

        if Set.count nextNext = 0 then
            Set.count nextInBasin
        else
            getBasinSize' nextInBasin nextNext

    getBasinSize' (Set.empty) (Set.ofList [ coord ])

let solve2 input =
    input |> lowestPoints |> Seq.map (getBasinSize input) |> Seq.sortDescending |> Seq.take 3 |> Seq.reduce (*)

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "2199943210"; "3987894921"; "9856789892"; "8767896789"; "9899965678" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 15

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 1134
