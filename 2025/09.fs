module AoC202509

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.lines (pint64 .>> pchar ',' .>>. pint64)

let getRectangleSize (x1, y1) (x2, y2) =
    let width = abs (x2 - x1) + 1L
    let height = abs (y2 - y1) + 1L
    width * height

let innerRectangle (x1, y1) (x2, y2) =
    let x0 = min x1 x2 + 1L
    let x3 = max x1 x2 - 1L
    let y0 = min y1 y2 + 1L
    let y3 = max y1 y2 - 1L

    if x0 > x3 || y0 > y3 then
        None
    else
        Some((x0, y0), (x3, y3))

let rectangleIntersects ((ax, ay), (bx, by)) ((cx, cy), (dx, dy)) =
    let x0 = max (min ax bx) (min cx dx)
    let x1 = min (max ax bx) (max cx dx)
    let y0 = max (min ay by) (min cy dy)
    let y1 = min (max ay by) (max cy dy)
    x0 <= x1 && y0 <= y1

let isInPolygon edges ((ax, ay), (bx, by)) =
    match innerRectangle (ax, ay) (bx, by) with
    | None -> true
    | Some rect -> edges |> List.forall (not << rectangleIntersects rect)

let solve1 input =
    Utils.allPairs input |> Seq.map (uncurry getRectangleSize) |> Seq.max

let solve2 input =
    let edges = input @ [ List.head input ] |> List.pairwise

    Utils.allPairs input
    |> Seq.filter (isInPolygon edges)
    |> Seq.map (fun p -> uncurry getRectangleSize p, p)
    |> Seq.maxBy fst
    |> fst

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 50L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 24L
