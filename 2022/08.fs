module AoC202208

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    sepEndBy1 (many1 (satisfy isDigit |>> (string >> int32)) |>> Array.ofList) (skipChar '\n') |>> Array.ofList

let getViewRays input (x, y) =
    let lenY = length input
    let lenX = item 0 input |> length

    [ List.rev [ 0 .. (x - 1) ] |> map (fun xx -> Array.item2d xx y input)
      [ x + 1 .. (lenX - 1) ] |> map (fun xx -> Array.item2d xx y input)
      List.rev [ 0 .. (y - 1) ] |> map (fun yy -> Array.item2d x yy input)
      [ y + 1 .. (lenY - 1) ] |> map (fun yy -> Array.item2d x yy input) ]


let solve1 input =
    Seq.allPairs (seq { 0 .. length input - 1 }) (seq { 0 .. (item 0 input |> length) - 1 })
    |> filter (fun (y, x) ->
        let current = Array.item2d x y input

        getViewRays input (x, y) |> exists (not << exists ((<=) current)))
    |> length

let solve2 input =
    Seq.allPairs (seq { 0 .. length input - 1 }) (seq { 0 .. (item 0 input |> length) - 1 })
    |> Seq.map (fun (y, x) ->
        let current = Array.item2d x y input

        let measureViewDist s =
            let measured = List.takeWhile (fun t -> t < current) s |> List.length
            min (measured + 1) (List.length s)

        getViewRays input (x, y) |> map measureViewDist |> List.reduce (*))
    |> Seq.max


let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "30373"; "25512"; "65332"; "33549"; "35390"; "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 21

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 8
