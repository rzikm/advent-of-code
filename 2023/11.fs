module AoC202311

open AdventOfCode
open FSharpPlus
open FParsec

type Cell =
    | Empty
    | Galaxy

let parser =
    let pcell =
        choice [ skipChar '.' >>% Empty
                 skipChar '#' >>% Galaxy ]

    ParseUtils.grid pcell

let getGalaxyIndexes input =
    Seq.indexed input
    |> Seq.collect (fun (y, row) ->
        row |> Seq.indexed |> Seq.filter (snd >> (=) Galaxy) |> Seq.map (fun (x, _) -> (x, y)))
    |> List.ofSeq

let measureDistance expRow expCol expandIndex input (x1, y1) (x2, y2) =
    let dy =
        [ (min y1 y2) .. (max y1 y2) ] |> List.sumBy (fun y -> if expRow y then expandIndex else 1L)

    let dx =
        [ (min x1 x2) .. (max x1 x2) ] |> List.sumBy (fun x -> if expCol x then expandIndex else 1L)

    dx + dy - 2L

let shouldExpRow input =
    Utils.memoize (fun row -> input |> Array.item row |> Array.contains Galaxy |> not)

let shouldExpCol input =
    Utils.memoize (fun col -> input |> Array.exists (Array.item col >> (=) Galaxy) |> not)

let solve dist input =
    let galaxies = getGalaxyIndexes input
    let shouldExpRow = shouldExpRow input
    let shouldExpCol = shouldExpCol input

    List.allPairs galaxies galaxies
    |> List.filter (uncurry (<))
    |> List.sumBy (fun (l, r) -> measureDistance shouldExpRow shouldExpCol dist input l r)

let solution = makeSolution () parser (solve 2L) (solve 1000000L)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "...#......"
           ".......#.."
           "#........."
           ".........."
           "......#..."
           ".#........"
           ".........#"
           ".........."
           ".......#.."
           "#...#....."
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 374L

    [<Theory>]
    [<InlineData(1, 5, 4, 9, 9)>]
    [<InlineData(0, 9, 4, 9, 5)>]
    let ``Example part 1 - concrete pair`` x1 y1 x2 y2 dist =
        let input = parseTestInput parser input
        let shouldExpRow = shouldExpRow input
        let shouldExpCol = shouldExpCol input
        measureDistance shouldExpRow shouldExpCol 2 input (x1, y1) (x2, y2) |> should equal (int64 dist)
        measureDistance shouldExpRow shouldExpCol 2 input (x2, y2) (x1, y1) |> should equal (int64 dist)

    [<Fact>]
    let ``Example part 2`` () =
        parseTestInput parser input |> solve 10L |> should equal 1030L
