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

let getExpansions input =
    let rows =
        input
        |> Seq.indexed
        |> Seq.filter (fun (y, row) -> not <| Array.contains Galaxy row)
        |> Seq.map fst
        |> List.ofSeq

    let columns =
        [ 0 .. ((Array.item 0 input |> Array.length) - 1) ]
        |> List.filter (fun col -> not <| Array.exists (Array.item col >> (=) Galaxy) input)
        |> List.ofSeq

    rows, columns

let measureDistance expRow expCol expandIndex (x1, y1) (x2, y2) =
    let (minx, miny) = (min x1 x2, min y1 y2)
    let (maxx, maxy) = (max x1 x2, max y1 y2)

    let dx =
        int64 (maxx - minx) + (expandIndex - 1L) * (List.count (fun i -> minx < i && i < maxx) expCol |> int64)

    let dy =
        int64 (maxy - miny) + (expandIndex - 1L) * (List.count (fun i -> miny < i && i < maxy) expRow |> int64)

    dx + dy

let solve dist input =
    let galaxies = getGalaxyIndexes input
    let expRow, expCol = getExpansions input

    List.allPairs galaxies galaxies
    |> List.filter (uncurry (<))
    |> List.sumBy (fun (l, r) -> measureDistance expRow expCol dist l r)

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
        let expRow, expCol = getExpansions input

        measureDistance expRow expCol 2 (x1, y1) (x2, y2) |> should equal (int64 dist)
        measureDistance expRow expCol 2 (x2, y2) (x1, y1) |> should equal (int64 dist)

    [<Fact>]
    let ``Example part 2`` () =
        parseTestInput parser input |> solve 10L |> should equal 1030L
