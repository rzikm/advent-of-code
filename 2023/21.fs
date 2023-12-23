module AoC202321

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.grid (anyOf ".#S")

let getStoppablePlots steps grid =
    //
    // simplification: Assume that
    //   - grid is a square (and length is an odd number)
    //   - the start is in the middle
    //   - there is a straight clear path from center to the edge of the tile
    //
    // The simplifications don't need to hold if the movement is restricted to
    // the sinlge tile
    //
    // the walked cells spread out in a diamond pattern, so we can visualize the
    // calculation on individual tiles as follows:
    //
    //                       D
    //                     F C F
    //                   F E A E F
    //                 F E A B A E F
    //               F E A B A B A E F
    //             C D A B A S A B A C D
    //               F E A B A B A E F
    //                 F E A B A E F
    //                   F E A E F
    //                     F C F
    //                       D
    //
    // S: Starting tile
    // A: Fully walked tile, Odd number of steps from the start
    // B: Fully walked tile, Even number of steps from the start
    // C: Potentially partially walked tile, we run the algorithm starting from center
    //    of the edge of the tile
    // D: Partially walked tile (or not at all), we run the algorithm starting from the
    //    center of the tile
    // E: Potentially partially walked tile, we run the algorithm starting from the
    //    corner of the tile
    // F: Partially walked tile (or not at all), we run the algorithm starting from the
    //    corner of the tile
    //

    let tileSize, _ = Array.bounds2d grid
    let start = Array.findIndex2d ((=) 'S') grid

    let fNeighbors =
        Graph.Grid.makeFNeighbors grid (fun _ (_, cell) -> if cell <> '#' then Some 1 else None)

    // Calculates number of cells in B and A from above
    let (evenCount, oddCount) =
        let distances = Graph.flood fNeighbors start

        let doCalculate parity =
            distances |> Seq.count (fun (_, d) -> d <= steps && d % 2 = parity) |> int64

        (doCalculate <| steps % 2), (doCalculate <| 1 - steps % 2)

    let fullTilesInSingleLine = (steps - tileSize) / tileSize

    // Calculates numbers of A and B tiles from above
    let (fullEven, fullOdd) =
        let evens = int64 <| fullTilesInSingleLine / 2
        let odds = int64 fullTilesInSingleLine - evens

        let totalEvens = evens * (2L * evens + 2L) / 2L
        let totalOdds = odds * odds // = odds * (2 * odds - 1 + 1) / 2
        (4L * totalEvens + 1L, 4L * totalOdds)

    let calcWalked pos alreadyWalked =
        Graph.flood fNeighbors pos
        |> Seq.takeWhile (fun (_, d) -> d + alreadyWalked <= steps)
        |> Seq.count (fun (_, d) -> (d + alreadyWalked) % 2 = steps % 2)

    // Calculates sums of C and D tiles from above
    let (cardinalsInner, cardinalsOuter) =
        let midp = fst start
        let alreadyWalked = midp + fullTilesInSingleLine * tileSize + 1

        [ (0, midp); (midp, 0); (tileSize - 1, midp); (midp, tileSize - 1) ]
        |> Seq.map (fun p -> (calcWalked p alreadyWalked, calcWalked p (alreadyWalked + tileSize)))
        |> Seq.map (Tuple2.map int64)
        |> Seq.reduce Tuple2.add

    // Calculates sums of E and F tiles from above
    let (edgesInner, edgesOuter) =
        let midp = fst start
        let alreadyWalked = 2 * (midp + 1) + (fullTilesInSingleLine - 1) * tileSize

        [ (0, 0); (0, tileSize - 1); (tileSize - 1, 0); (tileSize - 1, tileSize - 1) ]
        |> Seq.map (fun p -> (calcWalked p alreadyWalked, calcWalked p (alreadyWalked + tileSize)))
        |> Seq.map (Tuple2.map int64)
        |> Seq.reduce Tuple2.add

    fullEven * evenCount
    + fullOdd * oddCount
    + cardinalsInner
    + cardinalsOuter
    + (int64 fullTilesInSingleLine) * edgesInner
    + (int64 fullTilesInSingleLine + 1L) * edgesOuter

let solve1 input = getStoppablePlots 64 input

let solve2 input = getStoppablePlots 26501365 input

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "..........."
           ".....###.#."
           ".###.##..#."
           "..#.#...#.."
           "....#.#...."
           ".##..S####."
           ".##..#...#."
           ".......##.."
           ".##.#.####."
           ".##..##.##."
           "..........." |]

    [<Fact>]
    let ``Example part 1`` () =
        parseTestInput parser input |> getStoppablePlots 6 |> should equal 20L

    let input2 =
        [| "..........."
           "..#.....##."
           ".#.......#."
           "....#......"
           "....#.#...."
           ".....S....."
           "...##..#..."
           "......#...."
           ".#.......#."
           "..#.....##."
           "..........." |]

    [<Theory>]
    [<InlineData(5)>]
    [<InlineData(6)>]
    [<InlineData(11)>]
    [<InlineData(12)>]
    [<InlineData(13)>]
    [<InlineData(16)>]
    [<InlineData(17)>]
    [<InlineData(22)>]
    [<InlineData(23)>]
    [<InlineData(24)>]
    let ``Part 2 using part 1 - steps `` steps =
        let input = parseTestInput parser input2

        let naiveAlg steps grid =
            let start = Array.findIndex2d ((=) 'S') grid

            let fNeighbors pos =
                let dims = Array.bounds2d grid

                Tuple2.neighbors4 pos
                |> Seq.choose (fun p ->
                    let pp = Tuple2.map2 Math.modulo p dims
                    if Array.item2dp pp grid <> '#' then Some(p, 1) else None)

            let distances = Graph.flood fNeighbors start

            distances |> Seq.takeWhile (snd >> flip (<=) steps) |> Seq.count (fun (_, d) -> d % 2 = steps % 2)

        let expected = naiveAlg steps input
        getStoppablePlots steps input |> should equal <| int64 expected
