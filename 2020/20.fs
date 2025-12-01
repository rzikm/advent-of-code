module AoC202020

open Utils
open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pbit = choice [ pchar '.'; pchar '#' ] |>> (=) '#'
    let pline = parray 10 pbit .>> skipChar '\n'

    let ptile =
        skipString "Tile " >>. pint64 .>> skipChar ':' .>> skipChar '\n' .>>. parray 10 pline

    sepEndBy1 ptile (skipChar '\n')

let getTopEdge (_, tile) = Array.item 0 tile
let getRightEdge (_, tile) = Array.map Array.last tile
let getBottomEdge (_, tile) = Array.item (Array.length tile - 1) tile
let getLeftEdge (_, tile) = Array.map Array.head tile

let getEdges tile =
    seq {
        yield getTopEdge tile
        yield getRightEdge tile
        yield getBottomEdge tile
        yield getLeftEdge tile
    }

let rotate (id, data) = (id, Array.rotate2dClockwise data)

let flipVertically (id, data) = (id, Array.map Array.rev data)

let flipHorizontally (id, data) = (id, Array.rev data)

let getOrientations tile =
    seq {
        yield! Seq.init 4 id |> Seq.scan (fun s _ -> rotate s) tile
        yield! Seq.init 4 id |> Seq.scan (fun s _ -> rotate s) (flipHorizontally tile)
    }

let matchesLR left right = getRightEdge left = getLeftEdge right

let matchesTB top bottom = getBottomEdge top = getTopEdge bottom

let edgeMatchExact edge tile =
    getEdges tile |> Seq.exists (fun e -> e = edge)

let edgeMatch edge tile =
    getEdges tile |> Seq.exists (fun e -> e = edge || e = (Array.rev edge))

let assembleImage tiles =
    let isUnmatchedEdge (id, _) edge =
        tiles |> List.exists (fun t -> fst t <> id && edgeMatch edge t) |> not

    // first, partition the tiles by unmatchable edges (guaranteed by the assignment)
    let getUnmatchedEdges tile =
        tile |> getEdges |> Seq.filter (isUnmatchedEdge tile) |> Seq.length

    let byUnmatchedEdges = tiles |> List.groupBy (getUnmatchedEdges)

    let corners = byUnmatchedEdges |> List.find (fst >> (=) 2) |> snd
    let first = List.head corners
    let corners = List.tail corners

    let edges = byUnmatchedEdges |> List.find (fst >> (=) 1) |> snd
    let inner = byUnmatchedEdges |> List.find (fst >> (=) 0) |> snd

    let orientCornerTopLeft tile =
        // top left corner has unmatched edges on top and left which corresponds to first and last item in `getEdges` being unmatched
        let rec orient tile =
            let edges = getEdges tile |> Seq.map (isUnmatchedEdge tile) |> List.ofSeq

            match edges with
            | [ true; false; false; true ] -> tile
            | _ -> tile |> rotate |> orient

        orient tile

    let size = List.length tiles |> sqrt

    let pickTile left top from =
        let next =
            from
            |> List.pick (fun tile ->
                getOrientations tile
                |> Seq.tryFind (fun otile ->
                    let leftMatch =
                        left |> Option.map (fun l -> matchesLR l otile) |> Option.defaultValue true

                    let topMatch =
                        top |> Option.map (fun t -> matchesTB t otile) |> Option.defaultValue true

                    leftMatch && topMatch))

        let restTiles = from |> List.filter (fun t -> fst t <> fst next)

        next, restTiles

    let rec buildFirstRow row edgeTiles cornerTiles =
        let prev = List.last row

        if List.length row < size - 1 then
            let next, restEdges = pickTile (Some prev) None edgeTiles
            buildFirstRow (row @ [ next ]) restEdges cornerTiles
        else
            let next, restCorners = pickTile (Some prev) None cornerTiles
            (row @ [ next ], edgeTiles, restCorners)

    let (firstRow, edges, corners) =
        buildFirstRow [ orientCornerTopLeft first ] edges corners

    let rec buildRows prevRows innerTiles edgeTiles cornerTiles =
        let prevRow = List.last prevRows

        let rec buildRow prevRow row innerTiles edgeTiles =
            let left = List.tryLast row
            let top = Some(Array.item (List.length row) prevRow)

            if List.length row > 0 && List.length row < size - 1 then
                let next, restTiles = pickTile left top innerTiles
                buildRow prevRow (row @ [ next ]) restTiles edgeTiles
            else
                let next, restTiles = pickTile left top edgeTiles

                if List.length row = 0 then
                    buildRow prevRow (row @ [ next ]) innerTiles restTiles
                else
                    (row @ [ next ]), innerTiles, restTiles

        if List.length prevRows < size - 1 then
            let nextRow, innerTiles, edgeTiles = buildRow prevRow [] innerTiles edgeTiles

            buildRows (prevRows @ [ Array.ofList nextRow ]) innerTiles edgeTiles corners
        else
            let nextRow, _, _ = buildRow prevRow [] edgeTiles cornerTiles

            prevRows @ [ Array.ofList nextRow ] |> Array.ofList

    buildRows [ Array.ofList firstRow ] inner edges corners

let solve1 input =
    let image = assembleImage input

    let corners =
        [ image |> Array.head |> Array.head
          image |> Array.head |> Array.last
          image |> Array.last |> Array.head
          image |> Array.last |> Array.last ]

    corners |> List.map fst |> List.reduce (*)

let solve2 input =
    let image = assembleImage input

    let stripBorders items =
        items
        |> Array.tail
        |> Array.rev
        |> Array.tail
        |> Array.rev
        |> Array.map (Array.tail >> Array.rev >> Array.tail >> Array.rev)

    // flatten the image
    let flatImage =
        image |> Array.collect (Array.map (snd >> stripBorders) >> Array.transpose >> Array.map (Array.collect id))

    let imgSize = Array.length flatImage

    let pattern =
        [| "                  # "; "#    ##    ##    ###"; " #  #  #  #  #  #   " |]
        |> Array.map (String.toSeq >> Seq.map ((=) '#') >> Array.ofSeq)

    let patSizeY = Array.length pattern
    let patSizeX = Array.item 0 pattern |> Array.length

    let tryMatchAt image x y =
        Seq.allPairs (seq { 0 .. (patSizeX - 1) }) (seq { 0 .. (patSizeY - 1) })
        |> Seq.exists (fun (xx, yy) -> Array.item2d xx yy pattern && not <| Array.item2d (x + xx) (y + yy) image)
        |> not

    let occurences =
        (0, flatImage)
        |> getOrientations
        |> Seq.map (fun (_, img) ->
            Seq.allPairs (seq { 0 .. (imgSize - patSizeX - 1) }) (seq { 0 .. (imgSize - patSizeY - 1) })
            |> Seq.filter (fun (x, y) -> tryMatchAt img x y)
            |> Seq.length)
        |> Seq.max

    let getBitCount = Array.sumBy (Array.sumBy (fun b -> if b then 1 else 0))

    getBitCount flatImage - occurences * (getBitCount pattern)


let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "Tile 2311:"
           "..##.#..#."
           "##..#....."
           "#...##..#."
           "####.#...#"
           "##.##.###."
           "##...#.###"
           ".#.#.#..##"
           "..#....#.."
           "###...#.#."
           "..###..###"
           ""
           "Tile 1951:"
           "#.##...##."
           "#.####...#"
           ".....#..##"
           "#...######"
           ".##.#....#"
           ".###.#####"
           "###.##.##."
           ".###....#."
           "..#.#..#.#"
           "#...##.#.."
           ""
           "Tile 1171:"
           "####...##."
           "#..##.#..#"
           "##.#..#.#."
           ".###.####."
           "..###.####"
           ".##....##."
           ".#...####."
           "#.##.####."
           "####..#..."
           ".....##..."
           ""
           "Tile 1427:"
           "###.##.#.."
           ".#..#.##.."
           ".#.##.#..#"
           "#.#.#.##.#"
           "....#...##"
           "...##..##."
           "...#.#####"
           ".#.####.#."
           "..#..###.#"
           "..##.#..#."
           ""
           "Tile 1489:"
           "##.#.#...."
           "..##...#.."
           ".##..##..."
           "..#...#..."
           "#####...#."
           "#..#.#.#.#"
           "...#.#.#.."
           "##.#...##."
           "..##.##.##"
           "###.##.#.."
           ""
           "Tile 2473:"
           "#....####."
           "#..#.##..."
           "#.##..#..."
           "######.#.#"
           ".#...#.#.#"
           ".#########"
           ".###.#..#."
           "########.#"
           "##...##.#."
           "..###.#.#."
           ""
           "Tile 2971:"
           "..#.#....#"
           "#...###..."
           "#.#.###..."
           "##.##..#.."
           ".#####..##"
           ".#..####.#"
           "#..#.#..#."
           "..####.###"
           "..#.#.###."
           "...#.#.#.#"
           ""
           "Tile 2729:"
           "...#.#.#.#"
           "####.#...."
           "..#.#....."
           "....#..#.#"
           ".##..##.#."
           ".#.####..."
           "####.#.#.."
           "##.####..."
           "##..#.##.."
           "#.##...##."
           ""
           "Tile 3079:"
           "#.#.#####."
           ".#..######"
           "..#......."
           "######...."
           "####.#..#."
           ".#...#.##."
           "#.#####.##"
           "..#.###..."
           "..#......."
           "..#.###..."
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 20899048083289L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 273
