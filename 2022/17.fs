module AoC202217

open AdventOfCode
open Utils
open FSharpPlus
open FParsec

type Direction =
    | Left
    | Right

let parser = many1 (charReturn '<' Left <|> charReturn '>' Right) |>> Array.ofList

type Piece =
    | Horizontal
    | Cross
    | Corner
    | Vertical
    | Box

let shapeWidth =
    function
    | Horizontal -> 4
    | Cross -> 3
    | Corner -> 3
    | Vertical -> 1
    | Box -> 2

let shapeHeight =
    function
    | Horizontal -> 1
    | Cross -> 3
    | Corner -> 3
    | Vertical -> 4
    | Box -> 2

let protrusions =
    function
    | Horizontal -> [ (0, 3) ]
    | Cross -> [ (1, 1); (0, 2); (1, 1) ]
    | Corner -> [ (0, 2); (2, 2); (2, 2) ]
    | Vertical -> [ (0, 0); (0, 0); (0, 0); (0, 0) ]
    | Box -> [ (0, 1); (0, 1) ]

let applyDirection column shape (x, y) direction =
    let canMove =
        protrusions shape
        |> List.mapi (fun i p -> (y - i), p)
        |> List.forall (fun (i, (sx, ex)) ->

            match direction, (List.tryItem i column) with
            | Left, None -> x > 0
            | Right, None -> x < 7 - shapeWidth shape
            | Left, Some (row) -> Array.tryItem (x + sx - 1) row |> Option.map not |> Option.defaultValue false
            | Right, Some (row) -> Array.tryItem (x + ex + 1) row |> Option.map not |> Option.defaultValue false)

    if canMove then
        (if direction = Left then x - 1 else x + 1), y
    else
        (x, y)

let tryMoveDown column shape (x, y) =
    if y + 1 >= List.length column then
        None // reached bottom
    else
        let canMove =
            protrusions shape
            |> List.mapi (fun i p -> (y - i), p)
            |> List.forall (fun (i, (sx, ex)) ->

                match List.tryItem (i + 1) column with
                | None -> true
                | Some row -> { x + sx .. x + ex } |> Seq.forall (fun xx -> Array.item xx row |> not))

        if canMove then Some(x, y + 1) else None

let placeShape column shape (x, y) =
    let prefix = (List.take (max 0 (y - shapeHeight shape + 1))) column

    let middle =
        protrusions shape
        |> List.mapi (fun i (sp, ep) ->
            match List.tryItem (y - i) column with
            | None -> Array.init 7 (fun i -> x + sp <= i && i <= x + ep)
            | Some row -> Array.mapi (fun i b -> b || x + sp <= i && i <= x + ep) row)
        |> List.rev

    let tail =
        column |> List.toSeq |> Seq.skip (min (List.length column) (y + 1)) |> Seq.truncate 50 |> Seq.toList

    List.concat [ prefix; middle; tail ]

let solve count input =
    let getModulo array i =
        Array.item (i % (Array.length array)) array

    let shapes = [| Horizontal; Cross; Corner; Vertical; Box |]

    let getShape = getModulo shapes
    let getDirection = getModulo input

    let doShape (column, topHeight, shapeIndex, moveIndex) =
        let shape = getShape shapeIndex

        let rec doShape' (column, position, moveIndex) =
            let newPos = applyDirection column shape position (getDirection moveIndex)

            match tryMoveDown column shape newPos with
            | Some newPos' -> doShape' (column, newPos', moveIndex + 1)
            | None ->
                let newTop = max topHeight (topHeight - (snd newPos + 1) + shapeHeight shape)
                (placeShape column shape newPos), newTop, shapeIndex + 1, moveIndex + 1

        doShape' (column, (2, -4), moveIndex)

    // search until a state repeats
    let rec doSearch s m =
        let res = doShape s
        let (c, h, si, mi) = res
        let top = List.truncate 20 c

        let key = (top, si % 5, mi % (Array.length input))

        match Map.tryFind key m with
        | None ->
            let newMap = Map.add key res m
            doSearch res newMap
        | Some r -> (r, res)

    let (first, second) = doSearch ([], 0, 0, 0) Map.empty
    let (_, h1, s1, _) = first
    let (_, h2, s2, _) = second
    let cycleLen = int64 (s2 - s1)
    let cycleHeight = int64 (h2 - h1)
    let cycleCount = (count - int64 s1) / cycleLen
    let remainder = count - cycleLen * cycleCount - int64 s1
    let (_, h3, _, _) = (doShape ^ (int32 remainder)) second

    int64 (h1 + h3 - h2) + cycleCount * cycleHeight

// c |> List.map (Array.map (fun b -> if b then "#" else ".") >> String.concat "") |> String.concat "\n"

let solution = makeSolution () parser (solve 2022L) (solve 1000000000000L)

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``applyDirection on empty column`` () =
        applyDirection [] Horizontal (2, -4) Left |> should equal (1, -4)
        applyDirection [] Horizontal (2, -4) Right |> should equal (3, -4)
        applyDirection [] Horizontal (0, -4) Left |> should equal (0, -4)
        applyDirection [] Horizontal (3, -4) Right |> should equal (3, -4)

    [<Fact>]
    let ``applyDirection on full column`` () =
        let column = [ [| false; false; true; false; true; false; false |] ]
        applyDirection column Vertical (0, 0) Left |> should equal (0, 0)
        applyDirection column Vertical (0, 0) Right |> should equal (1, 0)
        applyDirection column Vertical (1, 0) Right |> should equal (1, 0)
        applyDirection column Vertical (3, 0) Left |> should equal (3, 0)
        applyDirection column Vertical (3, 0) Right |> should equal (3, 0)
        applyDirection column Vertical (5, 0) Right |> should equal (6, 0)
        applyDirection column Vertical (5, 0) Left |> should equal (5, 0)

    [<Fact>]
    let ``tryMoveDown on empty column`` () =
        tryMoveDown [] Horizontal (0, -2) |> should equal (Some(0, -1))
        tryMoveDown [] Horizontal (0, -1) |> should equal None

    [<Fact>]
    let ``tryMoveDown on full column`` () =
        let column =
            [ [| false; true; false; false; false; true; false |]
              [| false; false; true; false; true; false; false |]
              [| false; false; true; false; true; false; false |] ]

        tryMoveDown column Cross (2, -1) |> should equal (Some(2, 0))
        tryMoveDown column Cross (2, 0) |> should equal (Some(2, 1))
        tryMoveDown column Cross (2, 1) |> should equal None

    [<Fact>]
    let ``placeShape places cross`` () =
        let column =
            [ [| false; true; false; false; false; true; false |]
              [| false; false; true; false; true; false; false |]
              [| false; false; true; false; true; false; false |] ]

        let expected =
            [ [| false; false; false; true; false; false; false |]
              [| false; true; true; true; true; true; false |]
              [| false; false; true; true; true; false; false |]
              [| false; false; true; false; true; false; false |] ]

        placeShape column Cross (2, 1) |> should equal expected

    [<Fact>]
    let ``placeShape places on empty`` () =
        let expected = [ [| false; false; true; true; true; true; false |] ]

        placeShape [] Horizontal (2, 0) |> should equal expected


    let input = [| ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"; "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 3068L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 1514285714288L
