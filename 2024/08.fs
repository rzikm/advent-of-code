module AoC202408

open AdventOfCode
open FSharpPlus
open FSharpPlus.Lens
open FParsec

type Cell =
    | Empty
    | Antenna of char

let parser =
    let pCell =
        let pempty = charReturn '.' Empty

        let pantenna =
            getPosition .>>. satisfy (fun c -> isLetter c || isDigit c)
            >>= (fun (pos, c) ->
                getUserState
                >>= (Tuple2.mapItem2 <| List.append [ (c, (int pos.Column - 1, int pos.Line - 1)) ] >> setUserState)
                >>% Antenna c)

        attempt
        <| (getPosition >>= (fun pos -> getUserState >>= (setl _1 (int pos.Column, int pos.Line) >> setUserState))
            >>. choice [ pempty; pantenna ])

    ParseUtils.grid pCell >>. getUserState
    |>> (fun (bounds, antenas) -> (bounds, List.groupBy fst antenas |> List.map (Tuple2.mapItem2 (List.map snd))))

let solve generateAntinodes (bounds, antenas) =
    antenas
    |> Seq.collect (fun (c, positions) ->
        Utils.allPairs positions |> Seq.collect (uncurry generateAntinodes) |> filter (Tuple2.inBounds (0, 0) bounds))
    |> distinct
    |> length

let solve1 =
    let getAntinodes p0 p1 =
        let diff = Tuple2.sub p1 p0
        [ Tuple2.add p1 diff; Tuple2.sub p0 diff ]

    solve getAntinodes

let solve2 (bounds, antenas) =
    let getAntinodesRay p0 p1 =
        let diff = Tuple2.sub p1 p0

        p0
        |> Seq.unfold (fun (p: int * int) ->
            (p, Tuple2.add p diff) |> Option.returnIf (fst >> Tuple2.inBounds (0, 0) bounds))

    let getAntinodes p0 p1 =
        (Seq.append (getAntinodesRay p0 p1) (getAntinodesRay p1 p0))

    solve getAntinodes (bounds, antenas)

let solution = makeSolution ((0, 0), []) parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "............"
           "........0..."
           ".....0......"
           ".......0...."
           "....0......."
           "......A....."
           "............"
           "............"
           "........A..."
           ".........A.."
           "............"
           "............" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 14

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 34
