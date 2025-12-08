module AoC202508

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    ParseUtils.lines (tuple3 (pint64 .>> pchar ',') (pint64 .>> pchar ',') pint64)

let lookupRepresentant x representants =
    let rec loop x y representants =
        let parent = Map.find y representants
        let representants = Map.add x parent representants

        if parent = y then
            parent, representants
        else
            loop y parent representants

    loop x x representants

let joinComponent a b representants =
    let arep, representants = lookupRepresentant a representants
    let brep, representants = lookupRepresentant b representants

    if arep = brep then
        false, representants
    else
        true, Map.add arep brep representants

let areConnected a b representants =
    let ra, representants = lookupRepresentant a representants
    let rb, representants = lookupRepresentant b representants
    ra = rb, representants

let solve1impl count input =
    let representants = input |> Seq.map Tuple2.broadcast |> Map.ofSeq

    let toJoin =
        input |> Utils.allPairs |> Seq.sortBy (uncurry Tuple3.euclideanDist) |> Seq.take count

    let representants =
        toJoin |> Seq.fold (fun representants (a, b) -> joinComponent a b representants |> snd) representants

    input
    |> groupBy (flip lookupRepresentant representants >> fst)
    |> List.map snd
    |> List.map List.length
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold (*) 1

let solve1 input = solve1impl 1000 input

let solve2 input =
    let rec loop pairs representants =
        match pairs with
        | [] -> failwith "Failed to connect all components"
        | (a, b) :: rest ->
            let newConnection, representants = joinComponent a b representants

            if not newConnection then
                loop rest representants
            else if Seq.exists (fun p -> not (areConnected p a representants |> fst)) input then
                loop rest representants
            else
                item1 a * item1 b

    let toJoin =
        input |> Utils.allPairs |> Seq.sortBy (uncurry Tuple3.euclideanDist) |> Seq.toList

    let representants = input |> Seq.map Tuple2.broadcast |> Map.ofSeq

    loop toJoin representants


let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "162,817,812"
           "57,618,57"
           "906,360,560"
           "592,479,940"
           "352,342,300"
           "466,668,158"
           "542,29,236"
           "431,825,988"
           "739,650,466"
           "52,470,668"
           "216,146,977"
           "819,987,18"
           "117,168,530"
           "805,96,715"
           "346,949,466"
           "970,615,88"
           "941,993,340"
           "862,61,35"
           "984,92,344"
           "425,690,689" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 (makeSolution () parser (solve1impl 10) solve2) input |> should equal 40

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 25272L
