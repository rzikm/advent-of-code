module AoC202414

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let point = pint32 .>> pchar ',' .>>. pint32
    ParseUtils.lines (skipString "p=" >>. point .>> skipString " v=" .>>. point)

let getPosAtTime bounds t pos dir =
    Tuple2.map2 Math.modulo (Tuple2.add pos (Tuple2.scale t dir)) bounds

let solve11 bounds t input =
    let getQuadrant pos =
        match Tuple2.map2 compare pos (Tuple2.div bounds (2, 2)) with
        | -1, -1 -> Some 1
        | 1, -1 -> Some 2
        | -1, 1 -> Some 3
        | 1, 1 -> Some 4
        | _ -> None

    input
    |> List.map (uncurry <| getPosAtTime bounds t)
    |> List.choose (getQuadrant)
    |> List.countBy id
    |> List.map snd
    |> List.reduce (*)

let solve1 input = solve11 (101, 103) 100 input

let solve22 bounds input =
    //
    // Solution to part 2 requires manual inspection of the output, by looking at subsequent
    // images, we see that at certain intervals, many of the robots in the same horizontal
    // or vertical band, for vertical bands, they occur every 93 + i * 103 iterations, and for horizontal
    // bands, they occur every 14 + i * 101 iterations. First iteration when both intersect is then
    // 6377, and there we can see the christmas tree on the screen.
    //

    // code left here for reference how the image was printed
    // let rec iterate i inc =
    //     let robots =
    //         input |> List.map (fun (pos, dir) -> (getPosAtTime bounds i pos dir, dir))

    //     let lookup = List.map fst robots |> Set.ofList

    //     let image =
    //         [ 0 .. (snd bounds) - 1 ]
    //         |> List.map (fun y ->
    //             [ 0 .. (fst bounds) - 1 ]
    //             |> List.map (fun x -> if lookup.Contains(x, y) then "X" else " ")
    //             |> String.concat "")
    //         |> String.concat "\n"

    //     if (i - 403) % 103 = 0 && (i - 14) % 101 = 0 then
    //         printfn "Iteration %d" i
    //         printfn "%s" image
    //         printfn "======"
    //         System.Console.ReadLine() |> ignore

    //     // 13, 114, 215
    //     if i < Tuple2.reduce (*) bounds then
    //         iterate (i + inc) inc
    //     else
    //         ()

    6377

let solve2 input = solve22 (101, 103) input

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "p=0,4 v=3,-3"
           "p=6,3 v=-1,-3"
           "p=10,3 v=-1,2"
           "p=2,0 v=2,-1"
           "p=0,0 v=1,3"
           "p=3,0 v=-2,-2"
           "p=7,6 v=-1,-3"
           "p=3,0 v=-1,-2"
           "p=9,3 v=2,3"
           "p=7,3 v=-1,2"
           "p=2,4 v=2,-3"
           "p=9,5 v=-3,-3" |]

    [<Fact>]
    let ``Example part 1`` () =
        parseTestInput parser input |> solve11 (7, 11) 100 |> should equal 12

//     [<Fact>]
//     let ``Example part 2`` () =
//         testPart2 solution input |> should equal 0
