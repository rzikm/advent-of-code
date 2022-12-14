module AoC202212

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    sepEndBy1 (many1 (satisfy isLetter) |>> Array.ofList) (pchar '\n') |>> Array.ofList

let solve toChar input =
    let start =
        input
        |> Array.indexed
        |> Array.pick (fun (y, row) -> Array.tryFindIndex ((=) 'E') row |> Option.map (fun x -> (x, y)))

    let neighbors v =
        [ (0, 1); (0, -1); (1, 0); (-1, 0) ]
        |> List.toSeq
        |> Seq.map (Tuple2.add v)
        |> Seq.choose (fun n ->
            let vc = uncurry Array.item2d v input

            uncurry Array.tryItem2d n input
            |> Option.bind (fun nc ->
                // uppercase letters have lower ASCII values than lowercase :/
                if (vc <> 'E' && int32 vc <= int32 nc + 1) || (nc = 'S' && vc = 'a') || (nc = 'z' && vc = 'E') then
                    Some(n, 1 (* dist *) )
                else
                    None))

    Graph.aStar (Utils.constf 0) neighbors (fun v -> uncurry Array.item2d v input = toChar) [ start ] |> snd

let solution = makeSolution parser (solve 'S') (solve 'a')

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "Sabqponm"; "abcryxxl"; "accszExk"; "acctuvwj"; "abdefghi"; "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 31

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 29
