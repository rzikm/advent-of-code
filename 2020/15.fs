module AoC202015

open AdventOfCode
open FSharpPlus
open FParsec

let parser = sepBy1 pint32 (pchar ',')

let spokenNumbers startingNumbers count =
    seq {
        yield! startingNumbers

        let ages, nextTurn =
            startingNumbers |> List.fold (fun (map, age) i -> (Map.add i age map, age + 1)) (Map.empty, 1)

        yield!
            seq { nextTurn..count }
            |> Seq.scan
                (fun (ages, _, previouslyUsed) turn ->

                    let next =
                        match previouslyUsed with
                        | Some age -> turn - age - 1
                        | None -> 0

                    let wasUsedBefore = Map.tryFind next ages

                    (Map.add next turn ages, next, wasUsedBefore))
                (ages, List.last startingNumbers, None)
            |> Seq.map (fun (_, i, _) -> i)
            |> Seq.skip 1
    }

let solve1 input = spokenNumbers input 2020 |> Seq.last

let solve2 input =
    spokenNumbers input 30000000 |> Seq.last

let solution = makeSolution parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = "0,3,6"

    [<Fact>]
    let ``Example part 1`` () =
        let nums = input |> String.split (seq { "," }) |> Seq.map int32 |> List.ofSeq

        spokenNumbers nums 10 |> List.ofSeq |> should equal [ 0; 3; 6; 0; 3; 3; 1; 0; 4; 0 ]

    [<Theory>]
    [<InlineData("0,3,6", 436)>]
    [<InlineData("1,3,2", 1)>]
    [<InlineData("2,1,3", 10)>]
    [<InlineData("1,2,3", 27)>]
    [<InlineData("2,3,1", 78)>]
    [<InlineData("3,2,1", 438)>]
    [<InlineData("3,1,2", 1836)>]
    let ``Part 1 additional tests`` input expected =
        testPart1 solution [| input |] |> should equal expected
