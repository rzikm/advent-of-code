module AoC201509

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pCity = many1Chars (satisfy isLetter)
    let line = tuple3 pCity (pstring " to " >>. pCity) (pstring " = " >>. pint32)
    sepEndBy1 line (pchar '\n')

let solve mm input =
    let distances =
        input |> List.fold (fun m (f, t, d) -> m |> Map.add (f, t) d |> Map.add (t, f) d) (Map.empty)

    input
    |> List.collect (fun (f, t, _) -> [ f; t ])
    |> List.distinct
    |> Utils.permutations
    |> Seq.map (fun p -> List.pairwise p |> List.sumBy (flip Map.find distances))
    |> Seq.reduce mm

let solution = makeSolution () parser (solve min) (solve max)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "London to Dublin = 464"
           "London to Belfast = 518"
           "Dublin to Belfast = 141"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 605

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 982
