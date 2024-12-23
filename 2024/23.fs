module AoC202423

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    ParseUtils.lines (many1Satisfy isAsciiLetter .>> skipChar '-' .>>. many1Satisfy isAsciiLetter)

let findClique fNeighbors size n =
    let rec findClique' acc n =
        if n = 0 then
            seq { (acc |> List.ofSeq) }
        else
            let maxItem = acc |> Set.maxElement

            let candidates =
                acc
                |> Seq.map (fNeighbors >> List.filter (flip (>) maxItem) >> Set.ofList)
                |> Set.intersectMany
                |> Set.toList

            candidates |> Seq.collect (fun v -> findClique' (Set.add v acc) (n - 1))

    findClique' (Set.singleton n) (size - 1)

let findCliques edges =
    let allVertices =
        edges |> Seq.collect (fun (a, b) -> [ a; b ]) |> Seq.distinct |> Seq.toList |> List.sort

    let neighbors =
        let m =
            edges
            |> Seq.collect (fun e -> [ e; Tuple2.swap e ])
            |> Seq.groupBy fst
            |> Seq.toMap fst (snd >> Seq.map snd >> Seq.toList)

        fun v -> Map.find v m

    fun size -> allVertices |> Seq.collect (findClique neighbors size)

let solve1 input =
    findCliques input 3 |> Seq.count (List.exists (String.startsWith "t"))

let solve2 input =
    let findCliques = findCliques input

    Seq.unfold (fun n -> findCliques n |> Seq.tryHead |> Option.map (fun v -> v, n + 1)) 3
    |> Seq.last
    |> List.sort
    |> String.concat ","

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "kh-tc"
           "qp-kh"
           "de-cg"
           "ka-co"
           "yn-aq"
           "qp-ub"
           "cg-tb"
           "vc-aq"
           "tb-ka"
           "wh-tc"
           "yn-cg"
           "kh-ub"
           "ta-co"
           "de-co"
           "tc-td"
           "tb-wq"
           "wh-td"
           "ta-ka"
           "td-qp"
           "aq-cg"
           "wq-ub"
           "ub-vc"
           "de-ta"
           "wq-aq"
           "wq-vc"
           "wh-yn"
           "ka-de"
           "kh-ta"
           "co-tc"
           "wh-qp"
           "tb-vc"
           "td-yn" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 7

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal "co,de,ka,ta"
