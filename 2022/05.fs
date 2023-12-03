module AoC202205

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pCrate =
        choice [ stringReturn "   " None
                 skipChar '[' >>. satisfy isLetter .>> skipChar ']' |>> Some ]

    let pline = sepBy1 pCrate (skipChar ' ')

    let pcrates =
        many1Till (pline .>> skipChar '\n') (skipChar ' ' >>. pint32)
        |>> (fun lists ->
            let maxlen = lists |> List.map List.length |> List.max

            lists
            |> List.map (fun l -> List.replicate (maxlen - List.length l) None |> List.append l)
            |> List.transpose
            |> List.map (List.choose id))
        .>> skipRestOfLine true

    let pmove =
        tuple3 (skipString "move " >>. pint32) (skipString " from " >>. pint32) (skipString " to " >>. pint32)

    let pmoves = sepEndBy1 pmove (skipChar '\n')

    pcrates .>> spaces .>>. pmoves

let doMove rev crates (count, src, dst) =
    let toMove, rest = List.item (src - 1) crates |> List.splitAt count
    let destination = List.item (dst - 1) crates |> List.append (rev toMove)

    crates |> List.updateAt (src - 1) rest |> List.updateAt (dst - 1) destination

let solve rev (crates, moves) =
    List.fold (doMove rev) crates moves |> List.map (List.head >> string) |> String.concat ""

let solution = makeSolution () parser (solve List.rev) (solve id)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "    [D]    "
           "[N] [C]    "
           "[Z] [M] [P]"
           " 1   2   3 "
           ""
           "move 1 from 2 to 1"
           "move 3 from 1 to 3"
           "move 2 from 2 to 1"
           "move 1 from 1 to 2"
           "" |]

    [<Fact>]
    let ``Simple move`` () =
        let crates, _ = parseTestInput parser input
        doMove List.rev crates (2, 2, 1) |> should equal [ [ 'C'; 'D'; 'N'; 'Z' ]; [ 'M' ]; [ 'P' ] ]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal "CMZ"

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal "MCD"
