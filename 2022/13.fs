module AoC202213

open AdventOfCode
open FSharpPlus
open FParsec

type Token =
    | List of Token list
    | Number of int

let parser =
    let p, pref = createParserForwardedToRef ()
    let pList = pchar '[' >>. sepBy p (pchar ',') .>> pchar ']'
    pref.Value <- (pint32 |>> Number) <|> (pList |>> List)

    let ppair = pList .>> pchar '\n' .>>. pList .>> pchar '\n'
    sepEndBy1 ppair (pchar '\n')

let rec compare l r =
    match l, r with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | l :: ll, r :: rl ->
        let comp =
            match l, r with
            | Number ln, Number rn -> sign (ln - rn)
            | Number _, List rl -> compare [ l ] rl
            | List ll, Number _ -> compare ll [ r ]
            | List ll, List rl -> compare ll rl

        if comp <> 0 then comp else compare ll rl

let solve1 input =
    input |> List.choosei (fun i (l, r) -> if compare l r < 0 then Some(i + 1) else None) |> List.sum

let solve2 input =
    let dividers = [ [ List [ Number 2 ] ]; [ List [ Number 6 ] ] ]

    input
    |> List.collect (fun (l, r) -> [ l; r ])
    |> List.append dividers
    |> List.sortWith compare
    |> List.choosei (fun i t -> if List.contains t dividers then Some(i + 1) else None)
    |> List.reduce (*)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Theory>]
    [<InlineData("[1,1,3,1,1]", "[1,1,5,1,1]", -1)>]
    [<InlineData("[[1],[2,3,4]]", "[[1],4]", -1)>]
    [<InlineData("[9]", "[[8,7,6]]", 1)>]
    [<InlineData("[[4,4],4,4]", "[[4,4],4,4,4]", -1)>]
    [<InlineData("[7,7,7,7]", "[7,7,7]", 1)>]
    [<InlineData("[]", "[3]", -1)>]
    [<InlineData("[[[]]]", "[[]]", 1)>]
    [<InlineData("[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]", 1)>]
    let ``Example part 1 - individual comparisons`` left right expected =
        parseTestInput parser [| left; right; "" |] |> List.exactlyOne |> uncurry compare |> should equal expected

    let input =
        [| "[1,1,3,1,1]"
           "[1,1,5,1,1]"
           ""
           "[[1],[2,3,4]]"
           "[[1],4]"
           ""
           "[9]"
           "[[8,7,6]]"
           ""
           "[[4,4],4,4]"
           "[[4,4],4,4,4]"
           ""
           "[7,7,7,7]"
           "[7,7,7]"
           ""
           "[]"
           "[3]"
           ""
           "[[[]]]"
           "[[]]"
           ""
           "[1,[2,[3,[4,[5,6,7]]]],8,9]"
           "[1,[2,[3,[4,[5,6,0]]]],8,9]"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 13

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 140
