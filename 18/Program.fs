module Program

open FParsec;
open Printf;
open System.Text;
open System.IO;

type  Num =
    | Number of int
    | Pair of Num * Num

module Num =
    let private pPair =
        let p, ref = createParserForwardedToRef ()
        ref :=
            (pint32 |>> Number) <|>
            (pchar '[' >>. p .>> pchar ',' .>>. p .>> pchar ']' |>> Pair)
        p

    let parse input =
        match run pPair input with
        | Success(res, _, _) -> res
        | Failure(err, _, _) -> failwith err

    let toString num =
        let rec recurse sb num =
            match num with
            | Number x -> bprintf sb "%d" x
            | Pair (l, r) -> bprintf sb "[%a,%a]" recurse l recurse r
        let sb = StringBuilder()
        recurse sb num
        sb.ToString()

    let reduceOnce num =
        let rec addLeft n num =
            match num with
            | Number i -> Number (n + i)
            | Pair (l, r) -> Pair (addLeft n l, r)

        let rec addRight n num =
            match num with
            | Number i -> Number (n + i)
            | Pair (l, r) -> Pair (l, addRight n r)

        let rec explode depth num =
            match num with
            | Number _ -> None, num
            | Pair (left, right) ->
                if depth = 4 then
                    let (Number l) = left
                    let (Number r) = right
                    Some (Some l, Some r), Number 0
                else
                    match explode (depth + 1) left with
                    | Some (sl, Some r), num -> Some (sl, None), Pair (num, addLeft r right)
                    | Some s, num -> Some s, Pair (num, right)
                    | None, _ ->
                        match explode (depth + 1) right with
                        | Some (Some l, sr), num -> Some (None, sr), Pair (addRight l left, num)
                        | s, num -> s, Pair (left, num)

        let rec split num =
            match num with
            | Number n when n > 9 -> Some (Pair (Number (n / 2), Number (n - n / 2)))
            | Number _ -> None
            | Pair (l, r) ->
                split l |> Option.map (fun nl -> Pair (nl, r)) |> Option.orElseWith (fun () ->
                    split r |> Option.map (fun nr -> Pair(l, nr)))

        match explode 0 num with
        | Some _, newNum -> newNum
        | _ -> split num |> Option.defaultValue num

    let reduce num =
        let rec reduce' num =
            let reduced = reduceOnce num
            if num = reduced then
                reduced
            else
                reduce' reduced

        reduce' num

    let add left right = reduce (Pair (left, right))

    let rec magnitude = function
        | Number n -> n
        | Pair (l, r) -> (3 * magnitude l) + (2 * magnitude r)

let part1 input = input |> Seq.reduce Num.add |> Num.magnitude

let part2 input =
    (input, input) ||> Seq.allPairs |> Seq.filter (fun (l, r) -> l <> r)
    |> Seq.map (fun (l, r) -> Num.add l r) |> Seq.map Num.magnitude |> Seq.max

module Tests =
    open Xunit
    open Num
    open FsUnit

    [<Fact>]
    let ``Parse [[1,2],[[3,4],5]]`` () =
        let expected = Pair (Pair (Number 1, Number 2),Pair (Pair (Number 3, Number 4), Number 5))
        parse "[[1,2],[[3,4],5]]" |> should equal expected

    [<Fact>]
    let ``Num to String`` () =
        let num = Pair (Pair (Number 1, Number 2),Pair (Pair (Number 3, Number 4), Number 5))
        toString num |> should equal "[[1,2],[[3,4],5]]"

    [<Theory>]
    [<InlineData("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")>]
    [<InlineData("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")>]
    [<InlineData("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")>]
    [<InlineData("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")>]
    [<InlineData("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")>]
    [<InlineData("[10,1]", "[[5,5],1]")>]
    [<InlineData("[1,11]", "[1,[5,6]]")>]
    let ``Reduce samples`` (num, expected) =
        parse num |> reduceOnce |> toString |> should equal expected

    [<Theory>]
    [<InlineData("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")>]
    let ``Add samples`` (left, right, result) =
        (parse left, parse right) ||> add |> toString |> should equal result

    [<Theory>]
    [<InlineData()>]
    [<InlineData("[[1,2],[[3,4],5]]", 143)>]
    [<InlineData("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384)>]
    [<InlineData("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445)>]
    [<InlineData("[[[[3,0],[5,3]],[4,4]],[5,5]]", 791)>]
    [<InlineData("[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137)>]
    [<InlineData("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488)>]
    let ``Magnitude examples`` (num, expected) =
        parse num |> magnitude |> should equal expected

    let exampleInput () =
        [ "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]";
        "[[[5,[2,8]],4],[5,[[9,9],0]]]";
        "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]";
        "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]";
        "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]";
        "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]";
        "[[[[5,4],[7,7]],8],[[8,3],8]]";
        "[[9,3],[[9,9],[6,[4,9]]]]";
        "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]";
        "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]" ]
        |> List.map parse

    [<Fact>]
    let ``Example Part 1`` () =
        exampleInput () |> part1 |> should equal 4140

    [<Fact>]
    let ``Example Part 2`` () =
        exampleInput () |> part2 |> should equal 3993

[<EntryPoint>]
let main _ =
    let input =
        File.ReadLines("input.txt")
        |> Seq.map Num.parse
        |> List.ofSeq

    printfn "%A" (part1 input)
    printfn "%A" (part2 input)
    0
