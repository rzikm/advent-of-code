module AoC202421

open AdventOfCode
open FSharpPlus
open FParsec

let parser = ParseUtils.lines (many1 (anyOf "0123456789A"))

let getButtonSequence getPos avoid f t =
    let isViablePath avoid from path =
        path
        |> List.map (fun c ->
            match c with
            | '>' -> (1, 0)
            | '<' -> (-1, 0)
            | '^' -> (0, -1)
            | 'v' -> (0, 1)
            | _ -> failwith "Invalid key")
        |> List.scan Tuple2.add from
        |> List.forall ((<>) avoid)

    let (dx, dy) = Tuple2.sub (getPos t) (getPos f)
    let dxs = List.replicate (abs dx) (if dx > 0 then '>' else '<')
    let dys = List.replicate (abs dy) (if dy > 0 then 'v' else '^')

    Utils.permutations (dxs @ dys)
    |> Seq.distinct
    |> Seq.filter (isViablePath avoid (getPos f))
    |> Seq.map (fun p -> p @ [ 'A' ])

let getButtonSequencesNumpad =
    let getPos =
        function
        | '7' -> (0, 0)
        | '8' -> (1, 0)
        | '9' -> (2, 0)
        | '4' -> (0, 1)
        | '5' -> (1, 1)
        | '6' -> (2, 1)
        | '1' -> (0, 2)
        | '2' -> (1, 2)
        | '3' -> (2, 2)
        | '0' -> (1, 3)
        | 'A' -> (2, 3)
        | _ -> failwith "Invalid key"

    getButtonSequence getPos (0, 3)

let getButtonSequencesDpad =
    let getPos =
        function
        | '^' -> (1, 0)
        | 'A' -> (2, 0)
        | '<' -> (0, 1)
        | 'v' -> (1, 1)
        | '>' -> (2, 1)
        | _ -> failwith "Invalid key"

    getButtonSequence getPos (0, 0)

let getButtonPressCount level code =
    let rec eval l =
        'A' :: l |> List.pairwise |> List.sumBy (fun (f, t) -> getCount (level, f, t))

    and getCount =
        Utils.memoizerec
        <| fun frec (level, f, t) ->
            match level with
            | 0 -> 1L
            | _ ->
                let eval l =
                    'A' :: l |> List.pairwise |> List.sumBy (fun (f, t) -> frec (level - 1, f, t))

                getButtonSequencesDpad f t |> Seq.map eval |> Seq.min


    'A' :: code |> List.pairwise |> List.sumBy (fun (f, t) -> getButtonSequencesNumpad f t |> Seq.map eval |> Seq.min)

let solve n input =
    let getComplexity code =
        let i = List.takeWhile (isDigit) code |> String.ofList |> int64
        i * getButtonPressCount n code

    input |> List.sumBy getComplexity

let solution = makeSolution () parser (solve 2) (solve 25)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "029A"; "980A"; "179A"; "456A"; "379A" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 126384L

    [<Theory>]
    [<InlineData("029A", 0, "<A^A>^^AvvvA")>]
    [<InlineData("029A", 1, "v<<A>>^A<A>AvA<^AA>A<vAAA>^A")>]
    [<InlineData("029A", 2, "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A")>]
    [<InlineData("980A", 2, "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A")>]
    [<InlineData("179A", 2, "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A")>]
    [<InlineData("456A", 2, "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A")>]
    [<InlineData("379A", 2, "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A")>]
    let ``Example part 1 - single code`` code level expected =
        List.ofSeq code |> getButtonPressCount level |> should equal (int64 <| String.length expected)
