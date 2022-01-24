module AoC202007

open AdventOfCode
open FSharpPlus
open FParsec
open Utils

let parser =
    let pspace = pchar ' '

    let pword = many1Satisfy (fun c -> System.Char.IsWhiteSpace c |> not)

    let pcolor =
        pword .>> pspace .>>. pword
        |>> (fun (l, r) -> l + " " + r)

    let pbags =
        pcolor
        .>> pspace
        .>> pstring "bag"
        .>> optional (pchar 's')

    let pcontainedBags = sepBy1 (pint32 .>> pspace .>>. pbags) (pstring ", ")

    let pcontainedNothing = pstring "no other bags" >>% []

    let pline =
        pbags .>> pstring " contain "
        .>>. (pcontainedBags <|> pcontainedNothing)
        .>> skipRestOfLine false

    sepEndBy1 pline (pchar '\n')

let part1 input =
    let folder state (color, containedList) =
        containedList
        |> List.fold
            (fun state (_, innerColor) ->
                match Map.tryFind innerColor state with
                | Some l -> Map.add innerColor (color :: l) state
                | None -> Map.add innerColor (List.singleton color) state)
            state

    let containedMap = List.fold folder Map.empty input

    let reachables frec color =
        match Map.tryFind color containedMap with
        | Some l ->
            let r = Set.ofList l
            l |> map frec |> fold Set.union r
        | None -> Set.empty

    memoizerec reachables "shiny gold" |> Set.count

let part2 input =
    let containingMap = Map.ofList input

    let totalBags frec color =
        Map.find color containingMap
        |> List.sumBy (fun (count, innerColor) -> count * (1 + frec innerColor))

    memoizerec totalBags "shiny gold"

let solve input = 0

let solution = makeSolution parser part1 part2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "light red bags contain 1 bright white bag, 2 muted yellow bags."
           "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
           "bright white bags contain 1 shiny gold bag."
           "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
           "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
           "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
           "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
           "faded blue bags contain no other bags."
           "dotted black bags contain no other bags." |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 4

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 32

    [<Fact>]
    let ``Example part 2 - 2`` () =
        let input =
            [| "shiny gold bags contain 2 dark red bags."
               "dark red bags contain 2 dark orange bags."
               "dark orange bags contain 2 dark yellow bags."
               "dark yellow bags contain 2 dark green bags."
               "dark green bags contain 2 dark blue bags."
               "dark blue bags contain 2 dark violet bags."
               "dark violet bags contain no other bags." |]

        testPart2 solution input |> should equal 126
