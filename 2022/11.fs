module AoC202211

open AdventOfCode
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens
open FParsec
open Utils

type Operation =
    | Plus of int64
    | Times of int64
    | Square

type Monkey =
    { Id: int32
      Items: int64 list
      Operation: Operation
      Test: int64
      Outcome: int32 * int32 }

// necessary lenses
let inline _items f m =
    f m.Items <&> fun x -> { m with Items = x }

let inline _test f m =
    f m.Test <&> fun x -> { m with Test = x }

let parser =
    let startItems = pstring "  Starting items: " >>. sepBy1 pint64 (pstring ", ")

    let operation =
        pstring "  Operation: new = "
        >>. choice [ stringReturn "old * old" Square
                     pstring "old + " >>. pint64 |>> Plus
                     pstring "old * " >>. pint64 |>> Times ]

    let test = pstring "  Test: divisible by " >>. pint64
    let ifTrue = pstring "    If true: throw to monkey " >>. pint32
    let ifFalse = pstring "    If false: throw to monkey " >>. pint32

    let newline = skipChar '\n'

    let monkey =
        tuple5
            (pstring "Monkey " >>. pint32 .>> pchar ':' .>> newline)
            (startItems .>> newline)
            (operation .>> newline)
            (test .>> newline)
            (ifTrue .>> newline .>>. ifFalse .>> newline)
        |>> (fun (id, items, op, test, outcome) ->
            { Id = id; Items = items; Test = test; Operation = op; Outcome = outcome })

    sepEndBy1 monkey newline

let solve rounds worry input =
    let worry = worry input // init the worry modifier function
    let monkeys = input |> List.map (fun m -> (m.Id, m)) |> Map.ofList
    let inspectCount = input |> List.map (fun m -> (m.Id, 0L)) |> Map.ofList

    let redistributeItem ownerMonkey monkeys level =
        let level =
            match ownerMonkey.Operation with
            | Plus i -> level + i |> worry
            | Times i -> level * i |> worry
            | Square -> level * level |> worry

        let targetMonkey =
            match level % ownerMonkey.Test with
            | 0L -> fst ownerMonkey.Outcome
            | _ -> snd ownerMonkey.Outcome

        monkeys |> over (Map._item targetMonkey << _Some << _items) (flip List.append [ level ])

    let doMonkey (monkeys, inspectCount) monkeyId =
        let monkey = Map.find monkeyId monkeys

        // set empty list on current monkey and redistribute the items
        (monkeys
         |> setl (Map._item monkeyId << _Some << _items) []
         |> flip (List.fold (redistributeItem monkey)) monkey.Items,
         // increment inspection counter
         inspectCount |> over (Map._item monkeyId << _Some) ((+) (List.length monkey.Items |> int64)))

    let doRound state =
        input |> List.map (fun m -> m.Id) |> List.fold doMonkey state

    (doRound ^ rounds) (monkeys, inspectCount)
    |> snd
    |> Map.values
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)

let worry1 _ i = i / 3L

let worry2 (input: Monkey list) =
    let test = input |> List.map (fun m -> m.Test) |> List.reduce (*)
    fun i -> i % test

let solution = makeSolution parser (solve 20 worry1) (solve 10000 worry2)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "Monkey 0:"
           "  Starting items: 79, 98"
           "  Operation: new = old * 19"
           "  Test: divisible by 23"
           "    If true: throw to monkey 2"
           "    If false: throw to monkey 3"
           ""
           "Monkey 1:"
           "  Starting items: 54, 65, 75, 74"
           "  Operation: new = old + 6"
           "  Test: divisible by 19"
           "    If true: throw to monkey 2"
           "    If false: throw to monkey 0"
           ""
           "Monkey 2:"
           "  Starting items: 79, 60, 97"
           "  Operation: new = old * old"
           "  Test: divisible by 13"
           "    If true: throw to monkey 1"
           "    If false: throw to monkey 3"
           ""
           "Monkey 3:"
           "  Starting items: 74"
           "  Operation: new = old + 3"
           "  Test: divisible by 17"
           "    If true: throw to monkey 0"
           "    If false: throw to monkey 1"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 10605L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 2713310158L
