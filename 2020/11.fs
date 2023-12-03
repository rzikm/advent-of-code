module AoC202011

open AdventOfCode
open FSharpPlus
open FParsec
open Utils

type Cell =
    | Empty
    | Seat
    | Occupied

let parser =
    let pempty = charReturn '.' Empty
    let pseat = charReturn 'L' Seat
    let poccupied = charReturn '#' Occupied
    let pcell = choice [ pempty; pseat; poccupied ]
    let pnewline = pchar '\n'

    let line = many1 pcell |>> Array.ofList

    sepEndBy line pnewline |>> Array.ofList

let nextRound counter threshold (input: Cell [] []) =
    Array.init (Array.length input) (fun y ->
        Array.init (Array.length (Array.item y input)) (fun x ->
            match Array.item2d x y input with
            | Empty -> Empty
            | Seat -> if counter x y input = 0 then Occupied else Seat
            | Occupied -> if counter x y input >= threshold then Seat else Occupied))

let run input counter threshold =
    let rec finalState input =
        let next = nextRound counter threshold input

        if next <> input then finalState next else next

    finalState input |> Array.sumBy (Array.sumBy (fun c -> if c = Occupied then 1 else 0))

let easyCounter x y array =
    Array.neighbors2d8 x y array |> Seq.filter ((=) Occupied) |> Seq.length

let easy input = run input easyCounter 4

let hard input =
    let counter x y array =
        Tuple2.neighbors8 (x, y)
        |> Seq.choose (fun (xx, yy) -> Array.tryItem2d xx yy array)
        |> Seq.filter ((=) Occupied)
        |> Seq.length

    run input counter 5

let solution = makeSolution () parser easy hard

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "L.LL.LL.LL"
           "LLLLLLL.LL"
           "L.L.L..L.."
           "LLLL.LL.LL"
           "L.LL.LL.LL"
           "L.LLLLL.LL"
           "..L.L....."
           "LLLLLLLLLL"
           "L.LLLLLL.L"
           "L.LLLLL.LL" |]

    [<Fact>]
    let ``Example step 1`` () =
        let expected =
            [| "#.##.##.##"
               "#######.##"
               "#.#.#..#.."
               "####.##.##"
               "#.##.##.##"
               "#.#####.##"
               "..#.#....."
               "##########"
               "#.######.#"
               "#.#####.##" |]
            |> parseTestInput parser

        input |> parseTestInput parser |> nextRound easyCounter 4 |> should equal expected

    [<Fact>]
    let ``Example step 2`` () =
        let expected =
            [| "#.LL.L#.##"
               "#LLLLLL.L#"
               "L.L.L..L.."
               "#LLL.LL.L#"
               "#.LL.LL.LL"
               "#.LLLL#.##"
               "..L.L....."
               "#LLLLLLLL#"
               "#.LLLLLL.L"
               "#.#LLLL.##" |]
            |> parseTestInput parser

        input |> parseTestInput parser |> (nextRound easyCounter 4) ^ 2 |> should equal expected

    [<Fact>]
    let ``Example final step`` () =
        let expected =
            [| "#.#L.L#.##"
               "#LLL#LL.L#"
               "L.#.L..#.."
               "#L##.##.L#"
               "#.#L.LL.LL"
               "#.#L#L#.##"
               "..L.L....."
               "#L#L##L#L#"
               "#.LLLLLL.L"
               "#.#L#L#.##" |]
            |> parseTestInput parser

        input |> parseTestInput parser |> (nextRound easyCounter 4) ^ 5 |> should equal expected

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 37
