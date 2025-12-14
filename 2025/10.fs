module AoC202510

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pTarget =
        skipChar '[' >>. many (charReturn '.' false <|> charReturn '#' true) .>> skipChar ']'

    let pButton = between (skipChar '(') (skipChar ')') (sepBy1 pint32 (pchar ','))
    let pButtons = sepEndBy1 pButton (skipChar ' ')
    let pJoltage = between (skipChar '{') (skipChar '}') (sepBy1 pint32 (pchar ','))
    let pLine = tuple3 (pTarget .>> skipChar ' ') pButtons pJoltage
    ParseUtils.lines pLine .>> eof

let pushButton f state button =
    state |> List.mapi (fun i v -> if List.contains i button then f v else v)

let getConfigureButtonPresses (target: bool list) (buttons: int list list) =
    buttons |> Utils.allSubsets |> Seq.filter (fun b -> List.fold (pushButton not) target b |> List.forall not)

let solve1 input =
    input
    |> List.sumBy (fun (target, buttons, _) ->
        getConfigureButtonPresses target buttons |> Seq.map List.length |> Seq.min)

let solve2 input =
    let solveForJoltage buttons joltages =
        Utils.memoizerec
            (fun loop (joltages) ->
                if List.exists (fun i -> i < 0) joltages then
                    100000000L
                else if joltages |> List.forall ((=) 0) then
                    0L
                else
                    let parities = joltages |> List.map (fun j -> j % 2 = 1)

                    getConfigureButtonPresses parities buttons
                    |> Seq.map (fun bs ->
                        let newJoltages =
                            bs |> List.fold (pushButton (fun j -> j - 1)) joltages |> List.map (fun j -> j / 2)

                        (List.length bs |> int64) + 2L * loop newJoltages)
                    |> Seq.sort
                    |> Seq.tryHead
                    |> Option.defaultValue 100000000L)
            joltages

    input |> List.sumBy (fun (_, buttons, joltages) -> solveForJoltage buttons joltages)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
           "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
           "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 7

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 33L
