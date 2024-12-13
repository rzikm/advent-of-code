module AoC202413

open AdventOfCode
open FParsec

type Machine = { ButtonA: int64 * int64; ButtonB: int64 * int64; Prize: int64 * int64 }

let parser =
    let parseButton c =
        pstring $"Button {c}: X+" >>. pint64 .>> pstring ", Y+" .>>. pint64 .>> newline

    let parsePrize = pstring "Prize: X=" >>. pint64 .>> pstring ", Y=" .>>. pint64

    let parseMachine =
        tuple3 (parseButton 'A') (parseButton 'B') parsePrize
        |>> fun (a, b, p) -> { ButtonA = a; ButtonB = b; Prize = p }

    sepEndBy1 parseMachine spaces

let solveMachine (m: Machine) =
    let (ax, ay), (bx, by), (px, py) = m.ButtonA, m.ButtonB, m.Prize

    // solve equation system
    //     px = a * ax + b * bx
    //     py = a * ay + b * by

    // ay * (1) - ax * (2) gives
    //     ay * px - ax * py = (a * ax * ay - a * ax * ay) + b * (bx * ay - ax * by)
    //     b = (ay * px - ax * py) / (bx * ay - ax * by)

    let b = (ay * px - ax * py) / (bx * ay - ax * by)
    let a = (px - b * bx) / ax

    // check integral solution
    if (a * ax + b * bx = px) && (a * ay + b * by = py) then
        Some(a, b)
    else
        None

let solve1 input =
    input |> List.choose solveMachine |> List.sumBy (fun (a, b) -> 3L * a + b)

let solve2 input =
    input
    |> List.map (fun (m: Machine) -> { m with Prize = Tuple2.add (Tuple2.broadcast 10000000000000L) m.Prize })
    |> solve1

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "Button A: X+94, Y+34"
           "Button B: X+22, Y+67"
           "Prize: X=8400, Y=5400"
           ""
           "Button A: X+26, Y+66"
           "Button B: X+67, Y+21"
           "Prize: X=12748, Y=12176"
           ""
           "Button A: X+17, Y+86"
           "Button B: X+84, Y+37"
           "Prize: X=7870, Y=6450"
           ""
           "Button A: X+69, Y+23"
           "Button B: X+27, Y+71"
           "Prize: X=18641, Y=10279" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 480L

    [<Theory>]
    [<InlineData(94, 34, 22, 67, 8400, 5400, 80, 40)>]
    let ``Example part 1 - individual machines`` ax ay bx by px py ra rb =
        let machine = { ButtonA = (ax, ay); ButtonB = (bx, by); Prize = (px, py) }

        let expected =
            match (ra, rb) with
            | (0, 0) -> None
            | _ -> Some(int64 ra, int64 rb)

        solveMachine machine |> should equal expected
