module AoC202403

open AdventOfCode
open FParsec

type Instruction =
    | Mul of int * int
    | Do
    | Dont

let parser =
    let pmul =
        attempt (pstring "mul(" >>. pint32 .>> pchar ',' .>>. pint32 .>> pchar ')' |>> Mul)

    let pdo = stringReturn "do()" Do
    let pdont = stringReturn "don't()" Dont

    many1 ((choice [ pmul; pdo; pdont ] |>> Some) <|> (anyChar >>% None)) |>> List.choose id

let solve1 =
    List.sumBy (function
        | Mul (a, b) -> a * b
        | _ -> 0)

let solve2 =
    List.fold
        (fun (acc, s) m ->
            match m with
            | Mul (a, b) -> (acc + (if s then a * b else 0), s)
            | Do -> (acc, true)
            | Dont -> (acc, false))
        (0, true)
    >> fst

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input1 =
        [| "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" |]

    let input2 =
        [| "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input1 |> should equal 161

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input2 |> should equal 48
