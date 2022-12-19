module AoC201510

open Utils
open AdventOfCode
open FSharpPlus
open FParsec

let parser = many1Chars (satisfy isDigit)

let solve n input =
    let step input =
        let rec convert (acc: System.Text.StringBuilder) (input: char list) =
            match input with
            | [] -> acc.ToString()
            | a :: _ ->
                let count = Seq.takeWhile ((=) a) input |> Seq.length
                acc.Append count |> ignore
                acc.Append a |> ignore

                let rest = input |> List.skip count

                convert acc rest

        let sb = new System.Text.StringBuilder()

        input |> String.toList |> convert sb

    (step ^ n) input |> String.length

let solution = makeSolution parser (solve 40) (solve 50)
