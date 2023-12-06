module AoC201901

open AdventOfCode
open FSharpPlus
open FParsec

let parser = sepEndBy1 pint64 spaces

let getRequiredFuel mass = max (mass / 3L - 2L) 0L

let getTotalRequiredFuel mass =
    let rec loop acc mass =
        match getRequiredFuel mass with
        | 0L -> acc
        | fuel -> loop (acc + fuel) fuel

    loop 0L mass

let solution =
    makeSolution () parser (List.sumBy getRequiredFuel) (List.sumBy getTotalRequiredFuel)

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Theory>]
    [<InlineData(12, 2)>]
    [<InlineData(14, 2)>]
    [<InlineData(1969, 654)>]
    [<InlineData(100756, 33583)>]
    let ``Example part 1`` mass fuel =
        getRequiredFuel mass |> should equal (int64 fuel)

    [<Theory>]
    [<InlineData(14, 2)>]
    [<InlineData(1969, 966)>]
    [<InlineData(100756, 50346)>]
    let ``Example part 2`` mass fuel =
        getTotalRequiredFuel mass |> should equal (int64 fuel)
