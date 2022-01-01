module AdventOfCode

open System
open FParsec

type Solution = {
    parser : Parser<Object, unit>
    solve1 : Object -> Object
    solve2 : Object -> Object
}

let makeSolution
    (parser : Parser<'input, unit>)
    (solve1 : 'input -> 'solution)
    (solve2 : 'input -> 'solution2) =
    {
        parser = parser |>> fun res -> upcast res
        solve1 = fun input -> (solve1 (downcast input)) :> Object
        solve2 = fun input -> (solve2 (downcast input)) :> Object
    }

let parseTestInput parser input =
    match run parser (String.concat "\n" input) with
    | Success (res, _, _) -> res
    | Failure (err, _, _) -> failwith err

let testPart1 (solution : Solution) input =
    parseTestInput solution.parser input
    |> solution.solve1

let testPart2 (solution : Solution) input =
    parseTestInput solution.parser input
    |> solution.solve2
