module AdventOfCode

open System
open FParsec

type Solution =
    { parse: IO.Stream -> string -> Result<Object, string>
      solve1: Object -> Object
      solve2: Object -> Object }

let makeSolution
    (defaultParserState: 'state)
    (parser: Parser<'input, 'state>)
    (solve1: 'input -> 'solution)
    (solve2: 'input -> 'solution2)
    =
    let parse stream name =
        match runParserOnStream parser defaultParserState name stream Text.Encoding.UTF8 with
        | Success (res, _, _) -> Result.Ok(res :> Object)
        | Failure (err, _, _) -> Result.Error((err.ToString()))

    { parse = parse
      solve1 = fun input -> (solve1 (downcast input)) :> Object
      solve2 = fun input -> (solve2 (downcast input)) :> Object }

let parseTestInput parser input =
    match runParserOnString parser () (String.concat "\n" input) "input" with
    | Success (res, _, _) -> res
    | Failure (err, _, _) -> failwith err

let private parseTestInputFromSolution (solution: Solution) input =
    let stream =
        new System.IO.MemoryStream(System.Text.Encoding.UTF8.GetBytes(String.concat "\n" input))

    match solution.parse stream "input" with
    | Result.Ok (res) -> res
    | Result.Error (err) -> failwith err

let testPart1 (solution: Solution) input =
    parseTestInputFromSolution solution input |> solution.solve1

let testPart2 (solution: Solution) input =
    parseTestInputFromSolution solution input |> solution.solve2
