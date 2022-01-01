module Program

open System
open FSharpPlus
open AdventOfCode

let parseArgs (args : string[]) =
    match args with
    | [| num |] ->
        match Int32.TryParse num with
        | true, day when day >= 1 && day <= 25 ->
            Ok day
        | _ ->  Error "Argument must be a single number between 1 and 25"
    | _ ->  Error "Argument must be a single number between 1 and 25"

let getSolution (day : int) =
    let getType typename =
        match Type.GetType typename with
        | null -> Error (sprintf "Unable to find type %s" typename)
        | t -> Ok t

    let className = $"AoC2020{day:D2}"
    getType className >>= (fun (t : Type) ->
        match t.GetMember "solution" |> Array.tryHead with
        | None -> Error $"Type {className} does not have member 'solution'"
        | Some me ->
            match me with
            | :? System.Reflection.PropertyInfo as fi ->
                let solution : Solution = downcast fi.GetMethod.Invoke(null, null)
                Ok (day, solution)
            | _ -> Error $"{className}.solution is not a property")

let runSolution (day : int, solution : Solution) =
    let res =
        match FParsec.CharParsers.runParserOnFile
            (solution.parser) () ($"in/{day:D2}.in") (System.Text.Encoding.UTF8) with
        | FParsec.CharParsers.Success (input, _, _) -> Ok input
        | FParsec.CharParsers.Failure (error, _, _) -> Error error

    res >>= (fun input ->
        let easy = solution.solve Easy input
        let hard = solution.solve Hard input
        Ok (easy, hard))

[<EntryPoint>]
let main args =
    let res =
        args
        |> parseArgs
        >>= getSolution
        >>= runSolution

    match res with
    | Ok r -> printfn "%A" r; 0
    | Error s -> printfn "%s" s; 1
