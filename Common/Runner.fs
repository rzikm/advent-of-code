module Runner

open System
open FSharpPlus
open AdventOfCode

let parseArgs (args: string []) =
    match args with
    | [| num |] ->
        match Int32.TryParse num with
        | true, day when day >= 1 && day <= 25 -> Ok [ day ]
        | _ -> Error "Argument must be a single number between 1 and 25"
    | _ -> Ok [ 1..25 ]

let getSolution classPrefix (day: int) =
    let getType typename =
        match Type.GetType typename with
        | null -> Error(sprintf "Unable to find type %s" typename)
        | t -> Ok t

    let className = $"{classPrefix}{day:D2}"

    getType className
    >>= (fun (t: Type) ->
        match t.GetMember "solution" |> Array.tryHead with
        | None -> Error $"Type {className} does not have member 'solution'"
        | Some me ->
            match me with
            | :? System.Reflection.PropertyInfo as fi ->
                let solution: Solution = downcast fi.GetMethod.Invoke(null, null)
                Ok(day, solution)
            | _ -> Error $"{className}.solution is not a property")

let runWithStopwatch func arg =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = func arg
    let elapsed = sw.ElapsedMilliseconds
    (res, elapsed)

let private httpClient =
    let sessionFile = "../session"

    if not <| IO.File.Exists sessionFile then
        raise (new Exception("Neither the input file or the session file does not exist"))

    let session = IO.File.ReadAllText sessionFile
    let handler = new Net.Http.HttpClientHandler()

    let cookieContainer = new Net.CookieContainer()
    cookieContainer.SetCookies(new Uri("https://adventofcode.com"), session)
    handler.CookieContainer <- cookieContainer

    new Net.Http.HttpClient(handler)

let downloadInput year day =

    let bytes =
        httpClient
            .GetByteArrayAsync(
                $"https://adventofcode.com/{year}/day/{day}/input"
            )
            .Result

    IO.File.WriteAllBytes($"in/{day:D2}.in", bytes)

let runSolution year (day: int, solution: Solution) =
    let filename = $"in/{day:D2}.in"

    try
        if not <| IO.File.Exists filename then
            downloadInput year day

        use stream = System.IO.File.OpenRead filename

        let res =
            match
                runWithStopwatch
                    (FParsec.CharParsers.runParserOnStream solution.parser () filename stream)
                    System.Text.Encoding.UTF8
                with
            | FParsec.CharParsers.Success (input, _, _), time -> Ok(input, time)
            | FParsec.CharParsers.Failure (error, _, _), _ -> Error error

        res
        >>= (fun (input, time) ->
            let easy = runWithStopwatch solution.solve1 input
            let hard = runWithStopwatch solution.solve2 input
            Ok(time, easy, hard))
    with
    | ex -> Error(ex.Message)

let run year args =
    let res =
        args |> parseArgs |> Result.map (Seq.map (fun d -> d, getSolution $"AoC{year}" d >>= runSolution year))

    let print (day, result) =

        match result with
        | Ok (time, (r1, t1), (r2, t2)) ->
            printfn "Day %d (parsed in %d ms):" day time
            printfn "   Part 1: %O (%d ms)" r1 t1
            printfn "   Part 2: %O (%d ms)" r2 t2
        | Error err ->
            printfn "Day %d" day
            printfn "   %s" err

    match res with
    | Ok l ->
        Seq.iter print l
        0
    | Error s ->
        printfn "%s" s
        1
