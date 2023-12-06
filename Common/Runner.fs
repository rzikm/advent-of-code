module Runner

open Argu
open System
open FSharpPlus
open AdventOfCode

type Arguments =
    | [<MainCommand>] Day of int list
    | [<Unique; AltCommandLine("-s")>] Submit of int

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Day _ -> "Day which to run"
            | Submit _ -> "Submit solution for part specified part. Requires a single day to be specified"

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
                Ok(solution)
            | _ -> Error $"{className}.solution is not a property")

let runWithStopwatch func arg =
    let sw = Diagnostics.Stopwatch.StartNew()
    let res = func arg
    let elapsed = sw.Elapsed.TotalMilliseconds
    (res, elapsed)

let private httpClient =
    let sessionFile = "../session"

    if not <| IO.File.Exists sessionFile then
        raise (new Exception("The session file does not exist"))

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

let submitResult year day part result =
    try
        let response =
            httpClient
                .PostAsync(
                    $"https://adventofcode.com/{year}/day/{day}/answer",
                    new Net.Http.FormUrlEncodedContent(
                        Map.ofList [ ("level", part.ToString())
                                     ("answer", result.ToString()) ]
                    )
                )
                .Result

        response.EnsureSuccessStatusCode() |> ignore
        let body = response.Content.ReadAsStringAsync().Result

        Text
            .RegularExpressions
            .Regex
            .Match(
                body,
                ".*<article><p>([^<]*[.!?])*",
                Text.RegularExpressions.RegexOptions.Singleline
            )
            .Groups.[1]
            .Value
        |> printfn "%s"
    with
    | e -> printfn "Error submitting result: %s" e.Message

type PartResult = { Part: int; Time: double; Result: Result<obj, string> }
type RunResult = { ParsingTime: double; Parts: PartResult list }

let printResult day (result: Result<RunResult, string>) =
    match result with
    | Ok (result) ->
        printfn "Day %d (parsed in %.2f ms):" day result.ParsingTime

        result.Parts
        |> List.iter (fun part ->
            printf "   Part %d (%.2f ms): " part.Part part.Time

            match part.Result with
            | Ok res -> printfn "%O" res
            | Error err -> printfn "Error: %s" err)
    | Error err -> printfn "Day %d Error: %s" day err


let runSolutionPart part solution input =
    try
        if part = 1 then
            Ok(solution.solve1 input)
        else
            Ok(solution.solve2 input)
    with
    | e -> Error(e.Message)

let runSolution year day parts =
    let filename = $"in/{day:D2}.in"

    try
        let solution = getSolution $"AoC{year}" day |> Result.get

        if not <| IO.File.Exists filename then
            downloadInput year day

        use stream = System.IO.File.OpenRead filename

        let res =
            let r, t = runWithStopwatch (solution.parse stream) filename
            r |> Result.map (fun r -> r, t)

        res
        |> Result.map (fun (input, time) ->
            { ParsingTime = time
              Parts =
                parts
                |> List.map (fun part ->
                    let (result, time) = runWithStopwatch (runSolutionPart part solution) input
                    { Part = part; Time = time; Result = result })

            })
    with
    | ex -> Error(ex.Message)

let run year args =
    let parser = ArgumentParser.Create<Arguments>()

    try
        let results = parser.Parse args

        let days =
            results.PostProcessResults(
                <@ Day @>,
                List.map (fun i -> if i < 0 || i > 25 then failwith "Invalid day" else i)
            )
            |> List.tryHead
            |> Option.defaultValue [ 1..25 ]

        let submit =
            results.PostProcessResults(
                <@ Submit @>,
                (fun i ->
                    if List.length days <> 1 then
                        failwith "In order to submit, a single day must be specified"

                    if i < 0 || i > 2 then failwith "Invalid part" else i)
            )
            |> List.tryHead

        match submit with
        | Some part ->
            let day = List.head days
            let result = runSolution year day [ part ]
            printResult day result

            match result with
            | Ok ({ Parts = [ { Result = Ok (res) } ] }) -> submitResult year day part res
            | _ -> printfn "Nothing to submit"

        | None -> days |> List.iter (fun day -> runSolution year day [ 1; 2 ] |> printResult day)

        0

    with
    | e ->
        printfn "%s" e.Message
        1
