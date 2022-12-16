module AoC202216

open AdventOfCode
open FSharpPlus
open FSharpPlus.Lens
open FSharpPlus.Data
open FParsec

let parser =
    let pvalve = many1Chars (satisfy isAsciiUpper)

    let pline =
        pstring "Valve " >>. pvalve
        .>>. tuple2
                 (pstring " has flow rate=" >>. pint32
                  .>> (pstring "; tunnels lead to valves " <|> pstring "; tunnel leads to valve "))
                 (sepBy1 pvalve (pstring ", "))

    sepEndBy1 pline (pchar '\n')

let compressGraph start input =
    let m = input |> Map.ofList

    let fNeighbors v =
        Map.find v m |> snd |> Seq.map (fun n -> (n, 1))

    // interesting valves are the starting one and the nonzero one
    let valves =
        start :: (input |> List.filter (snd >> fst >> (<>) 0) |> List.map fst) |> List.distinct

    valves
    |> List.map (fun v ->
        let rate = Map.find v m |> fst

        let neighbors =
            Graph.shortestPaths fNeighbors v (List.except [ v ] valves) |> List.map (fun (n, (_, dist)) -> (n, dist))

        v, (rate, neighbors))
    |> Map.ofList

let solve timeLimit partitions input =
    let compressed =
        compressGraph "AA" input |> Map.map (fun _ (rate, ns) -> (rate, Map.ofList ns))

    let valves = Map.keys compressed |> Seq.sort |> List.ofSeq |> List.tail

    // maximum pressure released after 'timeleft' starting from 'current' and possibility of opening 'valves' valves
    let maxRateBy =
        Utils.memoizerec (fun frec (timeLeft, current, valves) ->
            let (rate, ns) = Map.find current compressed

            match Set.count valves with
            | 0 ->
                // no valves to open, count just the current valve
                ([ current ], rate * timeLeft)
            | _ ->
                valves
                |> Set.toSeq
                |> Seq.choose (fun next ->
                    // move to next and open it
                    let dist = Map.find next ns + 1

                    if dist < timeLeft then
                        let (path, flowAfterNext) = frec (timeLeft - dist, next, Set.remove next valves)
                        Some(current :: path, flowAfterNext + rate * timeLeft)
                    else
                        None)
                |> Seq.append [ ([ current ], rate * timeLeft) ]
                |> Seq.maxBy snd)

    partitions valves
    |> Seq.map (fun santaValves ->
        let elephantValves = List.except santaValves valves

        (santaValves, elephantValves)
        |> Tuple2.map (fun vs -> (maxRateBy (timeLimit, "AA", Set.ofList vs)))
        |> fun ((_, lr), (_, rr)) -> lr + rr)
    |> Seq.max

let solution =
    makeSolution parser (solve 30 Seq.singleton) (solve 26 Utils.allSubsets) //(solveAlt 26 2)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
           "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
           "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
           "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
           "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
           "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
           "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
           "Valve HH has flow rate=22; tunnel leads to valve GG"
           "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
           "Valve JJ has flow rate=21; tunnel leads to valve II"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 1651

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 1707
