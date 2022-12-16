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

let solve timeLimit count input =
    let compressed = compressGraph "AA" input

    let rec search (time, rate, released, poss, opened) =
        if time = timeLimit then
            (time, rate, released, poss, opened)
        else
            match List.tryFindIndex (snd >> (=) 0) poss with
            | None ->
                // everybody is traveling/working, fast forward by the lowest time
                let (n, dt) = poss |> List.minBy snd
                search (time + dt, rate, released + dt * rate, (over (List.traverse << _2) (flip (-) dt) poss), opened)

            | Some i ->
                let current = poss |> List.item i |> fst
                // somebody reached a valve and opened it
                let (valveRate, neighbors) = Map.find current compressed
                let rate = valveRate + rate

                // state if he waits until the end of time limit
                let ifWaited =
                    [ search (time, rate, released, setl (List._item i << _Some) ("AA", timeLimit - time) poss, opened) ]

                // state if he moves to open some valve
                let ifMoved =
                    neighbors
                    |> List.choose (fun (n, dist) ->
                        // travel + one minute to fully open the valve
                        if time + dist + 1 < timeLimit && not <| List.contains n opened then
                            Some(
                                search (
                                    time,
                                    rate,
                                    released,
                                    setl (List._item i << _Some) (n, dist + 1) poss,
                                    n :: opened
                                )
                            )
                        else
                            None)

                List.concat [ ifWaited; ifMoved ] |> List.maxBy (fun (_, _, r, _, _) -> r)

    let (_, _, released, _, _) = search (0, 0, 0, List.replicate count ("AA", 0), [])
    released

let solution = makeSolution parser (solve 30 1) (solve 26 2)

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
