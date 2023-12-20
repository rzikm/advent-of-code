module AoC202320

open AdventOfCode
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens
open FParsec

type PulseValue =
    | High
    | Low

type Pulse = string * PulseValue

type ModuleState =
    | Broadcaster
    | FlipFlop of bool
    | Conjunction of Pulse list

type Module = { name: string; state: ModuleState; connections: string list }

let inline _state f s =
    f s.state <&> fun state -> { s with state = state }

let inline _connections f s =
    f s.connections <&> fun connections -> { s with connections = connections }

let inline _Conjunction f =
    prism'
        Conjunction
        (function
        | Conjunction x -> Some x
        | _ -> None)
        f

let inline _FlipFlop f =
    prism'
        FlipFlop
        (function
        | FlipFlop x -> Some x
        | _ -> None)
        f

let parser =
    let name = many1Satisfy isLetter

    let state =
        choice [ stringReturn "broadcaster" Broadcaster |>> Tuple2.create "broadcaster"
                 skipChar '%' >>. name |>> flip Tuple2.create (FlipFlop false)
                 skipChar '&' >>. name |>> flip Tuple2.create (Conjunction []) ]

    let connections = sepBy1 name (skipString ", ")

    let module_ =
        state .>> skipString " -> " .>>. connections
        |>> fun (state, connections) -> { name = fst state; state = snd state; connections = connections }

    let initializeState modules =
        let lookup = Seq.toLookupMap (fun m -> m.name) modules

        let folder mods (m: Module) =
            m.connections
            |> List.fold
                (fun mods connection ->
                    over (Map._item connection << _Some << _state << _Conjunction) (List.cons (m.name, Low)) mods)
                mods

        List.fold folder lookup modules

    ParseUtils.lines module_ |>> initializeState

let sendPulse modules =
    let newPulses value m =
        m.connections |> List.map (fun c -> (m.name, c, value))

    let rec sendPulse' acc pulses state =
        match pulses with
        | [] -> acc, state
        | (src, dst, value) :: pulses ->
            let acc = (src, dst, value) :: acc

            match Map.tryFind dst state with
            | None -> sendPulse' acc pulses state
            | Some m ->
                match m.state with
                | Broadcaster -> sendPulse' acc (pulses @ newPulses value m) state
                | FlipFlop s ->
                    match value with
                    | High -> sendPulse' acc pulses state // no change
                    | Low ->
                        let pulses = pulses @ newPulses (if not s then High else Low) m
                        let state = over (Map._item dst << _Some << _state << _FlipFlop) not state
                        sendPulse' acc pulses state
                | Conjunction values ->
                    let values = setl (items << filtered (fst >> (=) src) << _2) value values

                    let pulses =
                        let allHigh = allOf (items << _2) ((=) High) values
                        pulses @ newPulses (if allHigh then Low else High) m

                    sendPulse' acc pulses (setl (Map._item dst << _Some << _state << _Conjunction) values state)

    sendPulse' [] [ ("button", "broadcaster", Low) ] modules

let solve1 input =
    let f (p, state) =
        let pulses, state = sendPulse state
        let counts = List.countBy item3 pulses
        let low = (counts |> List.find (fst >> (=) Low) |> snd |> int64)
        let high = (counts |> List.find (fst >> (=) High) |> snd |> int64)
        (Tuple2.add (low, high) p, state)

    Utils.applyN 1000 f ((0L, 0L), input) |> fst |> Tuple2.reduce (*)

let solve2 input =
    // the structure of the network is such that there are multiple disjunct areas which cycle
    // independently and then join in the penultimate module before rx via a conjunction
    // the solution is to find periods of each area and find their least common multiple

    let runTillRxFires state =
        let rec runTillRxFires' c state =
            let pulses, state = sendPulse state

            if (pulses |> List.exists (fun (_, dst, value) -> dst = "rx" && value = Low)) then
                c + 1L
            else
                runTillRxFires' (c + 1L) state

        runTillRxFires' 0L state

    let penultimate =
        input |> Map.values |> Seq.find (fun m -> m.connections = [ "rx" ])

    let values =
        match penultimate.state with
        | Conjunction values -> values
        | _ -> failwith "unexpected state"

    values
    |> List.map (fun v ->
        setl (Map._item penultimate.name << _Some << _state << _Conjunction) [ v ] input |> runTillRxFires)
    |> Math.lcm

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "broadcaster -> a"
           "%a -> inv, con"
           "&inv -> b"
           "%b -> con"
           "&con -> output" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 11687500L
