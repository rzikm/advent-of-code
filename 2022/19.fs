module AoC202219

open AdventOfCode
open FSharpPlus
open FSharpPlus.Lens
open FSharpPlus.Data
open FParsec

type Mineral =
    | Ore
    | Clay
    | Obsidian
    | Geode

let mineralToIndex =
    function
    | Ore -> 0
    | Clay -> 1
    | Obsidian -> 2
    | Geode -> 3

let parser =
    let space = pchar ' ' <|> pchar '\n'

    let minerals =
        choice [ stringReturn "ore" Ore
                 stringReturn "clay" Clay
                 stringReturn "obsidian" Obsidian
                 stringReturn "geode" Geode ]

    let probot =
        let resource = pint32 .>> pchar ' ' .>>. minerals

        pstring "Each " >>. minerals .>> pstring " robot costs " >>. sepEndBy1 (resource) (pstring " and ")
        .>> pchar '.'

    let pblueprint =
        pstring "Blueprint " >>. pint32 .>> pchar ':' .>> space
        .>>. parray 4 (skipMany (pchar ' ') >>. probot .>> space)

    sepEndBy1 pblueprint (opt space)

let simulateBlueprint time blueprint =
    let maxCosts =
        blueprint
        |> Array.fold
            (fun s recipe ->
                recipe |> List.fold (fun s (count, ore) -> Array.mapAt (mineralToIndex ore) (max count) s) s)
            [| 0; 0; 0; time * time |]

    let rec simulateStep currentMax (timeLeft, resources, robots) =
        let ifWaited = Array.last robots * timeLeft + Array.last resources |> max currentMax

        //
        // recursively wait until we gather enough resources for creating each type of robot
        //

        // can we surpass current maximum?
        let maxEstimate =
            let r = Array.last robots
            let c = Array.last resources
            c + timeLeft * (r + timeLeft + r) / 2

        if maxEstimate < currentMax then
            currentMax
        else
            blueprint
            |> Seq.indexed
            |> Seq.rev // start with geoid-mining robot
            // check if we even mine the ingredients
            |> Seq.filter (snd >> List.forall (fun (_, ore) -> Array.item (mineralToIndex ore) robots > 0))
            // can we spend the additional mineral?
            |> Seq.filter (fun (i, _) ->
                let c = Array.item i maxCosts
                let r = Array.item i robots
                let res = Array.item i resources

                res + timeLeft * r < timeLeft * c)
            |> Seq.fold
                (fun currentMax (i, recipe) ->

                    // how long until we have the resources
                    let waitTime =
                        recipe
                        |> List.map (fun (count, ore) ->
                            let currentlyHave = Array.item (mineralToIndex ore) resources
                            let toMine = count - currentlyHave

                            if (toMine <= 0) then
                                0
                            else
                                let miners = Array.item (mineralToIndex ore) robots
                                toMine / miners + sign (toMine % miners))
                        |> List.max

                    if waitTime >= timeLeft - 1 then
                        currentMax // can't build the robot in time for it to matter
                    else
                        let afterWait = Array.map2 (fun c r -> c + r * (waitTime + 1)) resources robots

                        let afterBuild =
                            recipe
                            |> List.fold
                                (fun ress (count, ore) -> Array.mapAt (mineralToIndex ore) (flip (-) count) ress)
                                afterWait

                        simulateStep currentMax (timeLeft - waitTime - 1, afterBuild, Array.mapAt i ((+) 1) robots))
                ifWaited

    simulateStep 0 (time, [| 0; 0; 0; 0 |], [| 1; 0; 0; 0 |])

let solve1 input =
    List.sumBy (fun (i, blueprint) -> i * simulateBlueprint 24 (blueprint)) input

let solve2 input =
    input |> List.truncate 3 |> List.map (fun (i, blueprint) -> simulateBlueprint 32 (blueprint)) |> List.reduce (*)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "Blueprint 1:"
           "  Each ore robot costs 4 ore."
           "  Each clay robot costs 2 ore."
           "  Each obsidian robot costs 3 ore and 14 clay."
           "  Each geode robot costs 2 ore and 7 obsidian."
           ""
           "Blueprint 2:"
           "  Each ore robot costs 2 ore."
           "  Each clay robot costs 3 ore."
           "  Each obsidian robot costs 3 ore and 8 clay."
           "  Each geode robot costs 3 ore and 12 obsidian."
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 33

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal (62 * 56)
