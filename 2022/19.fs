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
            [| 0; 0; 0 |]

    let rec simulateStep (timeLeft, resources, robots) =
        let ifWaited = Array.last robots * timeLeft + Array.last resources

        //
        // recursively wait until we gather enough resources for creating each type of robot
        //

        blueprint
        |> Seq.indexed
        |> Seq.choose (fun (i, recipe) ->
            // check if we even mine the ingredients
            if recipe |> List.forall (fun (_, ore) -> Array.item (mineralToIndex ore) robots > 0) then

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

                // can we spend the additional mineral?
                let canSpendIt =
                    Array.tryItem i maxCosts
                    |> Option.map (fun c ->
                        let r = Array.item i robots
                        let res = Array.item i resources

                        res + timeLeft * r < timeLeft * c)
                    |> Option.defaultValue true

                if waitTime >= timeLeft - 1 || not <| canSpendIt then
                    None
                else
                    let afterWait = Array.map2 (fun c r -> c + r * (waitTime + 1)) resources robots

                    let afterBuild =
                        recipe
                        |> List.fold
                            (fun ress (count, ore) -> Array.mapAt (mineralToIndex ore) (flip (-) count) ress)
                            afterWait

                    (timeLeft - waitTime - 1, afterBuild, Array.mapAt i ((+) 1) robots) |> simulateStep |> Some
            else
                None)
        |> flip Seq.append [ ifWaited ]
        |> Seq.max

    simulateStep (time, [| 0; 0; 0; 0 |], [| 1; 0; 0; 0 |])

let solve1 input =
    List.sumBy (fun (i, blueprint) -> i * simulateBlueprint 24 (blueprint)) input

let solve2 input =
    input |> List.truncate 3 |> List.map (fun (i, blueprint) -> simulateBlueprint 32 (blueprint)) |> List.reduce (*)

let solution = makeSolution parser solve1 solve2

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
