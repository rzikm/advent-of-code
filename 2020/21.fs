module AoC202021

open Utils
open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pingr = many1Chars (satisfy isLetter)
    let allerg = sepBy1 (pingr) (skipString ", ")

    let line =
        sepEndBy1 pingr (skipChar ' ') .>> skipString "(contains " .>>. allerg .>> skipChar ')'

    sepEndBy1 line (skipChar '\n')

let foodsContainingIngredient foods ingredient =
    foods |> List.filter (fun f -> fst f |> List.contains ingredient)

let foodsContainingAllergen foods allergen =
    foods |> List.filter (fun f -> snd f |> List.contains allergen)

let isAllergenFree foods ing =
    let suspectAlergens =
        foodsContainingIngredient foods ing |> List.collect snd |> List.distinct

    // for each allergen check if there exists a food which contains
    // the same allergen but does not contain the ingredient
    suspectAlergens
    |> List.forall (fun al -> foodsContainingAllergen foods al |> List.exists (fst >> List.contains ing >> not))

let solve1 foods =
    foods |> List.collect fst |> filter (isAllergenFree foods) |> length

let solve2 foods =
    let foods =
        foods |> List.map (fun (ings, al) -> (ings |> List.filter (not << isAllergenFree foods), al))

    // compute possible pairings and use brute-force bipartite matching algorithm
    let possibilities =
        foods
        |> List.collect snd
        |> List.distinct
        |> List.map (fun al ->
            (al, foodsContainingAllergen foods al |> List.map (fst >> Set.ofList) |> Set.intersectMany |> List.ofSeq))
        |> Map.ofList

    findMatching possibilities |> Option.get |> List.sortBy fst |> List.map snd |> String.concat ","

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
           "trh fvjkl sbzzf mxmxvkd (contains dairy)"
           "sqjhc fvjkl (contains soy)"
           "sqjhc mxmxvkd sbzzf (contains fish)"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 5

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal "mxmxvkd,sqjhc,fvjkl"
