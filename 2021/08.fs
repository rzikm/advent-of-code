module AoC202108

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pSet =
        many1Chars (choice [ pchar 'a'; pchar 'b'; pchar 'c'; pchar 'd'; pchar 'e'; pchar 'f'; pchar 'g' ])
        |>> (String.toSeq >> Set.ofSeq)

    let pLine = sepEndBy pSet (pchar ' ') .>> pstring "| " .>>. sepBy pSet (pchar ' ')

    sepEndBy pLine (pchar '\n')

let solve1 input =
    let segmentCounts = [ 2; 4; 3; 7 ]

    input
    |> List.sumBy (fun (_, digits) ->
        digits |> List.filter (fun x -> List.contains (Set.count x) segmentCounts) |> List.length)

let solve2 input =
    let getMapping (digits: Set<char> list) =
        let pick f =
            List.pick (fun x -> if f x then Some(x) else None)

        let (<&>) l r x = l x && r x

        let d1 = digits |> pick (Set.count >> (=) 2)
        let d7 = digits |> pick (Set.count >> (=) 3)
        let d8 = digits |> pick (Set.count >> (=) 7)
        let d4 = digits |> pick (Set.count >> (=) 4)
        let d3 = digits |> pick (Set.count >> (=) 5 <&> Set.isSubset d7)
        let d9 = digits |> pick (Set.count >> (=) 6 <&> Set.isSubset d4)
        let d5 = digits |> pick (Set.count >> (=) 5 <&> Set.isSubset (Set.difference d9 d1))
        let d2 = digits |> pick (Set.count >> (=) 5 <&> (<>) d5 <&> (<>) d3)
        let d0 = digits |> pick (Set.count >> (=) 6 <&> (<>) d9 <&> Set.isSubset d1)
        let d6 = digits |> pick (Set.count >> (=) 6 <&> (<>) d9 <&> (<>) d0)

        Map.ofList
            [ (d0, 0)
              (d1, 1)
              (d2, 2)
              (d3, 3)
              (d4, 4)
              (d5, 5)
              (d6, 6)
              (d7, 7)
              (d8, 8)
              (d9, 9) ]

    let getValue ((digits: Set<char> list), (valueDigits: Set<char> list)) =
        let mapping = getMapping digits

        List.foldBack (fun digit (total, exp) -> (total + exp * (Map.find digit mapping), exp * 10)) valueDigits (0, 1)
        |> fst

    List.sumBy getValue input

let solution = makeSolution parser solve1 solve2
