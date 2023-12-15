module AoC202315

open AdventOfCode
open FSharpPlus
open FSharpPlus.Lens
open FSharpPlus.Data
open FParsec

let parser =
    many1 (many1CharsTill ParseUtils.notWhitespace (skipChar ',' <|> skipNewline <|> eof))

let hash input =
    Seq.fold (fun acc x -> ((acc + int32 x) * 17) % 256) 0 input

type Op =
    | Remove
    | Add of int

let getOperation label =
    let remove = charReturn '-' Remove
    let Add = skipChar '=' >>. pint32 |>> Add
    let parser = many1CharsTillApply (anyChar) (remove <|> Add) Tuple2.create
    Utils.parseInput parser label

let solve1 input = List.sumBy hash input

let solve2 input =
    let folder state op =
        let l, op = getOperation op

        let updater list =
            match op with
            | Remove -> list |> List.filter (fst >> (<>) l)
            | Add v ->
                let inline lens a b =
                    (List.traverse << filtered (fst >> (=) l) << _2) a b

                match preview (lens) list with
                | Some _ -> setl lens v list
                | None -> List.append list [ (l, v) ]

        over (Map._item <| hash l) (Option.defaultValue [] >> updater >> Option.returnIf (not << List.isEmpty)) state

    let lenses = input |> List.fold folder Map.empty

    let folder acc i x =
        let focusPower = Seq.indexed x |> Seq.sumBy (fun (ii, (_, l)) -> (ii + 1) * l)
        acc + focusPower * (i + 1)

    Map.fold folder 0 lenses

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" |]

    [<Theory>]
    [<InlineData("rn=1", 30)>]
    [<InlineData("ot=7", 231)>]
    let ``Hash works`` input expected = hash input |> should equal expected

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 1320

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 145
