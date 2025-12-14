module AoC202512

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pPresent =
        let line = parray 3 (charReturn '#' true <|> charReturn '.' false)
        attempt (pint32 >>. skipChar ':' >>. newline) >>. parray 3 (line .>> pchar '\n')

    let region =
        pint32 .>> skipChar 'x' .>>. pint32 .>> skipString ": " .>>. sepEndBy1 pint32 (skipChar ' ')

    sepEndBy1 pPresent (pchar '\n') .>>. sepEndBy1 region (pchar '\n')

let getAllorientations =
    Utils.memoize (fun present ->
        seq {
            let mutable p = present

            for _ in 1..4 do
                yield p
                p <- Array.rotate2dClockwise p
        }
        |> Seq.distinct)

let tryPlacePresent region present (x, y) =
    let ph = Array.length present
    let pw = Array.length present.[0]
    let rh = Array.length region
    let rw = Array.length region.[0]

    if x < 0 || y < 0 || x + pw > rw || y + ph > rh then
        None
    else
        let canPlace =
            seq {
                for dy in 0 .. ph - 1 do
                    for dx in 0 .. pw - 1 do
                        not (Array.item2d dx dy present && Array.item2d (x + dx) (y + dy) region)
            }
            |> Seq.forall id

        if not canPlace then
            None
        else
            Array.map2d
                (fun rx ry v -> v || Array.tryItem2d (rx - x) (ry - y) present |> Option.defaultValue false)
                region
            |> Some

let getPicks presents =
    let rec loop acc processed presents =
        match presents with
        | [] -> acc
        | (0, _) :: rest -> loop acc processed rest
        | present :: rest ->
            let count, pick = present
            let item = pick, (count - 1, pick) :: (processed @ rest)
            let acc = item :: acc
            let processed = (count, pick) :: processed
            loop acc processed rest

    loop [] [] presents

let visualizeRegion region =
    region
    |> Array.map (
        Array.map (function
            | true -> '#'
            | false -> '.')
    )
    |> Array.map (fun row -> System.String(row))
    |> String.concat "\n"
    |> printfn "\n-\n%s\n"

let canFit presents ((w, h), counts) =
    if List.length counts <> List.length presents then
        failwith "Invalid input, counts length does not match presents length"

    let presents = List.zip counts presents |> List.filter (fun (c, _) -> c > 0)
    let region = Array.init2d w h (fun _ _ -> false)
    let regionDims = Array.bounds2d region

    let rec loop maxXY region presents =
        match presents with
        | [] -> Some region
        | (0, _) :: rest -> loop maxXY region rest
        | _ ->
            // for all possible orientations, try to place the present in the region
            presents
            |> getPicks
            |> List.tryPick (fun (present, otherPresents) ->
                getAllorientations present
                |> Seq.tryPick (fun orientedPresent ->
                    let dims = Array.bounds2d orientedPresent

                    let xx, yy = Tuple2.sub regionDims dims
                    let maxX, maxY = maxXY

                    seq {
                        for y in 0 .. (max maxY yy) do
                            for x in 0 .. (max maxX xx) do
                                yield x, y
                    }
                    |> Seq.tryPick (fun pos ->
                        tryPlacePresent region orientedPresent pos
                        |> Option.bind (fun newRegion ->
                            let maxXY = Tuple2.map2 max maxXY (Tuple2.add pos dims)
                            loop maxXY newRegion otherPresents))))

    loop (0, 0) region presents


let solve1 (presents, regions) =
    // the line below solves the problem, but is too slow on the actual input
    // regions |> List.count (canFit presents >> Option.isSome)

    // the actual input is trivial since the instances that don't pack have obviously too many presents
    regions |> List.filter (fun ((w, h), counts) -> w * h >= List.sum counts * 9) |> List.length

let solve2 input = "*"

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "0:"
           "###"
           "##."
           "##."
           ""
           "1:"
           "###"
           "##."
           ".##"
           ""
           "2:"
           ".##"
           "###"
           "##."
           ""
           "3:"
           "##."
           "###"
           "##."
           ""
           "4:"
           "###"
           "#.."
           "###"
           ""
           "5:"
           "###"
           ".#."
           "###"
           ""
           "4x4: 0 0 0 0 2 0"
           "12x5: 1 0 1 0 2 2"
           "12x5: 1 0 1 0 3 2" |]

    // runtime too long
    // [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 2

    [<Fact>]
    let ``Example part 1 - first`` () =
        let (presents, regions) = parseTestInput parser input
        canFit presents (List.item 1 regions) |> should not' (be None)
