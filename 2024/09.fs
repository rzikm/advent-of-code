module AoC202409

open AdventOfCode
open FSharpPlus
open FParsec

let parser = many1 (satisfy isDigit |>> (fun s -> int64 s - int64 '0'))

let parseDiskMap blocksMap =
    let blocks =
        List.mapFold (fun offset len -> (offset, len), offset + len) 0L blocksMap |> fst

    let (files, gaps, _, _) =
        blocks
        |> List.fold
            (fun (files, gaps, isFile, nextId) block ->
                match isFile with
                | true -> ((nextId, block) :: files, gaps, false, nextId + 1L)
                | false -> (files, (block) :: gaps, true, nextId))
            ([], [], true, 0L)

    List.rev files, (List.rev gaps |> List.filter (fun (_, len) -> len > 0L))

let solve1 input =
    let (files, gaps) = parseDiskMap input
    let filesRev = List.rev files

    let rec f acc files gaps =
        match files with
        | (id, (start, flen)) :: files ->
            match gaps with
            | (gstart, glen) :: gaps when gstart < start ->
                let len = min flen glen

                let gaps =
                    if glen = len then
                        gaps
                    else
                        (gstart + len, glen - len) :: gaps

                let files =
                    if flen = len then
                        files
                    else
                        (id, (start, flen - len)) :: files

                f ((id, (gstart, len)) :: acc) files gaps
            | _ -> f ((id, (start, flen)) :: acc) files gaps
        | [] -> acc

    f [] filesRev gaps |> List.sumBy (fun (id, (gstart, glen)) -> id * glen * (gstart + gstart + glen - 1L) / 2L)


let solve2 input =
    let (files, gaps) = parseDiskMap input
    let filesRev = List.rev files

    let rec moveSegmentLeft gaps (start, len) =
        match gaps with
        | (gstart, glen) :: gapsRest when gstart < start ->
            if len = glen then
                Some((gstart, glen), gapsRest)
            else if len < glen then
                Some((gstart, len), (gstart + len, glen - len) :: gapsRest)
            else
                moveSegmentLeft gapsRest (start, len) |> Option.map (fun (gap, gaps) -> (gap, (gstart, glen) :: gaps))
        | _ -> None

    let rec f acc files gaps =
        match files with
        | (id, pos) :: files ->
            match moveSegmentLeft gaps pos with
            | Some (gap, gaps) -> f ((id, gap) :: acc) files gaps
            | None -> f ((id, pos) :: acc) files gaps
        | [] -> acc

    f [] filesRev gaps |> List.sumBy (fun (id, (gstart, glen)) -> id * glen * (gstart + gstart + glen - 1L) / 2L)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "2333133121414131402" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 1928L

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 2858L
