module AoC202207

open AdventOfCode
open FSharpPlus
open FSharpPlus.Lens
open FParsec

type Entry =
    | Directory of Entry list * string
    | File of int * string

type Command =
    | Cd of string
    | Ls of Entry list

let parser =
    let pcd = skipString "$ cd " >>. restOfLine true |>> Cd

    let pdirectory =
        skipString "dir " >>. restOfLine true |>> (fun n -> Directory([], n))

    let pfile = pint32 .>> skipChar ' ' .>>. restOfLine true |>> File

    let pls =
        skipString "$ ls" >>. skipRestOfLine true >>. many1 (pdirectory <|> pfile) |>> Ls

    many1 (pcd <|> pls)

let entryName =
    function
    | Directory (_, name) -> name
    | File (_, name) -> name

let inline _entries f =
    function
    | Directory (es, name) -> f es <&> fun x -> Directory(x, name)
    | _ -> failwith "Expected Directory"

let constructTree commands =
    let evalCommand (cd, rootEntry) command =
        let rec updateEntries path entries entry =
            match path with
            | [] -> setl _entries entries entry
            | dir :: rest ->
                let index = entry ^. _entries |> List.findIndex (entryName >> (=) dir)
                entry |> over (_entries << List._item index << _Some) (updateEntries rest entries)

        match command with
        | Cd "/" -> ([], rootEntry)
        | Cd ".." -> (List.tail cd, rootEntry)
        | Cd name -> (name :: cd, rootEntry)
        | Ls entries -> (cd, updateEntries (List.rev cd) entries rootEntry)

    commands |> List.fold evalCommand ([], Directory([], "/")) |> snd

let rec collectTotalSizes entry =
    match entry with
    | File (size, _) -> [], size
    | Directory (entries, name) ->
        let subs = entries |> List.map collectTotalSizes
        let subdirs = subs |> List.collect fst
        let total = subs |> List.sumBy snd
        (name, total) :: subdirs, total

let solve1 input =
    let dirSizes = collectTotalSizes (constructTree input) |> fst
    dirSizes |> List.filter (fun (n, size) -> size <= 100000) |> List.sumBy snd

let solve2 input =
    let dirSizes, total = collectTotalSizes (constructTree input)
    let free = 70000000 - total
    let needed = 30000000 - free
    dirSizes |> List.sortBy snd |> List.find (fun (_, size) -> size >= needed) |> snd

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "$ cd /"
           "$ ls"
           "dir a"
           "14848514 b.txt"
           "8504156 c.dat"
           "dir d"
           "$ cd a"
           "$ ls"
           "dir e"
           "29116 f"
           "2557 g"
           "62596 h.lst"
           "$ cd e"
           "$ ls"
           "584 i"
           "$ cd .."
           "$ cd .."
           "$ cd d"
           "$ ls"
           "4060174 j"
           "8033020 d.log"
           "5626152 d.ext"
           "7214296 k"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 95437

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 24933642
