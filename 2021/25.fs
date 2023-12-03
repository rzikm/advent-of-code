module AoC202125

open AdventOfCode
open FParsec
open Utils

type Cucumber =
    | Vertical
    | Horizontal

let parser =
    let pnone = pchar '.' >>% None
    let pvertical = pchar 'v' >>% Some Vertical
    let phorizontal = pchar '>' >>% Some Horizontal

    let pcell = choice [ pvertical; phorizontal; pnone ]

    sepEndBy (many1 pcell |>> Array.ofList) spaces |>> Array.ofList

let gridToString grid =
    let cellToString =
        function
        | None -> "."
        | Some Vertical -> "v"
        | Some Horizontal -> ">"

    grid |> Array.map (fun row -> row |> Seq.map cellToString |> String.concat "") |> String.concat "\n"

let advanceAll (dx, dy) cucumber grid =
    let item x y = grid |> Array.item y |> Array.item x

    let lenY = Array.length grid
    let lenX = Array.item 0 grid |> Array.length

    Array.init lenY (fun y ->
        Array.init lenX (fun x ->
            match item x y with
            | Some c as cc when c <> cucumber -> cc
            | Some _ as cc ->
                // this cucumber can move elsewhere
                let (succX, succY) = ((x + dx + lenX) % lenX, (y + dy + lenY) % lenY)

                match item succX succY with
                | None -> None
                | c -> cc
            | None ->
                // some cucumber may move here
                let (predX, predY) = ((x - dx + lenX) % lenX, (y - dy + lenY) % lenY)

                match item predX predY with
                | Some c when c <> cucumber -> None
                | c -> c))

let simulateStep = advanceAll (1, 0) Horizontal >> advanceAll (0, 1) Vertical

let solve input =
    let rec run grid c =
        let next = simulateStep grid
        if grid = next then c else run next (c + 1)

    run input 1

let solution = makeSolution () parser solve (fun _ -> "*")

module Tests =
    open Xunit
    open FsUnit

    let private parse input =
        match FParsec.CharParsers.run parser (String.concat "\n" input) with
        | Success (res, _, _) -> res
        | Failure (err, _, _) -> failwith err

    let input =
        [| "v...>>.vv>"
           ".vv>>.vv.."
           ">>.>v>...v"
           ">>v>>.>.v."
           "v>v.vv.v.."
           ">.>>..v..."
           ".vv..>.>v."
           "v.v..>>v.v"
           "....v..v.>" |]

    [<Fact>]
    let ``One step`` () =
        input |> parse |> simulateStep |> gridToString |> should equal
        <| String.concat
            "\n"
            [| "....>.>v.>"
               "v.v>.>v.v."
               ">v>>..>v.."
               ">>v>v>.>.v"
               ".>v.v...v."
               "v>>.>vvv.."
               "..v...>>.."
               "vv...>>vv."
               ">.v.v..v.v" |]

    [<Fact>]
    let ``Example`` () =
        testPart1 solution input |> should equal 58
