module Program

open System.IO;

type Cucumber = | Vertical | Horizontal

let parse input =
    let parseCell = function
        | '.' -> None
        | 'v' -> Some Vertical
        | '>' -> Some Horizontal
        | _ -> failwith "Invalid input"

    input |> Array.map (Seq.map parseCell >> Array.ofSeq)

let gridToString grid =
    let cellToString = function
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
                | c -> c
        )
    )

let simulateStep =
    advanceAll (1, 0) Horizontal >> advanceAll (0, 1) Vertical

let part1 input = 
    let rec run grid c =
        let next = simulateStep grid
        if grid = next then c
        else run next (c + 1)

    run input 1

module Tests =
    open Xunit
    open FsUnit

    let exampleInput () =
        [| 
            "v...>>.vv>";
            ".vv>>.vv..";
            ">>.>v>...v";
            ">>v>>.>.v.";
            "v>v.vv.v..";
            ">.>>..v...";
            ".vv..>.>v.";
            "v.v..>>v.v";
            "....v..v.>"
        |] |> parse;

    [<Fact>]
    let ``One step`` () =
        exampleInput ()
        |> simulateStep
        |> gridToString
        |> should equal <| String.concat "\n" [|
            "....>.>v.>";
            "v.v>.>v.v.";
            ">v>>..>v..";
            ">>v>v>.>.v";
            ".>v.v...v.";
            "v>>.>vvv..";
            "..v...>>..";
            "vv...>>vv.";
            ">.v.v..v.v" |] 

    [<Fact>]
    let ``Example Part 1`` () =
        exampleInput () |> part1 |> should equal 58

[<EntryPoint>]
let main _ =
    let input = parse (File.ReadAllLines "input.txt")

    printfn "%A" (part1 input)
    0
