module Program

open System.IO;

let boolToInt = function
    | true -> 1
    | false -> 0

let enhanceAt algorithm (outside, scan) x y =
    let getItem scan x y =
        scan |> Array.tryItem y |> Option.bind (Array.tryItem x)
        |> Option.defaultValue outside

    let index =
        Seq.allPairs [ y - 1; y; y + 1 ] [ x - 1; x; x + 1 ]
        |> Seq.map (fun (y, x) -> getItem scan x y |> boolToInt)
        |> Seq.reduce (fun l r -> l * 2 + r)
    Array.item index algorithm

let enhance algorithm (outside, scan) =
    // all outside pixels follow the same rule:
    // - if dark, then all surroundings are dark and enhancement computes to index 0
    // - if light, then all surroundings are light and enhancement computes to index 511
    let enhanceOutside = function
        | false -> Array.item 0 algorithm
        | true -> Array.item 511 algorithm

    let newScan = Array.init (Array.length scan + 2) (fun y ->
        Array.init (Array.length (Array.item 0 scan) + 2) (fun x ->
            enhanceAt algorithm (outside, scan) (x - 1) (y - 1)))

    (enhanceOutside outside, newScan)

let parse input =
    let isLight = (=) '#'
    let alg = Array.head input |> Seq.map isLight |> Array.ofSeq
    let scan = Seq.skip 2 input |> Seq.map (Seq.map isLight >> Array.ofSeq) |> Array.ofSeq
    alg, scan

let enhanceN algorithm scan n =
    seq { 1..n } |> Seq.fold (fun s _ -> enhance algorithm s) scan

let run (alg, scan) n = enhanceN alg (false, scan) n |> snd |> Seq.sumBy (Seq.sumBy boolToInt)
let part1 input = run input 2
let part2 input = run input 50

module Tests =
    open Xunit
    open FsUnit.Xunit

    let boolToLight = function
        | true -> '#'
        | false -> '.'

    let exampleInput () =
        let rawInput = [|
            "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
            ""
            "#..#."
            "#...."
            "##..#"
            "..#.."
            "..###"
        |]
        parse rawInput

    [<Fact>]
    let ``Example enhance pixel`` () =
        let alg, scan = exampleInput()
        enhanceAt alg (false, scan) 2 2 |> should equal true

    [<Fact>]
    let ``Simple enhance`` () =
        let alg, scan = exampleInput()
        let expected = [|
            [| '.'; '#'; '#'; '.'; '#'; '#'; '.'; |]
            [| '#'; '.'; '.'; '#'; '.'; '#'; '.'; |]
            [| '#'; '#'; '.'; '#'; '.'; '.'; '#'; |]
            [| '#'; '#'; '#'; '#'; '.'; '.'; '#'; |]
            [| '.'; '#'; '.'; '.'; '#'; '#'; '.'; |]
            [| '.'; '.'; '#'; '#'; '.'; '.'; '#'; |]
            [| '.'; '.'; '.'; '#'; '.'; '#'; '.'; |]
        |]
        enhanceN alg (false, scan) 1 |> snd |> Array.map (Array.map boolToLight)
        |> should equal expected

    [<Fact>]
    let ``Simple double enhance`` () =
        let alg, scan = exampleInput()
        let expected = [|
            [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.' |]
            [| '.'; '#'; '.'; '.'; '#'; '.'; '#'; '.'; '.' |]
            [| '#'; '.'; '#'; '.'; '.'; '.'; '#'; '#'; '#' |]
            [| '#'; '.'; '.'; '.'; '#'; '#'; '.'; '#'; '.' |]
            [| '#'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '#' |]
            [| '.'; '#'; '.'; '#'; '#'; '#'; '#'; '#'; '.' |]
            [| '.'; '.'; '#'; '.'; '#'; '#'; '#'; '#'; '#' |]
            [| '.'; '.'; '.'; '#'; '#'; '.'; '#'; '#'; '.' |]
            [| '.'; '.'; '.'; '.'; '#'; '#'; '#'; '.'; '.' |]
        |]
        enhanceN alg (false, scan) 2 |> snd |> Array.map (Array.map boolToLight)
        |> should equal expected

    [<Fact>]
    let ``Example Part 1`` () =
        exampleInput () |> part1 |> should equal 35

    [<Fact>]
    let ``Example Part 2`` () =
        exampleInput () |> part2 |> should equal 3351

[<EntryPoint>]
let main _ =
    let input = parse (File.ReadAllLines("input.txt"))
    printfn "%A" (part1 input)
    printfn "%A" (part2 input)
    0
