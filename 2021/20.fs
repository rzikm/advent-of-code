module AoC202120

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let pbit = anyOf [ '#'; '.' ] |>> (=) '#'
    let pcode = parray 512 pbit

    let pimage = sepEndBy1 (many1 pbit |>> Array.ofList) spaces |>> Array.ofList

    pcode .>> spaces .>>. pimage

let boolToInt =
    function
    | true -> 1
    | false -> 0

let enhanceAt algorithm (outside, scan) x y =
    let getItem scan x y =
        scan |> Array.tryItem y |> Option.bind (Array.tryItem x) |> Option.defaultValue outside

    let index =
        Seq.allPairs [ y - 1; y; y + 1 ] [
            x - 1
            x
            x + 1
        ]
        |> Seq.map (fun (y, x) -> getItem scan x y |> boolToInt)
        |> Seq.reduce (fun l r -> l * 2 + r)

    Array.item index algorithm

let enhance algorithm (outside, scan) =
    // all outside pixels follow the same rule:
    // - if dark, then all surroundings are dark and enhancement computes to index 0
    // - if light, then all surroundings are light and enhancement computes to index 511
    let enhanceOutside =
        function
        | false -> Array.item 0 algorithm
        | true -> Array.item 511 algorithm

    let newScan =
        Array.init (Array.length scan + 2) (fun y ->
            Array.init (Array.length (Array.item 0 scan) + 2) (fun x ->
                enhanceAt algorithm (outside, scan) (x - 1) (y - 1)))

    (enhanceOutside outside, newScan)

let enhanceN algorithm scan n =
    seq { 1..n } |> Seq.fold (fun s _ -> enhance algorithm s) scan

let run n (alg, scan) =
    enhanceN alg (false, scan) n |> snd |> Seq.sumBy (Seq.sumBy boolToInt)

let solution = makeSolution () parser (run 2) (run 50)

module Tests =
    open Xunit
    open FsUnit.Xunit

    let boolToLight =
        function
        | true -> '#'
        | false -> '.'

    let private parse input =
        match FParsec.CharParsers.run parser (String.concat "\n" input) with
        | Success (res, _, _) -> res
        | Failure (err, _, _) -> failwith err

    let input =
        [| "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
           ""
           "#..#."
           "#...."
           "##..#"
           "..#.."
           "..###" |]

    [<Fact>]
    let ``Example enhance pixel`` () =
        let alg, scan = parse input
        enhanceAt alg (false, scan) 2 2 |> should equal true

    [<Fact>]
    let ``Simple enhance`` () =
        let alg, scan = parse input

        let expected =
            [| [| '.'; '#'; '#'; '.'; '#'; '#'; '.' |]
               [| '#'; '.'; '.'; '#'; '.'; '#'; '.' |]
               [| '#'; '#'; '.'; '#'; '.'; '.'; '#' |]
               [| '#'; '#'; '#'; '#'; '.'; '.'; '#' |]
               [| '.'; '#'; '.'; '.'; '#'; '#'; '.' |]
               [| '.'; '.'; '#'; '#'; '.'; '.'; '#' |]
               [| '.'; '.'; '.'; '#'; '.'; '#'; '.' |] |]

        enhanceN alg (false, scan) 1 |> snd |> Array.map (Array.map boolToLight) |> should equal expected

    [<Fact>]
    let ``Simple double enhance`` () =
        let alg, scan = parse input

        let expected =
            [| [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.' |]
               [| '.'; '#'; '.'; '.'; '#'; '.'; '#'; '.'; '.' |]
               [| '#'; '.'; '#'; '.'; '.'; '.'; '#'; '#'; '#' |]
               [| '#'; '.'; '.'; '.'; '#'; '#'; '.'; '#'; '.' |]
               [| '#'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '#' |]
               [| '.'; '#'; '.'; '#'; '#'; '#'; '#'; '#'; '.' |]
               [| '.'; '.'; '#'; '.'; '#'; '#'; '#'; '#'; '#' |]
               [| '.'; '.'; '.'; '#'; '#'; '.'; '#'; '#'; '.' |]
               [| '.'; '.'; '.'; '.'; '#'; '#'; '#'; '.'; '.' |] |]

        enhanceN alg (false, scan) 2 |> snd |> Array.map (Array.map boolToLight) |> should equal expected

    [<Fact>]
    let ``Example Part 1`` () =
        testPart1 solution input |> should equal 35

    [<Fact>]
    let ``Example Part 2`` () =
        testPart2 solution input |> should equal 3351
