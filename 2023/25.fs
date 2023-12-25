module AoC202325

open AdventOfCode
open FSharpPlus
open FSharpPlus.Lens
open FParsec

let parser =
    let node = many1Chars letter

    let folder acc (node, neighbors) =
        let acc =
            over (Map._item node) (Option.defaultValue [] >> List.append neighbors >> Some) acc

        neighbors
        |> List.fold
            (fun acc neighbor -> over (Map._item neighbor) (Option.defaultValue [] >> List.cons node >> Some) acc)
            acc

    ParseUtils.lines (node .>> skipString ": " .>>. sepBy1 node (skipChar ' ')) |>> (List.fold folder Map.empty)

let solve1 input =
    let u = Map.keys input |> Seq.head

    let fNeighbors a =
        Map.find a input |> Seq.map (fun n -> (n, 1))

    let (_, edges) =
        Map.keys input |> Seq.skip 1 |> Seq.map (fun v -> Graph.findMaxFlow fNeighbors u v) |> Seq.find (fst >> (=) 3)

    let edges = edges |> Seq.map (fun (a, b, _) -> a, b) |> Set.ofSeq

    let fNeighbors2 a =
        input |> Map.find a |> Seq.filter (fun n -> not (edges.Contains(a, n)))

    let cc = Graph.connectedComponent fNeighbors2 u |> List.length
    cc * (input.Count - cc)

let solve2 input = "*"

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "jqt: rhn xhk nvd"
           "rsh: frs pzl lsr"
           "xhk: hfx"
           "cmg: qnr nvd lhk bvb"
           "rhn: xhk bvb hfx"
           "bvb: xhk hfx"
           "pzl: lsr hfx nvd"
           "qnr: nvd"
           "ntq: jqt hfx bvb xhk"
           "nvd: lhk"
           "lsr: lhk"
           "rzs: qnr cmg lsr rsh"
           "frs: qnr lhk lsr" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 54
