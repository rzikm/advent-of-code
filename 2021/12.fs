module AoC202112

open AdventOfCode
open FSharpPlus
open FParsec

type Vertex =
    | Start
    | End
    | Small of string
    | Large of string

type Edge = Vertex * Vertex

type Graph = { vertices: Set<Vertex>; neighbors: Map<Vertex, Set<Vertex>> }

let vertices (g: Graph) = g.vertices
let neighbors (g: Graph) (v: Vertex) = Map.find v g.neighbors

let parse =
    let pstart = pstring "start" >>% Start
    let pend = pstring "end" >>% End
    let psmall = many1Chars (satisfy isLower) |>> Small
    let plarge = many1Chars (satisfy isUpper) |>> Large
    let pvertex = choice [ pstart; pend; psmall; plarge ]

    let pedge =
        pvertex .>> pchar '-' .>>. pvertex |>> (fun (l, r) -> Set.ofList [ l; r ])

    sepEndBy pedge spaces
    |>> (fun edges ->
        let vertices = edges |> Set.unionMany

        let neighbors =
            vertices
            |> Seq.map (fun x ->
                (x,
                 (edges
                  |> List.filter (Set.contains x)
                  |> Set.unionMany
                  |> (fun s -> Set.difference s (Set.singleton x)))))
            |> Map.ofSeq

        { vertices = vertices; neighbors = neighbors })

let solve1 graph =
    let allPaths =
        let rec allPaths' (v :: past) visited =
            seq {
                match v with
                | End -> yield v :: past
                | _ ->
                    for nextV in Set.difference (neighbors graph v) visited do
                        match nextV with
                        | Large _ -> yield! allPaths' (nextV :: v :: past) visited
                        | _ -> yield! allPaths' (nextV :: v :: past) (Set.add nextV visited)
            }

        allPaths' [ Start ] (Set.singleton Start) |> Seq.map List.rev

    allPaths |> Seq.length

let solve2 graph =
    let allPaths =
        let rec allPaths' (v :: past) visited singleDouble =
            seq {
                match v with
                | End -> yield v :: past
                | _ ->
                    for nextV in Set.difference (neighbors graph v) visited do
                        match nextV, singleDouble with
                        | Large _, _ -> yield! allPaths' (nextV :: v :: past) visited singleDouble
                        | Small _, None ->
                            yield! allPaths' (nextV :: v :: past) visited (Some nextV)
                            yield! allPaths' (nextV :: v :: past) (Set.add nextV visited) singleDouble
                        | _ -> yield! allPaths' (nextV :: v :: past) (Set.add nextV visited) singleDouble
            }

        allPaths' [ Start ] (Set.singleton Start) None |> Seq.distinct |> Seq.map List.rev

    allPaths |> Seq.length

let solution = makeSolution parse solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input = [| "start-A"; "start-b"; "A-c"; "A-b"; "b-d"; "A-end"; "b-end" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal 10

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 103
