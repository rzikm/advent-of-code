open System.IO;

type Vertex =
    | Start
    | End
    | Small of string
    | Large of string

type Edge = Vertex * Vertex

type Graph = {
    vertices : Set<Vertex>
    neighbors : Map<Vertex, Set<Vertex>>
}

let vertices (g : Graph) = g.vertices
let neighbors (g : Graph) (v : Vertex) = Map.find v g.neighbors

let graph =
    let parseVertex = function
        | "start" -> Start | "end" -> End
        | s when s = s.ToLower() -> Small s | s -> Large s

    let parseEdge (line : string) =
        let [| v1; v2 |] = line.Split('-') |> Array.map parseVertex
        Set.ofList [v1; v2]

    let edges =
        File.ReadLines("input.txt")
        |> Seq.map parseEdge |> List.ofSeq

    let vertices = edges |> Set.unionMany

    let neighbors =
        vertices
        |> Seq.map (fun x ->
                     (x, (edges |> List.filter (Set.contains x) |> Set.unionMany |> (fun s -> Set.difference s (Set.singleton x)))))
        |> Map.ofSeq

    { vertices = vertices; neighbors = neighbors }

let part1 =
    let allPaths =
        let rec allPaths' (v::past) visited = seq {
            match v with
            | End -> yield v::past
            | _ ->
                for nextV in Set.difference (neighbors graph v) visited do
                    match nextV with
                    | Large _ -> yield! allPaths' (nextV::v::past) visited
                    | _ -> yield! allPaths' (nextV::v::past) (Set.add nextV visited)
            }

        allPaths' [Start] (Set.singleton Start) |> Seq.map List.rev

    allPaths |> Seq.length

printfn "%A" part1

let part2 =
    let allPaths =
        let rec allPaths' (v::past) visited singleDouble = seq {
            match v with
            | End -> yield v::past
            | _ ->
                for nextV in Set.difference (neighbors graph v) visited do
                    match nextV, singleDouble with
                    | Large _, _ -> yield! allPaths' (nextV::v::past) visited singleDouble
                    | Small _, None ->
                        yield! allPaths' (nextV::v::past) visited (Some nextV)
                        yield! allPaths' (nextV::v::past) (Set.add nextV visited) singleDouble
                    | _ -> yield! allPaths' (nextV::v::past) (Set.add nextV visited) singleDouble
        }
        allPaths' [Start] (Set.singleton Start) None |> Seq.distinct |> Seq.map List.rev

    allPaths |> Seq.length

printfn "%A" part2
