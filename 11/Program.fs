open System.IO;

let input =
    File.ReadAllLines("input.txt")
    |> Array.map (Seq.map (fun x -> int x - int '0') >> Array.ofSeq)

let cloneMap arr =
    Array.map (Array.map id) arr

let mapAt (grid : int[][]) f (x, y) =
    grid.[y].[x] <- f grid.[y].[x]

let increment (grid : int[][]) =
    for x in 0..(grid.[0].Length - 1) do
        for y in 0..(grid.Length - 1) do
            mapAt grid ((+) 1) (x, y)

let findFlashes grid =
    grid |> Seq.indexed |> Seq.collect (fun (y, row) ->
        row |> Seq.indexed |> Seq.choose (fun (x, i) -> if i > 9 then Some (x,y) else None))
    |> List.ofSeq

let getNeighbors (x, y) =
    let xs = [x-1; x; x+1] |> List.filter (fun x -> x >= 0 && x < input.[0].Length)
    let ys = [y-1; y; y+1] |> List.filter (fun y -> y >= 0 && y < input.Length)
    List.allPairs xs ys |> List.filter ((<>) (x,y))

let processFlashAt grid coord =
    let flashInc = function
        | 0 -> 0
        | x -> x + 1

    mapAt grid (fun _ -> 0) coord
    getNeighbors coord |> Seq.iter (mapAt grid flashInc)

let doStep grid =
    let mutable flashCount = 0
    increment grid
    let mutable flashes = findFlashes grid
    while not <| List.isEmpty flashes do
        flashCount <- flashCount + List.length flashes
        flashes |> List.iter (processFlashAt grid)
        flashes <- findFlashes grid
    flashCount

let part1 =
    seq{1..100} |> Seq.fold (fun (g, c) _ -> (g, c + doStep g)) (cloneMap input, 0) |> snd

printfn "%A" part1

let part2 =
    let rec doStep' grid i =
        doStep grid |> ignore
        if grid |> Array.forall (Array.forall ((=) 0)) then i else doStep' grid (i + 1)

    doStep' (cloneMap input) 1

printfn "%A" part2
