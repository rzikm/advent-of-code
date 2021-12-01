// For more information see https://aka.ms/fsharp-console-apps
//
open System.IO

let Part1 () =
    let result =
        File.ReadLines("input.txt")
        |> Seq.map int
        |> Seq.pairwise
        |> Seq.filter (fun (x,y) -> x < y)
        |> Seq.length

    printf "%d" result

let Part2 () =
    let result =
        File.ReadLines("input.txt")
        |> Seq.map int
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> Seq.pairwise
        |> Seq.filter (fun (x,y) -> x < y)
        |> Seq.length

    printf "%d" result

Part2 ()
