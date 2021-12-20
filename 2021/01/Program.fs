open System.IO

let uncurry f (a, b) = f a b

let Part1 () =
    let result =
        File.ReadLines("input.txt")
        |> Seq.map int
        |> Seq.pairwise
        |> Seq.filter (uncurry (<))
        |> Seq.length

    printf "%d" result

let Part2 () =
    let result =
        File.ReadLines("input.txt")
        |> Seq.map int
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> Seq.pairwise
        |> Seq.filter (uncurry (<))
        |> Seq.length

    printf "%d" result

Part2 ()
