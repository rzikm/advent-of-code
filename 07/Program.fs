open System.IO;

let input =
    (File.ReadAllText("input.txt").Split(',')) |> Array.map int

let run cost =
    let min = Array.min input
    let max = Array.max input

    seq {min..max} |> Seq.map (fun x -> Array.sumBy ((-) x >> abs >> cost) input) |> Seq.min

let part1 = run id
printfn "%A" part1

let part2 = run (fun x -> x * (x + 1) / 2)
printfn "%A" part2