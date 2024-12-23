module Seq

let loop s =
    seq {
        while true do
            yield! s
    }

let count prediacate s =
    s |> Seq.fold (fun acc s -> if prediacate s then acc + 1 else acc) 0

let toLookupMap fkey seq =
    seq |> Seq.map (fun x -> fkey x, x) |> Map.ofSeq

let toMap fKey fValue seq =
    seq |> Seq.map (fun x -> fKey x, fValue x) |> Map.ofSeq
