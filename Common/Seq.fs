module Seq

let loop s =
    seq {
        while true do
            yield! s
    }

let count prediacate s =
    s |> Seq.fold (fun acc s -> if prediacate s then acc + 1 else acc) 0
