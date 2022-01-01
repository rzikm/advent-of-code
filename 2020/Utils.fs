module Utils

let rec subsets size list = seq {
    if size = 0 then yield []
    else
        match list with
        | [] -> yield! Seq.empty
        | head::tail ->
            for t in subsets (size - 1) tail do
                yield head::t
            yield! subsets size tail
}
