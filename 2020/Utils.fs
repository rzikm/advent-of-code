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

module Array =
    let item2d x y array =
        array |> Array.item y |> Array.item x

let parseInt (baze : int) (str : string) =
    System.Convert.ToInt32(str, baze)

let memoizerec f =
    let cache = new System.Collections.Generic.Dictionary<_,_>()
    let rec newf value =
        match cache.TryGetValue value with
        | true, res -> res
        | false, _ ->
            let res = f newf value
            cache.Add(value, res)
            res
    newf
