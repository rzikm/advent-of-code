// Ranges are [start, stop) intervals
module Range

let inline contains value (start, stop) = start <= value && value < stop

let inline create start count = (start, start + count)

let inline length (start, stop) =
    max LanguagePrimitives.GenericZero (stop - start)

let tryIntersect (start1, stop1) (start2, stop2) =
    let start = max start1 start2
    let stop = min stop1 stop2
    if start < stop then Some(start, stop) else None

let tryUnionOverlapping (start1, stop1) (start2, stop2) =
    if stop1 < start2 || stop2 < start1 then
        None
    else
        Some(min start1 start2, max stop1 stop2)

let diff value sub =
    seq {
        let (x1, x2) = value
        let (y1, y2) = sub

        let e1 = min x2 y1
        if (x1 < e1) then yield (x1, e1)

        let s2 = max x1 y2
        if (s2 < x2) then yield (s2, x2)
    }
    |> List.ofSeq

let xor value sub = diff value sub @ diff sub value

let diffMany values subs =
    let folder values sub =
        values |> List.collect (fun v -> diff v sub)

    subs |> List.fold folder values

let unionMany list =
    let rec loop acc =
        function
        | [] -> acc
        | [ x ] -> x :: acc
        | f :: s :: rest ->
            match tryUnionOverlapping f s with
            | Some merged -> loop acc (merged :: rest)
            | None -> loop (f :: acc) (s :: rest)

    List.sortBy fst list |> loop [] |> List.rev

let intersectMany list =
    list |> List.map Some |> List.reduce (Option.bind2 tryIntersect)


module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``Contains`` () =
        contains 5 (3, 10) |> should be True
        contains 3 (3, 10) |> should be True
        contains 9 (3, 10) |> should be True
        contains 2 (3, 10) |> should be False
        contains 10 (3, 10) |> should be False

    [<Fact>]
    let ``Union touching`` () =
        unionMany [ (1, 3); (3, 4) ] |> should equal [ (1, 4) ]

    [<Fact>]
    let ``Union overlapping`` () =
        unionMany [ (1, 5); (3, 6) ] |> should equal [ (1, 6) ]

    [<Fact>]
    let ``Union subset`` () =
        unionMany [ (1, 10); (3, 4) ] |> should equal [ (1, 10) ]

    [<Fact>]
    let ``Union longer`` () =
        unionMany [ (1, 2); (1, 10) ] |> should equal [ (1, 10) ]

    [<Fact>]
    let ``Intersect many`` () =
        intersectMany [ (1, 2); (1, 10) ] |> should equal (Some(1, 2))

    [<Theory>]
    [<InlineData(1, 10, 5, 15, 1, 5)>]
    [<InlineData(5, 15, 1, 10, 10, 15)>]
    let ``Diff with partial overlap`` x1 x2 y1 y2 e1 e2 =
        diff (x1, x2) (y1, y2) |> should equal [ (e1, e2) ]

    [<Fact>]
    let ``Diff compelete overlap`` () = diff (1, 10) (0, 15) |> should be Empty

    [<Fact>]
    let ``Diff remove middle `` () =
        diff (1, 10) (5, 6) |> should equal [ (1, 5); (6, 10) ]
