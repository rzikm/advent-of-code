module String

let tryFindLastSliceIndex (slice: string) (str: string) =
    match str.LastIndexOf(slice) with
    | -1 -> None
    | i -> Some(i)

let findLastSliceIndex (slice: string) (str: string) = str.LastIndexOf(slice)
