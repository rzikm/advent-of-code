module Option

let apply fOpt xOpt =
    match fOpt, xOpt with
    | Some f, Some x -> Some(f x)
    | _ -> None

let bind2 f a b =
    match a, b with
    | Some a, Some b -> f a b
    | _ -> None

let returnIf pred x = if pred x then Some x else None
