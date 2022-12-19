module Tuple2

open FSharpPlus

let map f (i1, i2) = (f i1, f i2)

let inline (<!>) f t = map f t

let apply (f1, f2) (i1, i2) = (f1 i1, f2 i2)

let inline (<*>) f t = apply f t

let inline add l r = (+) <!> l <*> r
let inline sub l r = (-) <!> l <*> r
let inline mul l r = (*) <!> l <*> r

let inline lt t1 t2 = (<) <!> t1 <*> t2 |> uncurry (&&)
let inline le t1 t2 = (<=) <!> t1 <*> t2 |> uncurry (&&)
let inline gt t1 t2 = (>) <!> t1 <*> t2 |> uncurry (&&)
let inline ge t1 t2 = (>=) <!> t1 <*> t2 |> uncurry (&&)

let inline manhattanDist t1 t2 = sub t1 t2 |> map abs |> uncurry (+)

let swap (a, b) = (b, a)

let neighbors4 (x, y) =
    seq {
        (x - 1, y)
        (x + 1, y)
        (x, y - 1)
        (x, y + 1)
    }

let neighbors8 (x, y) =
    seq {
        for xx in x - 1 .. x + 1 do
            for yy in y - 1 .. y + 1 do
                if xx <> x || yy <> y then yield (xx, yy)
    }
