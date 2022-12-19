module Tuple3

open FSharpPlus

let map f (i1, i2, i3) = (f i1, f i2, f i3)

let inline (<!>) f t = map f t

let apply (f1, f2, f3) (i1, i2, i3) = (f1 i1, f2 i2, f3 i3)

let inline (<*>) f t = apply f t

let inline add l r = (+) <!> l <*> r
let inline sub l r = (-) <!> l <*> r
let inline mul l r = (*) <!> l <*> r

let allTrue (a, b, c) = a && b && c

let inline lt t1 t2 = (<) <!> t1 <*> t2 |> allTrue
let inline le t1 t2 = (<=) <!> t1 <*> t2 |> allTrue
let inline gt t1 t2 = (>) <!> t1 <*> t2 |> allTrue
let inline ge t1 t2 = (>=) <!> t1 <*> t2 |> allTrue

let inline manhattanLen (x, y, z) = abs x + abs y + abs z
let inline manhattanDist t1 t2 = sub t1 t2 |> manhattanLen

let inline neighbors6 (x, y, z) =
    seq {
        yield (x + 1, y, z)
        yield (x - 1, y, z)
        yield (x, y + 1, z)
        yield (x, y - 1, z)
        yield (x, y, z + 1)
        yield (x, y, z - 1)
    }

let inline neighbors26 (x, y, z) =
    seq {
        for zz in z - 1 .. z + 1 do
            for xx in x - 1 .. x + 1 do
                for yy in y - 1 .. y + 1 do
                    if xx <> x || yy <> y || zz <> z then yield (xx, yy, zz)
    }
