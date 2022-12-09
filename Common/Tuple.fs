namespace Utils

open FSharpPlus

module Tuple2 =
    let map f (i1, i2) = (f i1, f i2)

    let inline (<!>) f t = map f t

    let apply (f1, f2) (i1, i2) = (f1 i1, f2 i2)

    let inline (<*>) f t = apply f t

    let inline add l r = (+) <!> l <*> r
    let inline sub l r = (-) <!> l <*> r
    let inline mul l r = (*) <!> l <*> r
