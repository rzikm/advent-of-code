﻿module Program

// computes a^b mod m using Horner's scheme in log b time
let powmod a b m =
    let mutable res = 1L
    let mutable mult = a
    let mutable exp = b

    while exp > 0L do
        if exp % 2L > 0L then
            res <- (res * mult) % m
        mult <- (mult * mult) % m
        exp <- exp / 2L
    res

// given an = a^n mod m; returns n
let discreteLog an a m =
    // from Fermat's small theorem
    // a^-1 = a^(m-2) mod m
    let ainv = powmod a (m - 2L) m

    let mutable an = an
    let mutable e = 1L
    while an <> a do
        an <- (an * ainv) % m
        e <- e + 1L
    e

let m = 20201227L

let getEncryptionKey cardPublic doorPublic =

    // cardPublic = 7^c mod 20201227
    // doorPublic = 7^d mod 20201227
    // key = 7^(cd) mod 20201227

    // find out factors c and d
    let c = discreteLog cardPublic 7L m
    let d = discreteLog doorPublic 7L m

    powmod 7L (c * d) m

module Tests =
    open FsUnit
    open Xunit

    [<Fact>]
    let ``Get card loop size`` () =
        discreteLog 5764801L 7L m |> should equal 8

    [<Fact>]
    let ``Get door loop size`` () =
        discreteLog 17807724L 7L m |> should equal 11

    [<Fact>]
    let ``Break example encryption`` () =
         getEncryptionKey 5764801L 17807724L |> should equal 14897079L

[<EntryPoint>]
let main _ =
    printfn "%A" (getEncryptionKey 8184785L 5293040L)
    0
