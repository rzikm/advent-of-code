module AoC202025

open AdventOfCode
open FParsec

let parser = pint64 .>> spaces .>>. pint64

let m = 20201227L

let getEncryptionKey cardPublic doorPublic =

    // cardPublic = 7^c mod 20201227
    // doorPublic = 7^d mod 20201227
    // key = 7^(cd) mod 20201227

    // find out factors c and d
    let c = Math.discreteLog cardPublic 7L m
    let d = Math.discreteLog doorPublic 7L m

    Math.powmod 7L (c * d) m

let solution =
    makeSolution () parser (fun i -> i ||> getEncryptionKey) (fun _ -> "*")

module Tests =
    open FsUnit
    open Xunit

    [<Fact>]
    let ``Get card loop size`` () =
        Math.discreteLog 5764801L 7L m |> should equal 8L

    [<Fact>]
    let ``Get door loop size`` () =
        Math.discreteLog 17807724L 7L m |> should equal 11L

    [<Fact>]
    let ``Break example encryption`` () =
        getEncryptionKey 5764801L 17807724L |> should equal 14897079L
