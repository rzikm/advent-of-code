module Program

open FParsec;
open System.IO;

let parse input = ()

let part1 input = ()

let part2 input = ()

module Tests =
    open Xunit
    open FsUnit

    [<Fact>]
    let ``SomeTest`` () = ()

    let exampleInput () = ()

    [<Fact>]
    let ``Example Part 1`` () =
        exampleInput () |> part1 // |> should equal

    [<Fact>]
    let ``Example Part 2`` () =
        exampleInput () |> part2 // |> should equal

[<EntryPoint>]
let main _ =
    let input = ()

    printfn "%A" (part1 input)
    // printfn "%A" (part2 input)
    0
