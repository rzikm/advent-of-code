module AoC202014

open AdventOfCode
open FSharpPlus
open FParsec

type BitMask =
    | NoChange
    | Set
    | Unset

type Instr =
    | SetMask of BitMask array
    | SetMem of int64 * int64

let parser =
    let setMask =
        pstring "mask = "
        >>. parray
                36
                (choice [ charReturn 'X' NoChange
                          charReturn '1' Set
                          charReturn '0' Unset ])
        |>> SetMask

    let setMem = pstring "mem[" >>. pint64 .>> pstring "] = " .>>. pint64 |>> SetMem
    sepEndBy1 (setMask <|> setMem) spaces

let applyMask mask value =
    Array.foldBack
        (fun bit (acc, c) ->
            match bit with
            | NoChange -> (if c &&& value <> 0L then acc + c else acc), c * 2L
            | Set -> (acc + c), (c * 2L)
            | Unset -> acc, (c * 2L))
        mask
        (0L, 1L)
    |> fst

let eval1 (memory, mask) instr =
    match instr with
    | SetMask newMask -> (memory, newMask)
    | SetMem (addr, value) -> (Map.add addr (applyMask mask value) memory, mask)

let allAddresses bitmask addr =
    Array.foldBack
        (fun bit (addresses, c) ->
            match bit with
            | NoChange -> addresses |> List.collect (fun a -> [ a; a + c ]), c * 2L
            | Set -> addresses |> List.map (fun a -> a + c), (c * 2L)
            | Unset -> addresses |> List.map (fun a -> if addr &&& c <> 0L then a + c else a), (c * 2L))
        bitmask
        ([ 0L ], 1L)
    |> fst

let eval2 (memory, mask) instr =
    match instr with
    | SetMask newMask -> (memory, newMask)
    | SetMem (addr, value) -> (allAddresses mask addr |> List.fold (fun m a -> Map.add a value m) memory, mask)

let solve eval input =
    input |> List.fold eval (Map.empty, Array.init 36 (fun _ -> NoChange)) |> fst |> Map.values |> Seq.sum

let solution = makeSolution () parser (solve eval1) (solve eval2)

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``Applies mask correctly`` () =
        let mask = [| Set; NoChange; Unset |]
        let value = 0b011L
        let expected = 0b110L
        applyMask mask value |> should equal expected

    [<Fact>]
    let ``Floats addresses correctly`` () =
        let mask = [| Unset; NoChange; Unset; NoChange; NoChange |]
        let addr = 0b11010L

        let expected =
            Set.ofList [ 16L
                         17L
                         18L
                         19L
                         24L
                         25L
                         26L
                         27L ]

        allAddresses mask addr |> Set.ofList |> should equal expected

    let input1 =
        [| "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
           "mem[8] = 11"
           "mem[7] = 101"
           "mem[8] = 0" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input1 |> should equal 165L

    let input2 =
        [| "mask = 000000000000000000000000000000X1001X"
           "mem[42] = 100"
           "mask = 00000000000000000000000000000000X0XX"
           "mem[26] = 1" |]

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input2 |> should equal 208L
