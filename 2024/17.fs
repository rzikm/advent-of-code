module AoC202417

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let reg c =
        skipString $"Register {c}: " >>. pint64 .>> newline

    let program =
        skipString "Program: " >>. sepBy1 pint32 (skipString ",") |>> Array.ofList

    let registers = tuple3 (reg 'A') (reg 'B') (reg 'C')

    registers .>> newline .>>. program

let run (ip, registers) program =
    let comboOp (a, b, c) =
        function
        | x when x >= 0L && x <= 3L -> x
        | 4L -> a
        | 5L -> b
        | 6L -> c
        | x -> failwithf "Invalid combo %d" x

    seq {
        let mutable doBreak = false
        let mutable ip = ip
        let mutable registers = registers

        while not doBreak do
            match program |> Array.tryItem ip, program |> Array.tryItem (ip + 1) with
            | None, _
            | _, None -> doBreak <- true
            | Some opc, Some arg ->
                let nip, nr, out =
                    match opc with
                    | 0 -> ip + 2, registers |> (fun (a, b, c) -> (a >>> (int <| comboOp registers arg), b, c)), None
                    | 1 -> ip + 2, registers |> (fun (a, b, c) -> (a, b ^^^ arg, c)), None
                    | 2 ->
                        ip + 2,
                        registers |> (fun (a, _, c) -> (a, (int32 <| comboOp registers arg) % 8 |> int64, c)),
                        None
                    | 3 ->
                        let ip = if item1 registers = 0L then ip + 2 else arg
                        ip, registers, None
                    | 4 -> ip + 2, registers |> (fun (a, b, c) -> (a, b ^^^ c, c)), None
                    | 5 -> ip + 2, registers, Some(comboOp registers arg % 8L)
                    | 6 -> ip + 2, registers |> (fun (a, b, c) -> (a, a >>> (int <| comboOp registers arg), c)), None
                    | 7 -> ip + 2, registers |> (fun (a, b, c) -> (a, b, a >>> (int <| comboOp registers arg))), None
                    | _ -> failwithf "Invalid opcode %d at %d" opc ip

                match out with
                | None -> ()
                | Some out -> yield int out

                ip <- nip
                registers <- nr
    }

let solve1 (registers, program) =
    run (0, registers) program |> Seq.map string |> String.concat ","

let solve2 ((_, b, c), program) =
    //
    // Another task where we need to analyze the specific input to find a pattern :/
    //
    // Raw program
    //     2,4,1,4,7,5,4,1,1,4,5,5,0,3,3,0
    //
    // Raw instructions
    //     bst 4            b = a % 8
    //     bxl 4            b = b ^ 4
    //     cdv 5            c = a >>> b
    //     bxc (1)          b = b ^ c
    //     bxl 4            b = b ^ 4
    //     out 5            print b % 8
    //     adv 3            a = a >>> 3
    //     jnz 0            if a <> 0 then repeat
    //
    // Better representation
    //
    //     let eval (a, b, c) =
    //         let mutable a = a
    //         let mutable b = b
    //         let mutable c = c
    //
    //         seq {
    //             while a <> 0L do
    //                 b <- a % 8L
    //                 b <- b ^^^ (a >>> int (b ^^^ 4))
    //                 yield b % 8L
    //                 a <- a >>> 3
    //         }
    //
    // Notice that b and c are not used across iterations, so we can just treat them as intermediate values.
    // Also, a is consumed 3 bits at a time, so we can just treat it as a queue of bits.
    // The algorithm is going to guess the next 3 bits and check if it leads to the desired output.

    let rec reverseEval (a: int64) output =
        match output with
        | [] -> Some(a)
        | x :: xs ->
            [ 0..7 ]
            |> List.tryPick (fun bb ->
                let a = (a <<< 3) ||| bb
                let b = a % 8L
                let printed = int32 <| (b ^^^ (a >>> int (b ^^^ 4))) % 8L

                if a <> 0 && printed = x then reverseEval a xs else None)

    program |> List.ofArray |> List.rev |> reverseEval 0L |> Option.get

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "Register A: 729"
           "Register B: 0"
           "Register C: 0"
           ""
           "Program: 0,1,5,4,3,0" |]

    [<Fact>]
    let ``Example part 1`` () =
        testPart1 solution input |> should equal "4,6,3,5,6,3,5,2,1,0"
