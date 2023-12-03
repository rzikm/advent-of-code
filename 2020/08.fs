module AoC202008

open AdventOfCode
open FSharpPlus
open FParsec

type Instruction =
    | Acc of int
    | Jmp of int
    | Nop of int

let parser =
    let acc = pstring "acc " >>. pint32 .>> restOfLine true |>> Acc

    let jmp = pstring "jmp " >>. pint32 .>> restOfLine true |>> Jmp

    let nop = pstring "nop " >>. pint32 .>> restOfLine true |>> Nop

    many1 <| choice [ acc; jmp; nop ] |>> Array.ofSeq

let run program =
    let cache = Array.zeroCreate<bool> <| Array.length program

    let rec runUntilLoop acc pc =
        match Array.tryItem pc program with
        | None -> (acc, pc)
        | Some instr ->
            if Array.item pc cache then
                (acc, pc)
            else
                Array.set cache pc true

                match instr with
                | Acc x -> runUntilLoop (acc + x) (pc + 1)
                | Jmp x -> runUntilLoop acc (pc + x)
                | Nop _ -> runUntilLoop acc (pc + 1)

    runUntilLoop 0 0

let easy input = run input |> fst

let hard input =
    let flip i =
        function
        | Nop x -> Array.updateAt i (Jmp x) input |> Some
        | Jmp x -> Array.updateAt i (Nop x) input |> Some
        | _ -> None

    input |> choosei flip |> Seq.map run |> find (snd >> (=) (Array.length input)) |> fst

let solve input = 0

let solution = makeSolution () parser easy hard
