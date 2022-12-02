module AoC202124

open AdventOfCode
open FSharpPlus
open FParsec

(*
This puzzle is a bit different, as it expects us to analyze the input, so here it is:
The input (the program) is composed of 14 blocks like:

    inp w
    mul x 0
    add x z
    mod x 26
    div z (1 or 26)
    add x v0 // this varies
    eql x w
    eql x 0
    mul y 0
    add y 25
    mul y x
    add y 1
    mul z y
    mul y 0
    add y w
    add y v1 // this varies
    mul y x
    add z y

which can be roughly decoded as:

    let eval (up, v0, v1) input z = 
        let mutable z = z
        let x = z % 26
    
        if not up then 
            z <- z / 26
    
        if x + v0 <> input then
            z <- z * 26 + input + v1
        z

Where only the z value is propagated to the next code block

The multiplication/division/modulo of z by 26 can be interpreted as pushing/poping values from a
stack. Further glance at the input reveals that in 7 cases, the v0 value is necessarily larger than
10 and z is not divided by 26, which implies that the value (input + v1) will be pushed to the
stack. In other cases, when z is divided by 26 (i.e. an item is removed from the stack), we need to
avoid taking the last branch to avoid pushing more into the stack.

For example, in 4th and 5th block, the (up, v0, v1) values are (true, 15, 14) and (false, -8, 1).
Regardless of the input_3 (the 4th digit), the code pushes (input_3 + 14) to the stack. During the
5th block, the value is popped out and the branch is not taken only if

    input_3 + 14 + (-8) <> input_4

From there we can deduce that

    input_4 = input_3 + 6

Other constraints on the input digit values can be found accordingly.
*)

let parser =
    let pNewline = pchar '\n'
    // redefine pstring such that it supports newlines in the string
    let pstring (str: string) =
        str.Split([| '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun s -> pstring s)
        |> Seq.reduce (fun l r -> l .>> pNewline >>. r)

    let pSequence =
        pstring
            "inp w
mul x 0
add x z
mod x 26
div z "
        >>. ((pstring "1" >>. preturn true) <|> (pstring "26" >>. preturn false))
        .>> pNewline
        .>> pstring
                "
add x "
        .>>. pint32
        .>> pNewline
        .>> pstring
                "
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y "
        .>>. pint32
        .>> pNewline
        .>> pstring
                "
mul y x
add z y"
        |>> (fun ((upDown, v1), v2) -> (upDown, v1, v2))

    parray 14 (pSequence .>> pNewline) |>> List.ofArray

let solve minmax prg =
    let rec f prg stack num index =
        match prg with
        | [] -> num
        | (true, _, n) :: prg -> f prg ((index, n) :: stack) num (index + 1)
        | (false, n, _) :: prg ->
            let (i, n0) = List.head stack
            let diff = n0 + n

            if diff > 0 then
                Array.set num index (minmax 9 (1 + diff))
                Array.set num i (minmax (9 - diff) 1)
            else
                Array.set num index (minmax (9 + diff) 1)
                Array.set num i (minmax 9 (1 - diff))

            f prg (List.tail stack) num (index + 1)

    f prg [] (Array.zeroCreate 14) 0 |> Array.map int64 |> Array.reduce (fun l r -> l * 10L + r)

let solution = makeSolution parser (solve max) (solve min)

// code in the following module was used for interactive investigation of the provided input and is
// unnecessary for the solution
module Unused =
    type Register =
        | W
        | X
        | Y
        | Z

    type RegisterOrNumber =
        | Register of Register
        | Number of int

    type Operation =
        | Inp
        | Add of RegisterOrNumber
        | Mul of RegisterOrNumber
        | Div of RegisterOrNumber
        | Mod of RegisterOrNumber
        | Eql of RegisterOrNumber

    type Instruction = Register * Operation

    let parse input =
        let pNewline = pchar '\n'

        let pReg =
            [ ('w', W); ('x', X); ('y', Y); ('z', Z) ] |> List.map (fun (c, r) -> pchar c >>. preturn r) |> choice

        let pRegOrNum = (pReg |>> Register) <|> (pint32 |>> Number)

        let pInp = pstring "inp" >>. spaces >>. pReg |>> (fun r -> Instruction(r, Inp))

        let pBinOp =
            [ ("add", Add); ("mul", Mul); ("div", Div); ("mod", Mod); ("eql", Eql) ]
            |> List.map (fun (str, factory) ->
                pstring str >>. spaces >>. pReg .>> spaces .>>. pRegOrNum |>> (fun (t, r) -> Instruction(t, factory r)))
            |> choice

        let pSequence = pInp .>> pNewline .>>. many (pBinOp .>> pNewline) |>> (List.Cons)
        let pProgram = parray 14 pSequence

        match run pProgram input with
        | Success(res, _, _) -> res
        | Failure(err, _, _) -> failwith err

    let update reg f (w, x, y, z) =
        match reg with
        | W -> (f w, x, y, z)
        | X -> (w, f x, y, z)
        | Y -> (w, x, f y, z)
        | Z -> (w, x, y, f z)

    let applyInstr input instr state op =
        let getOp regOrNum (w, x, y, z) =
            match regOrNum with
            | Number num -> num
            | Register reg ->
                match reg with
                | W -> w
                | X -> x
                | Y -> y
                | Z -> z

        match instr with
        | Inp -> input
        | Add arg -> op + getOp arg state
        | Mul arg -> op * getOp arg state
        | Div arg -> op / getOp arg state
        | Mod arg -> op % getOp arg state
        | Eql arg -> if op = getOp arg state then 1 else 0

    let processInstruction input state (target, instr) =
        update target (applyInstr input instr state) state

    let applyProc program state input =
        List.fold (processInstruction input) state program
