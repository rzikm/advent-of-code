module AoC202424

open AdventOfCode
open FSharpPlus
open FParsec
open Printf

type Gate =
    | And
    | Or
    | Xor

let parser =
    let name = many1Satisfy (fun c -> isDigit c || isAsciiLetter c)

    let input =
        name .>> skipString ": " .>>. (charReturn '1' true <|> charReturn '0' false)

    let gateType =
        [ stringReturn "AND" And; stringReturn "OR" Or; stringReturn "XOR" Xor ] |> choice

    let gate =
        name .>> skipChar ' ' .>>. gateType .>> skipChar ' ' .>>. name .>> skipString " -> " .>>. name
        |>> (fun (((l, t), r), s) -> (s, (t, (l, r))))

    (ParseUtils.lines input |>> Map.ofList) .>> skipNewline .>>. (ParseUtils.lines gate |>> Map.ofList)

let makeEval initial gates =
    Utils.memoizerec
    <| fun frec t ->
        match Map.tryFind t initial with
        | Some v -> v
        | None ->
            let (t, (l, r)) = Map.find t gates
            let l = frec l
            let r = frec r

            match t with
            | And -> l && r
            | Or -> l || r
            | Xor -> l <> r

let eval initial gates =
    let eval = makeEval initial gates
    Map.keys gates |> Seq.filter (String.startsWith "z") |> Seq.sort |> Seq.map eval

let solve1 (initial, gates) =
    eval initial gates |> Seq.rev |> Seq.fold (fun acc v -> acc * 2L + (if v then 1L else 0L)) 0L

let getInitials size (x, y) =
    [ 0..size ]
    |> List.collect (fun i ->
        [ (sprintf "x%02d" i, x >>> i &&& 1L = 1L)
          (sprintf "y%02d" i, y >>> i &&& 1L = 1L) ])
    |> Map.ofList

let swapGates (a, b) gates =
    let ag = Map.find a gates
    let bg = Map.find b gates
    gates |> Map.add a bg |> Map.add b ag

let solve2 (initial, gates) =
    let size = (initial |> Map.count) / 2

    let getDependencies t =
        let rec eval t =
            match Map.tryFind t gates with
            | None -> []
            | Some (_, (l, r)) -> l :: r :: eval l @ eval r

        eval t |> Seq.distinct |> Seq.sort |> Seq.toList

    let checkUpToI gates i =
        //
        // we assume all gates affecting only bis up to (i-1) are correct, we set those inputs
        // to 0, we also set all the inputs which affect only bits above are set to 1
        // rest is somewhat random to dig out any wiring errors
        //
        let args =
            [ ~~~ 0UL <<< i
              ~~~ 0UL <<< (i + 1)
              ~~~ 0UL <<< (i - 1)
              1UL <<< i
              (1UL <<< i) / 2UL ]
            |> List.map (fun i -> i &&& (((1UL <<< size)) - 1UL))
            |> List.map int64

        let pairs = Seq.allPairs args args

        pairs
        |> Seq.forall (fun (x, y) ->
            let expected = x + y

            let initials = getInitials size (x, y)
            let doEval = makeEval initials gates

            [ 0..i ]
            |> List.forall (fun ii ->
                // ugly try-catch to avoid exceptions from cyclic dependencies
                try
                    let res = doEval (sprintf "z%02d" ii)
                    let ex = ((expected >>> ii &&& 1L) = 1L)
                    res = ex
                with
                | _ -> false))

    let rec run acc gates i =
        if i > size then
            Some(acc)
        else if checkUpToI gates i then
            run acc gates (i + 1)
        else if List.length acc < 4 then
            //
            // there is a wiring error affecting bit i. check all options to swap a gate affecting bit i with a gate not
            // yet known to be correct (all gates affecting only bits up to i-1 are already considered correct)
            //
            let goodDependencies = getDependencies (sprintf "z%02d" (i - 1))

            let badDependencies =
                let badGate = sprintf "z%02d" i
                badGate :: getDependencies badGate |> List.except goodDependencies |> List.except (initial |> Map.keys)

            let allGates = Map.keys gates |> Seq.except goodDependencies

            let swapPosibilities =
                badDependencies
                |> Seq.collect (fun g ->
                    let gdeps = getDependencies g

                    allGates |> Seq.except goodDependencies |> Seq.except gdeps |> Seq.map (fun g2 -> (g, g2)))

            swapPosibilities
            |> Seq.tryPick (fun (a, b) ->
                let gates = swapGates (a, b) gates

                if checkUpToI gates i then
                    run ((a, b) :: acc) gates (i + 1)
                else
                    None)
        else
            None

    run [] gates 0 |> Option.get |> List.collect (fun (a, b) -> [ a; b ]) |> List.sort |> String.concat ","

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input1 =
        [| "x00: 1"
           "x01: 1"
           "x02: 1"
           "y00: 0"
           "y01: 1"
           "y02: 0"
           ""
           "x00 AND y00 -> z00"
           "x01 XOR y01 -> z01"
           "x02 OR y02 -> z02" |]

    let input2 =
        [| "x00: 1"
           "x01: 0"
           "x02: 1"
           "x03: 1"
           "x04: 0"
           "y00: 1"
           "y01: 1"
           "y02: 1"
           "y03: 1"
           "y04: 1"
           ""
           "ntg XOR fgs -> mjb"
           "y02 OR x01 -> tnw"
           "kwq OR kpj -> z05"
           "x00 OR x03 -> fst"
           "tgd XOR rvg -> z01"
           "vdt OR tnw -> bfw"
           "bfw AND frj -> z10"
           "ffh OR nrd -> bqk"
           "y00 AND y03 -> djm"
           "y03 OR y00 -> psh"
           "bqk OR frj -> z08"
           "tnw OR fst -> frj"
           "gnj AND tgd -> z11"
           "bfw XOR mjb -> z00"
           "x03 OR x00 -> vdt"
           "gnj AND wpb -> z02"
           "x04 AND y00 -> kjc"
           "djm OR pbm -> qhw"
           "nrd AND vdt -> hwm"
           "kjc AND fst -> rvg"
           "y04 OR y02 -> fgs"
           "y01 AND x02 -> pbm"
           "ntg OR kjc -> kwq"
           "psh XOR fgs -> tgd"
           "qhw XOR tgd -> z09"
           "pbm OR djm -> kpj"
           "x03 XOR y03 -> ffh"
           "x00 XOR y04 -> ntg"
           "bfw OR bqk -> z06"
           "nrd XOR fgs -> wpb"
           "frj XOR qhw -> z04"
           "bqk OR frj -> z07"
           "y03 OR x01 -> nrd"
           "hwm AND bqk -> z03"
           "tgd XOR rvg -> z12"
           "tnw OR pbm -> gnj" |]

    [<Fact>]
    let ``Example part 1 - 1`` () =
        testPart1 solution input1 |> should equal 4L

    [<Fact>]
    let ``Example part 1 - 2`` () =
        testPart1 solution input2 |> should equal 2024L
