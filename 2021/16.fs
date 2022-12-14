module AoC202116

#nowarn "40"

open AdventOfCode
open FSharpPlus
open FParsec

type Packet = Packet of Version: int * Content: PacketContents

and PacketContents =
    | Literal of Value: int64
    | Operator of Type: int * Subpackets: Packet list

let bitsToInt (bits: string) = System.Convert.ToInt32(bits, 2)

let parser =
    let pVersion = anyString 3 |>> bitsToInt

    let pType = anyString 3 |>> bitsToInt

    let pLiteral =
        let rec pLiteralRec acc =
            anyChar .>>. (anyString 4 |>> bitsToInt)
            >>= (fun (c, v) ->
                let next = acc * 16L + int64 v
                if c = '0' then preturn next else pLiteralRec next)

        pLiteralRec 0L |>> Literal

    let rec pSubpackets =
        let pFixedLen =
            parse {
                let! len = anyString 15 |>> bitsToInt
                let! subInput = anyString len

                match run (many pPacket) subInput with
                | Success (res, _, _) -> return res
                | Failure (err, _, _) -> return! fail err
            }

        let pFixedCount =
            parse {
                let! count = anyString 11 |>> bitsToInt
                return! parray count pPacket |>> List.ofArray
            }

        anyChar >>= (fun c -> if c = '0' then pFixedLen else pFixedCount)

    and pContent =
        pType
        >>= (fun t ->
            if t = 4 then
                pLiteral
            else
                pSubpackets |>> (fun sp -> Operator(t, sp)))

    and pPacket = pVersion .>>. pContent |>> Packet

    many1Chars (satisfy (fun c -> isAsciiLetter c || isDigit c))
    >>= (fun input ->
        let bitInput =
            input
            |> String.collect (fun c ->
                let i = System.Convert.ToInt32(c.ToString(), 16)
                System.Convert.ToString(i, 2).PadLeft(4, '0'))

        // Hack to restart parsing on binarized input
        match run pPacket bitInput with
        | Success (res, _, _) -> preturn res
        | Failure (err, _, _) -> failwith err)

let rec foldPacket fLiteral fOperator (Packet (ver, content)) =
    let recurse = foldPacket fLiteral fOperator

    match content with
    | Literal v -> fLiteral (ver, v)
    | Operator (t, subp) -> fOperator (ver, t, subp |> List.map (recurse))

let sumPacketVersions =
    let fLiteral (version, _) = version
    let fOperator (version, _, subSums) = version + List.sum subSums
    foldPacket fLiteral fOperator

let evalPacket =
    let fLiteral (_, value) = value

    let fOperator (_, t, args) =
        match t with
        | 0 -> List.sum args
        | 1 -> List.reduce (*) args
        | 2 -> List.min args
        | 3 -> List.max args
        | 5
        | 6
        | 7 ->
            match args with
            | [ v1; v2 ] ->
                match t with
                | 5 -> if v1 > v2 then 1L else 0L
                | 6 -> if v1 < v2 then 1L else 0L
                | _ (* 7 *)  -> if v1 = v2 then 1L else 0L
            | _ -> failwith "Expected exactly two arguments"
        | _ -> failwith "Invalid type"

    foldPacket fLiteral fOperator

let solve1 packet = sumPacketVersions packet

let solve2 packet = evalPacket packet

let solution = makeSolution parser solve1 solve2
