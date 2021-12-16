open System.IO;
open FParsec;

type Packet = Packet of Version : int * Content : PacketContents
and PacketContents =
    | Literal of Value : int64
    | Operator of Type : int * Subpackets : Packet list

let bitsToInt (bits : string) = System.Convert.ToInt32(bits, 2)

let pVersion = anyString 3 |>> bitsToInt

let  pType = anyString 3 |>> bitsToInt

let pLiteral =
    let rec pLiteralRec acc =
        anyChar .>>. (anyString 4 |>> bitsToInt) >>= (fun (c, v) ->
        let next = acc * 16L + int64 v
        if c = '0' then preturn next else pLiteralRec next)
    pLiteralRec 0L |>> Literal

let rec pSubpackets =
    let pFixedLen = parse {
        let! len = anyString 15 |>> bitsToInt
        let! subInput = anyString len
        match run (many pPacket) subInput with
        | Success(res, _, _) -> return res
        | Failure(err, _, _) -> return! fail err
    }
    let pFixedCount = parse {
        let! count = anyString 11 |>> bitsToInt
        return! parray count pPacket |>> List.ofArray
    }
    anyChar >>= (fun c -> if c = '0' then pFixedLen else pFixedCount)

and pContent = pType >>= (fun t ->
    if t = 4 then pLiteral else pSubpackets |>> (fun sp -> Operator(t, sp)))

and pPacket = pVersion .>>. pContent |>> Packet

let parsePacket input =
    let bitInput = input |> String.collect (fun c ->
        let i = System.Convert.ToInt32(c.ToString(), 16)
        System.Convert.ToString(i, 2).PadLeft(4, '0'))

    match run pPacket bitInput with
    | Success(res, _, _) -> res
    | Failure(err, _, _) -> failwith err

let packet = parsePacket (File.ReadAllText("input.txt").Trim())

let rec foldPacket fLiteral fOperator (Packet (ver, content)) =
    let recurse = foldPacket fLiteral fOperator
    match content with
    | Literal v -> fLiteral (ver, v)
    | Operator (t, subp) ->
        fOperator (ver, t, subp |> List.map (recurse))

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
        | 5 -> let [v1; v2] = args in if v1 > v2 then 1L else 0L
        | 6 -> let [v1; v2] = args in if v1 < v2 then 1L else 0L
        | 7 -> let [v1; v2] = args in if v1 = v2 then 1L else 0L
    foldPacket fLiteral fOperator

let part1 = sumPacketVersions packet
printfn "%d" part1

let part2 = evalPacket packet
printfn "%d" part2

