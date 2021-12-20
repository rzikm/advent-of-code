open FParsec;
open System.IO;

type Cell =
    | Unchecked of int
    | Checked of int

type Input = {
    Numbers : int list
    Bingos : Cell[][] list
}

let pNumbers = sepBy pint32 (pchar ',')

let pArraySep n p sep = p .>>. parray (n - 1) (sep >>. p) |>> fun (head, tail) -> Array.concat [|[|head|]; tail|]

let pLine = pArraySep 5 ((optional (pchar ' ')) >>. pint32 |>> Unchecked) (pchar ' ')
let pBingo = pArraySep 5 pLine (pchar '\n')
let pBingoList = many1 (pBingo .>> spaces)

let pInput = pNumbers .>> spaces .>>. pBingoList

type State = {
    Checked : int list
    Winner : Cell[][] option
    Players : Cell[][] list
}

let nums, initState =
    match run pInput (File.ReadAllText("input.txt")) with
    | Success((nums, bingos), _, _) ->
        nums, {
            Checked = []
            Winner = None
            Players = bingos
        }
    | Failure(err, _, _) -> failwith err

let checkCell n cell =
    match cell with
    | Unchecked x when x = n -> Checked n
    | x -> x

let checkNum n (bingo : Cell[][]) =
    bingo |>
    Array.map (Array.map (checkCell n))

let isWinner bingo =
    let isRowChecked row =
        match row with
        | [| Checked(_); Checked(_); Checked(_); Checked(_); Checked(_)|] -> true
        | _ -> false

    Array.exists isRowChecked bingo || bingo |> Array.transpose |> Array.exists isRowChecked

let findWinner (bingos : Cell[][] list) =
    let winners, notWinners = bingos |> List.partition isWinner
    match winners with
    | w::_ -> (Some w), notWinners
    | [] -> None, notWinners

let getUnchecked bingo =
    Array.collect (Array.choose (fun cell ->
        match cell with
        | Unchecked x -> Some x
        | _ -> None
    )) bingo


let part1 =
    let fold state num =
        match state.Winner with
        | Some w -> state
        | _ ->
            let newPlayers =
                state.Players
                |> List.map (checkNum num)

            let winner, _ = findWinner newPlayers

            {
                Checked = List.append [num] state.Checked
                Players = newPlayers
                Winner = winner
            }

    let finalState = List.fold fold initState nums
    (List.head finalState.Checked) * (finalState.Winner.Value |> getUnchecked |> Array.sum)

printfn "%A" part1

let part2 =
    let fold state num =
        match state.Players with
        | [] -> state
        | _ ->
            let winner, newPlayers = state.Players |> List.map (checkNum num) |> findWinner

            {
                Checked = List.append [num] state.Checked
                Winner = winner
                Players = newPlayers
            }

    let finalState = List.fold fold initState nums
    (List.head finalState.Checked) * (finalState.Winner.Value |> getUnchecked |> Array.sum)

printfn "%A" part2
