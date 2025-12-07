module ParseUtils

open FParsec

let lines line =
    sepEndBy1 (attempt line) (skipChar '\n')


let grid cell =
    let row = many1 cell |>> Array.ofList
    lines row |>> Array.ofList

let gridOf (cellList: (char * 'Cell) list) =
    choice (cellList |> List.map (fun (c, cell) -> charReturn c cell)) |> grid

let notWhitespace<'a> : Parser<char, 'a> =
    satisfy (fun c -> not <| System.Char.IsWhiteSpace c)

let skipSpaces: Parser<unit, unit> = skipMany (pchar ' ')

let sepBySpaces p = sepEndBy p skipSpaces
