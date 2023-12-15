module ParseUtils

open FParsec

let lines line = sepEndBy1 line (skipChar '\n')

let grid cell =
    let row = many1 cell |>> Array.ofList
    lines row |>> Array.ofList

let notWhitespace<'a> : Parser<char, 'a> =
    satisfy (fun c -> not <| System.Char.IsWhiteSpace c)
