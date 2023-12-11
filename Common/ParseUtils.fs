module ParseUtils

open FParsec

let grid cell =
    let row = many1 cell |>> Array.ofList
    sepEndBy1 row (skipChar '\n') |>> Array.ofList
