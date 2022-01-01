module AdventOfCode

open System.IO
open System
open FParsec

type Difficulty =
    | Easy
    | Hard

type Solution = {
    parser : Parser<Object, unit>
    solve : Difficulty -> Object -> Object
}

let makeSolution
    (parser : Parser<'input, unit>)
    (solve : Difficulty -> 'input -> 'solution) =
    {
        parser = parser |>> fun res -> upcast res
        solve = fun diff input -> (solve diff (downcast input)) :> Object
    }

let readStreamToString (stream : Stream) =
    use sr = new StreamReader(stream)
    sr.ReadToEnd ()
