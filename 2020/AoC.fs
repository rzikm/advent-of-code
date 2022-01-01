module AdventOfCode

open System.IO
open System
open FParsec

type Solution = {
    parser : Parser<Object, unit>
    solve1 : Object -> Object
    solve2 : Object -> Object
}

let makeSolution
    (parser : Parser<'input, unit>)
    (solve1 : 'input -> 'solution)
    (solve2 : 'input -> 'solution) =
    {
        parser = parser |>> fun res -> upcast res
        solve1 = fun input -> (solve1 (downcast input)) :> Object
        solve2 = fun input -> (solve2 (downcast input)) :> Object
    }

let readStreamToString (stream : Stream) =
    use sr = new StreamReader(stream)
    sr.ReadToEnd ()
