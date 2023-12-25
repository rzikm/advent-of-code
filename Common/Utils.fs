module Utils

open FSharpPlus
open FParsec

let rec subsets size list =
    seq {
        if size = 0 then
            yield []
        else
            match list with
            | [] -> yield! Seq.empty
            | head :: tail ->
                for t in subsets (size - 1) tail do
                    yield head :: t

                yield! subsets size tail
    }

let allPairs seq =
    subsets 2 seq |> Seq.map (fun l -> l.[0], l.[1])

let rec allSubsets list =
    seq {
        match list with
        | [] -> yield []
        | a :: rest ->
            let others = allSubsets rest
            yield! others
            yield! others |> Seq.map (fun l -> a :: l)
    }

let rec permutations list =
    seq {
        match list with
        | [] -> yield []
        | a :: rest ->
            yield!
                permutations rest
                |> Seq.collect (fun l ->
                    { 0 .. List.length l }
                    |> Seq.map (fun i ->
                        let l, r = List.splitAt i l
                        List.concat [ l; [ a ]; r ]))
    }

let rec applyN (n: int) (f: 'a -> 'a) (v: 'a) =
    match n with
    | n when n < 0 -> failwith "Invalid application count"
    | 0 -> v
    | 1 -> f v
    | _ -> applyN (n - 1) f (f v)

let (^) (f: 'a -> 'a) (n: int) (v: 'a) = applyN n f v

let parseInt (baze: int) (str: string) = System.Convert.ToInt32(str, baze)

let memoizerec f =
    let cache = new System.Collections.Generic.Dictionary<_, _>()

    let rec newf value =
        match cache.TryGetValue value with
        | true, res -> res
        | false, _ ->
            let res = f newf value
            cache.Add(value, res)
            res

    newf

let memoize f =
    let cache = new System.Collections.Generic.Dictionary<_, _>()

    let getValue value =
        match cache.TryGetValue value with
        | true, res -> res
        | false, _ ->
            let res = f value
            cache.Add(value, res)
            res

    getValue

let parseInput parser input =
    match FParsec.CharParsers.run parser input with
    | Success (res, _, _) -> res
    | Failure (err, _, _) -> failwith err

// finds a pairing between key and one of the values in the associated list
let findMatching possibilities =
    let rec find keys matching =
        match keys with
        | [] -> Some matching
        | key :: keys ->
            Map.find key possibilities
            |> List.except (matching |> List.map snd)
            |> List.tryPick (fun pos -> find keys ((key, pos) :: matching))

    find (Map.keys possibilities |> List.ofSeq) []

let konst v = fun _ -> v

let logValue format value =
    Printf.printfn format value
    value

let loopUntilRepeatWithKey f fkey start =
    let rec loop statesToIndex s index =
        let key = fkey s

        match Map.tryFind key statesToIndex with
        | Some (ss, loopStart) -> (ss, s, loopStart, index - loopStart)
        | None -> loop (Map.add key (s, index) statesToIndex) (f s) (index + 1)

    loop Map.empty start 0

let loopUntilRepeat f start =
    let firstState, _, loopStart, period = loopUntilRepeatWithKey f id start
    (firstState, loopStart, period)

let applyNWithRepeatDetection n f start =
    let rec loop statesToIndex s index =
        if n = index then
            s
        else
            match Map.tryFind s statesToIndex with
            | Some loopStart ->
                let period = index - loopStart
                let leftover = (n - loopStart) % period
                applyN leftover f s
            | None -> loop (Map.add s index statesToIndex) (f s) (index + 1)

    loop Map.empty start 0
