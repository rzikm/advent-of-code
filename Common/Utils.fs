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


let primeFactors n =
    seq {
        let mutable n = n
        let mutable p = 2L

        while n >= 2L do
            while n % p = 0L do
                yield p
                n <- n / p

            p <- p + 1L
    }
    |> Seq.countBy id
    |> Map.ofSeq

let lcm nums =
    let ipow n i =
        let rec f n i acc =
            if i = 0L then acc else f n (i - 1L) (acc * n)

        f n i 1L

    nums |> List.map primeFactors |> List.reduce (Map.unionWith max) |> Map.fold (fun s k v -> s * ipow k v) 1L


// computes (a * b) mod m even for large integers
// assumes 2 * m does not overflow
let multmod a b m =
    // use checked operators to catch overflows
    let (+) x y = Checked.op_Addition x y
    let (*) x y = Checked.op_Multiply x y

    let mutable res = 0L
    let mutable a = a % m
    let mutable b = b % m

    while b > 0L do
        if b % 2L > 0 then res <- (res + a) % m

        a <- (a * 2L) % m
        b <- b / 2L

    res

// computes a^b mod m using Horner's scheme in log b time
// assumes 2 * m does not overflow
let powmod a b m =
    // use checked operators to catch overflows
    let (+) x y = Checked.op_Addition x y
    let (*) x y = Checked.op_Multiply x y

    let mutable res = 1L
    let mutable mult = a
    let mutable exp = b

    while exp > 0L do
        if exp % 2L > 0L then res <- (res * mult) % m

        mult <- multmod mult mult m
        exp <- exp / 2L

    res

let negmod (a: int64) (m: int64) = m - ((a % m) + m) % m

let multinvmod a m =
    // from Fermat's small theorem
    // a^-1 = a^(m-2) mod m
    powmod a (m - 2L) m

// given an = a^n mod m; returns n
let discreteLog an a m =
    let ainv = multinvmod a m

    let mutable an = an
    let mutable e = 1L

    while an <> a do
        an <- (an * ainv) % m
        e <- e + 1L

    e

let parseInput parser input =
    match FParsec.CharParsers.run parser (String.concat "\n" input) with
    | Success (res, _, _) -> res
    | Failure (err, _, _) -> failwith err

let findMatching possibilities =
    let rec find keys matching =
        match keys with
        | [] -> Some matching
        | key :: keys ->
            Map.find key possibilities
            |> List.except (matching |> List.map snd)
            |> List.tryPick (fun pos -> find keys ((key, pos) :: matching))

    find (Map.keys possibilities |> List.ofSeq) []

let unionIntervals intervals =
    let rec unionIntervals' acc intervals =
        match intervals with
        | [] -> acc
        | [ i ] -> i :: acc

        | (s1, e1) :: (s2, e2) :: rest ->
            // due to sort we know that s1 <= s2
            if s1 = s2 then
                // sorting implies e1 <= e2 -> 2 is superset
                unionIntervals' acc ((s2, e2) :: rest)
            else if e1 < s2 then
                // disjoint
                unionIntervals' ((s1, e1) :: acc) ((s2, e2) :: rest)
            else if e2 <= e1 then
                // 2 is subset of 1
                unionIntervals' acc ((s1, e1) :: rest)
            else
                // partial overlap, union this range
                unionIntervals' acc ((s1, e2) :: rest)

    intervals |> List.sort |> unionIntervals' [] |> List.rev

let constf v _ = v

module Tests =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``lcm on primes`` () =
        let primes = List.map int64 [ 2; 7; 13 ]
        let expected = List.reduce (*) primes

        lcm primes |> should equal expected
