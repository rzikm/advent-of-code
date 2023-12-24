module AoC202324

open AdventOfCode
open FSharpPlus
open FParsec

let parser =
    let spc = skipChar ',' >>. spaces
    let number = pfloat
    let pvec = tuple3 (number .>> spc) (number .>> spc) number
    ParseUtils.lines (pvec .>> skipString " @" .>> spaces .>>. pvec)

let inline lineIntersects2d ((x0, y0), (dx0, dy0)) ((x1, y1), (dx1, dy1)) =
    // see https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line_segment
    // rename using names from the above link

    let x1, x2, x3, x4 = x0, x0 + dx0, x1, x1 + dx1
    let y1, y2, y3, y4 = y0, y0 + dy0, y1, y1 + dy1

    let denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

    if denominator = LanguagePrimitives.GenericZero then
        None
    else
        let t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denominator
        let u = ((x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)) / denominator

        let pos = Tuple2.add (x1, y1) (Tuple2.scale t (dx0, dy0))

        (pos, t, u) |> Some

let countIntersections filter hails =
    hails |> Utils.allPairs |> Seq.count (uncurry filter)

let collisionFilter minXY maxXY hail1 hail2 =
    let minXY = Tuple2.broadcast minXY
    let maxXY = Tuple2.broadcast maxXY

    let ((x1, y1, _), (dx1, dy1, _)), ((x2, y2, _), (dx2, dy2, _)) = hail1, hail2

    match lineIntersects2d ((x1, y1), (dx1, dy1)) ((x2, y2), (dx2, dy2)) with
    | Some ((x, y), t, u) when t >= 0.0 && u >= 0.0 && Tuple2.le minXY (x, y) && Tuple2.le (x, y) maxXY -> true
    | _ -> false

let solve1 input =
    countIntersections (collisionFilter 200000000000000.0 400000000000000.0) input

let solve2 input =
    // sample 2 trajectories
    let input = input |> List.take 3

    let ((x1, y1, z1), (dx1, dy1, dz1)), ((x2, y2, z2), (dx2, dy2, dz2)), ((x3, y3, z3), (dx3, dy3, dz3)) =
        input.[0], input.[1], input.[2]

    // equation system is
    //
    // x0 + t1 * dx0 = x1 + t1 * dx1
    // y0 + t1 * dy0 = y1 + t1 * dy1
    // z0 + t1 * dz0 = z1 + t1 * dz1
    //
    // x0 + t2 * dx0 = x2 + t2 * dx2
    // y0 + t2 * dy0 = y2 + t2 * dy2
    // z0 + t2 * dz0 = z2 + t2 * dz2
    //
    // x0 + t3 * dx0 = x3 + t3 * dx3
    // y0 + t3 * dy0 = y3 + t3 * dy3
    // z0 + t3 * dz0 = z3 + t3 * dz3
    //
    // we need to solve for x0, y0, z0, (and dx0, dy0, dz0, t1, t2, t3)
    //
    // Since the equations are non-linear, we will use Newton's method to solve them. First,
    // We rearrange the equations
    // and let x = [| x0, y0, z0, dx0, dy0, dz0, t1, t2, t3 |]
    let f (x: 'a array) =
        [| x.[0] + x.[6] * x.[3] - x1 - x.[6] * dx1
           x.[1] + x.[6] * x.[4] - y1 - x.[6] * dy1
           x.[2] + x.[6] * x.[5] - z1 - x.[6] * dz1

           x.[0] + x.[7] * x.[3] - x2 - x.[7] * dx2
           x.[1] + x.[7] * x.[4] - y2 - x.[7] * dy2
           x.[2] + x.[7] * x.[5] - z2 - x.[7] * dz2

           x.[0] + x.[8] * x.[3] - x3 - x.[8] * dx3
           x.[1] + x.[8] * x.[4] - y3 - x.[8] * dy3
           x.[2] + x.[8] * x.[5] - z3 - x.[8] * dz3 |]

    let setJacobian (arr: float array array) (x: float array) =
        let set row iw0 it dwn =
            let row = arr.[row]

            for col = 0 to Array.length x - 1 do
                row.[col] <- 0.0

            row.[iw0] <- 1.0 // df/w_0 = 1
            row.[iw0 + 3] <- x.[it] // df/dw_0 = t_n
            row.[it] <- x.[iw0 + 3] - dwn // df/t_n = w_0 - d_n

        set 0 0 6 dx1
        set 1 1 6 dy1
        set 2 2 6 dz1

        set 3 0 7 dx2
        set 4 1 7 dy2
        set 5 2 7 dz2

        set 6 0 8 dx3
        set 7 1 8 dy3
        set 8 2 8 dz3

        arr

    // The iteration algorithm proceeds as follows
    // solve
    //
    //   J(x_n) * -dx_n = f(x_n)
    //
    // for dx_n. And then set x_{n+1} = x_n + dx_n and repeat while dx_n is not small enough.

    let j = Array.init 9 (fun _ -> Array.zeroCreate 9)
    let x0 = [| 23.0; 12.0; 9.0; -1.0; 2.0; 4.0; 2.0; 4.0; 8.0 |]

    let rec iterate x =
        let fx = f x

        if Array.sumBy (fun x -> x * x) fx <> 0 then
            MatrixMath.findRoot (setJacobian j x) fx

            // make sure to round up to enforce integer solution
            let x' = Array.map2 (-) x fx |> Array.map round
            iterate x'
        else
            x

    iterate x0 |> Array.take 3 |> Array.sumBy (round >> int64)

let solution = makeSolution () parser solve1 solve2

module Tests =
    open Xunit
    open FsUnit.Xunit

    let input =
        [| "19, 13, 30 @ -2,  1, -2"
           "18, 19, 22 @ -1, -1, -2"
           "20, 25, 34 @ -2, -2, -4"
           "12, 31, 28 @ -1, -2, -1"
           "20, 19, 15 @  1, -5, -3"
           "" |]

    [<Fact>]
    let ``Example part 1`` () =
        parseTestInput parser input |> countIntersections (collisionFilter 7.0 27.0) |> should equal 2

    [<Fact>]
    let ``Example part 2`` () =
        testPart2 solution input |> should equal 47L
